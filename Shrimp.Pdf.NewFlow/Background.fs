// Learn more about F# at http://fsharp.org
namespace Shrimp.Pdf
open Shrimp.FSharp.Plus
open Shrimp.Pdf
open Shrimp.Pdf.PdfNames
open System.IO
#nowarn "0104"
open Shrimp.Pdf.Constants
open Shrimp.Pdf.Extensions
open Akkling
open Fake.IO

#nowarn "0104"
open Shrimp.Pdf.DSL
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Xobject
open Shrimp.Pdf.Colors
open iText.IO.Image
open System.Collections.Concurrent
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Geom
open Shrimp.Pdf.Parser
#nowarn "0104"

[<AutoOpen>]
module _SlimFlow_Common_BackgroundOrForeground =

    [<RequireQualifiedAccess>]
    type BackgoundAddtionFlow =
        | Keep 
        | ExtractObjects 
        | CustomCase of name: string * Flow<unit, unit>
    with 
        static member Custom(name, flow: Flow<_, _>) =
            let flow = flow ||>> ignore
            BackgoundAddtionFlow.CustomCase(name, flow)

        member x.Name =
            match x with 
            | Keep -> UNTITLED
            | ExtractObjects -> "ExtractObjects"
            | CustomCase (name, _) -> name


    
    [<RequireQualifiedAccess>]
    type BackgroundImageFileValue = 
        | Singleton of FsFullPath
        | Multiple  of FsFullPath list

    type BackgroundImageFile =
        { Value: BackgroundImageFileValue
          Pdf_AddtionalFlowAndName: BackgoundAddtionFlow
        }
    with 
        static member Singleton(path, ?addtionalFlow) =
            { Value = BackgroundImageFileValue.Singleton path 
              Pdf_AddtionalFlowAndName = defaultArg addtionalFlow BackgoundAddtionFlow.Keep
            }

        static member Multiple(paths, ?addtionalFlow) =
            { Value = BackgroundImageFileValue.Multiple paths 
              Pdf_AddtionalFlowAndName = defaultArg addtionalFlow BackgoundAddtionFlow.Keep
            }

    let private reuse name parameters f = Reuse(f = f, flowName = FlowName.Override(name, parameters))



    type PdfCanvas with 
        member private x.AddImageFittedIntoRectangleDSL(image, pageSize, asInline) =
            x.AddImageFittedIntoRectangle(image, pageSize, asInline)
            |> ignore
            x

        member x.AddImageFittedIntoRectangleDSLEx(image, pageSize, asInline, extGSState: FsExtGState option) =
            match extGSState with 
            | None -> x.AddImageFittedIntoRectangleDSL(image, pageSize, asInline)
            | Some extGSState -> 
                x
                    .SaveState()
                    .SetExtGState(extGSState)
                    .AddImageFittedIntoRectangleDSL(image, pageSize, asInline)
                    .RestoreState()
                    

        member private x.AddPdfXObjectDSLEx(backgroundPageBox: Rectangle, backgroundXObject: PdfXObject, readerPageBox: Rectangle, xEffect, yEffect, positionTweak: BackgroundPositionTweak, extGSState: FsExtGState option) =
            let readerPageBox =
                match positionTweak with 
                | BackgroundPositionTweak.SpecficRect rect -> rect.AsRectangle
                | BackgroundPositionTweak.OfPageBoxCase (_) -> readerPageBox

            let bk_x =  
                let baseX =  readerPageBox.GetX() - backgroundPageBox.GetX() 
                let lengthDiff = readerPageBox.GetWidth() - backgroundPageBox.GetWidth()
                match xEffect with 
                | XEffort.Left -> baseX
                | XEffort.Middle ->
                    baseX + lengthDiff / 2.f
                | XEffort.Right -> baseX + lengthDiff
                |> float

            let bk_x =
                match positionTweak with 
                | BackgroundPositionTweak.SpecficRect _ -> bk_x
                | BackgroundPositionTweak.OfPageBoxCase (xOffset, yOffset, _) -> bk_x + xOffset

            let bk_y = 
                let baseY = readerPageBox.GetY() - backgroundPageBox.GetY() 
                let lengthDiff = readerPageBox.GetHeight() - backgroundPageBox.GetHeight()
                match yEffect with 
                | YEffort.Bottom -> baseY
                | YEffort.Middle ->
                    baseY + lengthDiff / 2.f
                | YEffort.Top -> baseY + lengthDiff
                |> float
                
            
            let bk_y =
                match positionTweak with 
                | BackgroundPositionTweak.SpecficRect _ -> bk_y
                | BackgroundPositionTweak.OfPageBoxCase (xOffset, yOffset, _) -> bk_y + yOffset

            let affineTransform: AffineTransformRecord =
                let affineTransform =
                    { AffineTransformRecord.DefaultValue
                        with 
                            TranslateX = bk_x
                            TranslateY = bk_y
                    }
                    
                match backgroundXObject with 
                | :? PdfImageXObject ->
                    { affineTransform with 
                        ScaleX = 
                            backgroundPageBox.GetWidthF()

                        ScaleY = 
                            backgroundPageBox.GetHeightF()
                    }

                | _ -> affineTransform

            let affineTransform =
                match positionTweak with 
                | BackgroundPositionTweak.SpecficRect _ -> affineTransform
                | BackgroundPositionTweak.OfPageBoxCase (_, _, scale) -> 
                    match scale with
                    | None -> affineTransform
                    | Some scale ->
                        let scale_transform = 
                            AffineTransform.GetScaleInstance(scale)
                            |> AffineTransformRecord.ofAffineTransform

                        affineTransform.Concatenate(scale_transform)

            match extGSState with 
            | None -> 
                x
                    .AddXObject(backgroundXObject, affineTransform)

            | Some extGSState -> 
                x
                    .SaveState()
                    .SetExtGState(extGSState)
                    .AddXObject(backgroundXObject, affineTransform)
                    .RestoreState()
               
                   

    
    [<RequireQualifiedAccess>]
    type RenewableBackgroundImageFile =
        | BackgroundImageFile of BackgroundImageFile * createBackground: (PdfFile * BackgoundAddtionFlow -> BackgroundFile)
        | SlimBackground of WriteInfosFunc * ImagableSlimBackground

    type PdfPage with 
        member writerPage.AddBackgroundOrForegroundImage 
            (i: int,
             imageCache: System.Collections.Concurrent.ConcurrentDictionary<_, _>,
             image: RenewableBackgroundImageFile,
             choice: BackgroundOrForeground,
             ?addSelf,
             ?shadowColor,
             ?xEffect,
             ?yEffect,
             ?backgroundPositionTweak: PageNumber -> BackgroundPositionTweak,
             ?layerName: BackgroundAddingLayerOptions,
             ?extGSState: FsExtGState
            ) =
            let pageBox = 
                writerPage.GetActualBox()

            let pageSize = 
                pageBox
                |> PageSize

            let writerDoc = writerPage.GetDocument() :?> PdfDocumentWithCachedResources
            let xEffect = defaultArg xEffect XEffort.Middle
            let yEffect = defaultArg yEffect YEffort.Middle
            let addShadowColor pageSize (pdfCanvas: PdfCanvas) =
                match shadowColor with 
                | None -> pdfCanvas
                | Some (pdfFileShadow: NullablePdfCanvasColor) ->
                    pdfCanvas
                        .SaveState()
                        .SetFillColor(pdfFileShadow)
                        .Rectangle(pageSize)
                        .Fill()
                        .RestoreState()

            let pageNumber = i + 1

            let typedPageNumber = PageNumber pageNumber
            let pdfCanvas = 
                match choice with 
                | BackgroundOrForeground.Background ->
                    new OffsetablePdfCanvas(writerPage.NewContentStreamBefore(), writerPage.GetResources(), writerDoc)

                | BackgroundOrForeground.Foreground ->
                    new OffsetablePdfCanvas(writerPage.NewContentStreamAfter(), writerPage.GetResources(), writerDoc)
                    
            let shpLayerName =
                match image with 
                | RenewableBackgroundImageFile.BackgroundImageFile (image, createBackground) ->
                    layerName
                    |> Option.map(fun m -> m.BackgroundLayer.Name)

                | RenewableBackgroundImageFile.SlimBackground (writeInfosFunc, slimBackground) ->
                    slimBackground.ShpLayerName
                    |> Some

            let shpLayerPdfName =
                match image with 
                | RenewableBackgroundImageFile.BackgroundImageFile (image, createBackground) ->
                    match choice with 
                    | BackgroundOrForeground.Background -> PdfName.ShpLayerBK
                    | BackgroundOrForeground.Foreground -> PdfName.ShpLayerForeground

                | RenewableBackgroundImageFile.SlimBackground (writeInfosFunc, slimBackground) ->
                    slimBackground.ShpLayerPdfName

            let image = 
                let createImage createBackground (image: BackgroundImageFile) (imageFile: FsFullPath) (pdf_addtionFlow: BackgoundAddtionFlow) =
                    let name = pdf_addtionFlow.Name

                    imageCache.GetOrAdd((imageFile, name), valueFactory = fun (imageFile, name) ->
                        match imageFile.Path.ToLower() with 
                        | String.EndsWith ".pdf" -> 
                            let backgroundFile = createBackground(PdfFile imageFile, pdf_addtionFlow)


                            let backgroundInfos =

                                let pageBoxs = BackgroundFile.getPageBoxes backgroundFile
                                let totalPageNumber = pageBoxs.Length
                                let reader = new PdfDocument(new PdfReader(backgroundFile.ClearedPdfFile.Path))
          
                                {|  
                                    Close = fun () -> reader.Close()
                                    PageBoxAndXObject = 
                                        match image.Value with 
                                        | BackgroundImageFileValue.Singleton _ ->
                                            let index = (pageNumber-1) % totalPageNumber
                                            pageBoxs.[index], (reader.GetPage(index+1).CopyAsFormXObject(writerDoc) :> PdfXObject)

                                        | BackgroundImageFileValue.Multiple _ ->
                                            pageBoxs.[0], (reader.GetPage(1).CopyAsFormXObject(writerDoc) :> PdfXObject)
                                |}

                            backgroundInfos

                        | _ -> 
                            let imageData = ImageDataFactory.Create(imageFile.Path)
                            let xobject = PdfImageXObject(imageData)
                            {|
                                Close = ignore 
                                PageBoxAndXObject = 
                                    let width = 
                                        xobject.GetWidth() 
                                        |> float
                                        |> fun m -> m / float (imageData.GetDpiX())
                                        |> inch

                                    let height = 
                                        xobject.GetHeight() 
                                        |> float
                                        |> fun m -> m / float (imageData.GetDpiY())
                                        |> inch

                                    let pageBox = Rectangle.create 0 0 (width) (height)
                                    //let fsRect = FsRectangle.OfRectangle pageBox
                                    pageBox, xobject :> PdfXObject
                            |}
                    )

                match image with 
                | RenewableBackgroundImageFile.BackgroundImageFile (image, createBackground) ->
                    match image.Value with 
                    | BackgroundImageFileValue.Singleton image2 -> createImage createBackground image image2 image.Pdf_AddtionalFlowAndName
                    | BackgroundImageFileValue.Multiple images2 -> createImage createBackground image (images2.[i]) image.Pdf_AddtionalFlowAndName

                | RenewableBackgroundImageFile.SlimBackground (writeInfosFunc, slimBackground) ->
                    {|
                        Close = ignore
                        PageBoxAndXObject = 
                            slimBackground.GeneratePageBoxAndXObject(writeInfosFunc, writerDoc, pageBox)
                    |}


            let addImage() =    
                    
                match shpLayerName with 
                | None -> ()
                | Some shpLayerName ->
                    let stream = pdfCanvas.GetContentStream()
                    stream.Put(shpLayerPdfName, PdfString shpLayerName)
                    |> ignore


                pdfCanvas.AddPdfXObjectDSLEx(
                    backgroundPageBox = fst image.PageBoxAndXObject,
                    backgroundXObject  = snd image.PageBoxAndXObject,
                    readerPageBox = pageBox,
                    xEffect = xEffect,
                    yEffect = yEffect,
                    positionTweak = (
                        match backgroundPositionTweak with 
                        | None -> BackgroundPositionTweak.DefaultValue
                        | Some backgroundPositionTweak -> backgroundPositionTweak typedPageNumber
                    ),
                    extGSState = extGSState
                )
                |> ignore
                

            match layerName with 
            | None ->
                let addSelf() =
                    match addSelf with 
                    | None -> ()
                    | Some addSelf -> addSelf pdfCanvas

                match choice with 
                | BackgroundOrForeground.Background ->
                    addImage()

                    pdfCanvas
                    |> addShadowColor pageSize
                    |> ignore

                    addSelf()

                | BackgroundOrForeground.Foreground -> 
                    addSelf()
                    addImage()

            | Some layerName ->
                let bklayer = 
                    layerName.BackgroundLayer.CreateLayer(writerDoc)

                let addSelf() =
                    match addSelf with 
                    | None -> ()
                    | Some addSelf -> 
                        let currentLayer = layerName.CurrentLayer.CreateLayer(writerDoc)
                        pdfCanvas.BeginLayerUnion(currentLayer) |> ignore<PdfCanvas>
                        addSelf pdfCanvas
                        pdfCanvas.EndLayerUnion(currentLayer) |> ignore<PdfCanvas>

                match choice with 
                | BackgroundOrForeground.Background ->
                    let _addBKContents =
                        pdfCanvas.BeginLayerUnion(bklayer) |> ignore<PdfCanvas>
                        addImage()
                        pdfCanvas
                        |> addShadowColor pageSize
                        |> ignore

                        pdfCanvas.EndLayerUnion(bklayer) |> ignore<PdfCanvas>

                    addSelf()


                | BackgroundOrForeground.Foreground -> 
                    addSelf()

                    let __addBKContents = 
                        pdfCanvas.BeginLayerUnion(bklayer) |> ignore<PdfCanvas>
                        addImage()
                        pdfCanvas.EndLayerUnion(bklayer) |> ignore<PdfCanvas>

                    ()







