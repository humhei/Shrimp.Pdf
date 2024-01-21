// Learn more about F# at http://fsharp.org
namespace Shrimp.Pdf
open Shrimp.FSharp.Plus
open Shrimp.Pdf
open Shrimp.Pdf.Extract
open Shrimp.LiteDB

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

[<AutoOpen>]
module _Extract_BackgroundOrForeground =
    [<AutoOpen>]
    module private _BackgroundFileCache =
        let backgroundCache = 
            new LightWeightFileInfoDictionary<PdfFile * string, PdfFile, PdfPath * string>(
                cacheFileMapping = (fun (m, _) -> Path.changeExtension ".cache" m.Path),
                keyFilesGetter = (fun (m, _) -> [m.Path]),
                valueFilesGetter = (fun m -> [m.FileInfo]),
                primaryKeyGetter = (fun (m, name) -> primaryKey (m.PdfPath, name))
            )

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

    type BackgroundFile with 
        static member Create(pdfFile: PdfFile, addtionalFlow: BackgoundAddtionFlow) =
            let name = addtionalFlow.Name
            let clearedPdfFile = 
                backgroundCache.GetOrAddOrUpdate((pdfFile, name), fun () ->
                    let targetPath =
                        match name with 
                        | String.EqualIC UNTITLED ->
                            pdfFile.Path
                            |> Path.changeExtension "backgroundFile.cleared.pdf"

                        | name ->
                            let name = 
                                match name.LeftOf("-") with 
                                | Some name -> name
                                | None -> name

                            pdfFile.Path
                            |> Path.changeExtension (sprintf "%s.backgroundFile.cleared.pdf" name)


                    let newPdfFile: PdfFile = 
                        let flow = 
                            Flow.Reuse(
                                Reuses.ClearDirtyInfos()
                            )
                            <+>
                            (
                                match addtionalFlow with 
                                | BackgoundAddtionFlow.Keep -> Flow.dummy() ||>> ignore
                                | BackgoundAddtionFlow.ExtractObjects -> 
                                    Flow.Reuse(
                                        Reuses.ExtractIM(PageSelector.All, Selector.All(fun _ _ -> true))
                                    )
                                | BackgoundAddtionFlow.CustomCase (_, flow) ->  flow
                            )

                        runWithBackup targetPath pdfFile.Path (flow)
                        |> List.exactlyOne_DetailFailingText
                        |> fun flowModel -> flowModel.PdfFile

                    newPdfFile
                )
                |> Async.RunSynchronously

            (pdfFile, clearedPdfFile, Rotation.None)
            |> BackgroundFile



        static member Create(pdfFile: PdfFile) =
            BackgroundFile.Create(pdfFile, BackgoundAddtionFlow.Keep)

        static member Create(path: string) =
            PdfFile path
            |> BackgroundFile.Create

        static member Create(path: string, addtionalFlow) =
            BackgroundFile.Create(PdfFile path, addtionalFlow)

    
    [<RequireQualifiedAccess>]
    type BackgroundImageFileValue = 
        | Singleton of FsFullPath
        | Multiple  of FsFullPath list

    type BackgroundImageFile =
        { Value: BackgroundImageFileValue
          Pdf_AddtionalFlowAndName: BackgoundAddtionFlow}
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


    [<RequireQualifiedAccess>]
    type BackgroundPositionTweak =    
        | OfPageBox of offsetX: float * offsetY: float
        | SpecficRect of FsRectangle
    with 
        static member DefaultValue = OfPageBox(0, 0)

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
                | BackgroundPositionTweak.OfPageBox (_) -> readerPageBox

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
                | BackgroundPositionTweak.OfPageBox (xOffset, yOffset) -> bk_x + xOffset

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
                | BackgroundPositionTweak.OfPageBox (xOffset, yOffset) -> bk_y + yOffset

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
               
                   
                    
    


    type PdfPage with 
        member page.AddBackgroundOrForegroundImage 
            (i: int,
             imageCache: System.Collections.Concurrent.ConcurrentDictionary<_, _>,
             image: BackgroundImageFile,
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
                page.GetActualBox()

            let pageSize = 
                pageBox
                |> PageSize

            let writerDoc = page.GetDocument()
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
                    new PdfCanvas(page.NewContentStreamBefore(), page.GetResources(), writerDoc)

                | BackgroundOrForeground.Foreground ->
                    new PdfCanvas(page.NewContentStreamAfter(), page.GetResources(), writerDoc)
                    


            let image = 
                let createImage (imageFile: FsFullPath) (pdf_addtionFlow: BackgoundAddtionFlow) =
                    let name = pdf_addtionFlow.Name

                    imageCache.GetOrAdd((imageFile, name), valueFactory = fun (imageFile, name) ->
                        match imageFile.Path.ToLower() with 
                        | String.EndsWith ".pdf" -> 
                            let backgroundFile = BackgroundFile.Create(PdfFile imageFile, pdf_addtionFlow)


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

                match image.Value with 
                | BackgroundImageFileValue.Singleton image2 -> createImage image2 image.Pdf_AddtionalFlowAndName
                | BackgroundImageFileValue.Multiple images2 -> createImage (images2.[i]) image.Pdf_AddtionalFlowAndName

            let addImage() =    
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









    type Reuses with 
        
        static member private AddBackgroundOrForegroundImage(image: BackgroundImageFile, choice: BackgroundOrForeground, ?shadowColor, ?xEffect, ?yEffect, ?backgroundPositionTweak: PageNumber -> BackgroundPositionTweak, ?pageSelector, ?layerName: BackgroundAddingLayerOptions, ?extGSState: FsExtGState) =
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


            let pageSelector = defaultArg pageSelector PageSelector.All
            (fun flowModel (doc: SplitDocument) ->
                let imageCache = new System.Collections.Concurrent.ConcurrentDictionary<_, _>()
                let reader = doc.Reader
                let selectedPages = reader.GetPageNumbers(pageSelector)
                PdfDocument.getPages reader
                |> List.mapi (fun i readerPage ->
                    let pageNumber = i + 1
                    let typedPageNumber = PageNumber pageNumber
                    match List.contains (i+1) selectedPages with 
                    | true ->
                        let readerPageBox = readerPage.GetActualBox()

                        let pageSize = 
                            readerPageBox
                            |> PageSize

                        let writerPage = doc.Writer.AddNewPage(pageSize)
                        writerPage.SetPageBoxToPage(readerPage) |> ignore
                        let readerXObject = readerPage.CopyAsFormXObject(doc.Writer)

                        writerPage.AddBackgroundOrForegroundImage(
                            i,
                            imageCache,
                            image,
                            choice,
                            addSelf = (fun canvas -> canvas.AddXObjectAbs(readerXObject, 0.f, 0.f) |> ignore<PdfCanvas>),
                            ?shadowColor = shadowColor,
                            xEffect = xEffect,
                            yEffect = yEffect,
                            ?backgroundPositionTweak = backgroundPositionTweak,
                            ?layerName = layerName,
                            ?extGSState = extGSState
                        )
                    | false ->
                        let page = readerPage.CopyTo(doc.Writer)
                        doc.Writer.AddPage(page)
                        |> ignore
                    )
                    |> ignore


                imageCache
                |> List.ofSeq
                |> List.iter(fun m -> 
                    m.Value.Close()
                )

            )
            |> reuse 
                "AddBackgroundOrForeground"
                [
                    "pageSelector" => pageSelector.ToString()
                    "backgroundFile" => image.ToString()
                    "layer" => choice.ToString()
                    "xEffect" => xEffect.ToString()
                    "yEffect" => yEffect.ToString()
                ]

        
        //static member private AddBackgroundOrForegroundImage0(image: BackgroundImageFileValue, choice: BackgroundOrForeground, ?shadowColor, ?xEffect, ?yEffect, ?backgroundPositionTweak: PageNumber -> BackgroundPositionTweak, ?pageSelector, ?layerName: BackgroundAddingLayerOptions, ?extGSState: FsExtGState) =
        //    let xEffect = defaultArg xEffect XEffort.Middle
        //    let yEffect = defaultArg yEffect YEffort.Middle
        //    let addShadowColor pageSize (pdfCanvas: PdfCanvas) =
        //        match shadowColor with 
        //        | None -> pdfCanvas
        //        | Some (pdfFileShadow: NullablePdfCanvasColor) ->
        //            pdfCanvas
        //                .SaveState()
        //                .SetFillColor(pdfFileShadow)
        //                .Rectangle(pageSize)
        //                .Fill()
        //                .RestoreState()


        //    let pageSelector = defaultArg pageSelector PageSelector.All
        //    (fun flowModel (doc: SplitDocument) ->
        //        let imageCache = new System.Collections.Concurrent.ConcurrentDictionary<_, _>()
        //        let reader = doc.Reader
        //        let selectedPages = reader.GetPageNumbers(pageSelector)
        //        PdfDocument.getPages reader
        //        |> List.mapi (fun i readerPage ->
        //            let pageNumber = i + 1
        //            let typedPageNumber = PageNumber pageNumber
        //            match List.contains (i+1) selectedPages with 
        //            | true ->
        //                let readerPageBox = readerPage.GetActualBox()
        //                let readerXObject = readerPage.CopyAsFormXObject(doc.Writer)

        //                let pageSize = 
        //                    readerPageBox
        //                    |> PageSize

        //                let writerPage = doc.Writer.AddNewPage(pageSize)
        //                writerPage.SetPageBoxToPage(readerPage) |> ignore
        //                let pdfCanvas = new PdfCanvas(writerPage)

        //                let image = 
        //                    let createImage (imageFile: FsFullPath) =
        //                        imageCache.GetOrAdd(imageFile, valueFactory = fun imageFile ->
        //                            match imageFile.Path.ToLower() with 
        //                            | String.EndsWith ".pdf" -> 
        //                                let backgroundFile = BackgroundFile.Create imageFile.Path
        //                                let backgroundInfos =

        //                                    let pageBoxs = BackgroundFile.getPageBoxes backgroundFile
        //                                    let totalPageNumber = pageBoxs.Length
        //                                    let reader = new PdfDocument(new PdfReader(backgroundFile.ClearedPdfFile.Path))
          
        //                                    {|  
        //                                        Close = fun () -> reader.Close()
        //                                        PageBoxAndXObject = 
        //                                            match image with 
        //                                            | BackgroundImageFileValue.Singleton _ ->
        //                                                let index = (pageNumber-1) % totalPageNumber
        //                                                pageBoxs.[index], (reader.GetPage(index+1).CopyAsFormXObject(doc.Writer) :> PdfXObject)

        //                                            | BackgroundImageFileValue.Multiple _ ->
        //                                                pageBoxs.[0], (reader.GetPage(1).CopyAsFormXObject(doc.Writer) :> PdfXObject)
        //                                    |}

        //                                backgroundInfos

        //                            | _ -> 
        //                                let imageData = ImageDataFactory.Create(imageFile.Path)
        //                                let xobject = PdfImageXObject(imageData)
        //                                {|
        //                                    Close = ignore 
        //                                    PageBoxAndXObject = 
        //                                        let width = 
        //                                            xobject.GetWidth() 
        //                                            |> float
        //                                            |> fun m -> m / float (imageData.GetDpiX())
        //                                            |> inch

        //                                        let height = 
        //                                            xobject.GetHeight() 
        //                                            |> float
        //                                            |> fun m -> m / float (imageData.GetDpiY())
        //                                            |> inch

        //                                        let pageBox = Rectangle.create 0 0 (width) (height)
        //                                        //let fsRect = FsRectangle.OfRectangle pageBox
        //                                        pageBox, xobject :> PdfXObject
        //                                |}
        //                        )

        //                    match image with 
        //                    | BackgroundImageFileValue.Singleton image -> createImage image
        //                    | BackgroundImageFileValue.Multiple images -> createImage (images.[i])

        //                let addImage() =    
        //                    pdfCanvas.AddPdfXObjectDSLEx(
        //                        backgroundPageBox = fst image.PageBoxAndXObject,
        //                        backgroundXObject  = snd image.PageBoxAndXObject,
        //                        readerPageBox = readerPageBox,
        //                        xEffect = xEffect,
        //                        yEffect = yEffect,
        //                        positionTweak = (
        //                            match backgroundPositionTweak with 
        //                            | None -> BackgroundPositionTweak.DefaultValue
        //                            | Some backgroundPositionTweak -> backgroundPositionTweak typedPageNumber
        //                        ),
        //                        extGSState = extGSState
        //                    )
        //                    |> ignore
                            

        //                match layerName with 
        //                | None ->
        //                    match choice with 
        //                    | BackgroundOrForeground.Background ->
        //                        addImage()

        //                        pdfCanvas
        //                        |> addShadowColor pageSize
        //                        |> fun m -> m.AddXObjectAbs(readerXObject, 0.f, 0.f)
        //                        |> ignore

        //                    | BackgroundOrForeground.Foreground -> 
        //                        pdfCanvas
        //                            .AddXObjectAbs(readerXObject, 0.f, 0.f)
        //                        |> ignore

        //                        addImage()

        //                | Some layerName ->
        //                    let bklayer = 
        //                        layerName.BackgroundLayer.CreateLayer(doc.Writer)

        //                    let addBackground (pdfCanvas: PdfCanvas) =
        //                        pdfCanvas
        //                            .BeginLayerUnion(bklayer)
        //                            |> fun m -> 
        //                                addImage()
        //                                m.EndLayerUnion(bklayer)

        //                    let readerLayer = layerName.CurrentLayer.CreateLayer(doc.Writer)

        //                    let addReader ifAddShadowColor (pdfCanvas: PdfCanvas) =
        //                        pdfCanvas
        //                            .BeginLayerUnion(readerLayer)
        //                            |> fun m ->
                                        
        //                                match ifAddShadowColor with 
        //                                | true ->
        //                                    addShadowColor pageSize pdfCanvas
        //                                    |> ignore
        //                                | false -> ()

        //                                m
        //                                    .AddXObjectAbs(readerXObject, 0.f, 0.f)
        //                                    .EndLayerUnion(readerLayer)


        //                    match choice with 
        //                    | BackgroundOrForeground.Background ->
        //                        pdfCanvas
        //                        |> addBackground 
        //                        |> addReader true
        //                        |> ignore

        //                    | BackgroundOrForeground.Foreground -> 
        //                        pdfCanvas
        //                        |> addReader false
        //                        |> addBackground 
        //                        |> ignore

        //            | false ->
        //                let page = readerPage.CopyTo(doc.Writer)
        //                doc.Writer.AddPage(page)
        //                |> ignore
        //            )
        //            |> ignore


        //        imageCache
        //        |> List.ofSeq
        //        |> List.iter(fun m -> 
        //            m.Value.Close()
        //        )

        //    )
        //    |> reuse 
        //        "AddBackgroundOrForeground"
        //        [
        //            "pageSelector" => pageSelector.ToString()
        //            "backgroundFile" => image.ToString()
        //            "layer" => choice.ToString()
        //            "xEffect" => xEffect.ToString()
        //            "yEffect" => yEffect.ToString()
        //        ]

        static member AddBackgroundImage (backgroundFile, ?shadowColor, ?xEffect, ?yEffect, ?backgroundPositionTweak, ?pageSelector, ?layerName, ?extGSState) =
            Reuses.AddBackgroundOrForegroundImage(backgroundFile, BackgroundOrForeground.Background, ?shadowColor = shadowColor, ?xEffect = xEffect, ?yEffect = yEffect, ?backgroundPositionTweak = backgroundPositionTweak, ?pageSelector = pageSelector, ?layerName = layerName, ?extGSState = extGSState)

        static member AddForegroundImage (foregroundFile, ?shadowColor, ?xEffect, ?yEffect, ?backgroundPositionTweak, ?pageSelector, ?layerName, ?extGSState) =
            Reuses.AddBackgroundOrForegroundImage(foregroundFile, BackgroundOrForeground.Foreground, ?shadowColor = shadowColor, ?xEffect = xEffect, ?yEffect = yEffect, ?backgroundPositionTweak = backgroundPositionTweak, ?pageSelector = pageSelector, ?layerName = layerName, ?extGSState = extGSState)



    type PdfRunner with 
        static member AddBackgroundImage(pdfFile, backgroundFile, ?shadowColor, ?xEffect, ?yEffect, ?backgroundPositionTweak, ?pageSelector, ?layerName, ?extGSState, ?backupPdfPath) =
            Reuses.AddBackgroundImage(
                backgroundFile,
                ?shadowColor = shadowColor,
                ?xEffect = xEffect,
                ?yEffect = yEffect,
                ?backgroundPositionTweak = backgroundPositionTweak,
                ?pageSelector = pageSelector,
                ?layerName = layerName,
                ?extGSState = extGSState

            )    
            |> PdfRunner.Reuse(pdfFile, ?backupPdfPath = backupPdfPath)

        static member AddForegroundImage(pdfFile, backgroundFile, ?shadowColor, ?xEffect, ?yEffect, ?backgroundPositionTweak, ?pageSelector, ?layerName, ?extGSState, ?backupPdfPath) =
            Reuses.AddForegroundImage(
                backgroundFile,
                ?shadowColor = shadowColor,
                ?xEffect = xEffect,
                ?yEffect = yEffect,
                ?backgroundPositionTweak = backgroundPositionTweak,
                ?pageSelector = pageSelector,
                ?layerName = layerName,
                ?extGSState = extGSState

            )    
            |> PdfRunner.Reuse(pdfFile, ?backupPdfPath = backupPdfPath)



