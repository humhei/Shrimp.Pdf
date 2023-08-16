﻿namespace Shrimp.Pdf.Image

open iText.Kernel.Pdf
open iText.Kernel.Pdf.Layer

#nowarn "0104"
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open Shrimp.Pdf
open System.IO
open iText.Kernel.Pdf.Xobject
open Shrimp.Pdf.Parser
open System.Drawing
open Shrimp.FSharp.Plus
open Shrimp.Pdf.Colors
open Akkling
open Shrimp.Pdf.icms2
open iText.Layout.Element
open Fake.IO
open Shrimp.Pdf.icms2.client
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.IO.Image
open FSharp.Data
open System.Collections.Concurrent
open Shrimp.Pdf.js.shared
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Geom

[<AutoOpen>]
module _ModifierIM =


    type BmpFileRecord =
        { BmpFile: BmpFile 
          Size: Size 
          XDpi: float }



    type ImageInfo =
        static member VisibleBoundIs(relativePosition, areaGettingOptions) =
            fun pageModifingArgs (image: IntegratedImageRenderInfo) ->
                match image.VisibleBound() with 
                | None -> false
                | Some bound ->
                    let area = pageModifingArgs.Page.GetArea(areaGettingOptions)
                    bound.IsRelativeTo(relativePosition, area) 


    type InfoIM = 
        static member DenseBoundIs(relativePosition, areaGettingOptions, ?boundGettingStrokeOptions) =
            fun pageModifingArgs (info: IIntegratedRenderInfoIM) ->
                match info with 
                | IIntegratedRenderInfoIM.Vector vector ->
                    Info.DenseBoundIs(relativePosition, areaGettingOptions, ?boundGettingStrokeOptions = boundGettingStrokeOptions) pageModifingArgs vector

                | IIntegratedRenderInfoIM.Pixel image ->
                    ImageInfo.VisibleBoundIs(relativePosition, areaGettingOptions) pageModifingArgs image


        static member BoundIs(relativePosition, areaGettingOptions, ?boundGettingStrokeOptions) =
            fun pageModifingArgs (info: IIntegratedRenderInfoIM) ->
                match info with 
                | IIntegratedRenderInfoIM.Vector vector ->
                    Info.BoundIs(relativePosition, areaGettingOptions, ?boundGettingStrokeOptions = boundGettingStrokeOptions) pageModifingArgs vector

                | IIntegratedRenderInfoIM.Pixel image ->
                    ImageInfo.VisibleBoundIs(relativePosition, areaGettingOptions) pageModifingArgs image

        static member IsVisible() =
            fun pageModifingArgs (info: IIntegratedRenderInfoIM) ->
                match info with 
                | IIntegratedRenderInfoIM.Vector vector ->
                    Info.IsVisible() pageModifingArgs vector

                | IIntegratedRenderInfoIM.Pixel image ->
                    let bound = image.VisibleBound()
                    match bound with 
                    | Some bound -> true
                    | None -> false


        static member BoundIs_InsideOrCross_Of(areaGettingOptions, ?boundGettingStrokeOptions) =
            fun pageModifingArgs (info: IIntegratedRenderInfoIM) ->
                match info with 
                | IIntegratedRenderInfoIM.Vector vector ->
                    Info.BoundIs_InsideOrCross_Of
                        (areaGettingOptions, ?boundGettingStrokeOptions = boundGettingStrokeOptions)
                        pageModifingArgs vector
                | IIntegratedRenderInfoIM.Pixel image ->
                    let bound = image.VisibleBound()
                    match bound with 
                    | None -> false
                    | Some bound -> bound.Is_InsideOrCross_Of(pageModifingArgs.Page.GetArea(areaGettingOptions))


    let private client  = lazy GetDefaultClusterIcm2Client()

    [<RequireQualifiedAccess>]
    module BitmapColorValues =
        let toIndexableStorage (colorSpace: IndexableColorSpace) size (colorValues: BitmapColorValues option) =
            match colorSpace.IndexTable with 
            | None -> 
                match colorValues with 
                | None -> failwithf "colorValues cannot be empty here"
                | Some colorValues ->
                    colorValues.ToStorage() |> IndexableBitmapColorValuesStorage.Origin
            | Some indexTable ->
                let indexTableGroup = 
                    let chunkSize =
                        match colorSpace.ColorSpace with 
                        | ColorSpace.Rgb -> 3
                        | ColorSpace.Cmyk -> 4
                        | ColorSpace.Lab -> 3
                        | ColorSpace.Gray -> 1

                    indexTable
                    |> Array.chunkBySize chunkSize

                let bytes =     
                    match colorValues with 
                    | Some colorValues ->
                        colorValues.RemovePlaceHolder()
                        |> Array.collect(fun indexes ->
                            indexes
                            |> Array.collect(fun index ->
                                indexTableGroup.[int index]
                            )
                        )

                    | None -> indexTable

                let rawFile = System.IO.Path.ChangeExtension(Path.GetTempFileNameEx(), ".raw")
                File.WriteAllBytes(rawFile, bytes)
                (size, RawFile rawFile)
                |> IndexableBitmapColorValuesStorage.Indexed

    type IntegratedImageRenderInfo with 

        member internal x.SaveToTmpColorValuesStorage(indexableColorSpace) =
            
            let image = x.ImageRenderInfo.GetImage()
            let imageData = x.ImageData
            let storage = 
                match imageData with 
                | FsImageData.ImageData imageData ->
                    let bytes = image.GetImageBytes()
                    let colorValues = 
                        use stream = new MemoryStream(bytes)
                        use bitmap = new Bitmap(stream)
                        let colorValues = BitmapUtils.ReadColorValues(bitmap)
                        (colorValues)

                    BitmapColorValues.toIndexableStorage indexableColorSpace (BitmapSize.OfDrawingSize colorValues.Size) (Some colorValues)

                | FsImageData.IndexedRgb indexedRGB -> 
                    BitmapColorValues.toIndexableStorage indexableColorSpace (indexedRGB.Size() |> BitmapSize.OfDrawingSize) (None)

                    
                

            storage, imageData


    [<RequireQualifiedAccess>]
    module private SelectionModifierFixmentArguments =
        let OfIM(args: _SelectionModifierFixmentArgumentsIM<_>, info) =
            { CurrentRenderInfo = info 
              PageModifingArguments = args.PageModifingArguments }

    let private cache = 
        new ConcurrentDictionary<PdfImageXObject * Icc option * Icc * Intent , ImageDataOrImageXObject option>()


    type ModifierIM =
        static member ConvertImageColorSpace(inputIcc: Icc option, outputIcc: Icc, intent): ModifierIM<'userState> =
            fun (args: _SelectionModifierFixmentArgumentsIM<'userState>) ->

                match args.CurrentRenderInfoIM with 
                | IIntegratedRenderInfoIM.Pixel imageRenderInfo ->   
                    let key = 
                        (imageRenderInfo.ImageRenderInfo.GetImage(), inputIcc, outputIcc, intent)


                    match imageRenderInfo.ImageColorSpaceData with 
                    | ImageColorSpaceData.Indexable indexableColorSpace ->
                        let newImageData = 
                            cache.GetOrAdd(key, fun _ ->
                                    let colorSpace = indexableColorSpace.ColorSpace
                                    let inputIcc = 
                                        match inputIcc with 
                                        | Some inputIcc ->
                                            match inputIcc.ColorSpace = colorSpace with
                                            | false -> Some inputIcc
                                            | true -> None

                                        | _ -> 
                                            ColorSpace.DefaultIcc colorSpace
                                            |> Some

                                    match inputIcc with 
                                    | Some inputIcc ->
                                        let components =
                                            match outputIcc.ColorSpace with 
                                            | ColorSpace.Cmyk -> 4
                                            | ColorSpace.Gray -> 1
                                            | ColorSpace.Rgb  -> 3
                                            | ColorSpace.Lab  -> 3


                                        match inputIcc.ColorSpace, imageRenderInfo.ImageData.GetOriginalType() with 
                                        | ColorSpace.Cmyk, ImageType.JPEG ->
                                            let rawFile =
                                                let rawPath = Path.GetTempFileNameEx() |> Path.changeExtension ".raw"
                                                let bytes = imageRenderInfo.ImageData.GetData()
                                                File.writeBytes (rawPath) bytes
                                                RawFile rawPath

                                            let imageData = imageRenderInfo.ImageData

                                            let decodeTransfrom = 
                                                match indexableColorSpace.Decode with 
                                                | None -> [||]
                                                | Some decodes ->
                                                    let decodes = 
                                                        decodes.ToIntArray()

                                                    let maxValue = 255
                                                    [|
                                                        for i in 0 .. 2 .. (decodes.Length - 1) do
                                                            yield (decodes.[i+1] - decodes.[i]) * 256 ||| 0
                                                            yield (decodes.[i] * maxValue) ||| 0
                                                    |]



                                            let jpegInput: jpegInput =
                                                { rawFile = rawFile.Path
                                                  width = imageData.GetWidth()    |> int
                                                  height = imageData.GetHeight()  |> int
                                                  jpegOptions =
                                                    { jpegOptions.colorTransform = -1
                                                      decodeTransform = decodeTransfrom}
                                                  }


                                            let r =
                                                let json = Thoth.Json.Net.Encode.Auto.toString(4, jpegInput)
                                                let addr = Shared.remoteAddress_withRoute
                                                Http.RequestString(
                                                    addr,
                                                    headers = [ HttpRequestHeaders.ContentType HttpContentTypes.Json ],
                                                    body = HttpRequestBody.TextRequest json)
                                                |> RawFile

                                            let size = 
                                                BitmapSize.Create(
                                                    int <| imageData.GetWidth(),
                                                    int <| imageData.GetHeight())

                                            let newArrayRawFile: RawFile = 
                                                client.Value <? 
                                                    ServerMsg.ConvertImageColorSpace (
                                                        inputIcc,
                                                        IndexableBitmapColorValuesStorage.Express(size, r),
                                                        outputIcc,
                                                        intent
                                                    )
                                                |> Async.RunSynchronously

                                            let bytes = File.ReadAllBytes newArrayRawFile.Path

                                            let image = 
                                                ImageDataFactory.Create(
                                                    width = size.BitmapWidth,
                                                    height = size.BitmapHeight,
                                                    components = components,
                                                    bpc = imageData.GetBpc(),
                                                    data = bytes,
                                                    transparency = null
                                                )
                                            Some (ImageDataOrImageXObject.ImageData image)


                                        // Pdf.js PDFImage.buildImage => createImageData
                                        // Whilte itext ImageData bytes is totally different
                                        | ColorSpace.Cmyk, imageType -> failwithf "Not implemented when input %A is cmyk" imageType
                                        | _ ->
                                            
                                            match inputIcc = outputIcc with 
                                            | true -> None
                                            | false ->  
                                                let convertiable =
                                                    match indexableColorSpace.IndexTable with 
                                                    | None ->
                                                        match inputIcc with 
                                                        | Icc.Cmyk _ 
                                                        | Icc.Gray _ 
                                                        | Icc.Lab _ -> 
                                                            /// Some filters are not supported in itext, e.g DCT Decode filter, and so on 
                                                            //failwithf "Only rgb input image is supported for converting colorspace"
                                                            false
                                                        | Icc.Rgb _ -> true
                                                    | Some _ -> true

                                                match convertiable with 
                                                | false -> None
                                                | true ->
                                                    let storage, imageData = imageRenderInfo.SaveToTmpColorValuesStorage(indexableColorSpace)
                                                    let size = storage.Size
                                                    let newArrayRawFile: RawFile = 
                                                        client.Value <? 
                                                            ServerMsg.ConvertImageColorSpace (
                                                                inputIcc,
                                                                storage,
                                                                outputIcc,
                                                                intent
                                                            )
                                                        |> Async.RunSynchronously

                                                    let bytes = File.ReadAllBytes newArrayRawFile.Path

                                                    match imageData with 
                                                    | FsImageData.IndexedRgb _ ->
                                                        let xobject = imageRenderInfo.ImageRenderInfo.GetImage().GetPdfObject().Clone() :?> PdfStream
                                                        let xobject = PdfImageXObject(xobject)
                                                        match outputIcc with
                                                        | Icc.Gray _  ->
                                                            let colorSpace = xobject.GetPdfObject().GetAsArray (PdfName.ColorSpace)
                                                            let colorSpace = colorSpace.Clone() :?> PdfArray
                                                            colorSpace
                                                                .Set(1, PdfName.DeviceGray)
                                                                |> ignore

                                                            let bytesStream = 
                                                                new PdfStream(bytes)

                                                            colorSpace
                                                                .Set(3, bytesStream)
                                                                |> ignore

                                                            xobject.Put(PdfName.ColorSpace, colorSpace) |> ignore
                                                            ImageDataOrImageXObject.ImageXObject(xobject)
                                                            |> Some

                                                    | FsImageData.ImageData _ ->

                                                        let image = 
                                                            ImageDataFactory.Create(
                                                                width = size.BitmapWidth,
                                                                height = size.BitmapHeight,
                                                                components = components,
                                                                bpc = imageData.GetBpc(),
                                                                data = bytes,
                                                                transparency = null
                                                            )
                                                        Some (ImageDataOrImageXObject.ImageData image)
                                    | None -> None
                        
                                
                            )

                        match newImageData with 
                        | None -> ModifierPdfCanvasActions.KeepImage()
                        | Some imageData ->
                            let ctm = AffineTransformRecord.DefaultValue
                            ModifierPdfCanvasActions.NewImage(ctm, imageData)

                    | ImageColorSpaceData.ImageMask ->
                        let black = FsValueColor.BLACK.ToItextColor()
                        ModifierPdfCanvasActions.CreateActions_Image(
                            [ PdfCanvas.setFillColor (black) ]
                        )
                

                | IIntegratedRenderInfoIM.Vector renderInfo ->
                    ModifierPdfCanvasActions.Keep(renderInfo.Tag)


        static member ConvertImageToGray() =
            ModifierIM.ConvertImageColorSpace(None, Icc.Gray defaultGrayIcc.Value, defaultIntent.Value)


        static member ConvertAllObjectsToDeviceGray() =
            fun (args: _SelectionModifierFixmentArgumentsIM<'userState>) ->
                match args.CurrentRenderInfoIM with 
                | IIntegratedRenderInfoIM.Pixel imageRenderInfo ->
                    ModifierIM.ConvertImageToGray() args

                | IIntegratedRenderInfoIM.Vector renderInfo ->
                    //ModifierPdfCanvasActions.Keep(renderInfo.Tag)
                    let args = SelectionModifierFixmentArguments.OfIM(args, renderInfo) 
                    Modifier.ConvertColorsTo(Icc.Gray defaultGrayIcc.Value) args

            
        static member AddImageBorder(?fArgs): ModifierIM<'userState> =
            fun (args: _SelectionModifierFixmentArgumentsIM<'userState>) ->
                 match args.CurrentRenderInfoIM with 
                 | IIntegratedRenderInfoIM.Pixel imageRenderInfo ->
                    let bound = 
                        imageRenderInfo.VisibleBound()

                    match bound with 
                    | None -> 
                        ModifierPdfCanvasActions.KeepImage()

                    | Some bound ->
                        let ctm = 
                            AffineTransform.ofMatrix (imageRenderInfo.ImageRenderInfo.GetImageCtm())
                            |> AffineTransform.inverse


                        ModifierPdfCanvasActions.CreateSuffix_Image(
                            [ 
                              PdfCanvas.concatMatrixByTransform (AffineTransformRecord.ofAffineTransform ctm)
                              PdfCanvas.addRectangle bound (defaultArg fArgs id)
                            ]
                        )

                 | IIntegratedRenderInfoIM.Vector renderInfo ->
                     ModifierPdfCanvasActions.Keep(renderInfo.Tag)



        static member SetMaximunDpi(maximunDpi: int) =
            fun (args: _SelectionModifierFixmentArgumentsIM<'userState>) ->
                match args.CurrentRenderInfoIM with 
                | IIntegratedRenderInfoIM.Pixel imageRenderInfo ->  
                    let imageData = imageRenderInfo.ImageData
                    match imageRenderInfo.ImageColorSpaceData with 
                    | ImageColorSpaceData.ImageMask ->
                        ModifierPdfCanvasActions.KeepImage()
                        
                    | ImageColorSpaceData.Indexable indexableColorSpace ->
                        let colorSpace = indexableColorSpace.ColorSpace
                        match colorSpace with 
                        | ColorSpace.Rgb ->
                            let dpi = imageRenderInfo.Dpi
                            match dpi.X, dpi.Y with 
                            | SmallerOrEqual maximunDpi, SmallerOrEqual maximunDpi ->
                                ModifierPdfCanvasActions.KeepImage()
                        
                            | dpi_x, dpi_y ->
                                use bmp =   
                                    let bytes = imageData.GetData()
                                    new Bitmap(new MemoryStream(bytes))
                    
                                let newSize = 
                                    let scaleX =(float maximunDpi / float dpi_x)  

                                    let scaleY = (float maximunDpi / float dpi_y) 

                                    let multipleScale (px: int) (scale) = float px * scale |> round |> int

                                    let baseSize = bmp.Size
                                    Size(multipleScale baseSize.Width scaleX, multipleScale baseSize.Height scaleY)

                                use newBitmap = new Bitmap(bmp, newSize)

                                let colorValues = BitmapUtils.ReadColorValues(newBitmap)
                                let rgbValues = colorValues.GetAsRgbValues()

                                let rgbByteValues = 
                                    rgbValues
                                    |> Array.concat
                                    |> Array.map (fun v -> v * 255.f |> byte)

                                let image = 
                                    ImageDataFactory.Create(
                                        width = newSize.Width,
                                        height = newSize.Height,
                                        components = 3,
                                        bpc = imageData.GetBpc(),
                                        data = rgbByteValues,
                                        transparency = null
                                    )
                                    |> ImageDataOrImageXObject.ImageData
                    
                                let ctm = AffineTransformRecord.DefaultValue

                                ModifierPdfCanvasActions.NewImage(ctm, image)

                        | _ ->
                            ModifierPdfCanvasActions.KeepImage()
                            /// Some filters are not supported in itext, e.g DCT Decode filter, and so on 
                            //failwithf "Only rgb input image is supported for converting colorspace"


                | IIntegratedRenderInfoIM.Vector renderInfo ->
                    ModifierPdfCanvasActions.Keep(renderInfo.Tag)


    type ModifierAll =
        static member ChangeBlendMode(blendMode) =
            fun (args: _SelectionModifierFixmentArgumentsIM<'userState>) ->
                let originExtGState = args.CurrentRenderInfoIM.Value.GetGraphicsState() |> CanvasGraphicsState.getExtGState 
                let extGState = 
                    { originExtGState with 
                        BlendModes = [blendMode] }
                
                ModifierPdfCanvasActions.CreateActions_All args.Tag  [
                    PdfCanvas.setExtGState(extGState)
                ]



    type ModifyIM =
        static member ChangeImageBlendMode(blendMode, ?imageSelector) =
            Modify.Create_RecordIM(
                PageSelector.All,
                selectorAndModifiersList = [
                    { SelectorAndModifiersRecordIM.Name = "change image blend mode" 
                      Selector = Selector.ImageX(defaultArg imageSelector (fun _ _ -> true))
                      Modifiers = [ 
                        ModifierAll.ChangeBlendMode(blendMode)
                      ]}
                ])

        static member ConvertImagesToDeviceGray(?imageSelector) =
            Modify.Create_RecordIM(
                PageSelector.All,
                selectorAndModifiersList = [
                    { SelectorAndModifiersRecordIM.Name = "convert images to gray" 
                      Selector = Selector.ImageX(defaultArg imageSelector (fun _ _ -> true))
                      Modifiers = [ 
                        ModifierIM.ConvertImageToGray()
                      ]
                    }
                ])

        static member ConvertAllObjectsToDeviceGray(?selector, ?pageSelector, ?loggingPageCountInterval) =
            Modify.Create_RecordIM(
                defaultArg pageSelector PageSelector.All,
                selectorAndModifiersList = [
                    { SelectorAndModifiersRecordIM.Name = "convert all objects to gray" 
                      Selector = defaultArg selector (Selector.All (fun _ _ -> true))
                      Modifiers = [ 
                        ModifierIM.ConvertAllObjectsToDeviceGray()
                      ]
                    }
                ],
                ?loggingPageCountInterval = loggingPageCountInterval
            )



    type Flows with
        static member Overly_Clip_ConvertAllObjectsToGray(clippingPathSelector, area: Overly_Clip_ManipulateArea, ?excludingSelector, ?keepCompoundPath, ?loggingPageCountInterval) =
            Flows.Overly_Clip_Manipulate
                (clippingPathSelector,
                 area = area,
                 manipulate = ModifyIM.ConvertAllObjectsToDeviceGray(
                    ?selector = (
                        match excludingSelector with 
                        | Some excludingSelector ->
                            Selector.AND [
                                Selector.All(fun _ _ -> true)
                                Selector.Not(excludingSelector)
                            ]
                            |> Some
                        | None -> None
                    ),
                    ?loggingPageCountInterval = loggingPageCountInterval
                ),
                ?excludingSelector = excludingSelector,
                ?keepCompoundPath = keepCompoundPath
                )
            //|> Flow.Rename(
            //    "Overly_Clip_ConvertAllObjectsToGray",
            //    [
            //        "clippingPathSelector" => clippingPathSelector.ToString()
            //        "area" => area.ToString()
            //        "excludingSelector" => sprintf "%O" excludingSelector
            //    ]
            //)

    type RenderInfoStoppedException_EmptyPage(info) =
        inherit RenderInfoStoppedException(info)

    [<RequireQualifiedAccess>]
    type EmptablePageNumber =
        | Empty of PageNumber
        | NotEmpty of PageNumber

    [<RequireQualifiedAccess>]
    type BackgroundImageFile = 
        | Singleton of FsFullPath
        | Multiple  of FsFullPath list



    let private reuse name parameters f = Reuse(f = f, flowName = FlowName.Override(name, parameters))

    type PdfCanvas with 
        member x.AddImageFittedIntoRectangleDSL(image, pageSize, asInline) =
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
                    

    type Reuses with 
        
        static member private AddBackgroundOrForegroundImage(image: BackgroundImageFile, choice: BackgroundOrForeground, ?shadowColor, ?xEffect, ?yEffect, ?pageSelector, ?layerName: BackgroundAddingLayerOptions, ?extGSState: FsExtGState) =
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

                let reader = doc.Reader
                let selectedPages = reader.GetPageNumbers(pageSelector)
                PdfDocument.getPages reader
                |> List.mapi (fun i readerPage ->
                    
                    match List.contains (i+1) selectedPages with 
                    | true ->
                        let readerPageBox = readerPage.GetActualBox()
                        let readerXObject = readerPage.CopyAsFormXObject(doc.Writer)

                        let pageSize = 
                            readerPage.GetActualBox()
                            |> PageSize

                        let writerPage = doc.Writer.AddNewPage(pageSize)
                        writerPage.SetPageBoxToPage(readerPage) |> ignore
                        let pdfCanvas = new PdfCanvas(writerPage)
                        let image = 
                            match image with 
                            | BackgroundImageFile.Singleton image -> ImageDataFactory.Create(image.Path)
                            | BackgroundImageFile.Multiple images -> ImageDataFactory.Create(images.[i].Path)

                        match layerName with 
                        | None ->
                            match choice with 
                            | BackgroundOrForeground.Background ->
                                pdfCanvas.AddImageFittedIntoRectangleDSLEx(image, pageSize, asInline = false, extGSState = extGSState)
                                |> ignore

                                pdfCanvas
                                |> addShadowColor pageSize
                                |> fun m -> m.AddXObjectAbs(readerXObject, 0.f, 0.f)
                                |> ignore

                            | BackgroundOrForeground.Foreground -> 
                                pdfCanvas
                                    .AddXObjectAbs(readerXObject, 0.f, 0.f)
                                    .AddImageFittedIntoRectangle(image, pageSize, asInline = false)
                                |> ignore

                        | Some layerName ->
                            let bklayer = 
                                layerName.BackgroundLayer.CreateLayer(doc.Writer)

                            let addBackground (pdfCanvas: PdfCanvas) =
                                pdfCanvas
                                    .BeginLayerUnion(bklayer)
                                    |> fun m -> 
                                        m.AddImageFittedIntoRectangleDSLEx(image, pageSize, asInline = false, extGSState = extGSState)
                                        |> ignore

                                        m.EndLayerUnion(bklayer)

                            let readerLayer = layerName.CurrentLayer.CreateLayer(doc.Writer)

                            let addReader ifAddShadowColor (pdfCanvas: PdfCanvas) =
                                pdfCanvas
                                    .BeginLayerUnion(readerLayer)
                                    |> fun m ->
                                        
                                        match ifAddShadowColor with 
                                        | true ->
                                            addShadowColor pageSize pdfCanvas
                                            |> ignore
                                        | false -> ()

                                        m
                                            .AddXObjectAbs(readerXObject, 0.f, 0.f)
                                            .EndLayerUnion(readerLayer)


                            match choice with 
                            | BackgroundOrForeground.Background ->
                                pdfCanvas
                                |> addBackground 
                                |> addReader true
                                |> ignore

                            | BackgroundOrForeground.Foreground -> 
                                pdfCanvas
                                |> addReader false
                                |> addBackground 
                                |> ignore

                    | false ->
                        let page = readerPage.CopyTo(doc.Writer)
                        doc.Writer.AddPage(page)
                        |> ignore
                    )
                    |> ignore



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

        static member AddBackgroundImage (backgroundFile, ?shadowColor, ?xEffect, ?yEffect, ?pageSelector, ?layerName, ?extGSState) =
            Reuses.AddBackgroundOrForegroundImage(backgroundFile, BackgroundOrForeground.Background, ?shadowColor = shadowColor, ?xEffect = xEffect, ?yEffect = yEffect, ?pageSelector = pageSelector, ?layerName = layerName, ?extGSState = extGSState)

        static member AddForegroundImage (foregroundFile, ?shadowColor, ?xEffect, ?yEffect, ?pageSelector, ?layerName, ?extGSState) =
            Reuses.AddBackgroundOrForegroundImage(foregroundFile, BackgroundOrForeground.Foreground, ?shadowColor = shadowColor, ?xEffect = xEffect, ?yEffect = yEffect, ?pageSelector = pageSelector, ?layerName = layerName, ?extGSState = extGSState)



    type PdfRunner with 
        static member AddBackgroundImage(pdfFile, backgroundFile, ?shadowColor, ?xEffect, ?yEffect, ?pageSelector, ?layerName, ?extGSState, ?backupPdfPath) =
            Reuses.AddBackgroundImage(
                backgroundFile,
                ?shadowColor = shadowColor,
                ?xEffect = xEffect,
                ?yEffect = yEffect,
                ?pageSelector = pageSelector,
                ?layerName = layerName,
                ?extGSState = extGSState

            )    
            |> PdfRunner.Reuse(pdfFile, ?backupPdfPath = backupPdfPath)

        static member AddForegroundImage(pdfFile, backgroundFile, ?shadowColor, ?xEffect, ?yEffect, ?pageSelector, ?layerName, ?extGSState, ?backupPdfPath) =
            Reuses.AddForegroundImage(
                backgroundFile,
                ?shadowColor = shadowColor,
                ?xEffect = xEffect,
                ?yEffect = yEffect,
                ?pageSelector = pageSelector,
                ?layerName = layerName,
                ?extGSState = extGSState

            )    
            |> PdfRunner.Reuse(pdfFile, ?backupPdfPath = backupPdfPath)



        static member ReadEmptyPages(pdfFile: PdfFile, ignoreColors: FsColor list, ?inShadowMode) =
            let selector =
                let allTagColors = ignoreColors
                Selector.All(fun args info ->
                    match InfoIM.IsVisible() args info with 
                    | true ->
                        match info with 
                        | IIntegratedRenderInfoIM.Pixel image -> raise (new RenderInfoStoppedException_EmptyPage(info))
                        | IIntegratedRenderInfoIM.Vector vector ->
                            match Info.ColorIsOneOf(FillOrStrokeOptions.FillOrStroke, allTagColors, ValueEqualOptions.DefaultRough) args vector with 
                            | true -> true
                            | false -> raise (new RenderInfoStoppedException_EmptyPage(info))

                    | false -> true

                )

            PdfRunner.ReadInfosIMStoppable(pdfFile, selector, (fun args infos ->
                match infos with 
                | StoppableParsedRenderInfoIMs.Stopped stopped -> 
                    EmptablePageNumber.NotEmpty (PageNumber args.PageNum)

                | StoppableParsedRenderInfoIMs.NonStopped _ -> 
                    EmptablePageNumber.Empty (PageNumber args.PageNum)
            ), pageSelector = PageSelector.All, ?inShadowMode = inShadowMode)