namespace Shrimp.Pdf.Image
#nowarn "0104"
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open Shrimp.Pdf
open System.IO
open iText.Kernel.Pdf.Xobject
open Shrimp.Pdf.Parser
open System.Drawing
open Shrimp.FSharp.Plus
open System.Drawing
open Shrimp.Pdf.Colors
open Akkling
open Shrimp.Pdf.icms2
open iText.Layout.Element
open Fake.IO
open Shrimp.Pdf.icms2.client
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.IO.Image
open System.Collections.Concurrent

[<AutoOpen>]
module _ModifierIM =

    type BmpFileRecord =
        { BmpFile: BmpFile 
          Size: Size 
          XDpi: float }

    let private cache = 
        new ConcurrentDictionary<PdfImageXObject, IndexableBitmapColorValuesStorage * ImageData>()

    let private client  = lazy GetDefaultClusterIcm2Client()

    [<RequireQualifiedAccess>]
    module BitmapColorValues =
        let toIndexableStorage (colorSpace: IndexableColorSpace) (colorValues: BitmapColorValues) =
            match colorSpace.IndexTable with 
            | None -> 
                match colorSpace.ColorSpace with 
                | ColorSpace.Cmyk  -> 
                    let m = 
                        colorValues.GetAsCMYKValues()
                        |> Array.chunkBySize colorValues.Size.Width
                    ()
                | _ -> ()
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
                    colorValues.RemovePlaceHolder()
                    |> Array.collect(fun indexes ->
                        indexes
                        |> Array.collect(fun index ->
                            indexTableGroup.[int index]
                        )
                    )

                let rawFile = System.IO.Path.ChangeExtension(Path.GetTempFileName(), ".raw")
                File.WriteAllBytes(rawFile, bytes)
                (colorValues.Size, RawFile rawFile)
                |> IndexableBitmapColorValuesStorage.Indexed

    type IntegratedImageRenderInfo with 

        member x.SaveToTmpColorValuesStorage() =
            
            let image = x.ImageRenderInfo.GetImage()
            cache.GetOrAdd(image, fun image ->
                let bytes = image.GetImageBytes()

                let imageData = x.ImageData
                let storage = 
                    use stream = new MemoryStream(bytes)
                    use bitmap = new Bitmap(stream)
                    BitmapUtils.ReadColorValues(bitmap)
                    |> BitmapColorValues.toIndexableStorage x.IndexableColorSpace


                storage, imageData
            )


    [<RequireQualifiedAccess>]
    module private SelectionModifierFixmentArguments =
        let OfIM(args: _SelectionModifierFixmentArgumentsIM<_>, info) =
            { CurrentRenderInfo = info 
              PageModifingArguments = args.PageModifingArguments
              ConcatedTextInfos = args.ConcatedTextInfos }

    type ModifierIM =
        static member ConvertImageColorSpace(inputIcc: Icc option, outputIcc: Icc, intent): ModifierIM<'userState> =
            fun (args: _SelectionModifierFixmentArgumentsIM<'userState>) ->
                match args.CurrentRenderInfoIM with 
                | IIntegratedRenderInfoIM.Pixel imageRenderInfo ->  
                    let storage, imageData = imageRenderInfo.SaveToTmpColorValuesStorage()
                    let size = storage.Size
                    let inputIcc = 
                        match inputIcc with 
                        | Some inputIcc ->
                            match inputIcc.ColorSpace = imageRenderInfo.ColorSpace with
                            | false -> Some inputIcc
                            | true -> None

                        | _ -> 
                            ColorSpace.DefaultIcc imageRenderInfo.ColorSpace
                            |> Some
            
                    match inputIcc with 
                    | Some inputIcc ->
                        match inputIcc = outputIcc with 
                        | true -> ModifierPdfCanvasActions.KeepImage()
                        | false ->  
                            let convertiable =
                                match imageRenderInfo.IndexableColorSpace.IndexTable with 
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
                            | false ->  
                                ModifierPdfCanvasActions.KeepImage()

                            | true ->
                                let components =
                                    match outputIcc.ColorSpace with 
                                    | ColorSpace.Cmyk -> 4
                                    | ColorSpace.Gray -> 1
                                    | ColorSpace.Rgb  -> 3
                                    | ColorSpace.Lab  -> 3

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

                                let image = 
                                    ImageDataFactory.Create(
                                        width = size.Width,
                                        height = size.Height,
                                        components = components,
                                        bpc = 8,
                                        data = bytes,
                                        transparency = null
                                    )
                    
                                let ctm = AffineTransformRecord.DefaultValue

                                ModifierPdfCanvasActions.NewImage(ctm, image)
                
                    | None -> ModifierPdfCanvasActions.KeepImage()
                        

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
                        IImageRenderInfo.getBound imageRenderInfo

                    let ctm = 
                        AffineTransform.ofMatrix (imageRenderInfo.ImageRenderInfo.GetImageCtm())
                        |> AffineTransform.inverse

                    let transformedBound =  ctm.Transform(bound)

                    ModifierPdfCanvasActions.createSuffix_Image(
                        [ PdfCanvas.concatMatrixByTransform (AffineTransformRecord.ofAffineTransform ctm)
                          PdfCanvas.addRectangle transformedBound (defaultArg fArgs id)
                        ]
                    )

                 | IIntegratedRenderInfoIM.Vector renderInfo ->
                     ModifierPdfCanvasActions.Keep(renderInfo.Tag)



        static member SetMaximunDpi(maximunDpi: int) =
            fun (args: _SelectionModifierFixmentArgumentsIM<'userState>) ->
                match args.CurrentRenderInfoIM with 
                | IIntegratedRenderInfoIM.Pixel imageRenderInfo ->  
                    let imageData = imageRenderInfo.ImageData
                    match imageRenderInfo.ColorSpace with 
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
                                    bpc = 8,
                                    data = rgbByteValues,
                                    transparency = null
                                )
                    
                            let ctm = AffineTransformRecord.DefaultValue

                            ModifierPdfCanvasActions.NewImage(ctm, image)

                    | _ ->
                        ModifierPdfCanvasActions.KeepImage()
                        /// Some filters are not supported in itext, e.g DCT Decode filter, and so on 
                        //failwithf "Only rgb input image is supported for converting colorspace"


                | IIntegratedRenderInfoIM.Vector renderInfo ->
                    ModifierPdfCanvasActions.Keep(renderInfo.Tag)


    type ModifyIM =
        static member ConvertImagesToDeviceGray(?imageSelector) =
            Modify.Create_RecordIM(
                PageSelector.All,
                selectorAndModifiersList = [
                    { SelectorAndModifiersRecordIM.Name = "convert images to gray" 
                      Selector = Selector.ImageX(defaultArg imageSelector (fun _ _ -> true))
                      Modifiers = [ 
                        ModifierIM.ConvertImageToGray()
                      ]}
                ])

        static member ConvertAllObjectsToDeviceGray(?selector) =
            Modify.Create_RecordIM(
                PageSelector.All,
                selectorAndModifiersList = [
                    { SelectorAndModifiersRecordIM.Name = "convert all objects to gray" 
                      Selector = defaultArg selector (Selector.All (fun _ _ -> true))
                      Modifiers = [ 
                        ModifierIM.ConvertAllObjectsToDeviceGray()
                      ]}
                ])