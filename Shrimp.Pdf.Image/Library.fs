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
        new ConcurrentDictionary<PdfImageXObject, BitmapColorValuesStorage * ImageData>()

    let private client  = lazy GetDefaultClusterIcm2Client()




    type ImageRenderInfo with 

        member x.SaveToTmpColorValuesStorage() =
            let image = x.GetImage()
            cache.GetOrAdd(image, fun image ->
                let bytes = image.GetImageBytes()

                let imageData = ImageDataFactory.Create(bytes)

                let storage = 
                    use stream = new MemoryStream(bytes)
                    use bitmap = new Bitmap(stream)
                    let colorValues = BitmapUtils.ReadColorValues(bitmap)
                    colorValues.ToStorage()

                storage, imageData
            )



    type ImageData with 
        member x.FsColorSpace =
            match x.GetColorEncodingComponentsNumber() with 
            | 1 -> ColorSpace.Gray
            | 3 -> ColorSpace.Rgb
            | 4 -> ColorSpace.Cmyk
            | colorSpaceNumber -> failwithf "Cannot get colorSpace from %d" colorSpaceNumber

        //member imageData.FsSize() = 
        //    let dpi_x = imageData.GetDpiX()
        //    let dpi_y = imageData.GetDpiY()

        //    let size_px =
        //        { Width =  imageData.GetWidth() |> float
        //          Height = imageData.GetHeight()|> float }
            
        //    let size_inch =
        //        { Width =  size_px.Width  / float dpi_x
        //          Height = size_px.Height / float dpi_y }

        //    size_inch.MapValue(inchToMM)


    type ModifierIM =
        static member ConvertImageColorSpace(inputIcc: Icc option, outputIcc: Icc, intent): ModifierIM<'userState> =
            fun (args: _SelectionModifierFixmentArgumentsIM<'userState>) ->
                match args.CurrentRenderInfoIM with 
                | IIntegratedRenderInfoIM.Pixel imageRenderInfo ->
                    let storage, imageData = imageRenderInfo.ImageRenderInfo.SaveToTmpColorValuesStorage()
                    let size = storage.Size
                    let inputIcc = 
                        match inputIcc with 
                        | Some inputIcc ->
                            match inputIcc.ColorSpace = imageData.FsColorSpace with
                            | false -> Some inputIcc
                            | true -> None

                        | _ -> 
                            ColorSpace.DefaultIcc imageData.FsColorSpace
                            |> Some
            
                    match inputIcc with 
                    | Some inputIcc ->
                        match inputIcc = outputIcc with 
                        | true -> ModifierPdfCanvasActions.KeepImage()
                        | false ->  
                            match inputIcc with 
                            | Icc.Cmyk _ 
                            | Icc.Gray _ 
                            | Icc.Lab _ ->  
                                ModifierPdfCanvasActions.KeepImage()
                                /// Some filters are not supported in itext, e.g DCT Decode filter, and so on 
                                //failwithf "Only rgb input image is supported for converting colorspace"

                            | Icc.Rgb _ ->
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



        //static member SetMaximunDpi(maximunDpi: int) =
        //    fun (args: _SelectionModifierFixmentArgumentsIM<'userState>) ->
        //        match args.CurrentRenderInfoIM with 
        //        | IIntegratedRenderInfoIM.Pixel imageRenderInfo ->
        //            let bmpFile, size, imageData = imageRenderInfo.ImageRenderInfo.SaveToTmpColorValuesRawFile()
        //            failwith ""

        //        | IIntegratedRenderInfoIM.Vector renderInfo ->
        //            ModifierPdfCanvasActions.Keep(renderInfo.Tag)
