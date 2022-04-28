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
        new ConcurrentDictionary<PdfImageXObject, RawFile * Size * ImageData>()

    let private client  = lazy GetDefaultClusterIcm2Client()


    type ImageRenderInfo with 
        member x.SaveToTmpColorValuesRawFile() =
            let image = x.GetImage()
            cache.GetOrAdd(image, fun image ->
                let bytes = image.GetImageBytes()
                let imageData = ImageDataFactory.Create(bytes)
                use stream = new MemoryStream(bytes)
                let bitmap = new Bitmap(stream)
                let colorValues = BitmapUtils.ReadColorValues(bitmap)
                let text =
                    colorValues.Values
                    |> Array.map string
                    |> String.concat "\n"
                let rawFile = Path.GetTempFileName() |> Path.changeExtension ".raw" 
                File.WriteAllBytes(rawFile, colorValues.Values)
                RawFile rawFile, bitmap.Size, imageData
            )

    type ImageData with 
        member x.FsColorSpace =
            match x.GetColorEncodingComponentsNumber() with 
            | 1 -> ColorSpace.Gray
            | 3 -> ColorSpace.Rgb
            | 4 -> ColorSpace.Cmyk
            | colorSpaceNumber -> failwithf "Cannot get colorSpace from %d" colorSpaceNumber

    type ModifierIM =
        static member ConvertImageColorSpace(inputIcc: Icc option, outputIcc: Icc, intent): ModifierIM<'userState> =
            fun (args: _SelectionModifierFixmentArgumentsIM<'userState>) ->
                match args.CurrentRenderInfoIM with 
                | IIntegratedRenderInfoIM.Pixel imageRenderInfo ->
                    let rawFile, size, imageData = imageRenderInfo.ImageRenderInfo.SaveToTmpColorValuesRawFile()
                    
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
                                        rawFile,
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

        static member SetMaximunDpi(maximunDpi: int) =
            fun (args: _SelectionModifierFixmentArgumentsIM<'userState>) ->
                match args.CurrentRenderInfoIM with 
                | IIntegratedRenderInfoIM.Pixel imageRenderInfo ->
                    let bmpFile, size, imageData = imageRenderInfo.ImageRenderInfo.SaveToTmpColorValuesRawFile()
                    failwith ""

                | IIntegratedRenderInfoIM.Vector renderInfo ->
                    ModifierPdfCanvasActions.Keep(renderInfo.Tag)
