namespace Shrimp.Pdf.ImageConverter

open iText.Kernel.Pdf
open iText.Kernel.Pdf.Xobject
open iText.IO.Image
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Geom
open Shrimp.Pdf
open Shrimp.Pdf.Extensions

#nowarn "0104"
open Shrimp.FSharp.Plus
open System.IO
open Shrimp.Pdf.ImageConverter.Core
open Shrimp.Pdf.ImageConverter.Core.Cluster
open Akkling

[<AutoOpen>]
module _ImageConverter =
    let private client =
        lazy Client.create()


    [<RequireQualifiedAccess>]
    type ResizingTargetLength =    
        | Length of float
        | Vertical of float
        | Horizontal of float

    type ImageConverter = 
        static member ConvertPdfToJpeg(pdfFile: PdfFile, ?dpi, ?targetDir): Async<JpgFile list> =
            let targetDir = 
                defaultArg targetDir (Path.GetDirectoryName pdfFile.Path)
                |> FsFullPath

            let dpi = defaultArg dpi 300.f

            client.Value <? ServerMsg.ConvertPdfFileToImage(pdfFile, dpi, targetDir)

        static member ConvertImageToPdf(image: string, targetPath: PdfPath, ?targetLength: ResizingTargetLength, ?pageOrientation) =
            let pdfDoc = new PdfDocument(new PdfWriter(targetPath.Path))
            let image = ImageDataFactory.Create(image)
            let width = image.GetWidth()
            let height = image.GetHeight()

            let rect = 
                match targetLength with 
                | None -> Rectangle(width, height)
                | Some length ->    
                    match length with 
                    | ResizingTargetLength.Length length ->
                        let length = float32 length
                        let scaleX = 
                            length / width

                        let scaleY = 
                            length / height

                        let scale = min scaleX scaleY

                        Rectangle(width * scale, height * scale)


                    | ResizingTargetLength.Vertical length ->
                        let length = float32 length
                        let scale = length / height
                        Rectangle(width * scale, height * scale)


                    | ResizingTargetLength.Horizontal length ->
                        let length = float32 length
                        let scale = length / width
                        Rectangle(width * scale, height * scale)


            let canvas = new PdfCanvas(pdfDoc.AddNewPage(PageSize rect))

            canvas.AddImageFittedIntoRectangle(image, rect, asInline = false)
            |> ignore

            pdfDoc.Close()

            let pdfFile = PdfFile targetPath

            match pageOrientation with 
            | None -> pdfFile
            | Some pageOrientation ->
                let needRotation =
                    match FsSize.ofRectangle rect, pageOrientation with 
                    | FsSize.Uniform, _ -> false
                    | FsSize.Portrait, PageOrientation.Portrait
                    | FsSize.Landscape, PageOrientation.Landscape -> false
                    | _ -> true
                    
                match needRotation with 
                | true ->
                    Reuses.ChangePageOrientation(PageSelector.All, pageOrientation)
                    |> PdfRunner.Reuse(pdfFile)

                | false -> pdfFile