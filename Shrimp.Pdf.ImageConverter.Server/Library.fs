namespace Shrimp.Pdf.ImageConverter.Server
open System.Collections.Generic
open Akkling
open Shrimp.FSharp.Plus
open Shrimp.Pdf.ImageConverter.Core.Cluster
open PDFiumCore
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Formats.Png
open Fake.IO
open SixLabors.ImageSharp.PixelFormats
open DtronixPdf
open DtronixCommon
open FSharp.Control.Tasks.V2
open System.IO

module RemoteServer =

    let internal savePdfPageToTarget (dpi: float32) (target: JpgPath) (page: PdfPage) =
        task {
            let size = page.Size
            let scale = dpi / 72.f
            let! renderImage = 
                page.Render(
                    scale,
                    System.Nullable Color.White,
                    viewport = RectangleF(
                        0.f,
                        0.f,
                        size.Width * scale,
                        size.Height * scale
                    )
                )
            let image = renderImage.Image

            image.Metadata.HorizontalResolution <- float dpi
            image.Metadata.VerticalResolution <- float dpi


            do! image.SaveAsJpegAsync(target.Path)
            return JpgFile target
        }

    let internal convertPdfFileToJpegs(pdfFile: PdfFile, dpi: float32, target: FsFullPath) =
        let pdfFile =
            let fileName = Path.GetFileName pdfFile.Path
            let tmpDir = Path.GetTempPath() </> System.IO.Path.GetRandomFileName()
            Directory.ensure tmpDir

            let tmpFile = tmpDir </> fileName

            File.Copy(pdfFile.Path, tmpFile, true)
            PdfFile tmpFile

        Directory.ensure target.Path
        task {
            let! document = PdfDocument.Load(pdfFile.Path, null)
            let totalPageNumber = document.Pages
            let target = 
                target.Path </> (Path.GetFileNameWithoutExtension pdfFile.Path + ".jpg")
                |> JpgPath

            match totalPageNumber with 
            | 1 ->
                let! page = document.GetPage(0)
                
                let! targetFile = savePdfPageToTarget dpi target page
                do! document.DisposeAsync()
                
                return [targetFile]

            | i ->
                let r =
                    [0..totalPageNumber-1]
                    |> List.map(fun pageNum ->
                        let task = task {
                            let! page = document.GetPage(pageNum)
                            let target = 
                                Path.ChangeExtension(target.Path, $"{pageNum}.jpg") 
                                |> JpgPath

                            return! savePdfPageToTarget dpi target page
                        }
                        task.Result
                    )
                do! document.DisposeAsync()
                return r
                

        }
        

    let createServer() =
        Server.createAgent(fun ctx ->
            let logger = ctx.Log.Value
            let rec loop () = actor {
                let! msg = ctx.Receive()
               
                match msg with
                | ServerMsg.ConvertPdfFileToImage (pdfFile, dpi, target) ->
                    //let! r = convertPdfFileToJpeg pdfFile
                    let r = convertPdfFileToJpegs(pdfFile, dpi, target).Result

                    logger.Info (sprintf "[SHRIMP SERVER IMAGE CONVERTER] Convert %O to %O %A" pdfFile target dpi)

                    ctx.Sender() <! r
            }
            loop ()
        )


        
