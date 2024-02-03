namespace Shrimp.DtronixPdf
open Shrimp.FSharp.Plus
open SixLabors.ImageSharp
open Fake.IO
open DtronixPdf
open FSharp.Control.Tasks.V2
open System.IO
open NLog

module DtronixPdf =
    let savePdfPageToTarget  (dpi: float32) (target: JpgPath) (page: PdfPage) =
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

    let convertPdfFileToJpegs(pdfFile: PdfFile, dpi: float32, target: FsFullPath) =
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
                let! r =
                    [0..totalPageNumber-1]
                    |> List.map(fun pageNum ->
                        let task = task {
                            let! page = document.GetPage(pageNum)
                            let target = 
                                Path.ChangeExtension(target.Path, $"{pageNum}.jpg") 
                                |> JpgPath

                            return! savePdfPageToTarget dpi target page
                        }
                        task
                    )
                    |> System.Threading.Tasks.Task.WhenAll
                do! document.DisposeAsync()
                return (List.ofArray r)
                

        }
        

        
