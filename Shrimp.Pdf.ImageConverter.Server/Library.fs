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
        Shrimp.DtronixPdf.DtronixPdf.savePdfPageToTarget (dpi) target page

    let internal convertPdfFileToJpegs(pdfFile: PdfFile, dpi: float32, target: FsFullPath) =
        Shrimp.DtronixPdf.DtronixPdf.convertPdfFileToJpegs(pdfFile, dpi, target)
        

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


        
