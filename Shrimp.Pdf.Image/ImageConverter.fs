namespace Shrimp.Pdf.ImageConverter
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


    type ImageConverter = 
        static member ConvertPdfToJpeg(pdfFile: PdfFile, ?dpi, ?targetDir): Async<JpgFile list> =
            let targetDir = 
                defaultArg targetDir (Path.GetDirectoryName pdfFile.Path)
                |> FsFullPath

            let dpi = defaultArg dpi 300.f

            client.Value <? ServerMsg.ConvertPdfFileToImage(pdfFile, dpi, targetDir)
