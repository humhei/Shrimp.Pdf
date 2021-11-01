namespace Shrimp.Pdf
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open Shrimp.FSharp.Plus
open System.IO

[<AutoOpen>]
module DSL_Flow =
    type PdfRunner with 
        static member private Flow(pdfFile: PdfFile, ?backupPdfPath) = 
            fun flow ->
                let targetPdfFile = defaultArg backupPdfPath pdfFile.Path 

                match pdfFile.Path = targetPdfFile with 
                | false -> File.Copy(pdfFile.Path, targetPdfFile, true)
                | true -> ()

                run targetPdfFile (flow) 
                |> fun m -> m |> List.map (fun m -> m.PdfFile)


        static member OneFileFlow(pdfFile: PdfFile, ?backupPdfPath) = 
            
            fun (flow) ->
                match PdfRunner.Flow(pdfFile, ?backupPdfPath = backupPdfPath) flow with 
                | [pdfFile] -> pdfFile
                | [] -> failwith "Invalid token"
                | pdfFiles -> failwithf "Multiple pdfFiles %A are found" pdfFiles
