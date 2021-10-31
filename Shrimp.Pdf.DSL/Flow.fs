namespace Shrimp.Pdf
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open Shrimp.FSharp.Plus
open System.IO

[<AutoOpen>]
module DSL_Flow =
    type PdfRunner with 
        static member Flow(flow: Flow<_, _>, ?targetPdfFile) = 
            fun (inputPdfFile: PdfFile) ->
                let targetPdfFile = defaultArg targetPdfFile inputPdfFile.Path 

                match inputPdfFile.Path = targetPdfFile with 
                | false -> File.Copy(inputPdfFile.Path, targetPdfFile, true)
                | true -> ()

                run targetPdfFile (flow) 
                |> fun m -> m |> List.map (fun m -> m.PdfFile)


        static member OneFileFlow(flow: Flow<_, _>, ?targetPdfFile) = 
            fun (inputPdfFile: PdfFile) ->
            
                match PdfRunner.Flow(flow, ?targetPdfFile = targetPdfFile) inputPdfFile with 
                | [pdfFile] -> pdfFile
                | [] -> failwith "Invalid token"
                | pdfFiles -> failwithf "Multiple pdfFiles %A are found" pdfFiles
