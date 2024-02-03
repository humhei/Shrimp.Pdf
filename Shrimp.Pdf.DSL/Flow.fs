namespace Shrimp.Pdf
open Shrimp.FSharp.Plus
open System.IO

[<AutoOpen>]
module DSL_Flow =
    type PdfRunner with 
        static member private Flow(pdfFile: PdfFile, ?backupPdfPath, ?config) = 
            fun flow ->
                let targetPdfFile = defaultArg backupPdfPath pdfFile.Path 

                match pdfFile.Path = targetPdfFile with 
                | false -> File.Copy(pdfFile.Path, targetPdfFile, true)
                | true -> ()

          

                runWith (defaultArg config Configuration.DefaultValue) targetPdfFile (flow) 
                |> fun m -> m |> List.map (fun m -> m.PdfFile)


        static member OneFileFlow(pdfFile: PdfFile, ?backupPdfPath, ?config) = 
            
            fun (flow) ->
                match PdfRunner.Flow(pdfFile, ?backupPdfPath = backupPdfPath, ?config = config) flow with 
                | [pdfFile] -> pdfFile
                | [] -> failwith "Invalid token"
                | pdfFiles -> failwithf "Multiple pdfFiles %A are found" pdfFiles


        static member OneFileFlow__FlowModel(pdfFile: PdfFile, ?backupPdfPath, ?config) = 
            
            fun flow ->
                let targetPdfFile = defaultArg backupPdfPath pdfFile.Path 

                match pdfFile.Path = targetPdfFile with 
                | false -> File.Copy(pdfFile.Path, targetPdfFile, true)
                | true -> ()

                match runWith (defaultArg config Configuration.DefaultValue) targetPdfFile (flow) with 
                | [flowModel] -> flowModel
                | [] -> failwith "Invalid token"
                | flowModels -> failwithf "Multiple flowModels %A are found" flowModels

        static member OneFileFlow_UserState(pdfFile: PdfFile, ?backupPdfPath, ?config) = 
            fun flow ->
                let flowModel = 
                    flow
                    |> PdfRunner.OneFileFlow__FlowModel(pdfFile, ?backupPdfPath = backupPdfPath, ?config = config)
                
                flowModel.UserState
