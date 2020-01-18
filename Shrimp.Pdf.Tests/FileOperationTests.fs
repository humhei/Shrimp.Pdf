module FileOperationTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
open Fake.IO.Globbing.Operators
open Shrimp.Pdf.FileOperations
open System.IO

let fileOperationTests =
  testList "FileOperation Tests" [
    testCase "split document" <| fun _ -> 
        Flow.FileOperation (
            splitDocumentToMany (fun args ->
                { args with 
                    ChunkSize = 2
                    Override = true }
            )
        )
        |> runWithBackup "datas/file operation/split document.pdf" 
        |> ignore

    testCase "merge documents" <| fun _ -> 
        let targetFile = Path.GetFullPath("datas/file operation/merge documents/mergdDocuments.pdf")
        
        let inputFlowModels = 
            !! "datas/file operation/merge documents/*.pdf"
            |> List.ofSeq
            |> List.map (fun file ->
                { File = file 
                  UserState = () }
            )

        Flow.FileOperation (
            mergeDocuments (fun args ->
                { args with 
                    Override = true
                    TargetDocumentName = targetFile }
            )
        )
        |> runMany inputFlowModels
        |> ignore

    testCase "run many tests" <| fun _ -> 
        let targetFile = Path.GetFullPath("datas/file operation/merge documents/mergdDocuments.pdf")
        
        let inputFiles = 
            !! "datas/file operation/merge documents/inputs/*.pdf"
            |> List.ofSeq


        Flow.Reuse(
            Reuses.Resize(
                PageSelector.All,
                PageBoxKind.ActualBox,
                { Width = mm 6. 
                  Height = mm 5. }
            )
        )
        <+>
        Flow.FileOperation (
            mergeDocuments (fun args ->
                { args with 
                    Override = true
                    TargetDocumentName = targetFile }
            )
        )
        |> runManyWithBackup  inputFiles "datas/file operation/merge documents/outputs/"
        |> ignore

  ]