module FileOperationTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
open Fake.IO.Globbing.Operators
open System.IO
open Shrimp.FSharp.Plus

let fileOperationTests =
  testList "FileOperation Tests" [
    testCase "split document" <| fun _ -> 
        Flow.FileOperation (
            FileOperations.splitDocumentToMany (fun args ->
                { args with 
                    ChunkSize = 2
                    Override = true }
            )
        )
        |> runTest "datas/file operation/split document.pdf" 
        |> ignore

    testCase "split document PdfRunner" <| fun _ ->
        PdfRunner.SplitDocumentToMany(PdfFile "datas/file operation/split document.pdf")
        |> ignore

    testCase "merge documents" <| fun _ -> 
        let targetFile = Path.GetFullPath("datas/file operation/merge documents/mergdDocuments.pdf")
        
        let inputFiles = 
            !! "datas/file operation/merge documents/inputs/*.pdf"
            |> List.ofSeq


        Flow.FileOperation (
            FileOperations.mergeDocuments (fun args ->
                { args with 
                    Override = true
                    TargetDocumentPath = targetFile }
            )
        )
        |> runMany inputFiles
        |> ignore

    ftestCase "merge documents PdfRunner" <| fun _ ->
        let inputFiles = 
            !! "datas/file operation/merge documents/inputs/*.pdf"
            |> List.ofSeq
            |> List.map PdfFile
            |> AtLeastTwoList.Create

        let targetFile = Path.GetFullPath("datas/file operation/merge documents/mergdDocuments.pdf")
        PdfRunner.MergeDocuments(inputFiles, fArgs = fun args ->
            { args with 
                Override = true
                TargetDocumentPath = targetFile }
        )
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
            FileOperations.mergeDocuments (fun args ->
                { args with 
                    Override = true
                    TargetDocumentPath = targetFile }
            )
        )
        |> runManyWithBackup inputFiles "datas/file operation/merge documents/outputs/"
        |> ignore

  ]