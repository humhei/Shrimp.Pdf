module FileOperationTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
open Shrimp.Pdf.FileOperations

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

  ]