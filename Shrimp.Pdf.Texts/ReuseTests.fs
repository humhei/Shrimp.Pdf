module ReuseTests
open Expecto
open Shrimp.Pdf.Reuses
open Shrimp.Pdf
open Shrimp.Pdf.Imposing

let reuseTests =
  testList "Reuse Tests" [
    testCase "EOrder with items that has different types" <| fun _ -> 
        [impose (ImposingArguments.Create(fun arg -> 
            {arg with 
                BackgroundOp = 
                    Some { TableAlignment = TableAlignment.Trimmed 
                           Kind = BackgroundKind.PageSize FsPageSize.A3}}))]
        |> run "datas/reuse/impose.pdf" 
        |> ignore
  ]