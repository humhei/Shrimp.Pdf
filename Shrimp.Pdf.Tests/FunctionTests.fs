module FunctionTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
open Shrimp.Pdf.FileOperations

let functionTests =
  testList "Function Tests" [
    testCase "PageSelectorExpr tests" <| fun _ -> 
        let expr1 = PageSelectorExpr.create "1,2-R3"
        ()
  ]