module FlowTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Colors
open Shrimp.Pdf.RegisterableFonts.YaHei

let flowTests =

  testList "Flow Tests" [
    testCase "filterPages" <| fun _ -> 
        Flows.FilterPages(Text(TextInfo.TextContainsIC("15")))
        |> runTest "datas/flows/filterPages.pdf" 
        |> ignore

  ]