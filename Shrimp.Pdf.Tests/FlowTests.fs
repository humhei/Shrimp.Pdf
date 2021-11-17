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
        Flows.FilterPages(PageFilter(Text(TextInfo.TextContainsIC("15"))))
        |> runTest "datas/flows/filterPages.pdf" 
        |> ignore

    testCase "filterPages_infos" <| fun _ -> 
        Flows.FilterPages(
            PageFilter(
                InfosSelector.Text
                    (TextInfos.ExistsText_In_OneLine(fun text -> text = "36 -> 41"))
            )
        )
        |> runTest "datas/flows/filterPages_infos.pdf" 
        |> ignore

  ]