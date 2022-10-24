module FlowTests
open Expecto
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
open Shrimp.Pdf.DSL
open Shrimp.FSharp.Plus
open Shrimp.Pdf.Colors
open Shrimp.Pdf.Image
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
                    (TextInfos.ExistsLine(fun text -> text.ConcatedText() = "36->41"))
            )
        )
        |> runTest "datas/flows/filterPages_infos.pdf" 
        |> ignore

    testCase "filterPages_infos2 with duplicated texts" <| fun _ ->  
        Flows.FilterPages(
            PageFilter(
                InfosSelector.Text(fun args infos ->
                    (TextInfos.ExistsLine(fun text -> text.Contains "EXPE")) args infos
                )
            )
        )
        |> runTest "datas/flows/filterPages_infos2.pdf" 
        |> ignore

    testCase "overly and manipulate clippingArea test" <| fun _ ->  
        let excludingSelector = Selector.Path(Info.StrokeColorIs FsColor.RGB_RED)
        Flows.Overly_Clip_ConvertAllObjectsToGray(
             Info.StrokeColorIs FsColor.RGB_BLUE,
             area=Overly_Clip_ManipulateArea.ClippingArea,
             excludingSelector = excludingSelector
        )
        |> runTest "datas/flows/overly and manipulate clippingArea.pdf" 
        |> ignore

    testCase "overly and manipulate clippingArea test2" <| fun _ ->  
        let varnishColor =
            FsDeviceRgb.Create(0, 204, 255)
            |> FsValueColor.Rgb
            |> FsColor.valueColor

        //let excludingSelector = Selector.Path(Info.StrokeColorIs FsColor.RGB_BLUE)
        Flows.Overly_Clip_ConvertAllObjectsToGray(
             Info.StrokeColorIs varnishColor,
             area=Overly_Clip_ManipulateArea.ClippingArea
             //excludingSelector = excludingSelector
        )
        |> runTest "datas/flows/overly and manipulate clippingArea2.pdf" 
        |> ignore

  ]