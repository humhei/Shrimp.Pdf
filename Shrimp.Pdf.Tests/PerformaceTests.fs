module PerformanceTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open iText.Layout
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Extract
open Shrimp.Pdf.icms2
open Shrimp.FSharp.Plus
open Shrimp.Pdf.RegisterableFonts
open Shrimp.Pdf.RegisterableFonts.YaHei
open FontNames.Query
open Imposing
open Shrimp.Pdf.Parser
open Fake.IO
open iText.IO.Font.Constants
open iText.IO.Font
open iText.Kernel.Font
open Shrimp.Pdf.Image




let performanceTests =
  testList "Performance Tests" [
    testCase "Duplicate(Page.Copy) and modifyPage" <| fun _ ->
        let flow = 
            Flow.Reuse(
                Reuses.DuplicatePages(PageSelector.All, 100)
            )
            <+>
            Flow.Manipulate(
                ModifyPage.AddRectangleToCanvasRootArea(
                    AreaGettingOptions.PageBoxWithOffset(PageBoxKind.ActualBox, -Margin.MM6)
                )
            )

        flow
        |> runTest "datas/performance/Duplicate(Page.Copy) and modifyPage.pdf" 
        |> ignore


        pass()

    testCase "replace colors by copied xobjects" <| fun _ -> 
        Flow.Reuse(
            Reuses.DuplicatePages(PageSelector.All, 2)
        )
        <+>
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "replace colors by copied xobjects"
                      Selector = PathOrText(Info.FillColorIs FsColor.RGB_RED)
                      Modifiers = [Modifier.SetFillColor(DeviceGray.BLACK)]
                    }
                ]
            )  
        )
        |> runTest "datas/performance/replace colors by copied xobjects.pdf" 
        |> ignore

    testCase "replace colors by ref xobjects" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "replace colors by ref xobjects"
                      Selector = PathOrText(Info.FillColorIs FsColor.RGB_RED)
                      Modifiers = [Modifier.SetFillColor(DeviceGray.BLACK)]
                    }
                ],
                ops = PdfModifyOptions.ByRef
            )  
        )
        |> runTest "datas/performance/replace colors by ref xobjects.pdf" 
        |> ignore

  ]