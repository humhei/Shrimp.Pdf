module ParsingTests
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



let parsingTests =
  testList "Parsing Tests" [
    testCase "extract concated text" <| fun _ -> 
        let path = Path.getFullName "datas/parsing/extract concated text.pdf"
        let backUp = Path.changeExtension ".tests.pdf" path
        let texts = PdfRunner.ReadTextInfos(PdfFile path, backUp)
        match texts.[0].[0].Text with 
        | "EXPEDITEUR:" -> pass()
        | _ -> fail()

    ftestCase "extract dataTable in B255" <| fun _ -> 

        let flow =
            ModifyPage.ReadDataTable(
                { ColNum = 2 }, boundSelector = Info.StrokeColorIs FsColor.RGB_BLUE
            )

        Flow.Manipulate flow
        |> runTest "datas/parsing/extract dataTable in B255.pdf" 
        |> ignore
    ]