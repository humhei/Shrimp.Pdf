module FunctionTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
open Shrimp.Pdf.icms2
open Shrimp.Pdf.Colors

let functionTests =
  testList "Function Tests" [
    testCase "PageSelectorExpr tests" <| fun _ -> 
        //(ColorCard.Pantone PantoneColorEnum.``PANTONE 100 C``)
        //|> PdfCanvasColor.ColorCard
        //|> PdfCanvasColor.OfITextColor
        //let expr1 = PageSelectorExpr.create "1,2-R3"
        ()


  ]