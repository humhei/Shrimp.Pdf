module icms2Tests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open Shrimp.Pdf.icms2
open Shrimp.Pdf.RegisterableFonts.YaHei
open Imposing
open Shrimp.Pdf.icms2.client


let icmsTests =
  testList "icm tests" [
      testCase "black or white" <| fun _ -> 
          Flow.Manipulate (
              Modify.Create (
                  PageSelector.First,
                  [
                      { Name = "black or white"
                        Selector = PathOrText(fun _ _ -> true)
                        Modifiers = [Modifier.BlackOrWhite()] }
                        //Modifiers = [Modifier.ConvertColorsTo(Icc.Cmyk CmykIcc.JapanColor2001Coated)] }
                  ]
              )
          )
          |> runTest "datas/icms2/black or white.pdf" 
          |> ignore

      testCase "black or white2" <| fun _ -> 
          Flow.Manipulate (
              Modify.Create (
                  PageSelector.First,
                  [
                      { Name = "black or white2"
                        Selector = PathOrText(fun _ _ -> true)
                        Modifiers = [] }
                        //Modifiers = [Modifier.ConvertColorsTo(Icc.Cmyk CmykIcc.JapanColor2001Coated)] }
                  ]
              )
          )
          |> runTest "datas/icms2/black or white2.pdf" 
          |> ignore

      testCase "black or white inversed" <| fun _ -> 
          Flows.BlackOrWhite_Negative_Film(strokeWidthIncrement = StrokeWidthIncrenment.Create 0.3)
          |> runTest "datas/icms2/black or white inversed.pdf" 
          |> ignore
  ]