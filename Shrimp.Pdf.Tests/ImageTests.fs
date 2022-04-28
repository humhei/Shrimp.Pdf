module ImageTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
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
let imageTests = 
    testList "ImageTests" <| [
    
        //ftestCase "convert rgb image to gray" <| fun _ -> 
        //    let flow =
        //        Modify.Create_RecordIM(
        //            PageSelector.All,
        //            selectorAndModifiersList = [
        //                { SelectorAndModifiersRecordIM.Name = "convert rgb image to gray" 
        //                  Selector = Selector.ImageX(fun _ _ -> true)
        //                  Modifiers = [
        //                    ModifierIM.ConvertImageToGray()
        //                  ]}
        //            ]
        //            )

        //    Flow.Manipulate(
        //        flow
        //    )
        //    |> runTest "datas/image/convert rgb image to gray.pdf" 
        //    |> ignore

        //ftestCase "convert rgb image to gray2" <| fun _ -> 
        //    let flow =
        //        Modify.Create_RecordIM(
        //            PageSelector.All,
        //            selectorAndModifiersList = [
        //                { SelectorAndModifiersRecordIM.Name = "convert rgb image to gray" 
        //                  Selector = Selector.ImageX(fun _ _ -> true)
        //                  Modifiers = [
        //                    ModifierIM.ConvertImageToGray()
        //                  ]}
        //            ]
        //            )

        //    Flow.Manipulate(
        //        flow
        //    )
        //    |> runTest "datas/image/convert rgb image to gray2.pdf" 
        //    |> ignore

        testCase "convert cmyk image to gray1" <| fun _ -> 
            let flow =
                Modify.Create_RecordIM(
                    PageSelector.All,
                    selectorAndModifiersList = [
                        { SelectorAndModifiersRecordIM.Name = "convert cmyk image to gray1" 
                          Selector = Selector.ImageX(fun _ _ -> true)
                          Modifiers = [
                            ModifierIM.ConvertImageToGray()
                          ]}
                    ]
                    )

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/convert cmyk image to gray1.pdf" 
            |> ignore

        testCase "convert cmyk image to gray2" <| fun _ -> 
            let flow =
                Modify.Create_RecordIM(
                    PageSelector.All,
                    selectorAndModifiersList = [
                        { SelectorAndModifiersRecordIM.Name = "convert cmyk image to gray2" 
                          Selector = Selector.ImageX(fun _ _ -> true)
                          Modifiers = [
                            ModifierIM.ConvertImageToGray()
                          ]}
                    ]
                    )

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/convert cmyk image to gray2.pdf" 
            |> ignore

        ftestCase "convert cmyk image to gray3" <| fun _ -> 
            let flow =
                Modify.Create_RecordIM(
                    PageSelector.All,
                    selectorAndModifiersList = [
                        { SelectorAndModifiersRecordIM.Name = "convert cmyk image to gray3" 
                          Selector = Selector.ImageX(fun _ _ -> true)
                          Modifiers = [
                            ModifierIM.ConvertImageToGray()
                          ]
                        }
                    ]
                )

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/convert cmyk image to gray3.pdf" 
            //|> runTest "D:\VsCode\Workspace\Shrimp.Pdf\Shrimp.Pdf.Tests\datas\manipulate\cmyk\C.pdf"
            |> ignore

        testCase "convert cmyk image to gray4" <| fun _ -> 
            let flow =
                Modify.Create_RecordIM(
                    PageSelector.All,
                    selectorAndModifiersList = [
                        { SelectorAndModifiersRecordIM.Name = "convert cmyk image to gray4" 
                          Selector = Selector.ImageX(fun _ _ -> true)
                          Modifiers = [
                            ModifierIM.ConvertImageToGray()
                          ]}
                    ]
                    )

            Flow.Manipulate(
                flow
            )
            |> runTest "datas\manipulate\convert cmyk image to gray4.pdf"
            |> ignore
    
    ]