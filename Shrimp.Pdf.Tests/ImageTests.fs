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
open Shrimp.Pdf.ImageConverter
open Shrimp.Pdf.Image
let imageTests = 
    testList "ImageTests" <| [
    


        testCase "convert pdf to jpeg" <| fun _ -> 
            let pdfFile =
                @"datas/image/convert image to jpeg.pdf" 
                |> PdfFile

            let jpegs = 
                ImageConverter.ConvertPdfToJpeg(pdfFile)
                |> Async.RunSynchronously

            ()

        testCase "change blending mode" <| fun _ -> 
            let flow = ModifyIM.ChangeImageBlendMode(BlendMode.Multiply)

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/change blending mode.pdf" 
            |> ignore


        testCase "convert rgb image to gray" <| fun _ -> 
            let flow = ModifyIM.ConvertImagesToDeviceGray()

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/convert rgb image to gray.pdf" 
            |> ignore

        testCase "convert index rgb image to gray" <| fun _ -> 
            let flow = ModifyIM.ConvertImagesToDeviceGray()

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/convert index rgb image to gray.pdf" 
            |> ignore



        testCase "convert rgb image mask to gray" <| fun _ -> 
            let flow =
                Modify.Create_RecordIM(
                    PageSelector.All,
                    selectorAndModifiersList = [
                        { SelectorAndModifiersRecordIM.Name = "convert rgb image mask to gray" 
                          Selector = Selector.ImageX(fun _ _ -> true)
                          Modifiers = [
                            ModifierIM.ConvertImageToGray()
                          ]}
                    ]
                    )

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/convert rgb image mask to gray.pdf" 
            |> ignore

        testCase "add image border" <| fun _ -> 
            let flow =
                Modify.Create_RecordIM(
                    PageSelector.All,
                    selectorAndModifiersList = [
                        { SelectorAndModifiersRecordIM.Name = "add image border" 
                          Selector = Selector.ImageX(fun _ _ -> true)
                          Modifiers = [
                            ModifierIM.AddImageBorder()
                          ]}
                    ]
                )

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/add image border.pdf" 
            |> ignore

        testCase "add image border2" <| fun _ -> 
            let flow =
                Modify.Create_RecordIM(
                    PageSelector.All,
                    selectorAndModifiersList = [
                        { SelectorAndModifiersRecordIM.Name = "add image border" 
                          Selector = Selector.ImageX(fun args m -> 
                            let pageBox = args.Page.GetActualBox()
                            match m.VisibleBound() with 
                            | Some bound -> 
                                let b = bound.IsInsideOf(pageBox)
                                if b 
                                then b 
                                else b
                            | None -> false
                          )
                          Modifiers = [
                            ModifierIM.AddImageBorder(fun args ->
                                { args with StrokeColor = NullablePdfCanvasColor.valueColor FsDeviceCmyk.CYAN }
                            )

                          ]}
                    ]
                )

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/add image border2.pdf" 
            |> ignore

        testCase "convert rgb image to gray inside pageBox" <| fun _ -> 
            let flow =
                ModifyIM.ConvertImagesToDeviceGray(fun args image ->
                    let actualBox = args.Page.GetActualBox()
                    let bound = image.VisibleBound()
                    match bound with 
                    | Some bound -> bound.IsInsideOf(actualBox)
                    | None -> false
                )
               

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/convert rgb image to gray inside pageBox.pdf" 
            |> ignore



        testCase "convert all objects to gray" <| fun _ -> 
            let flow = ModifyIM.ConvertAllObjectsToDeviceGray()

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/convert all objects to gray.pdf" 
            |> ignore


        testCase "convert all objects to gray2" <| fun _ -> 
            let flow = ModifyIM.ConvertAllObjectsToDeviceGray()

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/convert all objects to gray2.pdf" 
            //|> runTest @"C:\Users\Jia\Desktop\convert all objects to gray22.pdf"
            |> ignore

        testCase "convert all objects to gray3" <| fun _ -> 
            let flow = 
                ModifyIM.ConvertAllObjectsToDeviceGray(
                    Selector.All(
                        InfoIM.BoundIs(
                            RelativePosition.Inbox,
                            AreaGettingOptions.PageBox(PageBoxKind.ActualBox)))
                    )

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/convert all objects to gray3.pdf" 
            |> ignore

        testCase "convert all objects to gray4" <| fun _ -> 
            let flow = ModifyIM.ConvertAllObjectsToDeviceGray()

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/convert all objects to gray4.pdf" 
            |> ignore

        testCase "convert all objects to gray5" <| fun _ -> 
            let flow = 
                ModifyIM.ConvertAllObjectsToDeviceGray(
                    Selector.All(
                        InfoIM.BoundIs(
                            RelativePosition.Inbox,
                            AreaGettingOptions.PageBox(PageBoxKind.ActualBox)))
                    )

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/convert all objects to gray5.pdf" 
            |> ignore

        testCase "convert all objects to gray6" <| fun _ -> 
            let flow = 
                ModifyIM.ConvertAllObjectsToDeviceGray(
                    Selector.All(
                        InfoIM.BoundIs(
                            RelativePosition.Inbox,
                            AreaGettingOptions.PageBox(PageBoxKind.ActualBox)))
                    )

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/convert all objects to gray6.pdf" 
            |> ignore

        testCase "set image maximun dpi to 150" <| fun _ -> 
            let flow =
                Modify.Create_RecordIM(
                    PageSelector.All,
                    selectorAndModifiersList = [
                        { SelectorAndModifiersRecordIM.Name = "set image maximun dpi to 150" 
                          Selector = Selector.ImageX(fun args image -> true)
                          Modifiers = [
                            ModifierIM.SetMaximunDpi(150)
                          ]}
                    ])

            Flow.Manipulate(
                flow
            )
            |> runTest "datas/image/set image maximun dpi to 150.pdf" 
            |> ignore

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

        testCase "convert cmyk image to gray3" <| fun _ -> 
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
            //|> runTest "D:\VsCode\Workspace\Shrimp.Pdf\Shrimp.Pdf.Tests\datas\image\cmyk\C.pdf"
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
            |> runTest "datas\image\convert cmyk image to gray4.pdf"
            |> ignore

        testCase "convert cmyk image to gray5" <| fun _ -> 
            let flow =
                Modify.Create_RecordIM(
                    PageSelector.All,
                    selectorAndModifiersList = [
                        { SelectorAndModifiersRecordIM.Name = "convert cmyk image to gray5" 
                          Selector = Selector.ImageX(fun _ _ -> true)
                          Modifiers = [
                            ModifierIM.ConvertImageToGray()
                          ]}
                    ]
                    )

            Flow.Manipulate(
                flow
            )
            |> runTest "datas\image\convert cmyk image to gray5.pdf"
            |> ignore

        testCase "convert cmyk image to gray6" <| fun _ -> 
            let flow =
                Modify.Create_RecordIM(
                    PageSelector.All,
                    selectorAndModifiersList = [
                        { SelectorAndModifiersRecordIM.Name = "convert cmyk image to gray6" 
                          Selector = Selector.ImageX(fun _ _ -> true)
                          Modifiers = [
                            ModifierIM.ConvertImageToGray()
                          ]}
                    ]
                    )

            Flow.Manipulate(
                flow
            )
            |> runTest "datas\image\cmyk\cmyk2.pdf"
            |> ignore



    ]