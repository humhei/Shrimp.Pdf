module ManipulateTests
open Expecto
open Shrimp.Pdf.Reuses
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Fake.IO
open System.IO
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.Kernel.Font
open iText.IO.Font


let manipulateTests =
  testList "Manipulates Tests" [
    testCase "change stroke color b255 to m100" <| fun _ -> 
        [
            manipulates PageSelector.First [] [
                (
                    RenderInfoSelector.Path (fun pathRenderInfo -> 
                        pathRenderInfo.GetStrokeColor() = DeviceRgb.BLUE),
                    SelectionModifier.Modify [
                        PdfCanvas.setStrokeColor DeviceCmyk.MAGENTA
                    ]
                )
            ]
        ]
        |> runWithBackup "datas/manipulate/change stroke color b255 to m100.pdf" 
        |> ignore

    testCase "add bound to text" <| fun _ -> 
        [
            manipulates PageSelector.All [] [
                (
                    RenderInfoSelector.Text (fun _textRenderInfo -> true),
                    SelectionModifier.AddNew (fun args -> 
                        let textRenderInfo = args.CurrentRenderInfo :?> TextRenderInfo
                        let border = TextRenderInfo.getBound textRenderInfo
                        [
                            PdfCanvas.setStrokeColor DeviceCmyk.CYAN
                            PdfCanvas.setLineWidth (mm 0.1)
                            PdfCanvas.rectangle border
                            PdfCanvas.stroke
                        ])
                )
            ]
        ]
        |> runWithBackup "datas/manipulate/add bound to text.pdf" 
        |> ignore

    testCase "add text to position" <| fun _ -> 
        [
            manipulates 
                PageSelector.All 
                [
                    (PageBoxKind.ActualBox, [
                        Canvas.addText 
                            "你好" 
                            (PdfFontFactory.Registerable (RegisterableFonts.AlibabaPuHuiTiBold))
                            (CanvasFontSize.Numeric 25.)
                            DeviceCmyk.MAGENTA
                            Rotation.None
                                (Position.LeftTop(0., 0.))
                    ])
                ] 
                []
        ]
        |> runWithBackup "datas/manipulate/add text to position.pdf" 
        |> ignore
    
    testCase "add page-scale text" <| fun _ -> 
        [
            manipulates 
                PageSelector.All 
                [
                    (PageBoxKind.ActualBox, [
                        Canvas.addText 
                            "你好" 
                            (PdfFontFactory.Registerable (RegisterableFonts.AlibabaPuHuiTiBold))
                            (CanvasFontSize.OfRootArea 0.8)
                            DeviceCmyk.MAGENTA
                            Rotation.None
                            (Position.Center(0., 0.)) 
                    ])
                ]
                []
        ]
        |> runWithBackup "datas/manipulate/add page-scale text.pdf" 
        |> ignore


  ]