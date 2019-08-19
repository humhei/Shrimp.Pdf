module ManipulateTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Fake.IO
open System.IO
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.Kernel.Font
open iText.IO.Font
open Shrimp.Pdf.Manipulates


let manipulateTests =
  testList "Manipulates Tests" [
    testCase "change stroke color b255 to m100" <| fun _ -> 
        Flow.Manipulate (
            modify 
                (PageSelector.First)
                [
                    (
                        (fun page ->
                            RenderInfoSelector.Path (fun pathRenderInfo -> 
                                pathRenderInfo.GetStrokeColor() = DeviceRgb.BLUE)
                        ),
                        SelectionModifier.Modify (fun args -> [
                            PdfCanvas.setStrokeColor (PdfCanvasColor.Specific DeviceCmyk.MAGENTA)
                            PdfCanvas.writeOperatorRange args.Close
                        ])
                    )
                ]
        )
        |> runWithBackup "datas/manipulate/change stroke color b255 to m100.pdf" 
        |> ignore

    testCase "add bound to text" <| fun _ -> 
        Flow.Manipulate (
            modify 
                PageSelector.All
                [
                    (
                        (fun page ->
                            RenderInfoSelector.Text (fun _textRenderInfo -> true)
                        ),
                        SelectionModifier.AddNew (fun args -> 
                            let textRenderInfo = args.CurrentRenderInfo :?> TextRenderInfo
                            let border = TextRenderInfo.getBound BoundGettingOptions.WithStrokeWidth textRenderInfo
                            [
                                PdfCanvas.addRectangle border (fun args ->
                                    { args with 
                                        StrokeColor = PdfCanvasColor.Specific DeviceCmyk.CYAN }
                                )
                            ])
                    )
                ]
        )
        |> runWithBackup "datas/manipulate/add bound to text.pdf" 
        |> ignore

    testCase "add text to position" <| fun _ -> 
        Flow.Manipulate (
            addNew
                PageSelector.All 
                PageBoxKind.ActualBox
                (fun _ ->
                    [
                        Canvas.addText "你好" (fun args ->
                            { args with 
                                PdfFontFactory = PdfFontFactory.Registerable (RegisterableFonts.AlibabaPuHuiTiBold)
                                CanvasFontSize = CanvasFontSize.Numeric 25. 
                                FontColor = DeviceCmyk.MAGENTA 
                                FontRotation = Rotation.None 
                                Position = Position.LeftTop(0., 0.)}
                        )
                    ] 
                )

        )
        |> runWithBackup "datas/manipulate/add text to position.pdf" 
        |> ignore
    
    testCase "add page-scaling text" <| fun _ -> 
        Flow.Manipulate(
            addNew
                PageSelector.All 
                PageBoxKind.ActualBox
                (fun _ ->
                    [
                        Canvas.addText "你好" (fun args ->
                            { args with 
                                PdfFontFactory = PdfFontFactory.Registerable (RegisterableFonts.AlibabaPuHuiTiBold)
                                CanvasFontSize = CanvasFontSize.OfRootArea 0.8 
                                FontColor = DeviceCmyk.MAGENTA 
                                FontRotation = Rotation.None 
                                Position = Position.Center(0., 0.)}
                        )
                    ]
                )
        )
        |> runWithBackup "datas/manipulate/add page-scaling text.pdf" 
        |> ignore

    ftestCase "trim to visible test" <| fun _ -> 
        Flow.Manipulate(
            trimToVisible PageSelector.All
        )
        |> runWithBackup "datas/manipulate/trim to visible.pdf" 
        |> ignore

  ]