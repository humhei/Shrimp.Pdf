module ManipulateTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open Shrimp.Pdf.icms2
open Shrimp.Pdf.RegisterableFonts.YaHei
open Imposing


let manipulateTests =
  testList "Manipulates Tests" [

    testCase "change separation color of pdfFunction2 PageNumber to m100" <| fun _ -> 

        let pageNumberSeparationColor: PdfCanvasColor =
            FsSeparation.Create("PageNumber", DeviceRgb(200, 0, 56))
            |> PdfCanvasColor.Separation

        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    SelectorAndModifiers(
                        "change separation color of pdfFunction2 PageNumber to m100",
                        Text(Info.ColorIsOneOf (FillOrStrokeOptions.Fill, [PdfCanvasColor.Registration ;pageNumberSeparationColor])),
                        [Modifier.SetFillColor(DeviceCmyk.MAGENTA)]
                    )
                ]
            ) 
        )
        |> runTest "datas/manipulate/change separation color of pdfFunction2 PageNumber to m100.pdf" 
        |> ignore

    testCase "remove specfic separation colors" <| fun _ -> 

        let colors = 
            [
                { Name = "CuttingLine_BLUE" 
                  Color = FsValueColor.OfItextColor DeviceRgb.BLUE
                  Transparency = 1. }

                FsSeparation.Create("PageNumber", DeviceRgb(200, 0, 56))
            ]
            |> List.map PdfCanvasColor.Separation

        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "remove specfic separation colors"
                      Selector = 
                        PathOrText(Info.ColorIsOneOf (FillOrStrokeOptions.FillOrStroke, colors))
                      Modifiers = [Modifier.CancelFillAndStroke()]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/remove specfic separation colors.pdf" 
        |> ignore

    testCase "change separation color of pdfFunction0 PageNumber to m100" <| fun _ -> 

        let pantone100C: PdfCanvasColor =
            PdfCanvasColor.ColorCard (ColorCard.Pantone PantoneColorEnum.``PANTONE 100 C``)

        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "change separation color of pdfFunction0 PageNumber to m100"
                      Selector = Path(Info.FillColorIs pantone100C)
                      Modifiers = [Modifier.SetFillColor(DeviceCmyk.MAGENTA)]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/change separation color of pdfFunction0 PageNumber to m100.pdf" 
        |> ignore

    testCase "change separation color of pdfFunction0 Registration to m100" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "change separation color of pdfFunction0 Registration to m100"
                      Selector = Path(Info.FillColorIs PdfCanvasColor.Registration)
                      Modifiers = [Modifier.SetFillColor(DeviceCmyk.MAGENTA)]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/change separation color of pdfFunction0 Registration to m100.pdf" 
        |> ignore

    testCase "change stroke color b255 to m100" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create(
                PageSelector.First,
                [
                    { Name = "change stroke color b255 to m100"
                      Selector = Path(Info.StrokeColorIs DeviceRgb.BLUE)
                      Modifiers = [Modifier.SetStrokeColor(DeviceCmyk.MAGENTA)]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/change stroke color b255 to m100.pdf" 
        |> ignore

    testCase "change red to black outside of trimbox" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "change red to black outside of trimbox"
                      Selector = PathOrText(Info.FillColorIs DeviceRgb.RED <&&> Info.BoundIsOutsideOf(AreaGettingOptions.PageBox PageBoxKind.TrimBox))
                      Modifiers = [Modifier.SetFillColor(DeviceGray.BLACK)]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/change red to black outside of trimbox.pdf" 
        |> ignore

    testCase "xobject_change stroke color b255 to m100" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create (
                PageSelector.First,
                [
                    { Name = "xobject_change stroke color b255 to m100"
                      Selector = Path(Info.StrokeColorIs DeviceRgb.BLUE)
                      Modifiers = [Modifier.SetStrokeColor(DeviceCmyk.MAGENTA)] }
                ]
            )
        )
        |> runTest "datas/manipulate/xobject_change stroke color b255 to m100.pdf" 
        |> ignore

    testCase "xobject_change stroke color b255 to m100 and then change m100 to c100" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create (
                PageSelector.First,
                [
                    { Name = "xobject_change stroke color b255 to m100"
                      Selector = Path(Info.StrokeColorIs DeviceRgb.BLUE)
                      Modifiers = [Modifier.SetStrokeColor(DeviceCmyk.MAGENTA)] }
                ]
            )
            <+>
            Modify.Create (
                PageSelector.First,
                [
                    { Name = "xobject_change stroke color m100 to c100"
                      Selector = Path(Info.StrokeColorIs DeviceCmyk.MAGENTA)
                      Modifiers = [Modifier.SetStrokeColor(DeviceCmyk.CYAN)] }
                ]
            )
        )
        |> runTest "datas/manipulate/xobject_change stroke color b255 to m100 and then change m100 to c100.pdf" 
        |> ignore

    testCase "black or white" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create (
                PageSelector.First,
                [
                    { Name = "black or white"
                      Selector = PathOrText(fun _ _ -> true)
                      Modifiers = [Modifier.BlackOrWhite()] }
                ]
            )
        )
        |> runTest "datas/manipulate/black or white.pdf" 
        |> ignore

    testCase "add bound to text" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "add bound to text"
                      Selector = Text(fun _ _ -> true) 
                      Modifiers = [
                        Modifier.AddRectangleToBound(fun args -> 
                            { args with StrokeColor = PdfCanvasColor.ITextColor DeviceCmyk.MAGENTA}
                        )
                      ]
                    }
                ]
            )
        )
        |> runTest "datas/manipulate/add bound to text.pdf" 
        |> ignore

    testCase "add bound to text2" <| fun _ -> 
        Flow.Reuse(
            Reuses.ClearDirtyInfos()
        )
        <+>
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "add bound to text"
                      Selector = Text(fun _ _ -> true) 
                      Modifiers = [
                        Modifier.AddRectangleToBound(fun args -> 
                            { args with StrokeColor = PdfCanvasColor.ITextColor DeviceCmyk.MAGENTA}
                        )
                      ]
                    }
                ]
            )
        )
        |> runTest "datas/manipulate/add bound to text2.pdf" 
        |> ignore

    testCase "add line to position" <| fun _ -> 
        Flow.Manipulate (
            ModifyPage.Create
                ("add line to position",
                  PageSelector.All,
                  Dummy,
                  PageModifier.Batch [
                    PageModifier.AddLine(
                      AreaGettingOptions.PageBox PageBoxKind.ActualBox,
                      Position.BottomMiddle (0., mm 3.2),
                      Position.BottomMiddle (0., 0.),
                      (fun args ->
                          { args with StrokeColor = PdfCanvasColor.Registration }
                      )
                    ) 

                    PageModifier.AddLine(
                      AreaGettingOptions.PageBox PageBoxKind.ActualBox,
                      Position.BottomMiddle (mm -3.5, mm 3.2),
                      Position.BottomMiddle (mm 3.5, mm 3.2),
                      (fun args ->
                          { args with StrokeColor = PdfCanvasColor.Registration }
                      )
                    ) 

                  ]

                )
        )
        |> runTest "datas/manipulate/add line to position.pdf" 
        |> ignore

    testCase "add colored texts to position" <| fun _ -> 
        Flow.Manipulate (
            ModifyPage.Create
                ("add colored texts to position",
                  PageSelector.All,
                  Dummy,
                  PageModifier.AddColoredTexts(
                    AreaGettingOptions.PageBox PageBoxKind.ActualBox,
                    [ 
                        { Text = "你好天气很好"
                          Color = PdfCanvasColor.ITextColor DeviceCmyk.CYAN }

                        { Text = "M"
                          Color = PdfCanvasColor.ITextColor DeviceCmyk.MAGENTA }

                        { Text = "是的啊"
                          Color = PdfCanvasColor.ITextColor DeviceCmyk.YELLOW }

                        { Text = "Unicode"
                          Color = PdfCanvasColor.ITextColor DeviceCmyk.BLACK }
                    ],
                    fun args -> 
                        { args with 
                            Position = Position.Center(0., 0.)
                            PdfFontFactory = FsPdfFontFactory.Registerable 
                                (RegisterableFonts.YaHei.yaHei RegisterableFonts.YaHei.FontWeight.Light)
                        }
                  )
                )
        )
        |> runTest "datas/manipulate/add colored texts to position.pdf" 
        |> ignore

    testCase "add colored texts to position2" <| fun _ -> 
        Flow.Manipulate (
            ModifyPage.Create
                ("add colored texts to position2",
                  PageSelector.All,
                  Dummy,
                  PageModifier.AddColoredTexts(
                    AreaGettingOptions.PageBox PageBoxKind.ActualBox,
                    [ 
                        { Text = "BLACK"
                          Color = PdfCanvasColor.ITextColor DeviceCmyk.CYAN }

                        { Text = "C= 8.0, M=100.0, Y=15.0, K=0.0"
                          Color = PdfCanvasColor.ITextColor DeviceCmyk.MAGENTA }

                    ],
                    fun args -> 
                        { args with 
                            Position = Position.Center(0., 0.)
                            PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                            HorizontalTextAlignment = Some iText.Layout.Properties.TextAlignment.RIGHT
                        }
                  )
                )
        )
        |> runTest "datas/manipulate/add colored texts to position2.pdf" 
        |> ignore

    testCase "add text to position" <| fun _ -> 
        Flow.Manipulate (
            ModifyPage.Create
                ("add text to position",
                  PageSelector.All,
                  Dummy,
                  PageModifier.Batch [
                    PageModifier.AddText(PageBoxKind.ActualBox, "你好Separation", fun args ->
                      { args with 
                          PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                          CanvasFontSize = CanvasFontSize.Numeric 25. 
                          FontColor = PdfCanvasColor.Separation (FsSeparation.Create("专色1", DeviceRgb.BLUE))
                          FontRotation = Rotation.None 
                          Position = Position.LeftTop(0., 0.)}
                    )

                    PageModifier.AddText(PageBoxKind.ActualBox, "你好LAB", fun args ->
                      { args with 
                          PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                          CanvasFontSize = CanvasFontSize.Numeric 25. 
                          FontColor = PdfCanvasColor.Lab {L = 50.f; a = 50.f; b = 50.f}
                          FontRotation = Rotation.None 
                          Position = Position.TopMiddle(0., 0.)}
                    )
                    PageModifier.AddText(
                        PageBoxKind.ActualBox,
                        "咬口正",
                        (fun args ->
                            { args with 
                                Position = Position.BottomMiddle (mm -5., mm 0.)
                                PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Regular)
                                //CanvasFontSize = CanvasFontSize.Numeric 8. 
                                //FontColor = PdfCanvasColor.Registration
                                //HorizontalTextAlignment = Some TextAlignment.RIGHT
                            }
                        )
                    )
                  ]
                )
        )
        |> runTest "datas/manipulate/add text to position.pdf" 
        |> ignore

    testCase "add text to position with cached fonts" <| fun _ -> 
        Flow.Manipulate (
            ModifyPage.Create
                ("add text to position with cached fonts1",
                  PageSelector.All,
                  Dummy,
                  PageModifier.AddText(PageBoxKind.ActualBox, "你好", fun args ->
                    { args with 
                        PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                        CanvasFontSize = CanvasFontSize.Numeric 25. 
                        FontColor = PdfCanvasColor.ITextColor DeviceCmyk.MAGENTA 
                        FontRotation = Rotation.None 
                        Position = Position.LeftTop(0., 0.)}
                  ) 
                )
        )
        <+>
        Flow.Manipulate (
            ModifyPage.Create
                ("add text to position with cached fonts2",
                  PageSelector.All,
                  Dummy,
                  fun args ->
                      PageModifier.AddText(PageBoxKind.ActualBox, "你好2", fun args ->
                        { args with 
                            PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                            CanvasFontSize = CanvasFontSize.Numeric 25. 
                            FontColor = PdfCanvasColor.ITextColor DeviceCmyk.MAGENTA 
                            FontRotation = Rotation.None 
                            Position = Position.LeftTop(mm 20., 0.)}
                      ) args
                )
        )
        <+>
        Flow.Reuse (
            Reuse.dummy()
        )
        <+>
        Flow.Manipulate (
            ModifyPage.Create
                ("add text to position with cached fonts3",
                  PageSelector.All,
                  Dummy,
                  PageModifier.AddText(PageBoxKind.ActualBox, "你好3", fun args ->
                    { args with 
                        PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                        CanvasFontSize = CanvasFontSize.Numeric 25. 
                        FontColor = PdfCanvasColor.ITextColor DeviceCmyk.MAGENTA 
                        FontRotation = Rotation.None 
                        Position = Position.LeftTop(mm 40., 0.)}
                  ) 
                )
        )
        |> runTest "datas/manipulate/add text to position with cached fonts.pdf" 
        |> ignore

    testCase "add text to position and trimToVisible" <| fun _ -> 
        Flow.Manipulate (
            ModifyPage.Create
                ("add text to position",
                  PageSelector.All,
                  Dummy,
                  PageModifier.AddText(PageBoxKind.ActualBox, "你", fun args ->
                    { args with 
                        PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                        CanvasFontSize = CanvasFontSize.Numeric 25. 
                        FontColor = PdfCanvasColor.ITextColor DeviceCmyk.MAGENTA 
                        FontRotation = Rotation.None 
                        Position = Position.LeftTop(0., 0.)}
                  ) 
                )
            <+>
            ModifyPage.TrimToVisible(PageSelector.All, Margin.Create 6.)
            <+>
            ModifyPage.Create
                ("add text to position",
                  PageSelector.All,
                  Dummy,
                  PageModifier.AddText(PageBoxKind.ActualBox, "你好", fun args ->
                    { args with 
                        PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                        CanvasFontSize = CanvasFontSize.Numeric 25. 
                        FontColor = PdfCanvasColor.ITextColor DeviceCmyk.MAGENTA 
                        FontRotation = Rotation.None 
                        Position = Position.LeftTop(mm 3., 0.)}
                  ) 
                )

        )

        |> runTest "datas/manipulate/add text to position and trimToVisible.pdf" 
        |> ignore
    
    testCase "add page-scaling text" <| fun _ -> 
        Flow.Manipulate(
            ModifyPage.Create(
                "add page-scaling text",
                PageSelector.All,
                Dummy,
                PageModifier.AddText(PageBoxKind.ActualBox, "你好", fun args ->
                { args with 
                    PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                    CanvasFontSize = CanvasFontSize.OfRootArea 0.8 
                    FontColor = PdfCanvasColor.ITextColor DeviceCmyk.MAGENTA 
                    FontRotation = Rotation.None 
                    Position = Position.Center(0., 0.)}
                )
            )
        )
        |> runTest "datas/manipulate/add page-scaling text.pdf" 
        |> ignore

    testCase "rotate page and add text to top left" <| fun _ -> 
        Flow.Reuse(
            Reuses.Rotate(PageSelector.All, Rotation.Counterclockwise)
        )
        <+>
        Flow.Manipulate(
            ModifyPage.Create(
                "rotate page and add text to top left",
                PageSelector.All,
                Dummy,
                PageModifier.AddText(PageBoxKind.ActualBox, "你好", fun args ->
                { args with 
                    PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                    CanvasFontSize = CanvasFontSize.Numeric 12.
                    FontColor = PdfCanvasColor.ITextColor DeviceCmyk.MAGENTA 
                    FontRotation = Rotation.None 
                    Position = Position.LeftTop(0., 0.)}
                )
            )
        )
        |> runTest "datas/manipulate/rotate page and add text to top left.pdf" 
        |> ignore

    testCase "add page-scaling multiLines-text" <| fun _ -> 
        Flow.Manipulate(
            ModifyPage.Create(
                "setPageBox",
                PageSelector.All,
                Dummy,
                PageModifier.SetPageBox (PageBoxKind.AllBox, Rectangle.create 0. 0. 500. 100.)
            )
            <+>
            ModifyPage.Create(
                "add page-scaling multiLines-text",
                PageSelector.All,
                Dummy,
                fun args ->

                    let pdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)

                    let text = "你好你好\n你好\n你好"

                    let canvasRootArea = 
                        let doc = args.Page.GetDocument() :?> PdfDocumentWithCachedResources
                        let pageBox = args.Page.GetPageBox(PageBoxKind.ActualBox)

                        let height = pageBox.GetHeightF()

                        let lineWidth =
                            let pdfFont = doc.GetOrCreatePdfFont(pdfFontFactory)
                            PdfFont.calcLineWidthWhenParagraphedHeightIs height text pdfFont

                        let x = pageBox.GetRightF() - (lineWidth * 0.8)
                        let y = pageBox.GetBottomF()

                        Rectangle.create x y (lineWidth * 0.8) height

                    PageModifier.AddText(
                        canvasRootArea,
                        text,
                        fun args ->
                            { args with 
                                PdfFontFactory = pdfFontFactory
                                CanvasFontSize = 
                                    CanvasFontSize.OfRootArea 0.8
                                FontColor = PdfCanvasColor.ITextColor DeviceCmyk.MAGENTA 
                                FontRotation = Rotation.None 
                                Position = Position.LeftMiddle (0., 0.)}
                    ) args
            ) 
        )
        |> runTest "datas/manipulate/add page-scaling multiLines-text.pdf" 
        |> ignore

    testCase "add rect to area" <| fun _ -> 
        Flow.Manipulate(
            ModifyPage.Create(
                "add rect to area",
                PageSelector.All,
                Dummy,
                PageModifier.AddRectangleToCanvasRootArea(AreaGettingOptions.PageBox PageBoxKind.ActualBox, fun args -> { args with FillColor = PdfCanvasColor.ITextColor DeviceRgb.BLACK})
            ) 
        )
        |> runTest "datas/manipulate/add rect to area.pdf" 
        |> ignore

    testCase "trim to visible test" <| fun _ -> 
        Flow.Manipulate(
            ModifyPage.TrimToVisible (PageSelector.All, (Margin.Create(mm 6.)))
        )
        |> runTest "datas/manipulate/trim to visible.pdf" 
        |> ignore

    testCase "trim to visible test 2" <| fun _ -> 
        Flow.Manipulate(
            ModifyPage.TrimToVisible(PageSelector.All, (Margin.Create(mm 6.)))
        )
        |> runTest "datas/manipulate/trim to visible2.pdf" 
        |> ignore


  ]