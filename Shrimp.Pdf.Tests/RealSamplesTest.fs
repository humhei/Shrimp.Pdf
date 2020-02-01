module RealSampleTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
open Shrimp.Pdf.Manipulates
open Shrimp.Pdf.Colors
open Shrimp.Pdf.DSL
open Shrimp.Pdf.icms2

let readB255Bound() =
    modifyPage ( 
        "read b255 bound",
        PageSelector.All,
        Path (
            Info.StrokeColorIs DeviceRgb.BLUE
            <&&> Info.BoundIsInsideOf(AreaGettingOptions.PageBox PageBoxKind.ActualBox)
        ),
        PageModifier.GetBoundOfSelector()
    )

let setTrimBoxToStrokeB255() = 
    readB255Bound()
    <+> 
    modifyPage( 
        "setTrimBoxToStrokeB255",
        PageSelector.All,
        Dummy,
        (fun args -> PageModifier.SetPageBox(PageBoxKind.TrimBox, args.PageUserState()) args)
    ) ||>> ignore

let retainTitleInfo (color: Color) = 
    { Name = (sprintf "retain title info %O" color)
      Selector = 
          Factory(fun args ->
            let pageEdge, titleArea = args.PageUserState()
            OR [
                PathOrText (
                    Info.BoundIsInsideOf(AreaGettingOptions.Specfic pageEdge.TopMiddle)
                    <&&> Info.BoundIsOutsideOf(AreaGettingOptions.Specfic titleArea)
                )
                PathOrText (
                    Info.BoundIsInsideOf(AreaGettingOptions.Specfic titleArea)
                    <&&> (!!(Info.FillColorIs color))
                )
            ])
      Modifiers = [ Modifier.CancelFillAndStroke() ]
    }

let blackAndWhiteTitleInfo() =
    { Name = "black and white title info"
      Selector = 
        Factory (fun args ->
            let pageEdge, titleArea = args.PageUserState()
            PathOrText (
                Info.BoundIsInsideOf(AreaGettingOptions.Specfic titleArea)
            )
        )
      Modifiers = 
        [
            Modifier.BlackOrWhite()
        ]
    }

let retainNavigationInfo (color: Color) =
    { Name = (sprintf "retain navigation info %O" color)
      Selector =
        PathOrText (fun args ->
            let pageEdge, _ = args.PageUserState()
            ( Info.BoundIsInsideOf(AreaGettingOptions.Specfic pageEdge.LeftMiddle)
                <&&> (!!(Info.FillColorIs color)) 
            ) args
        )
      Modifiers = [ Modifier.CancelFillAndStroke() ]
    }

let removeNavigationInfo() =
    { Name = (sprintf "remove navigation info")
      Selector =
        PathOrText (fun args ->
            let pageEdge, _ = args.PageUserState()
            ( Info.BoundIsCrossOf(AreaGettingOptions.Specfic pageEdge.LeftMiddle)
                <&&> Info.FillColorIs DeviceRgb.MAGENTA
            ) args
        )
      Modifiers =[ Modifier.CancelFillAndStroke() ]
    }



let getPageEdgeAndTitleArea(): Manipulate<ImposingDocument, _> =
    Manipulate.dummy
    <++>
    (
        readB255Bound()
        <+>
        modifyPage(
            "getPageEdge",
            PageSelector.All,
            Dummy,
            (fun args -> PageModifier.GetPageEdge(PageBoxKind.ActualBox, args.PageUserState()) args)
    )
    ) ||>> (fun (doc, pageEdges) ->
        let firstCellWidth = doc.GetFirstCell().Size.Width
        pageEdges
        |> List.map (fun pageEdge ->
            let titleArea = 
                Rectangle.create 
                <| pageEdge.TopMiddle.GetXF()
                <| pageEdge.TopMiddle.GetYF()
                <| firstCellWidth
                <| pageEdge.TopMiddle.GetHeightF()
            pageEdge, titleArea
        )
    )

let realSamplesTests =
  testList "real samples tests" [
    testCase "Layout to Confirm test" <| fun _ -> 

        Flow.Manipulate (setTrimBoxToStrokeB255())
        <+> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with
                    ColNums = [6]
                    RowNum = 3
                    Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable(Margin.Create(mm 6.))
                    Background = Background.Size FsSize.MAXIMUN
                    UseBleed = true
                }
            )
        )
        <+> 
        Flow.Manipulate (
            Manipulate.dummy
            <.+>
            (ModifyPage.trimToVisible PageSelector.All (Margin.Create(mm 2.)))
            <+> (getPageEdgeAndTitleArea())
            <+> 
            modify(
                PageSelector.All,
                [ retainTitleInfo DeviceRgb.RED
                  retainNavigationInfo DeviceRgb.MAGENTA ]
            )
        )
        
        |> runWithBackup "datas/real samples/Layout to Confirm.pdf" 
        |> ignore

    testCase "printing out test" <| fun _ -> 

        Flow.Manipulate (setTrimBoxToStrokeB255())
        <+> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with
                    ColNums = [0]
                    RowNum = 0
                    Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable(Margin.Create(mm 6.))
                    Background = Background.Size FsSize.A4
                    UseBleed = true
                    Cropmark = Some Cropmark.defaultValue
                    IsRepeated = true
                }
            )
        )
        <+> 
        Flow.Manipulate (
            (getPageEdgeAndTitleArea())
            <+> 
            modifyAsync (
                ModifyingAsyncWorker.Sync,
                PageSelector.All,
                [ retainTitleInfo DeviceRgb.MAGENTA
                  removeNavigationInfo() 
                  blackAndWhiteTitleInfo()
                ]
            )
        )
        
        |> runWithBackup "datas/real samples/printing out.pdf" 
        |> ignore

    testCase "add seam text and seam line test" <| fun _ -> 

        Flow.Manipulate (
            modifyPage
                ("add seam line",
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
            <+>
            modifyPage (
                "add seam text",
                PageSelector.All,
                Dummy,
                PageModifier.AddText(
                    PageBoxKind.ActualBox,
                    "咬口左右翻",
                    (fun args ->
                        { args with 
                            Position = Position.BottomMiddle (mm -15., mm 0.)
                            PdfFontFactory = FsPdfFontFactory.Registerable RegisterableFonts.AlibabaPuHuiTiBold 
                            CanvasFontSize = CanvasFontSize.Numeric 8. 
                            FontColor = PdfCanvasColor.Registration }
                    )
                )
            )

        )
        
        |> runWithBackup "datas/real samples/add seam text and seam line.pdf" 
        |> ignore

  ]