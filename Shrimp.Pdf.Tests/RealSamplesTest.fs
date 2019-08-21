module RealSampleTests
open Expecto
open Shrimp.Pdf.Reuses
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Reuses
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
open Shrimp.Pdf.Manipulates
open FParsec
open FParsec.CharParsers
open iText.Kernel.Geom
open Shrimp.Pdf.Parser
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Colors
type Page = PageSelector
type Selector = RenderInfoSelector 


let realSamplesTests =
  testList "real samples tests" [
    ftestCase "Layout Confirm___trim to b255_N-UP_remove left top R255B255_change left top r255 to k100 tests" <| fun _ -> 
        let readB255Bound() =
            modifyPage 
                Page.All
                (fun args ->
                    (Selector.PathIntegrated (fun pathIntegratedRenderInfo ->
                        let pathRenderInfo = pathIntegratedRenderInfo.RenderInfo
                        pathRenderInfo.GetStrokeColor() = DeviceRgb.BLUE
                        && IntegratedRenderInfo.isStrokeVisible pathIntegratedRenderInfo
                        && (AbstractRenderInfo.getBound BoundGettingOptions.WithoutStrokeWidth pathRenderInfo).IsInsideOf(args.Page.GetActualBox())
                    ))
                )
                (fun args renderInfos ->
                    let trimedBox = 
                        renderInfos
                        |> Seq.choose (IntegratedRenderInfo.asPathRenderInfo)
                        |> Seq.map (PathRenderInfo.getBound BoundGettingOptions.WithoutStrokeWidth)
                        |> Rectangle.ofRectangles
                    Some trimedBox
                )
        
        let (<@>) (a: #AbstractRenderInfo -> bool) (b: #AbstractRenderInfo -> bool) =
            fun renderInfo ->
                a renderInfo
                && b renderInfo
        let m = 
            RenderInfoSelector.Text(
                AbstractRenderInfo.hasFill
                <@>
                TextRenderInfo.hasFill
            )


        let readSize() = 
            readB255Bound()
            <+> 
            modifyPage 
                PageSelector.First
                (fun args ->
                    (RenderInfoSelector.Text (fun textRenderInfo ->
                        textRenderInfo.GetFillColor() = DeviceRgb.RED
                        && TextRenderInfo.hasFill textRenderInfo
                        && (TextRenderInfo.getBound BoundGettingOptions.WithoutStrokeWidth textRenderInfo).IsOutsideOf(args.UserState.[0])
                    ))
                )
                (fun args renderInfos ->
                    let textRenderInfos = 
                        renderInfos 
                        |> Seq.choose IntegratedRenderInfo.asTextRenderInfo
                        |> Seq.pick (fun renderInfo ->
                            let text = TextRenderInfo.getText renderInfo
                            let parser = 
                                (pfloat .>>. (pstring "×" >>. pfloat) .>> pstring "mm")
                                |>> (fun (width, height) -> {Width = width; Height = height})

                            match run  parser text with 
                            | Success (result, _ ,_ )-> Some result
                            | Failure (_, _ , _) -> None
                        )
                    Some textRenderInfos
                ) ||>> List.exactlyOne

        let setTrimBoxToStrokeB255() = 
            readB255Bound()
            <+> 
            modifyPage 
                PageSelector.All
                (fun _ -> RenderInfoSelector.Dummy)
                (fun args _ ->
                    let page = args.Page
                    let rect = args.UserState.[args.PageNum - 1]
                    page.SetTrimBox(rect)
                    |> ignore
                    None
                ) ||>> ignore


       



        Flow.Manipulate (
            setTrimBoxToStrokeB255()
        )
        <+> 
        Flow.Reuse (
            impose (fun _ ->
                (ImposingArguments.Create(fun args ->
                    { args with
                        ColNums = [6]
                        RowNum = 3
                        Margin = Margin.Create(mm 6)
                        Background = Background.Size FsSize.A0
                        UseBleed = true
                    }
                ))
            )
          ) 
        <+> 
        Flow.Manipulate (
            Manipulate.dummy
            <.+> trimToVisible (Margin.Create(mm 2.)) PageSelector.All
            <++> 
            (
                readB255Bound()
                <+>
                modifyPage
                    PageSelector.All
                    (fun args -> RenderInfoSelector.Dummy )
                    (fun args renderInfos ->
                        let b255Bound = args.UserState.[args.PageNum - 1]
                        let bound = PdfPage.getEdge b255Bound (PageBoxKind.ActualBox) args.Page
                        Some bound
                    )
            )
            <+>
            //modify
            //    PageSelector.All
            //    [
            //        (
            //            (fun args->
            //                let (_,_), pageEdges = args.UserState
            //                let pageEdge = pageEdges.[args.PageNum - 1]
            //                RenderInfoSelector.PathOrText (fun renderInfo ->
            //                    (AbstractRenderInfo.hasFill renderInfo
            //                    && (renderInfo.GetFillColor() = DeviceRgb.MAGENTA)
            //                    && (AbstractRenderInfo.getBound BoundGettingOptions.WithoutStrokeWidth renderInfo).IsInsideOf(pageEdge.LeftMiddle))
            //                    |> not
            //                )                    
            //            ),
            //            (SelectionModifier.DropColor)
            //        )
            //    ]
            //<+>
            modify
                PageSelector.All
                [
                    (
                        (fun args->
                            let (_, doc), pageEdges = args.UserState
                            let firstCellWidth = doc.GetFirstCell().Size.Width
                            let pageEdge = pageEdges.[args.PageNum - 1]
                            let titleArea = 
                                Rectangle.create 
                                <| pageEdge.TopMiddle.GetXF()
                                <| pageEdge.TopMiddle.GetYF()
                                <| firstCellWidth
                                <| pageEdge.TopMiddle.GetHeightF()

                            RenderInfoSelector.PathOrText (fun renderInfo ->
                                (
                                    (AbstractRenderInfo.getBound BoundGettingOptions.WithoutStrokeWidth renderInfo).IsInsideOf(pageEdge.TopMiddle)
                                    &&
                                    (not <| (AbstractRenderInfo.getBound BoundGettingOptions.WithoutStrokeWidth renderInfo).IsInsideOf(titleArea))
                                )
                                ||
                                (
                                    (AbstractRenderInfo.getBound BoundGettingOptions.WithoutStrokeWidth renderInfo).IsInsideOf(titleArea)
                                    &&
                                    (
                                        not 
                                            (AbstractRenderInfo.hasFill renderInfo
                                            && (renderInfo.GetFillColor() = DeviceRgb.RED))
                                    )
                                )
                            )                    
                        ),
                        (SelectionModifier.DropColor)
                    )
                ]
          )
        //<+>
          //Flow.Manipulate (
          //  addNew 
          //      PageSelector.All
          //      PageBoxKind.ActualBox
          //      (fun (textRenderInfo, textShiftToPage, imposingArguments) ->
          //          [
          //              Canvas.addText 
          //                  (TextRenderInfo.getText textRenderInfo)
          //                  (fun args ->
          //                      { args with 
          //                          CanvasFontSize = CanvasFontSize.OfArea (TextRenderInfo.getBound textRenderInfo)
          //                          Position = Position.LeftTop (textShiftToPage.x + imposingArguments.Margin.Left, - imposingArguments.Margin.Top + textShiftToPage.y)
          //                      }
          //                  )
          //          ]
          //      )
          //)
        
        |> runWithBackup "datas/real samples/Layout Confirm___trim to b255_N-UP_remove left top R255B255_change left top r255 to k100.pdf" 
        |> ignore

  ]