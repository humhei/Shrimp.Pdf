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

let realSamplesTests =
  testList "real samples tests" [
    testCase "Layout Confirm___trim to b255_N-UP_remove left top R255B255_change left top r255 to k100 tests" <| fun _ -> 
        let readB255Bound() =
            modifyPage 
                PageSelector.All
                (fun args ->
                    (RenderInfoSelector.Path (fun pathRenderInfo ->
                        pathRenderInfo.GetStrokeColor() = DeviceRgb.BLUE
                        && PathRenderInfo.hasStroke pathRenderInfo
                        && (PathRenderInfo.getBound BoundGettingOptions.WithoutStrokeWidth pathRenderInfo).IsInsideOf(args.Page.GetActualBox())
                    ))
                )
                (fun args renderInfos ->
                    let trimedBox = 
                        renderInfos
                        |> Seq.map (AbstractRenderInfo.getBound BoundGettingOptions.WithoutStrokeWidth)
                        |> Rectangle.ofRectangles
                    Some trimedBox
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
                        |> Seq.choose AbstractRenderInfo.asTextRenderInfo
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
            <.+> trimToVisible PageSelector.All
            <++> readB255Bound()
            <+> 
            modifyPage 
                PageSelector.All
                (fun args ->
                    args.UserState
                    failwith ""
                )
                (fun _ _ -> 
                    None
                )
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