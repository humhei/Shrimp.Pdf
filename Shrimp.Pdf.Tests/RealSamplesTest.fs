module RealSampleTests
open Expecto
open Shrimp.Pdf.Reuses
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Reuses
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
//open Shrimp.Pdf.FileOperations
open Shrimp.Pdf.Manipulates
open FParsec
open FParsec.CharParsers
open iText.Kernel.Geom

let realSamplesTests =
  testList "real samples tests" [
    testCase "Layout Confirm___trim to b255_N-UP_remove left top R255B255_change left top r255 to k100 tests" <| fun _ -> 
        Flow.Manipulate (
            modifyPage 
                PageSelector.All
                (fun page ->
                    (RenderInfoSelector.Path (fun pathRenderInfo ->
                        pathRenderInfo.GetStrokeColor() = DeviceRgb.BLUE
                        && PathRenderInfo.isStrokeVisible pathRenderInfo
                        && (PathRenderInfo.getBound pathRenderInfo).IsInsideOf(page.GetActualBox())
                    ))
                )
                (fun args renderInfos ->
                    let trimedBox = 
                        renderInfos
                        |> Seq.map AbstractRenderInfo.getBound
                        |> Rectangle.ofRectangles
                    PdfPage.setPageBox (PageBoxKind.AllBox) trimedBox args.Page |> ignore
                    None
                ) ||>> ignore
            <+>
            modifyPage 
                PageSelector.First
                (fun page ->
                    (RenderInfoSelector.Text (fun textRenderInfo ->
                        textRenderInfo.GetFillColor() = DeviceRgb.RED
                        && TextRenderInfo.isFillVisible textRenderInfo
                        && (TextRenderInfo.getBound textRenderInfo).IsOutsideOf(page.GetActualBox())
                    ))
                )
                (fun args renderInfos ->
                    let textRenderInfo = 
                        renderInfos 
                        |> Seq.choose AbstractRenderInfo.asTextRenderInfo
                        |> Seq.find (fun renderInfo ->
                            let text = TextRenderInfo.getText renderInfo
                            let parser = 
                                pfloat .>>. (pstring "×" >>. pfloat) .>> pstring "mm" 
                                |>> fun (width, height) -> {Width = width; Height = height}
                            match run  parser text with 
                            | Success (result, _ ,_ )-> true
                            | Failure (_, _ , _) -> false
                        )

                    let textShiftToPage =
                        let textBound = TextRenderInfo.getBound textRenderInfo
                        let pageBox = args.Page.GetActualBox()
                        let shiftX = textBound.GetLeftF() - pageBox.GetLeftF()
                        let shiftY = textBound.GetTopF() - pageBox.GetTopF()
                        new Point (shiftX, shiftY)

                    Some (textRenderInfo, textShiftToPage)
                ) ||>> (List.exactlyOne)
        )
        <+>
          Flow.Reuse (
            impose 
                (fun (textRenderInfo, textShiftToPage) ->
                    let textHeight = TextRenderInfo.getHeight textRenderInfo

                    (ImposingArguments.Create(fun args ->
                        { args with
                            ColNums = [6]
                            RowNum = 3
                            Margin = Margin.Create(mm 6, (textHeight + textShiftToPage.y) * 1.1, mm 6, mm 6)
                            Background = Background.Size FsSize.A0
                        }
                    ))
                ) ||>> (fun ((textRenderInfo,textShiftToPage), args, _) -> textRenderInfo, textShiftToPage, args.Value) 
          ) 
        <+>
          Flow.Manipulate (
            addNew 
                PageSelector.All
                PageBoxKind.ActualBox
                (fun (textRenderInfo, textShiftToPage, imposingArguments) ->
                    [
                        Canvas.addText 
                            (TextRenderInfo.getText textRenderInfo)
                            (fun args ->
                                { args with 
                                    CanvasFontSize = CanvasFontSize.OfArea (TextRenderInfo.getBound textRenderInfo)
                                    Position = Position.LeftTop (textShiftToPage.x + imposingArguments.Margin.Left, - imposingArguments.Margin.Top + textShiftToPage.y)
                                }
                            )
                    ]
                )
          )
        
        |> runWithBackup "datas/real samples/Layout Confirm___trim to b255_N-UP_remove left top R255B255_change left top r255 to k100.pdf" 
        |> ignore

  ]