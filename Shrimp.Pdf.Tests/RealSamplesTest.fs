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
open Shrimp.Pdf.DSL


let realSamplesTests =
  testList "real samples tests" [
    ftestCase "Layout Confirm___trim to b255_N-UP_remove left top R255B255_change left top r255 to k100 tests" <| fun _ -> 

        let readB255Bound() =
            modifyPage( 
                Page.All,
                Path (
                    Info.StrokeColoris DeviceRgb.BLUE
                    <&> Info.BoundIsInsideOfPageBox()
                ),
                Operator.GetBound()
            )

        let readSizes() = 
            readB255Bound()
            <+>
            modifyPage (
                PageSelector.All,
                Text (
                    Info.FillColoris DeviceRgb.RED
                    <&> fun args -> Info.BoundIsOutsideOf(args.UserState.[0]) args
                ),
                Operator.PickTexts(                   
                    (pfloat .>>. (pstring "×" >>. pfloat) .>> pstring "mm"
                    |>> (fun (width, height) -> {Width = width; Height = height}))
                )
            )

        let setTrimBoxToStrokeB255() = 
            readB255Bound()
            <+> 
            modifyPage( 
                PageSelector.All,
                Dummy,
                (fun args -> Operator.SetPageBox(args.PageUserState(), PageBoxKind.TrimBox) args)
            ) ||>> ignore

        Flow.Manipulate (setTrimBoxToStrokeB255())
        <+> 
        Flow.Reuse (
            impose (fun args ->
                { args with
                    ColNums = [6]
                    RowNum = 3
                    Margin = Margin.Create(mm 6)
                    Background = Background.Size FsSize.A0
                    UseBleed = true
                }
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
                modifyPage(
                    PageSelector.All,
                    Dummy,
                    (fun args -> Operator.GetPageEdge(args.PageUserState(), PageBoxKind.ActualBox) args)
                )
            ) ||>> (fun userState ->
                let (_, doc), pageEdges = userState
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
                ))
            <+> 
            modify(
                PageSelector.All,
                [
                    { Selector =
                        Factory(fun args ->
                            let pageEdge, titleArea = args.PageUserState()
                            OR [
                                PathOrText (
                                    Info.BoundIsInsideOf(pageEdge.TopMiddle)
                                    <&> Info.BoundIsOutsideOf(titleArea)
                                )
                                PathOrText (
                                    Info.BoundIsInsideOf(titleArea)
                                    <&> (!!(Info.FillColoris DeviceRgb.RED))
                                )
                            ]
                        )
                      Modifier = SelectionModifier.DropColor
                    }
                ]
            )
            <+>
            modify(
                PageSelector.All,
                [
                    { Selector =
                        PathOrText (fun args ->
                            let pageEdge, _ = args.UserState.[args.PageNum - 1]
                            ( Info.BoundIsInsideOf(pageEdge.LeftMiddle)
                                <&> (!!(Info.FillColoris DeviceRgb.MAGENTA)) 
                            ) args
                        )
                      Modifier = SelectionModifier.DropColor
                    }
                ]
            )
        )
        
        |> runWithBackup "datas/real samples/Layout Confirm___trim to b255_N-UP_remove left top R255B255_change left top r255 to k100.pdf" 
        |> ignore

  ]