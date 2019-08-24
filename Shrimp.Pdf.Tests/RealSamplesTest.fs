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
open Shrimp.Pdf.DSL
let readB255Bound() =
    modifyPage ( 
        "read b255 bound",
        PageSelector.All,
        Path (
            Info.StrokeColorIs DeviceRgb.BLUE
            <&> Info.BoundIsInsideOfPageBox()
        ),
        Operator.GetBound()
    )

let setTrimBoxToStrokeB255() = 
    readB255Bound()
    <+> 
    modifyPage( 
        "setTrimBoxToStrokeB255",
        PageSelector.All,
        Dummy,
        (fun args -> Operator.SetPageBox(args.PageUserState(), PageBoxKind.TrimBox) args)
    ) ||>> ignore

let retainTitleInfo color = 
    { Name = (sprintf "retain title info %O" color)
      Selector = 
          Factory(fun args ->
            let pageEdge, titleArea = args.PageUserState()
            OR [
                PathOrText (
                    Info.BoundIsInsideOf(pageEdge.TopMiddle)
                    <&> Info.BoundIsOutsideOf(titleArea)
                )
                PathOrText (
                    Info.BoundIsInsideOf(titleArea)
                    <&> (!!(Info.FillColorIs color))
                )
            ])
      Modifier = Modifier.DropColor
    }

let retainNavigationInfo color =
    { Name = (sprintf "retain navigation info %O" color)
      Selector =
        PathOrText (fun args ->
            let pageEdge, _ = args.PageUserState()
            ( Info.BoundIsInsideOf(pageEdge.LeftMiddle)
                <&> (!!(Info.FillColorIs color)) 
            ) args
        )
      Modifier = Modifier.DropColor
    }

let removeNavigationInfo() =
    { Name = (sprintf "remove navigation info")
      Selector =
        PathOrText (fun args ->
            let pageEdge, _ = args.PageUserState()
            ( Info.BoundIsCrossOf(pageEdge.LeftMiddle)
            ) args
        )
      Modifier = Modifier.DropColor
    }



let getPageEdgeAndTitleArea() =
    fun (doc: ImposingDocument) ->
        (
            readB255Bound()
            <+>
            modifyPage(
                "getPageEdge",
                PageSelector.All,
                Dummy,
                (fun args -> Operator.GetPageEdge(args.PageUserState(), PageBoxKind.ActualBox) args)
            )
        ) ||>> (fun userState ->
            //let doc: ImposingDocument = flowModel.UserState
            let pageEdges = userState
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
    |> Manipulate.ofConstraint

let realSamplesTests =
  testList "real samples tests" [
    testCase "Layout to Confirm test" <| fun _ -> 

        Flow.Manipulate (setTrimBoxToStrokeB255())
        <+> 
        Flow.Reuse (
            impose (fun args ->
                { args with
                    ColNums = [6]
                    RowNum = 3
                    Margin = Margin.Create(mm 6)
                    Background = Background.Size FsSize.MAXIMUN
                    UseBleed = true
                }
            )
        )
        <+> 
        Flow.Manipulate (
            Manipulate.dummy
            <.+>
            trimToVisible (Margin.Create(mm 2.)) PageSelector.All
            <+> (snd <<|| getPageEdgeAndTitleArea())
            <+> 
            modify( 
                PageSelector.All,
                [ retainTitleInfo DeviceRgb.RED
                  retainNavigationInfo DeviceRgb.MAGENTA ]
            )
        )
        
        |> runWithBackup "datas/real samples/Layout to Confirm.pdf" 
        |> ignore

    ftestCase "printing out test" <| fun _ -> 

        Flow.Manipulate (setTrimBoxToStrokeB255())
        <+> 
        Flow.Reuse (
            impose (fun args ->
                { args with
                    ColNums = [0]
                    RowNum = 0
                    Margin = Margin.Create(mm 6)
                    Background = Background.Size FsSize.A4
                    UseBleed = true
                    Cropmark = Some Cropmark.defaultValue
                    IsRepeated = true
                }
            )
        )
        <+> 
        Flow.Manipulate (
            Manipulate.dummy
            <+> (snd <<|| getPageEdgeAndTitleArea())
            <+> 
            modify (
                PageSelector.All,
                [ retainTitleInfo DeviceRgb.MAGENTA
                  removeNavigationInfo() ]
                )
        )
        
        |> runWithBackup "datas/real samples/printing out.pdf" 
        |> ignore

  ]