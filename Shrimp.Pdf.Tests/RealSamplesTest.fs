module RealSampleTests
open Expecto
open Shrimp.Pdf.Reuses
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
            <&> Info.BoundIsInsideOfPageBox()
        ),
        PageModifier.GetBound()
    )

let setTrimBoxToStrokeB255() = 
    readB255Bound()
    <+> 
    modifyPage( 
        "setTrimBoxToStrokeB255",
        PageSelector.All,
        Dummy,
        (fun args -> PageModifier.SetPageBox(args.PageUserState(), PageBoxKind.TrimBox) args)
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
      Modifiers = [ Modifier.DropColor() ]
    }

let blackAndWhiteTitleInfo() =
    { Name = "black and white title info"
      Selector = 
        Factory (fun args ->
            let pageEdge, titleArea = args.PageUserState()
            PathOrText (
                Info.BoundIsInsideOf(titleArea)
            )
        )
      Modifiers = 
        [
            Modify.BlackOrWhite()
        ]
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
      Modifiers = [ Modifier.DropColor() ]
    }

let removeNavigationInfo() =
    { Name = (sprintf "remove navigation info")
      Selector =
        PathOrText (fun args ->
            let pageEdge, _ = args.PageUserState()
            ( Info.BoundIsCrossOf(pageEdge.LeftMiddle)
                <&> Info.FillColorIs DeviceRgb.MAGENTA
            ) args
        )
      Modifiers =[ Modifier.DropColor() ]
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
                (fun args -> PageModifier.GetPageEdge(args.PageUserState(), PageBoxKind.ActualBox) args)
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
            (trimToVisible PageSelector.All (Margin.Create(mm 2.))  |> redirect fst)
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

    testCase "printing out test" <| fun _ -> 

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

  ]