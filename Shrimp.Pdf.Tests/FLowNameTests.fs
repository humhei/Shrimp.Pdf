module FlowNameTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Colors

let flowNameTests =
  let addText text position =
    ModifyPage.Create
        ( text,
          PageSelector.First,
          Dummy,
          PageModifier.Batch [
            PageModifier.AddText(PageBoxKind.ActualBox, text, fun args ->
              { args with 
                  PdfFontFactory = FsPdfFontFactory.Registerable (RegisterableFonts.AlibabaPuHuiTiBold)
                  CanvasFontSize = CanvasFontSize.Numeric 25. 
                  FontColor = PdfCanvasColor.Separation (FsSeparation.Create("专色1", DeviceRgb.BLUE))
                  FontRotation = Rotation.None 
                  Position = position}
            )
          ]
        )

  testList "FlowName Tests" [
    testCase "oneLevel tests" <| fun _ -> 
        Flow.Manipulate(
            addText "add text to left top" (Position.LeftTop(0., 0.))
            <+>
            (
                addText "add text to top middle" (Position.TopMiddle(0., 0.))
                <+>
                addText "add text to right top" (Position.RightTop(0., 0.))
            )
        )
        <+>
        (
            Flow.dummy()
            <+>
            Flow.Reuse(
                Reuses.Rotate(PageSelector.All, Rotation.Counterclockwise)
            )
        )
        |> runTest "datas/flowName/oneLevel.pdf" 
        |> ignore

    testCase "multipleLevels tests1" <| fun _ ->
        Flow.NamedFlow(
            FlowName.New "flow flowName0",
            Flow.NamedFlow(
                FlowName.New "flow flowName1",
                Flow.Reuse(
                    Reuses.Rotate(PageSelector.All, Rotation.Counterclockwise)
                )
            )
            <+>
            Flow.Manipulate(
                addText "add text to left top" (Position.LeftTop(0., 0.))
                <+>
                (
                    addText "add text to top middle" (Position.TopMiddle(0., 0.))
                    <+>
                    addText "add text to right top" (Position.RightTop(0., 0.))
                )
            )
        )

        |> runTest "datas/flowName/multipleLevels1.pdf" 
        |> ignore

    testCase "multipleLevels tests2" <| fun _ ->
        let flow = 
            Flow.Manipulate(
                Manipulate(
                    FlowName.Disable,
                    Manipulate.dummy()
                )
                <+>
                Manipulate(
                    FlowName.New "add text to left top 1",
                    (
                        Manipulate(
                            FlowName.New "add text to left top 2",
                            Manipulate(
                                FlowName.Disable,
                                addText "add text to left top 3" (Position.LeftTop(0., 0.)) 
                            )
                        )
                        <+>
                        Manipulate(
                             FlowName.New "add text to left top 4",
                             Manipulate.dummy()
                        )
                    )
                )
                
                <+>
                Manipulate(
                    FlowName.New "add text to top middle 1",
                    Manipulate(
                        FlowName.New "add text to top middle 2",
                        addText "add text to top middle 3" (Position.TopMiddle(0., 0.)) 
                    )
                )
                <+>
                addText "add text to right top" (Position.RightTop(0., 0.))
                <+>
                addText "add text to bottom middle" (Position.BottomMiddle(0., 0.))
            )
            <+>
            Flow.NamedFlow(
                FlowName.New "rotates flow1",
                Flow.NamedFlow(
                    FlowName.New "rotates flow2",
                    Flow.Reuse(
                        Reuses.Rotate(PageSelector.All, Rotation.Counterclockwise)
                        <+>
                        Reuses.Rotate(PageSelector.All, Rotation.Clockwise)

                    )
                    <+>
                    Flow.Reuse(
                        Reuses.Rotate(PageSelector.All, Rotation.Clockwise)
                    )
                )
            )

        Flow.NamedFlow(
            FlowName.New "flow flowName",
            flow
        )

        |> runTest "datas/flowName/multipleLevels2.pdf" 
        |> ignore
  ]