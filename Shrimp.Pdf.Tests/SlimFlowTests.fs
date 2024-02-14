module SlimFlowTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open iText.Layout
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Extract
open Shrimp.Pdf.icms2
open Shrimp.Pdf.SlimFlow
open Shrimp.FSharp.Plus
open Shrimp.Pdf.RegisterableFonts
open Shrimp.Pdf.RegisterableFonts.YaHei
open FontNames.Query
open Imposing
open Shrimp.Pdf.Parser
open Fake.IO
open iText.IO.Font.Constants
open iText.IO.Font
open iText.Kernel.Font
open Shrimp.Pdf.Image




let manipulatesTests =
  ftestList "slimflow manipulate tests" [
    testCase "move pageBox to origin test1" <| fun _ ->
        let flow =
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = ( 
                        SlimModifyPage.MovePageBoxToOrigin()
                    )
                ) 
            ) 

        flow
        |> runTest "datas/slimFlow/move pageBox to origin1.pdf" 
        |> ignore

        pass()

    testCase "move pageBox to origin test2" <| fun _ ->
        let flow =
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = ( 
                        SlimModifyPage.MovePageBoxToOrigin()
                        <+>
                        SlimModifyPage.MapInfos(fun args infos -> infos.SetColor().SetCuttingDie([FsColor.RGB_BLUE]))
                        <+>
                        SlimModifyPage.Func(fun args infos ->
                            SlimModifyPage.SetPageBox(fun m ->
                                { m with Rect = infos.CuttingDieInfos().VisibleBound().Value }
                            )
                        )


                    )
                ) 
            ) 

        flow
        |> runTest "datas/slimFlow/move pageBox to origin2.pdf" 
        |> ignore

        pass()

    testCase "TrimToVisible test" <| fun _ ->
        let flow =
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = (
                        SlimModifyPage.TrimToVisible()
                        <+>
                        SlimModifyPage.MovePageBoxToOrigin()
                    )
                )
            )

        flow
        |> runTest "datas/slimFlow/trimToVisible.pdf" 
        |> ignore

        pass()

    testCase "clear dirty infos test1" <| fun _ ->
        let flow =
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = (
                        SlimModifyPage.TrimToVisible()
                        <+> 
                        SlimModifyPage.ClearDirtyInfos()
                    )
                )
            )

        flow
        |> runTest "datas/slimFlow/clear dirty infos1.pdf" 
        |> ignore

        pass()

    testCase "clear dirty infos test2" <| fun _ ->
        let flow =
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = (
                        SlimModifyPage.TrimToVisible()
                        <+> 
                        SlimModifyPage.ClearDirtyInfos()
                    )
                )
            )

        flow
        |> runTest "datas/slimFlow/clear dirty infos2.pdf" 
        |> ignore

        pass()

    testCase "clear dirty infos test3" <| fun _ ->
        let flow =
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = (
                        SlimModifyPage.ClearDirtyInfos()
                        <+> 
                        SlimModifyPage.TrimToVisible()
                    )
                )
            )

        flow
        |> runTest "datas/slimFlow/clear dirty infos3.pdf" 
        |> ignore

        pass()



    testCase "scale test1" <| fun _ ->
        let flow =
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = (
                        SlimModifyPage.MovePageBoxToOrigin()
                        <+>
                        SlimModifyPage.Scale(2, 2)
                    )
                )
            )

        flow
        |> runTest "datas/slimFlow/scale1.pdf" 
        |> ignore

        pass()

    testCase "scale test2" <| fun _ ->
        let flow =
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = (
                        SlimModifyPage.ClearDirtyInfos()
                        <+>
                        SlimModifyPage.Scale(2, 2)
                    )
                ) 
            )

        flow
        |> runTest "datas/slimFlow/scale2.pdf" 
        |> ignore

        pass()

    testCase "scale test3" <| fun _ ->
        let flow =
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = (
                        SlimModifyPage.ClearDirtyInfos()
                        <+>
                        SlimModifyPage.Scale(2, 2)
                    )
                ) 
            )

        flow
        |> runTest "datas/slimFlow/scale3.pdf" 
        |> ignore

        pass()

    testCase "scale test4" <| fun _ ->
        let flow =
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = ( 
                        SlimModifyPage.MovePageBoxToOrigin()
                        <+>
                        SlimModifyPage.Scale(2, 2)
                    )
                ) 
            ) 

        flow
        |> runTest "datas/slimFlow/scale4.pdf" 
        |> ignore

        pass()

    testCase "compose flow test" <| fun _ ->
        let flow =
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = (
                        SlimModifyPage.MapInfos(fun args infos -> infos.SetColor().SetCuttingDie([FsColor.RGB_BLUE]))
                        <+>
                        SlimModifyPage.TrimToVisible()
                        <+> 
                        SlimModifyPage.ClearDirtyInfos()
                        <+>
                        SlimModifyPage.Func(fun args infos0 ->
                            let targetSize = 
                                { Width = mm 100 
                                  Height = mm 100 }

                            SlimModifyPage.SetPageBox(fun pageBoxSetter ->
                                let infos = infos0.CuttingDieInfos()
                                     
                                let bound = infos.VisibleBound()
                                match bound with 
                                | None -> failwithf "Cannot found any stroke color of BLUE, avaliable colors are %A" (infos0.AllPaths().AllColors())
                                | Some bound ->
                                    let margin = 
                                        Rectangle.calcMargin bound pageBoxSetter.Rect

                                    let scaleX = targetSize.Width / (bound.GetWidthF())
                                    let scaleY = targetSize.Height / (bound.GetHeightF())

                                    { pageBoxSetter with 
                                        TrimBoxMargin = 
                                            margin
                                            |> NegativeMargin.Create
                                            |> Some

                                        Scale = Some (scaleX, scaleY)
                                    }
                        )
                    )
                )
            ))

        flow
        |> runTest "datas/slimFlow/compose flow.pdf" 
        |> ignore

        pass()
  ]