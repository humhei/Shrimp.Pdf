module SlimFlowTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Extract
open Shrimp.Pdf.SlimFlow
open Shrimp.FSharp.Plus




let manipulatesTests =
  testList "slimflow manipulate tests" [
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
                                { m with Rect = infos.CuttingDieInfosBound().Value }
                            )
                        )


                    )
                ) 
            ) 

        flow
        |> runTest "datas/slimFlow/move pageBox to origin2.pdf" 
        |> ignore

        pass()

    ftestCase "TrimToVisible test" <| fun _ ->
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



    testCase "add background test1" <| fun _ ->
        let flow =
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = 
                        SlimModifyPage.AddBackgroundOrForeground(
                            BackgroundFile.Create(@"datas/slimFlow/add background1.bk.pdf"),
                            layerName = BackgroundAddingLayerOptions.Create("current", "background")
                        )
                )  
            )  

        flow
        |> runTest "datas/slimFlow/add background1.pdf" 
        |> ignore

        pass()

    testCase "add background test2" <| fun _ ->
        let flow =
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = 
                        SlimModifyPage.AddBackgroundOrForeground(
                            BackgroundFile.Create(@"datas/slimFlow/add background2.bk.pdf"),
                            layerName = BackgroundAddingLayerOptions.Create("current", "background"),
                            refOptions = SlimBackgroundRefOptions.XObject_Simply
                        )
                )  
            )  

        flow
        |> runTest "datas/slimFlow/add background2.pdf" 
        |> ignore

        pass()

    testCase "add background test3" <| fun _ ->
        let flow =
            let files = 
                [
                    @"datas/slimFlow/add background3.bk-1,3.pdf"
                    @"datas/slimFlow/add background3.bk-2.pdf"
                    @"datas/slimFlow/add background3.bk-1,3.pdf"
                ]
                |> List.map PdfFile
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = SlimModifyPage.AddBackgroundOrForeground(
                        files,
                        layerName = BackgroundAddingLayerOptions.Create("current", "background"),
                        refOptions = SlimBackgroundRefOptions.XObject_Simply

                    )
                )  
            )  

        flow
        |> runTest "datas/slimFlow/add background3.pdf" 
        |> ignore

        pass()
         
    testCase "add background test4" <| fun _ ->
        let flow =
            Flow.Reuse ( 
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = (
                        SlimModifyPage.AddBackgroundOrForeground(
                            BackgroundFile.Create(@"datas/slimFlow/add background4.bk.pdf"),
                            layerName = BackgroundAddingLayerOptions.Create("current", "background"),
                            refOptions = SlimBackgroundRefOptions.XObject_Simply
                        )
                        <+>
                        SlimModifyPage.ClearDirtyInfos() 
                        <+>
                        SlimModifyPage.MapInfos(fun args infos ->
                            infos
                        )
                    )
                )    
            )   

        flow
        |> runTest "datas/slimFlow/add background4.pdf" 
        |> ignore

        pass()

    testCase "add background test5" <| fun _ ->
        let flow =
            Flow.Reuse ( 
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = (
                        SlimModifyPage.AddBackgroundOrForeground(
                            BackgroundFile.Create(@"datas/slimFlow/add background5.bk1.pdf"),
                            layerName = BackgroundAddingLayerOptions.Create("current", "background1"),
                            refOptions = SlimBackgroundRefOptions.XObject_Simply

                        )
                        <+>
                        SlimModifyPage.AddBackgroundOrForeground(
                            BackgroundFile.Create(@"datas/slimFlow/add background5.bk2.pdf"),
                            layerName = BackgroundAddingLayerOptions.Create("current", "background2"),
                            refOptions = SlimBackgroundRefOptions.XObject_Simply
                        )
                        <+>
                        SlimModifyPage.ClearDirtyInfos() 
                        <+>
                        SlimModifyPage.MapInfos(fun args infos ->
                            infos
                        )
                        <+>
                        SlimModifyPage.AddBackgroundOrForeground(
                            BackgroundFile.Create(@"datas/slimFlow/add background5.fr.pdf"),
                            layerName = BackgroundAddingLayerOptions.Create("current", "fr")
                        )
                    )
                )    
            )   

        flow
        |> runTest "datas/slimFlow/add background5.pdf" 
        |> ignore

        pass()

    testCase "replace colors" <| fun _ ->
        let color = FsColor.valueColor (FsDeviceCmyk.Create(0.35f, 0f, 0f, 0f))
        let replacement = FsColor.CMYK_MAGENTA

        let flow =
                
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = (
                        SlimModifyPage.MapInfos(fun args infos ->
                            infos
                                .SetColor()
                                .SetCuttingDie([FsColor.RGB_BLUE])
                                .ReplaceColors([color], replacement)
                        )
                        <+>
                        SlimModifyPage.ClearDirtyInfos() 
                    )
                )    
            )   

        flow
        |> runTest "datas/slimFlow/replace Colors1.pdf" 
        |> ignore

        pass()

    testCase "compose flow test" <| fun _ ->
        let flow =
            Flow.Reuse (
                Reuses.SlimFlows(
                    PageSelector.All,
                    slimFlow = (
                        SlimModifyPage.AddBackgroundOrForeground(
                            BackgroundFile.Create(@"datas/slimFlow/compose flow.bk.pdf"),
                            layerName = BackgroundAddingLayerOptions.Create("current", "background"),
                            refOptions = SlimBackgroundRefOptions.XObject_Simply
                        )
                        <+>
                        SlimModifyPage.MapInfos(fun args infos -> 
                            infos
                                .SetColor()
                                .SetCuttingDie([FsColor.RGB_BLUE])
                        )
                        <+>
                        SlimModifyPage.ClearDirtyInfos()
                        <+>
                        SlimModifyPage.Func(fun args infos0 ->
                            let targetSize = 
                                { Width = mm 150
                                  Height = mm 100 }

                            SlimModifyPage.SetPageBox(fun pageBoxSetter ->
                                let bound = infos0.CuttingDieInfosBound()
                                match bound with 
                                | None -> failwithf "Cannot found any stroke color of BLUE, avaliable colors are %A" (infos0.AllColors())
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