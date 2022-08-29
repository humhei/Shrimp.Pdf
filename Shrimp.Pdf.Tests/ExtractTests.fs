﻿module ExtractTests
open Expecto
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
open Shrimp.Pdf.DSL
open Shrimp.FSharp.Plus
open iText.Kernel.Pdf
open iText.Kernel.Font
open Shrimp.Pdf.Image
open Shrimp.Pdf.Colors
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf.Extract
open Shrimp.Pdf

type Colors =
    /// 橙色
    static member PdfExtractorTagColor = FsColor.CreateRGB(247, 147, 30)

let extractTests =
  testList "Extract Tests" [
    
    testList "groupInto tests" [
        testCase "group into test1" <| fun _ ->
            Flow.Manipulate (
                ModifyPage.Create(
                    "ReadTextInfos",
                    PageSelector.All,
                    Selector.Text(fun _ _ -> true),
                    fun args infos ->
                        let infos =
                            infos
                            |> List.ofSeq
                            |> List.choose IIntegratedRenderInfo.asITextRenderInfo

                        let result = 
                            PageInfosSplitter.Groupby_CenterPointIsInside.Infos__GroupOrFilter_IntoOp(
                                [args.Page.GetActualBox()],
                                infos,
                                (fun m -> 
                                    let bound = ITextRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth m
                                    TargetPageBox(Some bound)
                                )
                            )

                        result

                )
            )
            |> runTest "datas/extract/group into.pdf" 
            |> ignore
            failwith ""

    ]

    testList "extract tests" [
        testCase "extract and scale" <| fun _ -> 
            Flow.Reuse (
                Reuses.TransformForEach(
                    Selector.PathOrText(Info.StrokeColorIs (FsColor.RGB_BLUE)),
                    transform = (fun bound -> Rectangle.applyMargin (-Margin.``MM3``) bound.Bound)
                )
            )
            |> runTest "datas/extract/extract and scale.pdf" 
            |> ignore


        testCase "extract and scale2" <| fun _ -> 
            Flow.Reuse (
                Reuses.TransformForEach(
                    Selector.PathOrText(Info.StrokeColorIs (FsColor.RGB_BLUE)),
                    transform = (fun bound -> Rectangle.applyMargin (-Margin.``MM3``) bound.Bound)
                )
            )
            |> runTest "datas/extract/extract and scale2.pdf" 
            |> ignore

        testCase "extract and scale3" <| fun _ -> 
            Flow.Reuse (
                Reuses.TransformForEach(
                    Selector.PathOrText(Info.StrokeColorIs (FsColor.RGB_BLUE)),
                    transform = (fun bound -> Rectangle.applyMargin (-Margin.``MM6``) bound.Bound)
                )
            )
            |> runTest "datas/extract/extract and scale3.pdf" 
            |> ignore

        testCase "extract and scale4" <| fun _ ->  
            Flow.Reuse (
                Reuses.AddForeground(
                    PdfFile (@"datas/extract/extract and scale4.template.pdf")
                )
                <+>
                Reuses.TransformForEach(
                    Selector.PathOrText(Info.StrokeColorIs (FsColor.RGB_BLUE)),
                    transform = (fun bound -> Rectangle.applyMargin (-Margin.``MM3``) bound.Bound),
                    suffixAction = TransformForEachSuffixAction.AddCropmarkAndCropToBorder(Cropmark.defaultValue, Margin.MM6),
                    textPicker = (
                        { TagColor = Some Colors.PdfExtractorTagColor
                          TransformTextPickers = (fun args bound infos ->
                              let coloredBoxWithTextInfos = ColoredBoxWithTexts.Pick(PageNumber args.PageNum, Colors.PdfExtractorTagColor, infos)
                              coloredBoxWithTextInfos :> System.IComparable
                          )}
                    )
                )
            )
            |> runTest "datas/extract/extract and scale4.pdf" 
            |> ignore

        testCase "extract paths tests" <| fun _ -> 

            Flow.Reuse (
                Reuses.ExtractPaths(PageSelector.All, Info.StrokeColorIs (FsColor.Separation cuttingLineSeparation), keepOriginPage = true)
            )
            |> runTest "datas/extract/extract paths tests.pdf" 
            |> ignore

        testCase "extract vectors tests" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(Info.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                )
            )
            |> runTest "datas/extract/extract vectors.pdf" 
            |> ignore

        testCase "extract vectors tests2" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(Info.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                )
            )
            |> runTest "datas/extract/extract vectors2.pdf" 
            |> ignore

        testCase "extract vectors tests3" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(Info.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                )
            )
            |> runTest "datas/extract/extract vectors3.pdf" 
            |> ignore

        testCase "extract vectors tests4" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(Info.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                )
            )
            |> runTest "datas/extract/extract vectors4.pdf" 
            |> ignore

        testCase "extract vectors tests5" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(Info.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                )
            )
            |> runTest "datas/extract/extract vectors5.pdf" 
            |> ignore

        testCase "extract vectors tests6" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(Info.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                )
            )
            |> runTest "datas/extract/extract vectors6.pdf" 
            |> ignore

        testCase "extract vectors tests7" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(Info.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                )
            )
            |> runTest "datas/extract/extract vectors7.pdf" 
            |> ignore


        testCase "extract vectors tests8" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(Info.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                )
            )
            |> runTest "datas/extract/extract vectors8.pdf" 
            |> ignore

        testCase "extract vectors tests9" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(Info.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                )
            )
            |> runTest "datas/extract/extract vectors9.pdf" 
            |> ignore

        testCase "extract vectors tests10" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(Info.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                )
            )
            |> runTest "datas/extract/extract vectors10.pdf" 
            |> ignore

        testCase "extract vectors tests11" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(Info.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                )
            )
            |> runTest "datas/extract/extract vectors11.pdf" 
            |> ignore

        testCase "extract vectors tests12" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(Info.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.TrimBox))
                )
            )
            |> runTest "datas/extract/extract vectors12.pdf" 
            |> ignore

        testCase "extract vectors tests13" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(Info.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.TrimBox))
                )
            )
            |> runTest "datas/extract/extract vectors13.pdf" 
            |> ignore


        testCase "extract vectors tests14" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(fun args info ->
                        let b = Info.FillColorIs FsColor.RGB_RED args info
                        if b && args.PageNum = 2 then ()
                        b
                    
                    )
                )
            )
            |> runTest "datas/extract/extract vectors14.pdf" 
            |> ignore

        
        testCase "extract vectors tests15" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(fun args info -> 
                        true
                    )
                )
            )
            |> runTest "datas/extract/extract vectors15.pdf" 
            |> ignore

        
        testCase "extract vectors tests16" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.PathOrText(fun args info -> 
                        true
                    )
                )
            )
            |> runTest "datas/extract/extract vectors16.pdf" 
            |> ignore




        testCase "extract object tests" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.All(InfoIM.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                )
            )
            |> runTest "datas/extract/extract objects.pdf" 
            |> ignore

        testCase "extract object tests2" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.All(InfoIM.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.TrimBox))
                )
            )
            |> runTest "datas/extract/extract objects2.pdf" 
            |> ignore

        testCase "extract objects tests3" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.All(InfoIM.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.TrimBox))
                )
            )
            |> runTest "datas/extract/extract objects3.pdf" 
            |> ignore

        
        testCase "extract objects tests4" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.All(InfoIM.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.TrimBox))
                )
            )
            |> runTest "datas/extract/extract objects4.pdf" 
            |> ignore

        testCase "extract objects tests5" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.All(InfoIM.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.TrimBox))
                )
            )
            |> runTest "datas/extract/extract objects5.pdf" 
            |> ignore

        testCase "extract objects tests7" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.All(InfoIM.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.TrimBox))
                )
            )
            |> runTest "datas/extract/extract objects7.pdf" 
            |> ignore


        testCase "extract objects tests8" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.All(InfoIM.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                )
            )
            |> runTest "datas/extract/extract objects8.pdf" 
            |> ignore


        ftestCase "extract objects tests9" <| fun _ -> 
            Flow.Reuse (
                Reuses.ExtractIM(
                    PageSelector.All,
                    Selector.All(InfoIM.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                )
            )
            |> runTest "datas/extract/extract objects9.pdf" 
            |> ignore
    ]



    testList "tile page tests" [


        testCase "tile pages and NUP for big data" <| fun _ -> 
            let colNum = 5 
            let rowNum = 8
            Flows.TilePages (
                TileTableIndexer.Create (colNum = colNum, rowNum = rowNum),
                Direction.Horizontal,
                pageTilingRenewOptions = PageTilingRenewOptions.VisibleInfosInActualBox (PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside_MM0))
            <+>
            Flow.Reuse(
                Reuses.Impose(fun args ->
                    { args with 
                        ColNums = [colNum]
                        RowNum = rowNum
                        Background = Background.Size FsSize.MAXIMUN
                    }
                )
            )

            <+>
            Flows.TilePages (TileTableIndexer.Create (colNum = colNum, rowNum = rowNum), Direction.Horizontal)
            <+>
            Flow.Reuse(
                Reuses.Impose(fun args ->
                    { args with 
                        ColNums = [colNum]
                        RowNum = rowNum
                        Background = Background.Size FsSize.MAXIMUN
                    }
                )
            )
            |> runTest "datas/extract/tile pages and NUP for big data.pdf" 
            |> ignore

        testCase "tile pages by colNum and rowNum tests" <| fun _ -> 
            (Flows.TilePages (TileTableIndexer.Create (colNum = 3, rowNum = 2), Direction.Vertical))
            |> runTest "datas/extract/tile pages by colNum and rowNum.pdf" 
            |> ignore

        testCase "tile pages for big data" <| fun _ -> 
            let tileTable = 
                TileTableIndexer.Create (
                    colNum = 2,
                    rowNum = 5
                )
            (Flows.TilePages (tileTable, Direction.Horizontal, pageTilingRenewOptions = PageTilingRenewOptions.VisibleInfosInActualBox PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside_MM0))
            |> runTest "datas/extract/tile pages for big data.pdf" 
            |> ignore

        testCase "tile pages by colNum and rowNum tests2" <| fun _ -> 
            let tileTable = 
                TileTableIndexer.Create (
                    colNum = 6,
                    rowNum = 4,
                    HSpacing = [mm 1.; mm 2.],
                    VSpacing = [mm 3.; mm 6.; mm 9.]
                )
            (Flows.TilePages (tileTable, Direction.Vertical, pageTilingRenewOptions = PageTilingRenewOptions.VisibleInfosInActualBox PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside_MM3))
            |> runTest "datas/extract/tile pages by colNum and rowNum2.pdf" 
            |> ignore

        testCase "tile pages by selector tests" <| fun _ -> 
            Flows.TilePages(
                Path(Info.StrokeColorIs FsColor.RGB_BLUE <&&> Info.BoundIsInsideOf(AreaGettingOptions.PageBox PageBoxKind.ActualBox)),
                sorter = SelectionSorter.Plane(mm 3., Direction.Vertical)
            )
            //<.+>
            //(Flow.Func(fun (pageTilingResults: PageTilingResults) ->
            //    Flows.PickFromPageTilingResult(pageTilingResults.PageTilingResultCounts, PageNumSequence.Create [1])
            //))

            |> runTest "datas/extract/tile pages by selector.pdf" 
            |> ignore

        testCase "tile pages by selector tests2" <| fun _ -> 
            Flows.TilePages
                (Path(Info.StrokeColorIs FsColor.RGB_BLUE <&&> Info.BoundIsInsideOf(AreaGettingOptions.PageBox PageBoxKind.ActualBox)),
                distincterOrTextPicker = 
                    PageTilingDistincterOrTextPicker.Distincter (
                        PageTilingDistincter.Text (fun args bound infos ->
                            let texts = 
                                infos
                                |> List.ofSeq
                                |> List.choose (IIntegratedRenderInfo.asITextRenderInfo)
                                |> List.filter(fun m -> 
                                    let textInfoBound = ITextRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth m
                                    textInfoBound.IsCenterPointInsideOf(bound.Bound)
                                )
                                |> List.map(fun m -> m.Text())

                            texts :> System.IComparable
                        )
                    ),
                pageTilingRenewOptions = 
                    (PageTilingRenewOptions.VisibleInfosInActualBox(PageTilingRenewInfosSplitter.``Groupby_DenseBoundIsInside_MM1.5``)),
                
                borderKeepingPageSelector = NullablePageSelector.Non,

                samplePageExtractingOptions = 
                    (SamplePageExtractingOptions.FirstPageFirstSelector (PdfPath @"C:\Users\Jia\Desktop\mySample.pdf"))
            )

            |> runTest "datas/extract/tile pages by selector2.pdf" 
            |> ignore

        testCase "tile pages by selector tests3" <| fun _ -> 
            Flow.Reuse (
                Reuses.AddForeground(
                    PdfFile (@"datas/extract/extract and scale4.template.pdf")
                )
            )
            <+>
            Flows.TilePages
                (Path(Info.StrokeColorIs FsColor.RGB_BLUE <&&> Info.BoundIsInsideOf(AreaGettingOptions.PageBox PageBoxKind.ActualBox)),
                distincterOrTextPicker = (PageTilingDistincterOrTextPicker.TextPicker(
                    { TagColor = Some Colors.PdfExtractorTagColor
                      TransformTextPickers = 
                        (fun args bound infos ->
                            let coloredBoxWithTextInfos = ColoredBoxWithTexts.Pick(PageNumber args.PageNum, Colors.PdfExtractorTagColor, infos)
                            coloredBoxWithTextInfos :> System.IComparable
                        )
                    }
                )),
                pageTilingRenewOptions = 
                    PageTilingRenewOptions.VisibleInfosInActualBox(
                        PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside_MM0
                    ),
                borderKeepingPageSelector = NullablePageSelector.First,
                transform = (fun rect -> Rectangle.applyMargin -Margin.MM3 rect.Bound)
            )
            <+>
            Flow.Reuse(
                Reuse.Func(fun (r: PageTilingResults) ->
                    match r.Layouts with 
                    | PageTilingLayoutResults.DistinctedOne r -> 
                        Reuses.Impose(fun args ->
                            { args with 
                                ColNums = r.ColNums
                                RowNum = r.RowNum
                                Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (Margin.MM6)
                                Cropmark = Some Cropmark.defaultValue
                            }
                        )

                    | _ -> failwith "Not implemented"
                )
            )
            |> runTest "datas/extract/tile pages by selector3.pdf" 
            |> ignore
    
        testCase "tile pages by selector tests5" <| fun _ -> 
            Flows.TilePagesAndNUp
                (Path(Info.StrokeColorIs FsColor.RGB_BLUE <&&> Info.BoundIsInsideOf(AreaGettingOptions.PageBox PageBoxKind.ActualBox)),
                //pageTilingRenewOptions = 
                //    (PageTilingRenewOptions.VisibleInfosInActualBox(PageTilingRenewInfosSplitter.``Groupby_DenseBoundIsInside_MM1.5``)),
                textPicker =( 
                   { TagColor = Some Colors.PdfExtractorTagColor
                     TransformTextPickers = 
                       (fun args bound infos ->
                           let coloredBoxWithTextInfos = ColoredBoxWithTexts.Pick(PageNumber args.PageNum, Colors.PdfExtractorTagColor, infos)
                           coloredBoxWithTextInfos :> System.IComparable
                       )
                   }
                ),
                pageTilingRenewInfosSplitter = PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside_MM0,
                borderKeepingPageSelector = NullablePageSelector.Non,
                transform = (fun rect -> Rectangle.applyMargin -Margin.MM3 rect.Bound),
                samplePageExtractingOptions = 
                    (SamplePageExtractingOptions.FirstPageFirstSelector (PdfPath @"C:\Users\Jia\Desktop\mySample.pdf"))
            )

            |> runTest "datas/extract/tile pages by selector5.pdf" 
            |> ignore

        testCase "tile pages by selector tests6" <| fun _ -> 
            Flows.TilePages
                (Path(Info.StrokeColorIs FsColor.RGB_BLUE <&&> Info.BoundIsInsideOf(AreaGettingOptions.PageBox PageBoxKind.ActualBox)),
                distincterOrTextPicker = 
                    PageTilingDistincterOrTextPicker.Distincter (
                        PageTilingDistincter.Text (fun args bound infos ->
                            let texts = 
                                infos
                                |> List.ofSeq
                                |> List.choose (IIntegratedRenderInfo.asITextRenderInfo)
                                |> List.filter(fun m -> 
                                    let textInfoBound = ITextRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth m
                                    textInfoBound.IsCenterPointInsideOf(bound.Bound)
                                )
                                |> List.map(fun m -> m.Text())

                            texts :> System.IComparable
                        )
                    ),
                pageTilingRenewOptions = 
                    (PageTilingRenewOptions.VisibleInfosInActualBox(PageTilingRenewInfosSplitter.``Groupby_DenseBoundIsInside_MM1.5``)),
                
                borderKeepingPageSelector = NullablePageSelector.All,

                samplePageExtractingOptions = 
                    (SamplePageExtractingOptions.FirstPageFirstSelector (PdfPath @"C:\Users\Jia\Desktop\mySample.pdf"))
            )

            |> runTest "datas/extract/tile pages by selector6.pdf" 
            |> ignore

        testCase "tile pages by selector tests7" <| fun _ -> 
            Flows.TilePages(
                Path(Info.StrokeColorIs FsColor.RGB_BLUE <&&> Info.BoundIsInsideOf(AreaGettingOptions.PageBox PageBoxKind.ActualBox)),
                sorter = SelectionSorter.Plane(mm 3., Direction.Vertical)
            )
            //<.+>
            //(Flow.Func(fun (pageTilingResults: PageTilingResults) ->
            //    Flows.PickFromPageTilingResult(pageTilingResults.PageTilingResultCounts, PageNumSequence.Create [1])
            //))

            |> runTest "datas/extract/tile pages by selector7.pdf" 
            |> ignore

        testCase "tile pages and NUP by selector" <| fun _ -> 
            Flow.Reuse (
                Reuses.AddForeground(
                    PdfFile (@"datas/extract/extract and scale4.template.pdf")
                )
            )
            <+>
            Flows.TilePagesAndNUp(
                 Path(Info.StrokeColorIs FsColor.RGB_BLUE <&&> Info.BoundIsInsideOf(AreaGettingOptions.PageBox PageBoxKind.ActualBox)),
                 textPicker =( 
                    { TagColor = Some Colors.PdfExtractorTagColor
                      TransformTextPickers = 
                        (fun args bound infos ->
                            let coloredBoxWithTextInfos = ColoredBoxWithTexts.Pick(PageNumber args.PageNum, Colors.PdfExtractorTagColor, infos)
                            coloredBoxWithTextInfos :> System.IComparable
                        )
                    }
                ),
                pageTilingRenewInfosSplitter = PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside_MM0,
                transform = (fun rect -> Rectangle.applyMargin -Margin.MM3 rect.Bound)
            )
            |> runTest "datas/extract/tile pages and NUP by selector.pdf" 
            |> ignore

        testCase "tile pages and NUP by selector2" <| fun _ -> 
            let pageNumbers =
                [1; 10]
                |> List.map PageNumber
                |> AtLeastTwoList.Create 

            Flow.Reuse (
                Reuses.AddForeground(
                    PdfFile (@"datas/extract/tile pages and NUP by selector2_CustomSupply.pdf")
                )
            )
            <+>
            Flows.TilePagesAndNUp(
                 Path(Info.StrokeColorIs FsColor.RGB_BLUE <&&> Info.BoundIsInsideOf(AreaGettingOptions.PageBox PageBoxKind.ActualBox)),
                 textPicker =( 
                    { TagColor = Some Colors.PdfExtractorTagColor
                      TransformTextPickers = 
                        (fun args bound infos ->
                            let coloredBoxWithTextInfos = ColoredBoxWithTexts.Pick(PageNumber args.PageNum, Colors.PdfExtractorTagColor, infos)
                            coloredBoxWithTextInfos :> System.IComparable
                        )
                    }
                ),
                pageTilingRenewInfosSplitter = PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside_MM0,
                transform = (fun rect -> Rectangle.applyMargin -Margin.MM3 rect.Bound),
                samplePageExtractingOptions = 

                    (SamplePageExtractingOptions.FirstPageMultipleSelectors (pageNumbers,PdfPath @"C:\Users\Jia\Desktop\mySample.pdf"))
            )
            |> runTest "datas/extract/tile pages and NUP by selector2.pdf" 
            |> fun m -> failwith ""
            |> ignore

        testCase "tile pages by selector tests8" <| fun _ -> 
            Flows.TilePages
                (Path(Info.StrokeColorIs FsColor.RGB_BLUE <&&> Info.BoundIsInsideOf(AreaGettingOptions.PageBox PageBoxKind.ActualBox)),
                distincterOrTextPicker = 
                    PageTilingDistincterOrTextPicker.Distincter (
                        PageTilingDistincter.Text (fun args bound infos ->
                            let texts = 
                                infos
                                |> List.ofSeq
                                |> List.choose (IIntegratedRenderInfo.asITextRenderInfo)
                                |> List.filter(fun m -> 
                                    let textInfoBound = ITextRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth m
                                    textInfoBound.IsCenterPointInsideOf(bound.Bound)
                                )
                                |> List.map(fun m -> m.Text())

                            texts :> System.IComparable
                        )
                    ),
                pageTilingRenewOptions = 
                    (PageTilingRenewOptions.UsingOriginPdfPage),
                    //(PageTilingRenewOptions.VisibleInfosInActualBox(PageTilingRenewInfosSplitter.``Groupby_DenseBoundIsInside_MM1.5``)),
                
                borderKeepingPageSelector = NullablePageSelector.All,

                samplePageExtractingOptions = 
                    (SamplePageExtractingOptions.FirstPageFirstSelector (PdfPath @"C:\Users\Jia\Desktop\mySample.pdf"))
            )

            |> runTest "datas/extract/tile pages by selector8.pdf" 
            |> ignore

    ]



]