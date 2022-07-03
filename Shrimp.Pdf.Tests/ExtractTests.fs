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
                    transform = (fun bound -> Rectangle.applyMargin (-Margin.``MM3``) bound.Bound)
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
    ]



    testList "tile page tests" [


        testCase "tile pages and NUP for big data" <| fun _ -> 
            let colNum = 5 
            let rowNum = 8
            Flow.Reuse (
                Reuses.TilePages (TileTableIndexer.Create (colNum = colNum, rowNum = rowNum), Direction.Horizontal)
                <+>
                Reuses.Impose(fun args ->
                    { args with 
                        ColNums = [colNum]
                        RowNum = rowNum
                        Background = Background.Size FsSize.MAXIMUN
                    }
                )
                <+>
                Reuses.TilePages (TileTableIndexer.Create (colNum = colNum, rowNum = rowNum), Direction.Horizontal)
                <+>
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
            Flow.Reuse (Reuses.TilePages (TileTableIndexer.Create (colNum = 3, rowNum = 2), Direction.Vertical))
            |> runTest "datas/extract/tile pages by colNum and rowNum.pdf" 
            |> ignore

        testCase "tile pages for big data" <| fun _ -> 
            let tileTable = 
                TileTableIndexer.Create (
                    colNum = 2,
                    rowNum = 5
                )
            Flow.Reuse (Reuses.TilePages (tileTable, Direction.Horizontal, pageTilingRenewOptions = PageTilingRenewOptions.VisibleInfosInActualBox PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside_MM0))
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
            Flow.Reuse (Reuses.TilePages (tileTable, Direction.Vertical, pageTilingRenewOptions = PageTilingRenewOptions.VisibleInfosInActualBox PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside_MM3))
            |> runTest "datas/extract/tile pages by colNum and rowNum2.pdf" 
            |> ignore

        testCase "tile pages by selector tests" <| fun _ -> 
            Flow.Reuse (
                Reuses.TilePages
                    (Path(Info.StrokeColorIs FsColor.RGB_BLUE <&&> Info.BoundIsInsideOf(AreaGettingOptions.PageBox PageBoxKind.ActualBox)),
                     sorter = SelectionSorter.Plane(mm 3., Direction.Vertical)
                )
                <.+>
                (Reuse.Func(fun (pageTilingResults: PageTilingResults) ->
                    Reuses.PickFromPageTilingResult(pageTilingResults.PageTilingResultCounts, PageNumSequence.Create [1])
                ))

            )
            |> runTest "datas/extract/tile pages by selector.pdf" 
            |> ignore

        ftestCase "tile pages by selector tests2" <| fun _ -> 
            Flow.Reuse (
                Reuses.TilePages
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
                    pageTilingRenewOptions = PageTilingRenewOptions.VisibleInfosInActualBox(PageTilingRenewInfosSplitter.``Groupby_DenseBoundIsInside_MM1.5``)
                )

            )
            |> runTest "datas/extract/tile pages by selector2.pdf" 
            |> ignore

        testCase "tile pages by selector tests3" <| fun _ -> 
            Flow.Reuse (
                Reuses.AddForeground(
                    PdfFile (@"datas/extract/extract and scale4.template.pdf")
                )
                <+>
                Reuses.TilePages
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
                    borderKeepingPageSelector = PageSelector.First,
                    transform = (fun rect -> Rectangle.applyMargin -Margin.MM3 rect.Bound)
                )
                <+>
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
    
        testCase "tile pages and NUP by selector" <| fun _ -> 
            Flow.Reuse (
                Reuses.AddForeground(
                    PdfFile (@"datas/extract/extract and scale4.template.pdf")
                )
                <+>
                Reuses.TilePagesAndNUp(
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
            )
            |> runTest "datas/extract/tile pages and NUP by selector.pdf" 
            |> ignore

    ]



]