﻿module ReuseTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
open Shrimp.Pdf.DSL

let reuseTests =
  testList "Reuse Tests" [
    testCase "imposing N-UP tests" <| fun _ -> 
        Flow.Reuse (
            Reuse.dummy()
            <+>
            Reuses.Impose
                (fun args ->
                    { args with 
                        DesiredSizeOp = Some { Width = mm 50.; Height = mm 50.}
                        ColNums = [4]
                        RowNum = 4
                        Cropmark = Some Cropmark.defaultValue
                        Background = Background.Size FsSize.A0
                        HSpaces = [mm 3.; mm 9.]
                        VSpaces = [mm 3.; mm 9.]
                        Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (Margin.Create(mm 30., mm 30., mm 30., mm 40.))
                        UseBleed = true
                    }
                ) ||>> fun imposingDocument -> imposingDocument.GetSheets()
        )
        |> runTest "datas/reuse/Imposing N-UP.pdf" 
        |> ignore

    testCase "imposing N-UP2 tests" <| fun _ -> 

        Flow.Reuse (
            Reuses.Rotate(
                PageSelector.All,
                Rotation.Counterclockwise
            )
            <+>
            Reuses.Impose
                (fun args ->
                    { args with 
                        DesiredSizeOp = Some { Width = mm 30.; Height = mm 40.}
                        ColNums = [6]
                        RowNum = 12
                        Cropmark = Some Cropmark.defaultValue
                        Background = Background.Size FsSize.MAXIMUN
                        Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable(Margin.Create(mm 6.))
                        CellRotation = CellRotation.R180WhenColNumIsEven
                    }
                )
        )
        |> runTest "datas/reuse/imposing N-UP2.pdf" 
        |> ignore

    testCase "imposing stepAndRepeat tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with 
                    DesiredSizeOp = Some { Width = mm 50.; Height = mm 50.}
                    ColNums = [2]
                    RowNum = 2
                    Cropmark = Some Cropmark.defaultValue
                    HSpaces = [mm 3.; mm 9.]
                    VSpaces = [mm 3.; mm 9.]
                    Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (Margin.Create (mm 0., mm 6., mm 9., mm 12.))
                    IsRepeated = true
                }
            )
        )
        |> runTest "datas/reuse/imposing stepAndRepeat.pdf" 
        |> ignore

    testCase "imposing stepAndRepeat2 tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with 
                    DesiredSizeOp = Some { Width = mm 80.; Height = mm 50.}
                    Cropmark = Some Cropmark.defaultValue
                    Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (Margin.Create(mm 6.))
                    Background = 
                        let size =
                            { Width = FsSize.A4.Width + mm 12. 
                              Height = FsSize.A4.Height + mm 12. }
                        Background.Size size
                    IsRepeated = true
                }
            )
        )
        |> runTest "datas/reuse/imposing stepAndRepeat2.pdf" 
        |> ignore


    testCase "Imposing when use bleed and bleedBox bigger than actualbox" <| fun _ -> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with 
                    DesiredSizeOp = Some { Width = mm 50.; Height = mm 50.}
                    ColNums = [4]
                    RowNum = 4
                    Cropmark = Some Cropmark.defaultValue
                    Background = Background.Size FsSize.A0
                    HSpaces = [mm 3.; mm 9.]
                    VSpaces = [mm 3.; mm 9.]
                    Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable(Margin.Create(mm 9.863, mm 11.201, 0., 0.))
                    UseBleed = true
                }
            )
        )
        |> runTest "datas/reuse/Imposing when use bleed and bleedBox bigger than actualbox.pdf" 
        |> ignore

    ftestCase "Imposing when cell roation is setted" <| fun _ -> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with 
                    DesiredSizeOp = Some { Width = mm 50.; Height = mm 30.}
                    ColNums = [4]
                    RowNum = 4
                    Cropmark = Some Cropmark.defaultValue
                    Background = Background.Size FsSize.A0
                    HSpaces = [mm 3.; mm 9.]
                    VSpaces = [mm 3.; mm 9.]
                    Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable(Margin.Create(mm 6., mm 6., mm 6., mm 6.))
                    UseBleed = true
                }
            )
        )
        |> runTest "datas/reuse/Imposing when cell roation is setted.pdf" 
        |> ignore

    testCase "Imposing when backgroudFile is setted" <| fun _ -> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with 
                    DesiredSizeOp = Some { Width = mm 30.; Height = mm 30.}
                    ColNums = [8]
                    RowNum = 11
                    Background = Background.File (BackgroundFile.Create "datas/reuse/backgroundFile.pdf")
                    HSpaces = [mm 4.]
                    VSpaces = [mm 3.]
                    Sheet_PlaceTable = Sheet_PlaceTable.At(Position.LeftTop(mm 9.863, mm -11.201))
                }
            )
        )
        |> runTest "datas/reuse/Imposing when backgroudFile is setted.pdf" 
        |> ignore

    testCase "clockwise all pages" <| fun _ -> 
        Flow.Reuse (Reuses.Rotate(PageSelector.All, Rotation.Clockwise))
        |> runTest "datas/reuse/clockwise all pages.pdf" 
        |> ignore

    testCase "clockwise all pages2" <| fun _ -> 
        Flow.Reuse (Reuses.Rotate(PageSelector.All, Rotation.Clockwise))
        |> runTest "datas/reuse/clockwise all pages2.pdf" 
        |> ignore

    testCase "clockwise all pages3" <| fun _ -> 
        Flow.Reuse (Reuses.Rotate(PageSelector.All, Rotation.Clockwise))
        |> runTest "datas/reuse/clockwise all pages3.pdf" 
        |> ignore

    testCase "duplicate all pages 15x tests" <| fun _ -> 
        Flow.Reuse (Reuses.DuplicatePages(PageSelector.All, 15))
        |> runTest "datas/reuse/duplicate all pages 15x.pdf" 
        |> ignore

    testCase "duplicate pages by page num sequence tests" <| fun _ -> 
        Flow.Reuse (Reuses.SequencePages (PageNumSequence.Create [1;1;1;3;4;5;8]))
        |> runTest "datas/reuse/duplicate pages by page num sequence.pdf" 
        |> ignore

    testCase "duplicate pages by page num sequence tests2" <| fun _ -> 
        Flow.Reuse (Reuses.SequencePages (PageNumSequence.Create [1;1;1;3;4;5;8]))
        |> runTest "datas/reuse/duplicate pages by page num sequence2.pdf" 
        |> ignore

    testCase "duplicate pages by page num sequence tests3" <| fun _ -> 
        Flow.Reuse (Reuses.SequencePages (PageNumSequence.Create [1, Rotation.Clockwise; 1, Rotation.Counterclockwise; 5, Rotation.Counterclockwise]))
        |> runTest "datas/reuse/duplicate pages by page num sequence3.pdf" 
        |> ignore


    testCase "duplicate pages by copied num sequence tests3" <| fun _ -> 
        Flow.Reuse (Reuses.DuplicatePages (PageSelector.All, CopiedNumSequence.Create [15;15;15;15]))
        |> runTest "datas/reuse/duplicate pages by copied num sequence.pdf" 
        |> ignore

    testCase "tile pages by colNum and rowNum tests" <| fun _ -> 
        Flow.Reuse (Reuses.TilePages (TileTable.Create (colNum = 3, rowNum = 2), Direction.Vertical))
        |> runTest "datas/reuse/tile pages by colNum and rowNum.pdf" 
        |> ignore

    testCase "tile pages by colNum and rowNum tests2" <| fun _ -> 
        let tileTable = 
            TileTable.Create (
                colNum = 6,
                rowNum = 4,
                HSpacing = [mm 1.; mm 2.],
                VSpacing = [mm 3.; mm 6.; mm 9.]
            )
        Flow.Reuse (Reuses.TilePages (tileTable, Direction.Vertical))
        |> runTest "datas/reuse/tile pages by colNum and rowNum2.pdf" 
        |> ignore

    testCase "tile pages by selector tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.TilePages
                (Path(Info.StrokeColorIs DeviceRgb.BLUE <&&> Info.BoundIsInsideOf(AreaGettingOptions.PageBox PageBoxKind.ActualBox)),
                 SelectionSorter.Plane(mm 3., Direction.Vertical)
            )
            <.+>
            (Reuse.Func(fun userState ->
                Reuses.PickFromPageTilingResult(userState, PageNumSequence.Create [1])
            ))
        )
        |> runTest "datas/reuse/tile pages by selector.pdf" 
        |> ignore

    testCase "trim to first stroke color" <| fun _ -> 
        let strokeColor = new DeviceRgb(0.498f, 0.616f, 0.725f)
        let pageBoxKind = PageBoxKind.AllBox
        Flow.Manipulate(
            ModifyPage.Create( 
                sprintf "Set %O to cuttingLineColors" pageBoxKind,
                PageSelector.All,
                Path (Info.StrokeColorIs strokeColor),
                (fun args infos -> 
                    let rects = 
                        infos
                        |> Seq.map (IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth)
                        |> List.ofSeq

                    let maxY = 
                        rects 
                        |> List.map (fun m -> m.GetYF())
                        |> List.max
                            
                    let minX = 
                        rects 
                        |> List.map (fun m -> m.GetXF())
                        |> List.min

                    let rect = 
                        rects
                        |> List.filter(fun m -> m.GetYF() @= maxY)
                        |> List.minBy(fun m -> m.GetX())

                    if (rect.GetXF() @= minX)
                    then
                        PageModifier.SetPageBox(pageBoxKind, rect) args infos
                    else failwithf "Cannot trim to the leftTop item, the top rect is %A, minX is %f" rect minX 
                )
            )
        )
      
        |> runTest "datas/reuse/trim to first stroke color.pdf" 
        |> ignore

    testCase "move pagebox to origin tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.MovePageBoxToOrigin(PageSelector.All)
        )
        |> runTest "datas/reuse/move pagebox to origin.pdf" 
        |> ignore

    testCase "resize pageSize to 7x4cm tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Resize(PageSelector.All, PageBoxKind.ActualBox, {Width = mm 70.; Height = mm 40.})
        )
        |> runTest "datas/reuse/resize pageSize to 7x4cm.pdf" 
        |> ignore


    testCase "resize pageSize to 7x4cm by trimbox tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Resize(PageSelector.All, PageBoxKind.TrimBox , { Width = mm 70.; Height = mm 40. })
        )
        |> runTest "datas/reuse/resize pageSize to 7x4cm by trimbox.pdf" 
        |> ignore

    testCase "resize pageSize to 29.7×21cm uniform tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Resize(PageSelector.All, PageResizingRotatingOptions.Keep, PageResizingScalingOptions.Uniform , FsSize.landscape {Width = mm 210.; Height = mm 297.})
        )
        |> runTest "datas/reuse/resize pageSize to 29.7×21cm uniform.pdf" 
        |> ignore

    testCase "scaling page 0.8 tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Scale(PageSelector.All, 0.8, 0.8)
        )
        |> runTest "datas/reuse/scaling page 0.8.pdf" 
        |> ignore
        
    testCase "insert pages tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Insert("datas/reuse/insertPagesResource.pdf")
        )
        |> runTest "datas/reuse/insertPages.pdf" 
        |> ignore


]