module ReuseTests
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
            Reuses.Impose
                (fun args ->
                    { args with 
                        DesiredSizeOp = Some { Width = mm 50; Height = mm 50}
                        ColNums = [4]
                        RowNum = 4
                        Cropmark = Some Cropmark.defaultValue
                        Background = Background.Size FsSize.A0
                        HSpaces = [mm 3; mm 9]
                        VSpaces = [mm 3; mm 9]
                        Margin = Margin.Create(mm 30, mm 30, mm 30, mm 40)
                        UseBleed = true
                    }
                )
        )
        |> runWithBackup "datas/reuse/Imposing N-UP.pdf" 
        |> ignore

    testCase "imposing stepAndRepeat tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with 
                    DesiredSizeOp = Some { Width = mm 50; Height = mm 50}
                    ColNums = [2]
                    RowNum = 2
                    Cropmark = Some Cropmark.defaultValue
                    HSpaces = [mm 3; mm 9]
                    VSpaces = [mm 3; mm 9]
                    Margin = Margin.Create(mm 0, mm 6, mm 9, mm 12)
                    IsRepeated = true
                }
            )
        )
        |> runWithBackup "datas/reuse/imposing stepAndRepeat.pdf" 
        |> ignore

    testCase "imposing stepAndRepeat2 tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with 
                    DesiredSizeOp = Some { Width = mm 80; Height = mm 50}
                    Cropmark = Some Cropmark.defaultValue
                    Margin = Margin.Create(mm 6)
                    Background = 
                        let size =
                            { Width = FsSize.A4.Width + mm 12 
                              Height = FsSize.A4.Height + mm 12 }
                        Background.Size size
                    IsRepeated = true
                }
            )
        )
        |> runWithBackup "datas/reuse/imposing stepAndRepeat2.pdf" 
        |> ignore


    testCase "Imposing when use bleed and bleedBox bigger than actualbox" <| fun _ -> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with 
                    DesiredSizeOp = Some { Width = mm 50; Height = mm 50}
                    ColNums = [4]
                    RowNum = 4
                    Cropmark = Some Cropmark.defaultValue
                    Background = Background.Size FsSize.A0
                    HSpaces = [mm 3; mm 9]
                    VSpaces = [mm 3; mm 9]
                    Margin = Margin.Create(mm 30, mm 30, mm 30, mm 40)
                    UseBleed = true
                }
            )
        )
        |> runWithBackup "datas/reuse/Imposing when use bleed and bleedBox bigger than actualbox.pdf" 
        |> ignore

    testCase "Imposing when cell roation is setted" <| fun _ -> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with 
                    DesiredSizeOp = Some { Width = mm 50; Height = mm 50}
                    ColNums = [4]
                    RowNum = 4
                    Cropmark = Some Cropmark.defaultValue
                    Background = Background.Size FsSize.A0
                    HSpaces = [mm 3; mm 9]
                    VSpaces = [mm 3; mm 9]
                    Margin = Margin.Create(mm 30, mm 30, mm 30, mm 40)
                    UseBleed = true
                    CellRotation = CellRotation.R180WhenRowNumIsEven
                }
            )
        )
        |> runWithBackup "datas/reuse/Imposing when cell roation is setted.pdf" 
        |> ignore

    testCase "clockwise all pages" <| fun _ -> 
        Flow.Reuse (Reuses.Rotate(PageSelector.All, Rotation.Clockwise))
        |> runWithBackup "datas/reuse/clockwise all pages.pdf" 
        |> ignore

    testCase "clockwise all pages2" <| fun _ -> 
        Flow.Reuse (Reuses.Rotate(PageSelector.All, Rotation.Clockwise))
        |> runWithBackup "datas/reuse/clockwise all pages2.pdf" 
        |> ignore

    testCase "duplicate all pages 15x tests" <| fun _ -> 
        Flow.Reuse (Reuses.DuplicatePages(PageSelector.All, 15))
        |> runWithBackup "datas/reuse/duplicate all pages 15x.pdf" 
        |> ignore

    testCase "duplicate pages by page num sequence tests" <| fun _ -> 
        Flow.Reuse (Reuses.SequencePages (PageNumSequence.create [1;1;1;3;4;5;8]))
        |> runWithBackup "datas/reuse/duplicate pages by sequence.pdf" 
        |> ignore

    testCase "duplicate pages by page num sequence tests2" <| fun _ -> 
        Flow.Reuse (Reuses.SequencePages (PageNumSequence.create [1;1;1;3;4;5;8]))
        |> runWithBackup "datas/reuse/duplicate pages by sequence2.pdf" 
        |> ignore

    testCase "duplicate pages by copied num sequence tests3" <| fun _ -> 
        Flow.Reuse (Reuses.DuplicatePages (PageSelector.All, CopiedNumSequence.create [15;15;15;15]))
        |> runWithBackup "datas/reuse/duplicate pages by copied num sequence.pdf" 
        |> ignore

    testCase "tile pages by colNum and rowNum tests" <| fun _ -> 
        Flow.Reuse (Reuses.TilePages (TileTable.create 3 2))
        |> runWithBackup "datas/reuse/tile pages by colNum and rowNum.pdf" 
        |> ignore

    testCase "tile pages by selector tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.TilePages
                (Path(Info.StrokeColorIs DeviceRgb.BLUE <&> Info.BoundIsInsideOfPageBox()))
        )
        |> runWithBackup "datas/reuse/tile pages by selector.pdf" 
        |> ignore

    testCase "move pagebox to origin tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.MovePageBoxToOrigin(PageSelector.All)
        )
        |> runWithBackup "datas/reuse/move pagebox to origin.pdf" 
        |> ignore

    testCase "resize pageSize to 7x4cm tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Resize(PageSelector.All, {Width = mm 70; Height = mm 40})
        )
        |> runWithBackup "datas/reuse/resize pageSize to 7x4cm.pdf" 
        |> ignore
]