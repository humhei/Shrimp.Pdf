module ReuseTests
open Expecto
open Shrimp.Pdf.Reuses
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Reuses
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors

let reuseTests =
  testList "Reuse Tests" [
    testCase "imposing N-UP tests" <| fun _ -> 
        Flow.Reuse (
            impose 
                (fun _ ->
                    (ImposingArguments.Create(fun args ->
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
                    }))
                )
        )
        |> runWithBackup "datas/reuse/Imposing N-UP.pdf" 
        |> ignore

    testCase "imposing stepAndRepeat tests" <| fun _ -> 
        Flow.Reuse (
            impose (fun _ ->
                (ImposingArguments.Create(fun args ->
                    { args with 
                        DesiredSizeOp = Some { Width = mm 50; Height = mm 50}
                        ColNums = [2]
                        RowNum = 2
                        Cropmark = Some Cropmark.defaultValue
                        HSpaces = [mm 3; mm 9]
                        VSpaces = [mm 3; mm 9]
                        Margin = Margin.Create(mm 0, mm 6, mm 9, mm 12)
                        IsRepeated = true
                    }))
            )
        )
                

        |> runWithBackup "datas/reuse/imposing stepAndRepeat.pdf" 
        |> ignore

    testCase "Imposing when use bleed and bleedBox bigger than actualbox" <| fun _ -> 
        Flow.Reuse (
            impose (fun _ ->
                (ImposingArguments.Create(fun args ->
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
                }))
            )
        )
        
                

        |> runWithBackup "datas/reuse/Imposing when use bleed and bleedBox bigger than actualbox.pdf" 
        |> ignore


    testCase "duplicate all pages 15x tests" <| fun _ -> 
        Flow.Reuse (duplicatePages PageSelector.All 15)
        |> runWithBackup "datas/reuse/duplicate all pages 15x.pdf" 
        |> ignore

    testCase "duplicate pages by sequence tests" <| fun _ -> 
        Flow.Reuse (sequencePages (PageNumSequence.create [1;1;1;3;4;5;8]))
        |> runWithBackup "datas/reuse/duplicate pages by sequence.pdf" 
        |> ignore

    testCase "tile pages by colNum and rowNum tests" <| fun _ -> 
        Flow.Reuse (tilePages (TileTable.create 3 2))
        |> runWithBackup "datas/reuse/tile pages by colNum and rowNum.pdf" 
        |> ignore

    testCase "tile pages by selector tests" <| fun _ -> 
        Flow.Reuse (
            tilePagesByRenderInfoSelectorFactory (fun page ->
                let actualBox = page.GetActualBox()
                (RenderInfoSelector.Path 
                    (fun pathRenderInfo -> 
                        pathRenderInfo.GetStrokeColor() = DeviceRgb.BLUE 
                        && PathRenderInfo.isVisible pathRenderInfo
                        && (PathRenderInfo.getBound BoundGettingOptions.WithoutStrokeWidth pathRenderInfo).IsInsideOf(actualBox))
                )
            )
        )
        |> runWithBackup "datas/reuse/tile pages by selector.pdf" 
        |> ignore
]