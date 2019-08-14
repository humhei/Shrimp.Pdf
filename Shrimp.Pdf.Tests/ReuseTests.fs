module ReuseTests
open Expecto
open Shrimp.Pdf.Reuses
open Shrimp.Pdf
open Shrimp.Pdf.Imposing

let reuseTests =
  testList "Reuse Tests" [
    testCase "imposing N-UP tests" <| fun _ -> 
        [ impose (ImposingArguments.Create(fun arg ->
            { arg with 
                DesiredSizeOp = Some { Width = mm 50; Height = mm 50}
                ColNums = [4]
                RowNum = 4
                Cropmark = Some Cropmark.defaultValue
                HSpaces = [mm 3; mm 9]
                VSpaces = [mm 3; mm 9]
                Margin = Margin.Create(mm 0, mm 6, mm 9, mm 12)
            }
        )) ]
        |> runWithBackup "datas/reuse/Imposing N-UP.pdf" 
        |> ignore

    testCase "imposing stepAndRepeat tests" <| fun _ -> 
        [ impose (ImposingArguments.Create(fun arg ->
            { arg with 
                DesiredSizeOp = Some { Width = mm 50; Height = mm 50}
                ColNums = [2]
                RowNum = 2
                Cropmark = Some Cropmark.defaultValue
                HSpaces = [mm 3; mm 9]
                VSpaces = [mm 3; mm 9]
                Margin = Margin.Create(mm 0, mm 6, mm 9, mm 12)
                IsRepeated = true
            }
        )) ]
        |> runWithBackup "datas/reuse/imposing stepAndRepeat.pdf" 
        |> ignore

  ]