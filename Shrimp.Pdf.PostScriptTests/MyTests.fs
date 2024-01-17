module MyTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.PostScript





let myTests =
  testList "My Tests" [
    testCase "cmyk pdf function4" <| fun _ ->
        let text =  
            "
                {1.000000
                3 1 roll
                1.000000 3 1 roll
                1.000000 3 1 roll
                5 -1 roll
                2 index
                -0.341176 mul
                1.000000 add
                mul
                1 index
                -0.765565 mul
                1.000000 add
                mul
                5 1 roll
                4 -1 roll
                2 index
                -0.490197 mul
                1.000000 add
                mul
                1 index
                -0.854452 mul
                1.000000 add
                mul
                4 1 roll
                3 -1 roll
                2 index
                -0.788240 mul
                1.000000 add
                mul
                1 index
                -0.853094 mul
                1.000000 add
                mul
                3 1 roll
                pop
                pop
                3 -1 roll
                2.200000 exp
                3 -1 roll
                2.200000 exp
                3 -1 roll
                2.200000 exp
                0.000000 3 index
                0.609700 mul
                add
                2 index
                0.205300 mul
                add
                1 index
                0.149200 mul
                add
                4 1 roll
                0.000000 3 index
                0.311100 mul
                add
                2 index
                0.625700 mul
                add
                1 index
                0.063200 mul
                add
                4 1 roll
                0.000000 3 index
                0.019500 mul
                add
                2 index
                0.060900 mul
                add
                1 index
                0.744500 mul
                add
                4 1 roll
                pop
                pop
                pop
                2 index
                0.964200 div
                0 index
                0.008856 gt
                {0.333330
                exp
                }
                {0.128410
                div
                0.137930 add
                }
                ifelse
                4 1 roll
                1 index
                1.000000 div
                0 index
                0.008856 gt
                {0.333330
                exp
                }
                {0.128410
                div
                0.137930 add
                }
                ifelse
                4 1 roll
                0 index
                0.824900 div
                0 index
                0.008856 gt
                {0.333330
                exp
                }
                {0.128410
                div
                0.137930 add
                }
                ifelse
                4 1 roll
                pop
                pop
                pop
                1 index
                116.000000 mul
                16.000000 sub
                4 1 roll
                2 index
                2 index
                sub
                500.000000 mul
                4 1 roll
                1 index
                1 index
                sub
                200.000000 mul
                4 1 roll
                pop
                pop
                pop
                }
                
            "

        let inputValues =
            List.replicate 4 1.
            |> List.map PostScriptValue.Double

        let r = PostScriptInterpreter.Create(inputValues, text)
        ()

  ]