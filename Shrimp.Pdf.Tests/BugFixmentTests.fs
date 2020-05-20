module BugFixmentTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
open Shrimp.Pdf.Manipulates
open Shrimp.Pdf.Colors
open Shrimp.Pdf.DSL
open Shrimp.Pdf.icms2



let bugFixmentTests =
  testList "bug Fixment Tests" [

    testCase "fsSepearationNRE" <| fun _ -> 
        Flow.Manipulate (
            Manipulate.Factory(fun _ _ ->
                Manipulate.dummy()
                <+>
                (
                    ModifyPage.Create(
                        "read all Colors",
                        PageSelector.All,
                        PathOrText (Info.IsVisible()),
                        fun args infos -> 
                            let colors = 
                                List.ofSeq infos
                                |> List.collect (IAbstractRenderInfo.getColors)
                                |> Colors.distinct
                                |> List.ofSeq
                            colors
                    ) ||>> (fun colors ->
                        let colors = List.concat colors
                        ()
                    )
                )
            )

        ) 
        |> runTest "datas/bugFixment/FsSeparationNRE.pdf" 
        |> ignore



  ]