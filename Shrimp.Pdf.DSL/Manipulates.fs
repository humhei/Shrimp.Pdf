namespace Shrimp.Pdf
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL

[<AutoOpen>]
module Manipulates =
    type ModifyPage with
        static member TrimToVisible (pageSelector: PageSelector, ?margin: Margin)  =
            let margin = defaultArg margin (Margin.Create 0.)
            ModifyPage.Create(
                "trim to visible",
                pageSelector,
                PathOrText (Info.IsVisible()),
                (fun args renderInfos ->
                    let bound = 
                        renderInfos
                        |> Seq.choose (IIntegratedRenderInfo.tryGetVisibleBound BoundGettingStrokeOptions.WithStrokeWidth)
                        |> Rectangle.ofRectangles

                    args.Page.SetActualBox(bound |> Rectangle.applyMargin margin)
                    |> ignore

                ),
                parameters = [
                    "margin" => margin.ToString()
                ]
            )  ||>> ignore