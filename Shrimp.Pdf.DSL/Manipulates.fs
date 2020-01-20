namespace Shrimp.Pdf
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL

[<AutoOpen>]
module Manipulates =
    [<RequireQualifiedAccess>]
    module ModifyPage =
        let trimToVisible pageSelector (margin: Margin)  =
            modifyPage(
                "trim to visible",
                pageSelector,
                PathOrText (Info.IsVisible()),
                (fun args renderInfos ->
                    let bound = 
                        renderInfos
                        |> Seq.choose (IIntegratedRenderInfo.tryGetVisibleBound BoundGettingOptions.WithStrokeWidth)
                        |> Rectangle.ofRectangles
                    args.Page.SetActualBox(bound |> Rectangle.applyMargin margin)
                    |> ignore
                ) 
            )  ||>> ignore