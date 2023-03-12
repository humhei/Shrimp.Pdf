namespace Shrimp.Pdf

open iText.Kernel.Pdf
open Shrimp.Pdf.Image

#nowarn "0104"
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open Shrimp.Pdf
open System.IO
open iText.Kernel.Pdf.Xobject
open Shrimp.Pdf.Parser
open System.Drawing
open Shrimp.FSharp.Plus
open Shrimp.Pdf.Colors
open Akkling
open Shrimp.Pdf.icms2
open iText.Layout.Element
open Fake.IO
open Shrimp.Pdf.icms2.client
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.IO.Image
open FSharp.Data
open System.Collections.Concurrent
open Shrimp.Pdf.js.shared

[<AutoOpen>]
module _ModifyPage  =
    type ModifyPage with
        static member TrimToVisible (pageSelector: PageSelector, ?margin: Margin)  =
            let margin = defaultArg margin (Margin.Create 0.)
            ModifyPage.CreateIM(
                "trim to visible",
                pageSelector,
                Selector.All (InfoIM.IsVisible()),
                (fun args renderInfos ->
                    let bounds = 
                        renderInfos
                        |> Seq.choose (IIntegratedRenderInfoIM.tryGetVisibleBound BoundGettingStrokeOptions.WithStrokeWidth)
                        |> AtLeastOneList.TryCreate

                    let bound =
                        bounds
                        |> Option.map Rectangle.ofRectangles

                    match bound with 
                    | Some bound ->
                        args.Page.SetActualBox(bound |> Rectangle.applyMargin margin)
                        |> ignore

                    | None -> 
                        failwithf "Cannot trim to visible as all infos are invisible"

                ),
                parameters = [
                    "margin" => margin.ToString()
                ]
            )  ||>> ignore
