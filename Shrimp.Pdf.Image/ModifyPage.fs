﻿namespace Shrimp.Pdf

open Shrimp.Pdf.Image

#nowarn "0104"
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open Shrimp.Pdf
open Shrimp.Pdf.Parser
open Shrimp.FSharp.Plus
open Akkling

[<AutoOpen>]
module __Image_ModifyPage  =
    type ModifyPage with
        static member SetPageBoxToSelections (pageSelector: PageSelector, selector, ?pageBoxKind, ?margin: Margin)  =
            let pageBoxKind = defaultArg pageBoxKind PageBoxKind.AllBox
            let margin = defaultArg margin (Margin.Create 0.)
            ModifyPage.CreateIM(
                "trim to selection",
                pageSelector,
                Selector.All (selector),
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
                        let bound = bound |> Rectangle.applyMargin margin
                        PdfPage.setPageBox pageBoxKind bound args.Page
                        |> ignore

                    | None -> 
                        failwithf "Cannot trim to visible as all infos are invisible"

                ),
                parameters = [
                    "margin" => margin.ToString()
                ]
            )  ||>> ignore


        static member TrimToVisible (pageSelector: PageSelector, ?margin: Margin)  =
            let margin = defaultArg margin (Margin.Create 0.)
            ModifyPage.SetPageBoxToSelections(pageSelector, InfoIM.IsVisible(), margin = margin, pageBoxKind = PageBoxKind.AllBox)
            |> Manipulate.rename
                "trim to visible"
                [
                    "margin" => margin.ToString()
                ]
            ||>> ignore
