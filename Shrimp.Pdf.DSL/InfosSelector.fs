﻿namespace Shrimp.Pdf.DSL
#nowarn "0104"
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Parser
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open Shrimp.FSharp.Plus
open System.Collections.Generic
open System.Linq


[<RequireQualifiedAccess>]
type InfosSelector<'userState> =
    | Path of (PageModifingArguments<'userState> -> seq<IntegratedPathRenderInfo> -> bool)
    | Text of (PageModifingArguments<'userState> -> seq<IntegratedTextRenderInfo> -> bool)
    | Factory of (PageModifingArguments<'userState> -> InfosSelector<'userState>)
    | PathOrText of (PageModifingArguments<'userState> -> seq<IIntegratedRenderInfo> -> bool)
    | All
    | AND of InfosSelector<'userState> list
    | OR of InfosSelector<'userState> list

[<RequireQualifiedAccess>]
module InfosSelector =
    open iText.Kernel.Pdf.Canvas.Parser




    let internal predicate (selector: InfosSelector<_>) (args: PageModifingArguments<_>) infos =
        let rec loop selector (args: PageModifingArguments<_>) infos =
            match selector with 
            | InfosSelector.Path predicate ->
                let infos =
                    infos
                    |> Seq.choose(IIntegratedRenderInfo.asIPathRenderInfo)

                predicate args infos

            | InfosSelector.Text predicate ->
                let infos =
                    infos
                    |> Seq.choose(IIntegratedRenderInfo.asITextRenderInfo)

                predicate args infos

            | InfosSelector.Factory factory ->
                loop (factory args) args infos


            | InfosSelector.PathOrText predicate ->
                predicate args infos
            
            | InfosSelector.All _ -> true

            | InfosSelector.AND selectors ->
                selectors
                |> List.forall(fun selector -> loop selector args infos)

            | InfosSelector.OR selectors ->
                selectors
                |> List.exists(fun selector -> loop selector args infos)

        loop selector args infos

[<AutoOpen>]
module _InfosSelector =

    

    type TextInfosPicker<'userState> = PageModifingArguments<'userState> -> seq<IntegratedTextRenderInfo> -> string list

    let private pickerToExists predicate (textInfosPicker: _ -> TextInfosPicker<_>) =
        let picker text =
            match predicate text with 
            | true -> Some text
            | false -> None

        fun args infos ->
            let result =
                textInfosPicker picker args infos

            match result with 
            | _ :: _ -> true
            | [] -> false


    type TextInfos = 

        static member PickText(picker: string -> string option): TextInfosPicker<_> = 
            fun (args: PageModifingArguments<_>) infos ->
                let infos =
                    infos
                    |> Seq.map (fun (m: IntegratedTextRenderInfo) -> m :> IIntegratedRenderInfo)

                PageModifier.PickTexts(picker) args infos

                
        static member PickText_In_OneLine(picker: string -> string option, ?delimiter: string, ?selectionGrouper: SelectionGrouper): TextInfosPicker<_> = 
            fun (args: PageModifingArguments<_>) infos ->
                let infos =
                    infos
                    |> Seq.map (fun (m: IntegratedTextRenderInfo) -> m :> IIntegratedRenderInfo)

                PageModifier.PickTexts_In_OneLine(picker, ?delimiter = delimiter, ?selectionGrouper = selectionGrouper) args infos


        static member ExistsText_In_OneLine(predicate: string -> bool, ?delimiter: string, ?selectionGrouper: SelectionGrouper) =
            fun picker ->
                TextInfos.PickText_In_OneLine(picker, ?delimiter = delimiter, ?selectionGrouper = selectionGrouper) 
            |> pickerToExists predicate


        static member ExistsText(predicate: string -> bool) =
            fun picker ->
                TextInfos.PickText(picker) 
            |> pickerToExists predicate