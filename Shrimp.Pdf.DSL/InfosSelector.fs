namespace Shrimp.Pdf.DSL
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

    let private (>>>>) fResult mapping = 
        fun (a: 'a) (b: 'b) -> 
            let r: 'result = fResult a b 
            mapping r

    type TextInfosPicker<'userState, 'result> = PageModifingArguments<'userState> -> seq<IntegratedTextRenderInfo> -> 'result 
    type TextInfosPicker<'userState> = TextInfosPicker<'userState, PdfConcatedTexts>

    let private pickerToExists predicate (textInfosPicker: (_ -> _ option) -> TextInfosPicker<_, _>) =
        let picker (text: _) =
            match predicate text with 
            | true -> Some (Unchecked.defaultof<_>)
            | false -> None

        fun args infos ->
            let result = textInfosPicker picker args infos

            match result with 
            | _ :: _ -> true
            | [] -> false


    type TextInfos = 

        static member PickTextInfo(picker: IntegratedTextRenderInfo -> _ option): TextInfosPicker<_, _> = 
            fun (args: PageModifingArguments<_>) infos ->
                let infos =
                    infos
                    |> Seq.map (fun (m: IntegratedTextRenderInfo) -> m :> IIntegratedRenderInfo)

                PageModifier.PickTextInfo(picker) args infos

        static member PickTextInfo_ExactlyOne(picker: IntegratedTextRenderInfo -> _ option): TextInfosPicker<_, _> = 
            fun (args: PageModifingArguments<_>) textInfos ->

                let infos =
                    textInfos
                    |> Seq.map (fun (m: IntegratedTextRenderInfo) -> m :> IIntegratedRenderInfo)

                let r = PageModifier.PickTextInfo(picker) args infos
                match r with 
                | [r] -> r
                | [] -> 
                    let records =
                        textInfos
                        |> Seq.map (fun m -> m.RecordValue)

                    failwithf "Cannnot found any text in %A by picker %A" records picker
                | rs -> 
                    let records =
                        textInfos
                        |> Seq.map (fun m -> m.RecordValue)
                    failwithf "Found multiple texts %A in %A by picker %A" rs records picker


        static member PickWord(picker: PdfConcatedWord -> PdfConcatedWord option): TextInfosPicker<_> = 
            fun (args: PageModifingArguments<_>) infos ->
                let infos =
                    infos
                    |> Seq.map (fun (m: IntegratedTextRenderInfo) -> m :> IIntegratedRenderInfo)

                let r = 
                    PageModifier.PickTextInfo(fun renderInfo->
                        let text = ITextRenderInfo.getPdfConcatedText renderInfo
                        picker text
                    ) args infos

                r
                |> PdfConcatedTexts.Words

                
        static member PickLine(picker: PdfConcatedLine -> PdfConcatedLine option, ?selectionGrouper: SelectionGrouper): TextInfosPicker<_> = 
            fun (args: PageModifingArguments<_>) infos ->
                let infos =
                    infos
                    |> Seq.map (fun (m: IntegratedTextRenderInfo) -> m :> IIntegratedRenderInfo)

                PageModifier.PickLine(picker, ?selectionGrouper = selectionGrouper) args infos
                |> PdfConcatedTexts.Lines


        static member ExistsLine(predicate: PdfConcatedLine -> bool, ?selectionGrouper: SelectionGrouper) =
            fun picker ->
                TextInfos.PickLine(picker, ?selectionGrouper = selectionGrouper) >>>> fun a -> a.AsList
            |> pickerToExists predicate


        static member ExistsWord(predicate: PdfConcatedWord -> bool) =
            fun picker ->
                TextInfos.PickWord(picker) >>>> fun a -> a.AsList
            |> pickerToExists predicate

        static member ExistsTextInfo(predicate: IntegratedTextRenderInfo -> bool) =
            fun (picker: IntegratedTextRenderInfo -> _) ->
                TextInfos.PickTextInfo(picker) 
            |> pickerToExists predicate