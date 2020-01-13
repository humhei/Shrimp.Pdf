namespace Shrimp.Pdf.DSL
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Parser
open Shrimp.Pdf
open Shrimp.Pdf.Colors


[<AutoOpen>]
module SelectorOperators =
    let (<&>) (a: PageModifingArguments<_> -> #IAbstractRenderInfo -> bool) (b:PageModifingArguments<_> -> #IAbstractRenderInfo -> bool) =
        fun args renderInfo ->
            a args renderInfo
            && b args renderInfo

    let (!!) (a: PageModifingArguments<_> -> #IAbstractRenderInfo -> bool) =
        fun args renderInfo ->
            not (a args renderInfo)


type Selector<'userState> =
    | Path of (PageModifingArguments<'userState> -> IntegratedPathRenderInfo -> bool)
    | Text of (PageModifingArguments<'userState> -> IntegratedTextRenderInfo -> bool)
    | Factory of (PageModifingArguments<'userState> -> Selector<'userState>)
    | PathOrText of (PageModifingArguments<'userState> -> IIntegratedRenderInfo -> bool)
    | Dummy
    | AND of Selector<'userState> list
    | OR of Selector<'userState> list

[<RequireQualifiedAccess>]
module internal Selector =
    let rec toRenderInfoSelector (args: PageModifingArguments<_>) selector =
        match selector with 
        | Selector.Path factory -> factory args |> RenderInfoSelector.Path
        | Selector.Text factory -> factory args |> RenderInfoSelector.Text 
        | Selector.PathOrText factory -> factory args |> RenderInfoSelector.PathOrText
        | Selector.Factory factory -> toRenderInfoSelector args (factory args)
        | Selector.Dummy -> RenderInfoSelector.Dummy
        | Selector.AND selectors -> 
            selectors
            |> List.map (toRenderInfoSelector args)
            |> RenderInfoSelector.AND
        | Selector.OR selectors ->
            selectors
            |> List.map (toRenderInfoSelector args)
            |> RenderInfoSelector.OR


type Info =

    static member IsVisible() =
        fun (args: PageModifingArguments<_>) (info: #IIntegratedRenderInfo) ->
            IIntegratedRenderInfo.isVisible info

    static member StrokeColorIs (color: Color) =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            IAbstractRenderInfo.hasStroke info
            && Color.equal (info.Value.GetStrokeColor()) color

    static member StrokeColorIs (color: PdfCanvasColor) =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            IAbstractRenderInfo.hasStroke info
            && color.IsEqualTo(info.Value.GetStrokeColor())

    static member StrokeColorIsOneOf (colors: PdfCanvasColor seq)  =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            colors
            |> Seq.exists (fun color -> 
                (Info.StrokeColorIs color) args info
            )

    static member StrokeColorIsOneOf (colors: Color seq)  =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            colors
            |> Seq.exists (fun color -> 
                (Info.StrokeColorIs color) args info
            )

    static member FillColorIs (color: Color) =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            IAbstractRenderInfo.hasFill info
            && Color.equal (info.Value.GetFillColor()) color

    static member FillColorIs (color: PdfCanvasColor) =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            IAbstractRenderInfo.hasFill info
            && color.IsEqualTo(info.Value.GetFillColor())

    static member FillColorIsOneOf (colors: Color seq) =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            colors
            |> Seq.exists (fun color -> Info.FillColorIs color args info)

    static member FillColorIsOneOf (colors: PdfCanvasColor seq) =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            colors
            |> Seq.exists (fun color -> Info.FillColorIs color args info)


    static member BoundIsCrossOf (rect, ?boundGettingOptions) =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            let boundGettingOptions = defaultArg boundGettingOptions BoundGettingOptions.WithoutStrokeWidth
            (IAbstractRenderInfo.getBound boundGettingOptions info).IsCrossOf(rect)

    static member BoundIsInsideOf (rect, ?boundGettingOptions) =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            let boundGettingOptions = defaultArg boundGettingOptions BoundGettingOptions.WithoutStrokeWidth
            (IAbstractRenderInfo.getBound boundGettingOptions info).IsInsideOf(rect)

    static member BoundIsInsideOfPageBox(?pageBoxKind: PageBoxKind, ?boundGettingOptions) =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            let pageBoxKind = defaultArg pageBoxKind PageBoxKind.ActualBox
            let pageBox = args.Page.GetPageBox(pageBoxKind)
            match boundGettingOptions with 
            | Some boundGettingOptions -> Info.BoundIsInsideOf (pageBox, boundGettingOptions) args info
            | None -> Info.BoundIsInsideOf (pageBox) args info

    static member BoundIsOutsideOf (rect, ?boundGettingOptions) =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            let boundGettingOptions = defaultArg boundGettingOptions BoundGettingOptions.WithoutStrokeWidth
            (IAbstractRenderInfo.getBound boundGettingOptions info).IsOutsideOf(rect)


    static member BoundIsOutsideOfPageBox(?pageBoxKind: PageBoxKind, ?boundGettingOptions) =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            let pageBoxKind = defaultArg pageBoxKind PageBoxKind.ActualBox
            let pageBox = args.Page.GetPageBox(pageBoxKind)
            match boundGettingOptions with 
            | Some boundGettingOptions -> Info.BoundIsInsideOf (pageBox, boundGettingOptions) args info
            | None -> Info.BoundIsOutsideOf (pageBox) args info


