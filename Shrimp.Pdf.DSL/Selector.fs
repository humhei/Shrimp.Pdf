namespace Shrimp.Pdf.DSL
#nowarn "0104"
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Parser
open Shrimp.Pdf
open Shrimp.Pdf.Colors


[<AutoOpen>]
module SelectorOperators =
    let (<&&>) (a: PageModifingArguments<_> -> #IAbstractRenderInfo -> bool) (b:PageModifingArguments<_> -> #IAbstractRenderInfo -> bool) =
        fun args renderInfo ->
            a args renderInfo
            && b args renderInfo

    let (<||>) (a: PageModifingArguments<_> -> #IAbstractRenderInfo -> bool) (b:PageModifingArguments<_> -> #IAbstractRenderInfo -> bool) =
        fun args renderInfo ->
            a args renderInfo
            || b args renderInfo

    let (!!) (a: PageModifingArguments<_> -> #IAbstractRenderInfo -> bool) =
        fun args renderInfo ->
            not (a args renderInfo)

    let reSharp (resharper: #IAbstractRenderInfo -> #IAbstractRenderInfo) (f: PageModifingArguments<_> -> #IAbstractRenderInfo -> bool) =
        fun args info ->
            f args (info |> resharper)

type SelectorTag =
    | Path = 0
    | Text = 1
    | PathOrText = 2

type Selector<'userState> =
    | Path of (PageModifingArguments<'userState> -> IntegratedPathRenderInfo -> bool)
    | Text of (PageModifingArguments<'userState> -> IntegratedTextRenderInfo -> bool)
    | Factory of (PageModifingArguments<'userState> -> Selector<'userState>)
    | PathOrText of (PageModifingArguments<'userState> -> IIntegratedRenderInfo -> bool)
    | Dummy
    | AND of Selector<'userState> list
    | OR of Selector<'userState> list

[<RequireQualifiedAccess>]
module Selector =
    let rec internal toRenderInfoSelector (args: PageModifingArguments<_>) selector =
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

type Info_BoundIs_Args (relativePosition: RelativePosition, ?areaGettingOptions, ?boundGettingStrokeOptions) =
    member x.RelativePosition = relativePosition

    member x.AreaGettingOptions = defaultArg areaGettingOptions (AreaGettingOptions.PageBox PageBoxKind.ActualBox)

    member val BoundGettingStrokeOptions = 
        defaultArg boundGettingStrokeOptions BoundGettingStrokeOptions.WithoutStrokeWidth


type Info =

    static member IsVisible() =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            IIntegratedRenderInfo.isVisible info

    static member ColorIs (fillOrStrokeOptions: FillOrStrokeOptions, predicate: Color -> bool, ?predicateCompose) =
        let predicate =
            match predicateCompose with 
            | Some predicateCompose -> predicate >> predicateCompose 
            | None -> predicate

        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            match fillOrStrokeOptions with 
            | FillOrStrokeOptions.FillOrStroke ->
                IAbstractRenderInfo.hasStroke info
                && predicate(info.Value.GetStrokeColor()) 
                || 
                    IAbstractRenderInfo.hasFill info
                    && predicate(info.Value.GetFillColor())

            | FillOrStrokeOptions.Fill ->
                IAbstractRenderInfo.hasFill info
                && predicate(info.Value.GetFillColor())

            | FillOrStrokeOptions.FillAndStroke ->
                IAbstractRenderInfo.hasStroke info
                && predicate(info.Value.GetStrokeColor()) 
                && 
                    IAbstractRenderInfo.hasFill info
                    && predicate (info.Value.GetFillColor())

            | FillOrStrokeOptions.Stroke ->
                IAbstractRenderInfo.hasStroke info
                && predicate(info.Value.GetStrokeColor())


    static member ColorIs (fillOrStrokeOptions: FillOrStrokeOptions, color: Color) =
        Info.ColorIs(fillOrStrokeOptions, Color.equal color)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)
    

    static member ColorIs (fillOrStrokeOptions: FillOrStrokeOptions, pdfCanvasColor: PdfCanvasColor) =
        Info.ColorIs(fillOrStrokeOptions, fun color -> pdfCanvasColor.IsEqualTo(color))
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member ColorIs (fillOrStrokeOptions: FillOrStrokeOptions, fsSeparation: FsSeparation) =
        Info.ColorIs(fillOrStrokeOptions, fun color -> fsSeparation.IsEqualTo(color, ValueEqualOptions.RoundedValue 3))
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member ColorIs (fillOrStrokeOptions: FillOrStrokeOptions, colorSpace: ColorSpace) =
        Info.ColorIs(fillOrStrokeOptions, fun color -> color.IsInColorSpace(colorSpace)) 
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)


    static member FillColorIs (color: Color) =
        Info.ColorIs(FillOrStrokeOptions.Fill, Color.equal color)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member FillColorIs (pdfCanvasColor: PdfCanvasColor) =
        Info.ColorIs(FillOrStrokeOptions.Fill, fun color -> pdfCanvasColor.IsEqualTo(color))
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member FillColorIs (colorSpace: ColorSpace) =
        Info.ColorIs(FillOrStrokeOptions.Fill, fun color -> color.IsInColorSpace(colorSpace))
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member FillColorIsNot (color: Color) =
        Info.ColorIs(FillOrStrokeOptions.Fill, Color.equal color, not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member FillColorIsNot (pdfCanvasColor: PdfCanvasColor) =
        Info.ColorIs(FillOrStrokeOptions.Fill, (fun color -> pdfCanvasColor.IsEqualTo(color)), not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member FillColorIsNot (colorSpace: ColorSpace) =
        Info.ColorIs(FillOrStrokeOptions.Fill, (fun color -> color.IsInColorSpace(colorSpace)), not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIs (color: Color) =
        Info.ColorIs(FillOrStrokeOptions.Stroke, Color.equal color)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIs (pdfCanvasColor: PdfCanvasColor) =
        Info.ColorIs(FillOrStrokeOptions.Stroke, fun color -> pdfCanvasColor.IsEqualTo(color))
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIs (colorSpace: ColorSpace) =
        Info.ColorIs(FillOrStrokeOptions.Stroke, fun color -> color.IsInColorSpace(colorSpace))
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIsNot (color: Color) =
        Info.ColorIs(FillOrStrokeOptions.Stroke, Color.equal color, not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIsNot (pdfCanvasColor: PdfCanvasColor) =
        Info.ColorIs(FillOrStrokeOptions.Stroke, (fun color -> pdfCanvasColor.IsEqualTo(color)), not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIsNot (colorSpace: ColorSpace) =
        Info.ColorIs(FillOrStrokeOptions.Stroke, (fun color -> color.IsInColorSpace(colorSpace)), not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member ColorIsOneOf (fillOrStrokeOptions: FillOrStrokeOptions, colors: Color list) =
        Info.ColorIs(fillOrStrokeOptions, fun color -> Colors.contains color colors)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member ColorIsOneOf (fillOrStrokeOptions: FillOrStrokeOptions, fsSeparations: FsSeparation list) =
        Info.ColorIs(fillOrStrokeOptions, fun color -> 
            FsSeparation.Contains(ValueEqualOptions.RoundedValue 3) color fsSeparations
        )
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member ColorIsOneOf (fillOrStrokeOptions: FillOrStrokeOptions, colors: PdfCanvasColor list) =
        Info.ColorIs(fillOrStrokeOptions, fun color -> PdfCanvasColor.Contains color colors)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member FillColorIsOneOf (colors: Color list) =
        Info.ColorIs(FillOrStrokeOptions.Fill, fun color -> Colors.contains color colors)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member FillColorIsNotOneOf (colors: Color list) =
        Info.ColorIs(FillOrStrokeOptions.Fill, (fun color -> Colors.contains color colors), not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIsOneOf (colors: Color list) =
        Info.ColorIs(FillOrStrokeOptions.Stroke, (fun color -> Colors.contains color colors))
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIsNotOneOf (colors: Color list) =
        Info.ColorIs(FillOrStrokeOptions.Stroke, (fun color -> Colors.contains color colors), not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member BoundIs (relativePosition, areaGettingOptions: AreaGettingOptions, ?boundGettingStrokeOptions) =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            let boundGettingOptions = defaultArg boundGettingStrokeOptions BoundGettingStrokeOptions.WithoutStrokeWidth

            let bound = IAbstractRenderInfo.getBound boundGettingOptions info

            let rect = args.Page.GetArea(areaGettingOptions)

            match relativePosition with 
            | RelativePosition.CrossBox -> bound.IsCrossOf(rect)
            | RelativePosition.Inbox -> bound.IsInsideOf(rect)
            | RelativePosition.OutBox -> bound.IsOutsideOf(rect)

    static member BoundIs (args: Info_BoundIs_Args) =
        Info.BoundIs(args.RelativePosition, (args.AreaGettingOptions) , args.BoundGettingStrokeOptions)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member BoundIsCrossOf (areaGettingOptions, ?boundGettingOptions) =
        Info.BoundIs(RelativePosition.CrossBox, areaGettingOptions = areaGettingOptions, ?boundGettingStrokeOptions = boundGettingOptions)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member BoundIsInsideOf (areaGettingOptions, ?boundGettingOptions) =
        Info.BoundIs(RelativePosition.Inbox, areaGettingOptions = areaGettingOptions, ?boundGettingStrokeOptions = boundGettingOptions)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member BoundIsOutsideOf (areaGettingOptions, ?boundGettingOptions) =
        Info.BoundIs(RelativePosition.OutBox, areaGettingOptions = areaGettingOptions, ?boundGettingStrokeOptions = boundGettingOptions)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)
