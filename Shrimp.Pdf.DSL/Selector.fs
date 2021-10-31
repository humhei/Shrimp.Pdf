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

    let (!!!) (a: PageModifingArguments<_> -> #IAbstractRenderInfo -> bool) =
        fun args renderInfo ->
            not (a args renderInfo)

    let reSharp (resharper: #IAbstractRenderInfo -> #IAbstractRenderInfo) (f: PageModifingArguments<_> -> #IAbstractRenderInfo -> bool) =
        fun args info ->
            f args (info |> resharper)

type SelectorTag =
    | Path = 0
    | Text = 1
    | PathOrText = 2

[<RequireQualifiedAccess>]
type SelectionSorter =
    | None
    | Plane of tolerance: float * direction: Direction 
with    
    /// (SelectionSorter.Plane (mm 3., Direction.Horizontal))
    static member DefaultValue =
        (SelectionSorter.Plane (mm 3., Direction.Horizontal))

    member sorter.Sort(rects: iText.Kernel.Geom.Rectangle list) =
        match sorter with 
        | SelectionSorter.None -> rects

        | SelectionSorter.Plane(tolerance, direction) ->
            let rec sortRects accum (rects: iText.Kernel.Geom.Rectangle list) =
                match rects with 
                | _ :: _ -> 
                    match direction with 
                    | Direction.Horizontal ->
                        let maxY = 
                            rects
                            |> List.map(fun m -> m.GetYF())
                            |> List.max

                        let (index, leftTopRect) = 
                            rects
                            |> List.indexed
                            |> List.filter(fun (index, m) -> 
                                abs(m.GetYF() - maxY) <= tolerance
                            )
                            |> List.minBy(fun (index, m) ->
                                m.GetX()
                            )

                        let leftRects = rects.[0 .. (index-1)] @ rects.[(index+1) .. (rects.Length-1)]

                        sortRects (leftTopRect :: accum) leftRects

                    | Direction.Vertical ->
                        let minX = 
                            rects
                            |> List.map(fun m -> m.GetXF())
                            |> List.min

                        let (index, leftTopRect) = 
                            rects
                            |> List.indexed
                            |> List.filter(fun (_, m) -> 
                                abs(m.GetXF() - minX) <= tolerance
                            )
                            |> List.maxBy(fun (_, m) ->
                                m.GetY()
                            )

                        let leftRects = rects.[0 .. (index-1)] @ rects.[(index+1) .. (rects.Length-1)]

                        sortRects (leftTopRect :: accum) leftRects

                | [] -> accum

            sortRects [] rects
            |> List.rev



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
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            IAbstractRenderInfo.ColorIs(fillOrStrokeOptions, predicate, ?predicateCompose = predicateCompose) info


    static member ColorIs (fillOrStrokeOptions: FillOrStrokeOptions, color: Color) =
        Info.ColorIs(fillOrStrokeOptions, Color.equal color)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)
    

    static member ColorIs (fillOrStrokeOptions: FillOrStrokeOptions, pdfCanvasColor: PdfCanvasColor) =
        Info.ColorIs(fillOrStrokeOptions, fun color -> pdfCanvasColor.IsEqualTo(color))
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member ColorIs (fillOrStrokeOptions: FillOrStrokeOptions, fsSeparation: FsSeparation) =
        Info.ColorIs(fillOrStrokeOptions, fun color -> fsSeparation.IsEqualTo(color, ValueEqualOptions.DefaultRoundedValue))
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
            FsSeparation.Contains(ValueEqualOptions.DefaultRoundedValue) color fsSeparations
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
