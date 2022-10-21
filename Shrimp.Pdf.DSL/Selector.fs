namespace Shrimp.Pdf.DSL

open iText.Kernel.Pdf.Canvas.Parser
open Newtonsoft.Json



#nowarn "0104"
open FParsec
open FParsec.CharParsers
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Parser
open Shrimp.Pdf
open iText.Kernel.Geom
open Shrimp.Pdf.Colors
open Shrimp.FSharp.Plus
open System.Collections.Generic
open System.Linq
open Shrimp.Pdf.Constants



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



[<AutoOpen>]
module _SelectionGrouper = 

    type private Bottom = Bottom of float
    with 
        member x.Value = 
            let (Bottom v) = x
            v

    [<RequireQualifiedAccess>]
    type SelectionGrouper =
        | None
        | Plane of tolerance: float 
    with    
        /// (SelectionGrouper.Plane (tolerance.Value))
        static member DefaultValue =
            (SelectionGrouper.Plane (tolerance.Value))


        member grouper.GroupBy(frect: 'info -> iText.Kernel.Geom.Rectangle, ?position: Position) = 
            let position = defaultArg position (Position.BottomMiddle(0, 0))

            fun (infos: #seq<'info> )  ->
                infos
                |> List.ofSeq
                |> List.map(fun m ->
                    let rect = frect m
                    rect, m
                )
                |> List.groupBy(fun (m, _) -> m.GetPoint(position).y |> NearbyPX)   
                |> List.map(fun (a, b) ->
                    let infos = 
                        b 
                        |> Seq.sortBy(fun (rect, info) -> rect.GetLeft()) 
                        |> Seq.map snd

                    a.Value, infos
                )
                |> Seq.sortBy (fun (bottom, _) -> -bottom)

        member grouper.GroupBy_Bottom(frect: 'info -> iText.Kernel.Geom.Rectangle) (infos: _ seq) = 
            grouper.GroupBy(frect, Position.BottomMiddle (0, 0)) (List.ofSeq infos)
          
type RectangleWithUserState<'UserState> =
    { UserState: 'UserState
      Rectangle: Rectangle }
with     
    member x.GetYF() = x.Rectangle.GetYF()
    member x.GetXF() = x.Rectangle.GetXF()
    member x.GetX() = x.Rectangle.GetX()
    member x.GetY() = x.Rectangle.GetY()

[<RequireQualifiedAccess>]
type SelectionSorter =
    | Non
    | Plane of tolerance: float * direction: Direction 
with    
    /// (SelectionSorter.Plane (mm 3., Direction.Horizontal))
    static member DefaultValue =
        (SelectionSorter.Plane (mm 3., Direction.Horizontal))

    static member CreatePlane(?tolerance, ?direction) =
        SelectionSorter.Plane(
            (defaultArg tolerance (mm 3.)),
            defaultArg direction Direction.Horizontal
        )

    member sorter.SortToLists_WithUserState(rects: RectangleWithUserState<_> list) =
        match rects with 
        | [] -> []
        | [rect] -> [[rect]]
        | _ ->
            match sorter with 
            | SelectionSorter.Non -> [rects]

            | SelectionSorter.Plane(tolerance, direction) ->
                let rec sortRects previousCoordinateUltraValue accum accums (rects: RectangleWithUserState<_> list) =
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

                            match previousCoordinateUltraValue with 
                            | Some (previousMaxY) when (abs(previousMaxY - maxY) <= tolerance) -> 
                                sortRects (Some maxY) (leftTopRect :: accum) accums leftRects
                            | None ->
                                sortRects (Some maxY) (leftTopRect :: accum) accums leftRects
                            | _ -> 
                                sortRects (Some maxY) [leftTopRect] (List.rev (accum) :: accums) leftRects

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

                            match previousCoordinateUltraValue with 
                            | Some (previousMinX) when (abs(previousMinX - minX) <= tolerance) ->
                                sortRects (Some minX) [] (List.rev (leftTopRect :: accum) :: accums) leftRects
                            | _ -> sortRects (Some minX) (leftTopRect :: accum) accums leftRects


                    | [] -> 
                        match accum with 
                        | [] -> accums
                        | _ -> List.rev accum :: accums


                sortRects None [] [] rects
                |> List.rev

    member sorter.SortBy(fRect: 'info -> Rectangle) (infos: 'info list) =
        let rects =
            infos
            |> List.map(fun info ->
                { Rectangle = fRect info 
                  UserState = (info)}
            )

        sorter.SortToLists_WithUserState(rects)
        |> List.map (List.map(fun m -> m.UserState))


    member sorter.SortToLists(rects: iText.Kernel.Geom.Rectangle list) =
        let rects =
            rects
            |> List.map(fun rect ->
                { UserState = () 
                  Rectangle = rect }
            )

        sorter.SortToLists_WithUserState(rects)
        |> List.map (List.map (fun m -> m.Rectangle))

    member sorter.Sort(rects: iText.Kernel.Geom.Rectangle list) =
        sorter.SortToLists(rects)
        |> List.concat

[<RequireQualifiedAccess>]
type SelectionDistincter =
    | Non
    | Plane of tolerance: float 
with    
    /// (SelectionSorter.Plane (tolerance 0.1)
    static member DefaultValue =
        (SelectionDistincter.Plane (tolerance.Value))


    member x.DistinctBy(frect: _ -> FsSize) values =
        match x with 
        | SelectionDistincter.Non -> values
        | SelectionDistincter.Plane tolerance ->
            let createNeayByPX(v) =
                NearbyPX(v, tolerance = tolerance)

            values
            |> AtLeastOneList.distinctBy_explictly<_, _>(fun m ->
                let rect = frect m
                createNeayByPX rect.Width, createNeayByPX rect.Height
            )

    member x.Distinct(rects) =
        x.DistinctBy id rects



type Selector<'userState> =
    | ImageX of (PageModifingArguments<'userState> -> IntegratedImageRenderInfo -> bool)
    | Path of (PageModifingArguments<'userState> -> IntegratedPathRenderInfo -> bool)
    | Text of (PageModifingArguments<'userState> -> IntegratedTextRenderInfo -> bool)
    | Factory of (PageModifingArguments<'userState> -> Selector<'userState>)
    | PathOrText of (PageModifingArguments<'userState> -> IIntegratedRenderInfo -> bool)
    | Dummy
    | AND of Selector<'userState> list
    | OR of Selector<'userState> list
    | Not of Selector<'userState>
with 
    static member All(predicate: PageModifingArguments<'userState> -> _ -> bool) = 
        Selector.OR[
            Selector.ImageX(fun args info -> predicate args (info:> IIntegratedRenderInfoIM) )
            Selector.Path(fun args info -> predicate args (info:> IIntegratedRenderInfoIM) )
            Selector.Text(fun args info -> predicate args (info:> IIntegratedRenderInfoIM) )
        ]




[<RequireQualifiedAccess>]
module Selector =


    let rec redirectSelectorUserState userState (selector: Selector<_>) =
        match selector with 
        | Selector.Path predicate ->
            fun (args: PageModifingArguments<_>) info ->
                predicate (args.MapUserState(fun _ -> userState)) info
            |> Selector.Path

        | Selector.Text predicate ->
            fun (args: PageModifingArguments<_>) info ->
                predicate (args.MapUserState(fun _ -> userState)) info
            |> Selector.Text

        | Selector.PathOrText predicate ->
            fun (args: PageModifingArguments<_>) info ->
                predicate (args.MapUserState(fun _ -> userState)) info
            |> Selector.PathOrText

        | Selector.ImageX predicate ->
            fun (args: PageModifingArguments<_>) info ->
                predicate (args.MapUserState(fun _ -> userState)) info
            |> Selector.ImageX

        | Selector.AND selectors ->
            selectors
            |> List.map (redirectSelectorUserState userState)
            |> Selector.AND

        | Selector.OR selectors ->
            selectors
            |> List.map (redirectSelectorUserState userState)
            |> Selector.OR

        | Selector.Not selector ->
            redirectSelectorUserState userState selector
            |> Selector.Not

        | Selector.Dummy _ -> Selector.Dummy
        | Selector.Factory fSelector -> 
            fun (args: PageModifingArguments<_>) ->
                redirectSelectorUserState userState (fSelector (args.MapUserState(fun _ -> userState))) 

            |> Selector.Factory

    let rec toRenderInfoSelector (args: PageModifingArguments<_>) selector =
        match selector with 
        | Selector.ImageX factory -> factory args |> RenderInfoSelector.Image
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
        | Selector.Not selector ->
            toRenderInfoSelector args selector
            |> RenderInfoSelector.Not


type Info_BoundIs_Args [<JsonConstructor>] (relativePosition: RelativePosition, ?areaGettingOptionsOp, ?boundGettingStrokeOptionsOp) =
    inherit POCOBaseEquatable<RelativePosition * AreaGettingOptions option * BoundGettingStrokeOptions option>(relativePosition, areaGettingOptionsOp, boundGettingStrokeOptionsOp)
    
    [<JsonProperty>]
    member x.RelativePosition = relativePosition

    [<JsonProperty>]
    member private x.AreaGettingOptionsOP = areaGettingOptionsOp


    member x.AreaGettingOptions = defaultArg areaGettingOptionsOp (AreaGettingOptions.PageBox PageBoxKind.ActualBox)

    member val BoundGettingStrokeOptions = 
        defaultArg boundGettingStrokeOptionsOp BoundGettingStrokeOptions.WithoutStrokeWidth

    [<JsonProperty>]
    member private x.BoundGettingStrokeOptionsOP = boundGettingStrokeOptionsOp





                

type TextInfo =
    static member FPrasec(parser: Parser<_, _>) =
        fun (args: PageModifingArguments<_>) (info: #ITextRenderInfo) ->
            match FParsec.CharParsers.run parser (ITextRenderInfo.getText info) with 
            | Success _ -> true
            | Failure _ -> false


    static member TextContainsIC(text: string) =
        fun (args: PageModifingArguments<_>) (info: #ITextRenderInfo) ->
            (ITextRenderInfo.getText info).Contains(text, true)

    static member FontNameIs(fontName) =
        fun (args: PageModifingArguments<_>) (info: #ITextRenderInfo) ->
            ITextRenderInfo.fontNameIs fontName info
        
    static member FontSizeIs(fontSize) =
        fun (args: PageModifingArguments<_>) (info: #ITextRenderInfo) ->
            fontSize @= ITextRenderInfo.getActualFontSize info

    static member FontNameAndSizeIs(fontName, fontSize: float) =
        fun (args: PageModifingArguments<_>) (info: #ITextRenderInfo) ->
            ITextRenderInfo.fontNameIs fontName info
            && (fontSize @= ITextRenderInfo.getActualFontSize info)
        



type Info =

    static member IsVisible() =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            IIntegratedRenderInfo.isVisible info


    static member IsFillVisible() =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            IIntegratedRenderInfo.isFillVisible info

    static member IsFillNotVisible() =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            IIntegratedRenderInfo.isFillVisible info
            |> not

    static member IsStrokeVisible() =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            IIntegratedRenderInfo.isStrokeVisible info

    static member IsStrokeNotVisible() =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            IIntegratedRenderInfo.isStrokeVisible info
            |> not

    static member internal ColorIs (fillOrStrokeOptions: FillOrStrokeOptions, predicate: FsColor -> bool, predicateCompose) =
        let predicate = predicate >> predicateCompose 
        
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            IAbstractRenderInfo.ColorIs(fillOrStrokeOptions, FsColor.OfItextColor >> predicate) info

    static member ColorIs (fillOrStrokeOptions: FillOrStrokeOptions, predicate: FsColor -> bool) =
        Info.ColorIs(fillOrStrokeOptions, predicate, id)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member AlternativeColorIs (fillOrStrokeOptions: FillOrStrokeOptions, predicate: AlternativeFsColor -> bool) =
        let predicate (fsColor: FsColor) =
            match fsColor.AsAlternativeFsColor with 
            | Some color -> predicate color
            | None -> false

        Info.ColorIs(fillOrStrokeOptions, predicate, id)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)


    static member ColorIs (fillOrStrokeOptions: FillOrStrokeOptions, color: FsColor) =
        Info.ColorIs(fillOrStrokeOptions, fun color' -> 
            FsColor.equal color (color')
        )
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

    static member FillColorIs (color: FsColor) =
        Info.ColorIs(FillOrStrokeOptions.Fill, fun color' -> 
            FsColor.equal color (color')
        )
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member FillColorIs (pdfCanvasColor: PdfCanvasColor) =
        Info.ColorIs(FillOrStrokeOptions.Fill, fun color -> pdfCanvasColor.IsEqualTo(color))
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member FillColorIs (colorSpace: ColorSpace) =
        Info.ColorIs(FillOrStrokeOptions.Fill, fun color -> color.IsInColorSpace(colorSpace))
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member FillColorIsNot (color: FsColor) =
        Info.IsFillNotVisible()
        <||>
        Info.ColorIs(FillOrStrokeOptions.Fill, (fun color' -> FsColor.equal color (color')), not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member FillColorIsNot (pdfCanvasColor: PdfCanvasColor) =
        Info.IsFillNotVisible()
        <||>
        Info.ColorIs(FillOrStrokeOptions.Fill, (fun color -> pdfCanvasColor.IsEqualTo(color)), not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member FillColorIsNot (colorSpace: ColorSpace) =
        Info.IsFillNotVisible()
        <||>
        Info.ColorIs(FillOrStrokeOptions.Fill, (fun color -> color.IsInColorSpace(colorSpace)), not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIs (color: FsColor) =
        Info.ColorIs(FillOrStrokeOptions.Stroke, (fun color' -> FsColor.equal color (color')))
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIs (pdfCanvasColor: PdfCanvasColor) =
        Info.ColorIs(FillOrStrokeOptions.Stroke, fun color -> pdfCanvasColor.IsEqualTo(color))
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIs (colorSpace: ColorSpace) =
        Info.ColorIs(FillOrStrokeOptions.Stroke, fun color -> color.IsInColorSpace(colorSpace))
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIsNot (color: FsColor) =
        Info.IsStrokeNotVisible()
        <||>
        Info.ColorIs(FillOrStrokeOptions.Stroke, FsColor.equal color, not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIsNot (pdfCanvasColor: PdfCanvasColor) =
        Info.IsStrokeNotVisible()
        <||>
        Info.ColorIs(FillOrStrokeOptions.Stroke, (fun color -> pdfCanvasColor.IsEqualTo(color)), not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIsNot (colorSpace: ColorSpace) =
        Info.IsStrokeNotVisible()
        <||>
        Info.ColorIs(FillOrStrokeOptions.Stroke, (fun color -> color.IsInColorSpace(colorSpace)), not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member ColorIsOneOf (fillOrStrokeOptions: FillOrStrokeOptions, colors: FsColor list, ?valueEqualOptions) =
        let valueEqualOptions = 
            ValueEqualOptions.DefaultRoundedValue
            |> defaultArg valueEqualOptions 

        Info.ColorIs(fillOrStrokeOptions, fun color -> FsColors.containsWith (valueEqualOptions) (color) colors)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member ColorIsOneOf (fillOrStrokeOptions: FillOrStrokeOptions, fsSeparations: FsSeparation list) =
        Info.ColorIs(fillOrStrokeOptions, fun color -> 
            FsSeparation.Contains(ValueEqualOptions.DefaultRoundedValue) color fsSeparations
        )
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member ColorIsOneOf (fillOrStrokeOptions: FillOrStrokeOptions, colors: PdfCanvasColor list) =
        Info.ColorIs(fillOrStrokeOptions, fun color -> PdfCanvasColor.Contains (color) colors)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member FillColorIsOneOf (colors: FsColor list) =
        Info.ColorIs(FillOrStrokeOptions.Fill, fun color -> FsColors.contains color colors)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member FillColorIsNotOneOf (colors: FsColor list) =
        Info.IsFillNotVisible()
        <||>
        Info.ColorIs(FillOrStrokeOptions.Fill, (fun color -> FsColors.contains color colors), not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIsOneOf (colors: FsColor list) =
        Info.ColorIs(FillOrStrokeOptions.Stroke, (fun color -> FsColors.contains color colors))
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member StrokeColorIsNotOneOf (colors: FsColor list) =
        Info.IsStrokeNotVisible()
        <||>
        Info.ColorIs(FillOrStrokeOptions.Stroke, (fun color -> FsColors.contains color colors), not)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member BoundSizeIs (predicate, ?isDense, ?boundGettingStrokeOptions) = 
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            let boundGettingStrokeOptions = defaultArg boundGettingStrokeOptions BoundGettingStrokeOptions.WithoutStrokeWidth
            let isDense = defaultArg isDense true

            let bound: iText.Kernel.Geom.Rectangle = 
                match isDense with 
                | true -> IAbstractRenderInfo.getDenseBound boundGettingStrokeOptions info
                | false -> IAbstractRenderInfo.getBound boundGettingStrokeOptions info

            predicate bound

    static member private BoundIs (isDense, relativePosition, areaGettingOptions: AreaGettingOptions, ?boundGettingStrokeOptions) =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            Info.BoundSizeIs(
                isDense = isDense,
                ?boundGettingStrokeOptions = boundGettingStrokeOptions,
                predicate = fun bound ->
                    let rect = args.Page.GetArea(areaGettingOptions)
                    match relativePosition with 
                    | RelativePosition.CrossBox -> bound.IsCrossOf(rect)
                    | RelativePosition.Inbox -> bound.IsInsideOf(rect)
                    | RelativePosition.OutBox -> bound.IsOutsideOf(rect)
            ) args info



    static member BoundIs (relativePosition, areaGettingOptions: AreaGettingOptions, ?boundGettingStrokeOptions) =
        Info.BoundIs(false, relativePosition, areaGettingOptions, ?boundGettingStrokeOptions = boundGettingStrokeOptions)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member BoundIs (args: Info_BoundIs_Args) =
        Info.BoundIs(args.RelativePosition, (args.AreaGettingOptions) , args.BoundGettingStrokeOptions)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member BoundIsCrossOf (areaGettingOptions, ?boundGettingStrokeOptions) =
        Info.BoundIs(RelativePosition.CrossBox, areaGettingOptions = areaGettingOptions, ?boundGettingStrokeOptions = boundGettingStrokeOptions)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member BoundIsInsideOf (areaGettingOptions, ?boundGettingStrokeOptions) =
        Info.BoundIs(RelativePosition.Inbox, areaGettingOptions = areaGettingOptions, ?boundGettingStrokeOptions = boundGettingStrokeOptions)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member BoundIs_InsideOrCross_Of (areaGettingOptions, ?boundGettingStrokeOptions) =
        !!! (Info.BoundIs(RelativePosition.OutBox, areaGettingOptions = areaGettingOptions, ?boundGettingStrokeOptions = boundGettingStrokeOptions))
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)


    static member PointOfBoundIsInsideOf (pointPosition: Position, areaGettingOptions, ?boundGettingStrokeOptions) =
        fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
            let boundGettingStrokeOptions = defaultArg boundGettingStrokeOptions BoundGettingStrokeOptions.WithoutStrokeWidth

            let bound = IAbstractRenderInfo.getBound boundGettingStrokeOptions info

            let point = bound.GetPoint(pointPosition)

            let rect = args.Page.GetArea(areaGettingOptions)

            point.IsInsideOf(rect)



    static member CenterPointOfBoundIsInsideOf (areaGettingOptions, ?boundGettingStrokeOptions) =
        Info.PointOfBoundIsInsideOf(Position.Center(0., 0.), areaGettingOptions, ?boundGettingStrokeOptions = boundGettingStrokeOptions)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member BottomPointOfBoundIsInsideOf (areaGettingOptions, ?boundGettingStrokeOptions) =
        Info.PointOfBoundIsInsideOf(Position.Center(0., 0.), areaGettingOptions, ?boundGettingStrokeOptions = boundGettingStrokeOptions)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)


    static member BoundIsOutsideOf (areaGettingOptions, ?boundGettingStrokeOptions) =
        Info.BoundIs(RelativePosition.OutBox, areaGettingOptions = areaGettingOptions, ?boundGettingStrokeOptions = boundGettingStrokeOptions)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    static member DenseBoundIs (relativePosition, areaGettingOptions, ?boundGettingStrokeOptions) =
        Info.BoundIs(true, relativePosition, areaGettingOptions = areaGettingOptions, ?boundGettingStrokeOptions = boundGettingStrokeOptions)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)


    static member DenseBoundIsOutsideOf (areaGettingOptions, ?boundGettingStrokeOptions) =
        Info.BoundIs(true, RelativePosition.OutBox, areaGettingOptions = areaGettingOptions, ?boundGettingStrokeOptions = boundGettingStrokeOptions)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)


    static member DenseBoundIsInsideOf (areaGettingOptions, ?boundGettingStrokeOptions) =
        Info.BoundIs(true, RelativePosition.Inbox, areaGettingOptions = areaGettingOptions, ?boundGettingStrokeOptions = boundGettingStrokeOptions)
        |> reSharp (fun (info: #IAbstractRenderInfo) -> info)
