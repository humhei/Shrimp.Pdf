namespace Shrimp.Pdf.Parser
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Listener
open System.Collections.Generic
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Extensions
open Shrimp.Pdf

[<RequireQualifiedAccess>]
module ClippingPathInfo =
    let tryGetClippingPath (info: ClippingPathInfo) = 
        match info.GetClippingPath() with 
        | null -> None
        | path -> 
            if path.GetSubpaths() |> Seq.forall(fun subPath -> subPath.GetPiecewiseLinearApproximation().Count = 0) then None
            else Some path

    let tryGetActualClippingArea (info) =
        tryGetClippingPath info
        |> Option.map (fun clippingPath ->
            clippingPath.GetSubpaths()
            |> Seq.collect(Subpath.toActualPoints (info.GetGraphicsState().GetCtm()))
            |> Rectangle.ofPoints
        )

type FillOrStrokeOptions =
    | Stroke = 0
    | Fill = 1

[<RequireQualifiedAccess>]
module internal AbstractRenderInfo =
    let isVisible (fillOrStrokeOptions: FillOrStrokeOptions) (clippingPathInfo) (info: AbstractRenderInfo) =

        match fillOrStrokeOptions with 
        | FillOrStrokeOptions.Fill -> AbstractRenderInfo.hasFill info
        | FillOrStrokeOptions.Stroke -> AbstractRenderInfo.hasStroke info
        | _ -> failwith "Invalid token"
        &&
            match info with 
            | :? PathRenderInfo as info -> not (info.IsPathModifiesClippingPath())
            | _ -> true
        && 
            match ClippingPathInfo.tryGetActualClippingArea clippingPathInfo with 
                | Some clippingBound ->
                    let bound = AbstractRenderInfo.getBound BoundGettingOptions.WithStrokeWidth info
                    match Rectangle.tryGetIntersection bound clippingBound with 
                    | Some _ -> true
                    | None -> false
                | None -> true

    let tryGetVisibleBound boundGettingOptions (clippingPathInfo) (info: AbstractRenderInfo) =
        if isVisible (FillOrStrokeOptions.Stroke) clippingPathInfo info || isVisible (FillOrStrokeOptions.Fill) clippingPathInfo info 
        then
            let bound = AbstractRenderInfo.getBound boundGettingOptions info
            match ClippingPathInfo.tryGetActualClippingArea clippingPathInfo with 
            | Some clippingBound -> Rectangle.tryGetIntersection bound clippingBound 
            | None -> 
                if bound.GetWidthF() @= 0. || bound.GetHeightF() @= 0. then None
                else
                    Some bound
        else None


[<RequireQualifiedAccess>]
type IntegratedRenderInfoKind =
    | Path = 0
    | Text  = 1

type IntegratedRenderInfo =
    { RenderInfo: AbstractRenderInfo 
      ClippingPathInfo: ClippingPathInfo
      Kind: IntegratedRenderInfoKind }

[<RequireQualifiedAccess>]
module IntegratedRenderInfo =

    let asPathRenderInfo (info: IntegratedRenderInfo) = 
        match info.Kind with
        | IntegratedRenderInfoKind.Path -> Some (info.RenderInfo :?> PathRenderInfo)
        | _ -> None 

    let asTextRenderInfo (info: IntegratedRenderInfo) =
        match info.Kind with
        | IntegratedRenderInfoKind.Text -> Some (info.RenderInfo :?> TextRenderInfo)
        | _ -> None 

    let isStrokeVisible (info: IntegratedRenderInfo) = 
        AbstractRenderInfo.isVisible FillOrStrokeOptions.Stroke info.ClippingPathInfo info.RenderInfo

    let isFillVisible (info: IntegratedRenderInfo) = 
        AbstractRenderInfo.isVisible FillOrStrokeOptions.Fill info.ClippingPathInfo info.RenderInfo

    let tryGetVisibleBound boundGettingOptions (info: IntegratedRenderInfo) =
        AbstractRenderInfo.tryGetVisibleBound boundGettingOptions info.ClippingPathInfo info.RenderInfo

    let isVisible (info: IntegratedRenderInfo) =
        isFillVisible info || isStrokeVisible info


module internal Listeners =

    type CurrentRenderInfoStatus =
        | Skiped = 0
        | Selected = 1

    [<AllowNullLiteral>]
    /// a type named FilteredEventListener is already defined in itext7
    type FilteredEventListenerEx(supportedEvents: EventType list, filter: IntegratedRenderInfo -> bool) =
        let mutable currentRenderInfo: IntegratedRenderInfo option = None
        let mutable currentRenderInfoStatus = CurrentRenderInfoStatus.Selected
        let mutable currentClippingPathInfo = null

        let parsedRenderInfos = List<IntegratedRenderInfo>()

        member this.CurrentRenderInfo = currentRenderInfo.Value

        member this.CurrentRenderInfoStatus = currentRenderInfoStatus

        member this.ParsedRenderInfos = parsedRenderInfos :> seq<IntegratedRenderInfo>


        interface IEventListener with 
            member this.EventOccurred(data, tp) = 
                match tp with 
                | EventType.CLIP_PATH_CHANGED -> 
                    currentClippingPathInfo <- (data :?> ClippingPathInfo)
                    currentClippingPathInfo.PreserveGraphicsState()

                | _ ->
                    let renderInfo = 
                        match data with 
                        | :? PathRenderInfo as pathRenderInfo ->
                            { ClippingPathInfo  = currentClippingPathInfo 
                              RenderInfo = pathRenderInfo
                              Kind = IntegratedRenderInfoKind.Path }
                        | :? TextRenderInfo as textRenderInfo ->
                            { ClippingPathInfo  = currentClippingPathInfo 
                              RenderInfo = textRenderInfo 
                              Kind = IntegratedRenderInfoKind.Text }
                        |_ -> failwith "Not implemented"
                       

                    if filter renderInfo then
                        renderInfo.RenderInfo.PreserveGraphicsState()

                        parsedRenderInfos.Add(renderInfo)
                        currentRenderInfo <- Some renderInfo
                        currentRenderInfoStatus <- CurrentRenderInfoStatus.Selected
                    else 
                        currentRenderInfoStatus <- CurrentRenderInfoStatus.Skiped
                        currentRenderInfo <- None

            member this.GetSupportedEvents() = 
                List supportedEvents :> ICollection<_>

    type DummyListener() =
        interface IEventListener with
            member this.EventOccurred(data,tp) = ()
            member this.GetSupportedEvents() =
                    List () :> ICollection<_>


[<RequireQualifiedAccess>]
type RenderInfoSelector = 
    | Path of (PathRenderInfo -> bool)
    | Text of (TextRenderInfo -> bool)
    | PathOrText of (AbstractRenderInfo -> bool)
    | PathIntegrated of (IntegratedRenderInfo -> bool)
    | TextIntegrated of (IntegratedRenderInfo -> bool)
    | Dummy
    | AND of RenderInfoSelector list
    | OR of RenderInfoSelector list

[<RequireQualifiedAccess>]
module RenderInfoSelector =
    let toEventTypes selector =
        let rec loop selector =
            match selector with 
            | RenderInfoSelector.Path _ -> [EventType.RENDER_PATH; EventType.CLIP_PATH_CHANGED]
            | RenderInfoSelector.Text _ -> [EventType.RENDER_TEXT; EventType.CLIP_PATH_CHANGED]
            | RenderInfoSelector.PathOrText _ -> [EventType.RENDER_TEXT; EventType.RENDER_PATH; EventType.CLIP_PATH_CHANGED]
            | RenderInfoSelector.PathIntegrated _ -> [EventType.RENDER_PATH; EventType.CLIP_PATH_CHANGED]
            | RenderInfoSelector.TextIntegrated _ -> [EventType.RENDER_TEXT; EventType.CLIP_PATH_CHANGED]
            | RenderInfoSelector.Dummy -> []
            | RenderInfoSelector.AND selectors ->
                selectors
                |> List.collect(loop)

            | RenderInfoSelector.OR selectors ->
                selectors
                |> List.collect(loop)

        loop selector
        |> List.distinct

    let toRenderInfoPredication (selector) =
        let rec loop selector =
            match selector with 
            | RenderInfoSelector.Path prediate -> 
                fun (renderInfo: IntegratedRenderInfo) ->
                    match renderInfo.RenderInfo with 
                    | :? PathRenderInfo as pathRenderInfo -> prediate pathRenderInfo
                    | _ -> false

            | RenderInfoSelector.PathIntegrated prediate ->
                fun (renderInfo: IntegratedRenderInfo) -> 
                    match renderInfo.Kind with 
                    | IntegratedRenderInfoKind.Text -> false
                    | IntegratedRenderInfoKind.Path -> prediate renderInfo
                    | _ -> failwith "Invalid token"

            | RenderInfoSelector.Text prediate -> 
                fun (renderInfo: IntegratedRenderInfo) ->
                    match renderInfo.RenderInfo with 
                    | :? TextRenderInfo as textRenderInfo -> prediate textRenderInfo
                    | _ -> false

            | RenderInfoSelector.PathOrText prediate -> 
                fun (renderInfo: IntegratedRenderInfo) ->
                    prediate renderInfo.RenderInfo

            | RenderInfoSelector.TextIntegrated prediate ->
                fun (renderInfo: IntegratedRenderInfo) ->
                    match renderInfo.Kind with 
                    | IntegratedRenderInfoKind.Text -> prediate renderInfo
                    | IntegratedRenderInfoKind.Path -> false
                    | _ -> failwith "Invalid token"

            | RenderInfoSelector.Dummy _ -> fun _ -> false


            | RenderInfoSelector.AND selectors ->
                fun (renderInfo: IntegratedRenderInfo) ->
                    selectors |> List.forall (fun selector -> loop selector renderInfo)

            | RenderInfoSelector.OR selectors ->
                fun (renderInfo: IntegratedRenderInfo) ->
                    selectors |> List.exists (fun selector -> loop selector renderInfo)

        loop selector



[<RequireQualifiedAccess>]
module PdfDocumentContentParser =
    open Listeners
    let parse (pageNum: int) (renderInfoSelector: RenderInfoSelector) (parser: PdfDocumentContentParser) =
        let et = RenderInfoSelector.toEventTypes renderInfoSelector
        let filter = RenderInfoSelector.toRenderInfoPredication renderInfoSelector
        let listener = new FilteredEventListenerEx(et, filter)

        match et with 
        | [] -> [] :> seq<IntegratedRenderInfo>
        | _ ->
            parser.ProcessContent(pageNum, listener).ParsedRenderInfos
        
