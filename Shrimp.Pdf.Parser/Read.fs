namespace Shrimp.Pdf.Parser
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Listener
open System.Collections.Generic
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Extensions
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open System.Reflection


[<RequireQualifiedAccess>]
module private AbstractRenderInfo =
    type VisibleGettingOptions =
        | Stroke = 0
        | Fill = 1

    let getParserGraphicsStatePreservation (info: AbstractRenderInfo) =
        match info with 
        | :? PathRenderInfoEx as info ->info.GetParserGraphicsStatePreservation()
        | :? TextRenderInfoEx as info -> info.GetParserGraphicsStatePreservation()
        | _ -> failwith "renderInfo should be either pathRenderInfoEx or TextRenderInfoEx"

    let isVisible (options: VisibleGettingOptions) (info: AbstractRenderInfo) =
        let parserGraphicsStatePreservation = getParserGraphicsStatePreservation info

        match options with 
        | VisibleGettingOptions.Fill -> AbstractRenderInfo.hasFill info
        | VisibleGettingOptions.Stroke -> AbstractRenderInfo.hasStroke info
        | _ -> failwith "Invalid token"
        &&
            match info with 
            | :? PathRenderInfoEx as info -> not (info.IsPathModifiesClippingPath())
            | _ -> true
        && 
            match parserGraphicsStatePreservation.TryGetClippingArea() with 
                | Some clippingBound ->
                    let bound = AbstractRenderInfo.getBound BoundGettingOptions.WithStrokeWidth info
                    match Rectangle.tryGetIntersection bound clippingBound with 
                    | Some _ -> true
                    | None -> false
                | None -> true

    let isStrokeVisible info = isVisible VisibleGettingOptions.Stroke info

    let isFillVisible info = isVisible VisibleGettingOptions.Fill info

    let tryGetVisibleBound  (info: AbstractRenderInfo)=
        match isStrokeVisible info, isFillVisible info with 
        | true, _ 
        | false, true ->
            let bound = AbstractRenderInfo.getBound BoundGettingOptions.WithStrokeWidth info
            match (getParserGraphicsStatePreservation info).TryGetClippingArea() with 
            | Some clippingBound -> Rectangle.tryGetIntersection bound clippingBound 
            | None -> Some bound
        | _ -> None

[<RequireQualifiedAccess>]
module PathRenderInfoEx =

    let isStrokeVisible (info: PathRenderInfoEx) =             
        AbstractRenderInfo.isStrokeVisible info

    let isFillVisible (info: PathRenderInfoEx) = AbstractRenderInfo.isFillVisible info         
        
    let isVisible (info: PathRenderInfoEx) = isFillVisible info || isStrokeVisible info

    let tryGetVisibleBound (info: PathRenderInfoEx) = AbstractRenderInfo.tryGetVisibleBound info


[<RequireQualifiedAccess>]
module TextRenderInfoEx =
    let isStrokeVisible (info: TextRenderInfoEx) =             
        AbstractRenderInfo.isStrokeVisible info

    let isFillVisible (info: TextRenderInfoEx) = AbstractRenderInfo.isFillVisible info         
        
    let isVisible (info: TextRenderInfoEx) = isFillVisible info || isStrokeVisible info

    let tryGetVisibleBound (info: TextRenderInfoEx) = AbstractRenderInfo.tryGetVisibleBound info



module internal Listeners =

    type CurrentRenderInfoStatus =
        | Skiped = 0
        | Selected = 1

    [<AllowNullLiteral>]
    /// a type named FilteredEventListener is already defined in itext7
    type FilteredEventListenerEx(supportedEvents: EventType list, filter: AbstractRenderInfo -> bool) =
        let mutable currentRenderInfo = null
        let mutable currentRenderInfoStatus = CurrentRenderInfoStatus.Selected
        let mutable currentClippingPathCtm = null
        let mutable currentClippingPathInfo = null

        let parsedRenderInfos = List<AbstractRenderInfo>()


        member this.CurrentRenderInfo = currentRenderInfo

        member this.CurrentRenderInfoStatus = currentRenderInfoStatus

        member this.ParsedRenderInfos = parsedRenderInfos :> seq<AbstractRenderInfo>


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
                            new PathRenderInfoEx(pathRenderInfo, currentClippingPathInfo) :> AbstractRenderInfo

                        | :? TextRenderInfo as textRenderInfo ->
                            new TextRenderInfoEx(textRenderInfo, currentClippingPathInfo) :> AbstractRenderInfo
                        | _ -> failwithf "Unspported renderinfo type %s" (data.GetType().FullName) 

                    if filter renderInfo then
                        renderInfo.PreserveGraphicsState()

                        parsedRenderInfos.Add(renderInfo)
                        currentRenderInfo <- renderInfo
                        currentRenderInfoStatus <- CurrentRenderInfoStatus.Selected
                    else 
                        currentRenderInfoStatus <- CurrentRenderInfoStatus.Skiped
                        currentRenderInfo <- null

            member this.GetSupportedEvents() = 
                List supportedEvents :> ICollection<_>

    type DummyListener() =
        interface IEventListener with
            member this.EventOccurred(data,tp) = ()
            member this.GetSupportedEvents() =
                    List () :> ICollection<_>


[<RequireQualifiedAccess>]
type RenderInfoSelector = 
    | Path of (PathRenderInfoEx -> bool)
    | Text of (TextRenderInfoEx -> bool)
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
                fun (renderInfo: AbstractRenderInfo) ->
                    match renderInfo with 
                    | :? PathRenderInfoEx as pathRenderInfo ->
                        prediate pathRenderInfo
                    | _ -> false

            | RenderInfoSelector.Text prediate -> 
                fun (renderInfo: AbstractRenderInfo) ->
                    match renderInfo with 
                    | :? TextRenderInfoEx as textRenderInfo ->
                        prediate textRenderInfo
                    | _ -> false

            | RenderInfoSelector.Dummy _ -> fun _ -> false

            | RenderInfoSelector.AND selectors ->
                fun (renderInfo: AbstractRenderInfo) ->
                    selectors |> List.forall (fun selector -> loop selector renderInfo)
                

            | RenderInfoSelector.OR selectors ->
                fun (renderInfo: AbstractRenderInfo) ->
                    selectors |> List.exists (fun selector -> loop selector renderInfo)

        loop selector



[<RequireQualifiedAccess>]
module PdfDocumentContentParser =
    open Listeners
    let parse (pageNum: int) (renderInfoSelector: RenderInfoSelector) (parser: PdfDocumentContentParser) =
        let et = RenderInfoSelector.toEventTypes renderInfoSelector
        let filter = RenderInfoSelector.toRenderInfoPredication renderInfoSelector
        let listener = new FilteredEventListenerEx(et,filter)

        match et with 
        | [] -> [] :> seq<AbstractRenderInfo>
        | _ ->
            parser.ProcessContent(pageNum, listener).ParsedRenderInfos
        
