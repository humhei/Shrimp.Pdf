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
module internal IAbstractRenderInfo =
    let isVisible (fillOrStrokeOptions: FillOrStrokeOptions) (clippingPathInfo) (info: IAbstractRenderInfo) =

        match fillOrStrokeOptions with 
        | FillOrStrokeOptions.Fill -> IAbstractRenderInfo.hasFill info
        | FillOrStrokeOptions.Stroke -> IAbstractRenderInfo.hasStroke info
        | _ -> failwith "Invalid token"
        &&
            match info with 
            | :? PathRenderInfo as info -> not (info.IsPathModifiesClippingPath())
            | _ -> true
        && 
            match ClippingPathInfo.tryGetActualClippingArea clippingPathInfo with 
                | Some clippingBound ->
                    let bound = IAbstractRenderInfo.getBound BoundGettingOptions.WithStrokeWidth info
                    match Rectangle.tryGetIntersection bound clippingBound with 
                    | Some _ -> true
                    | None -> false
                | None -> true

    let tryGetVisibleBound boundGettingOptions (clippingPathInfo) (info: IAbstractRenderInfo) =
        if isVisible (FillOrStrokeOptions.Stroke) clippingPathInfo info || isVisible (FillOrStrokeOptions.Fill) clippingPathInfo info 
        then
            let bound = IAbstractRenderInfo.getBound boundGettingOptions info
            match ClippingPathInfo.tryGetActualClippingArea clippingPathInfo with 
            | Some clippingBound -> Rectangle.tryGetIntersection bound clippingBound 
            | None -> 
                if bound.GetWidthF() @= 0. || bound.GetHeightF() @= 0. then None
                else
                    Some bound
        else None




[<AutoOpen>]
module rec ReadMutual = 

    type IntegratedRenderInfoTag =
        | Path = 0
        | Text = 1

    type IIntegratedRenderInfo =
        inherit IAbstractRenderInfo
        abstract member Tag: IntegratedRenderInfoTag
        abstract member ClippingPathInfo: ClippingPathInfo

    [<Struct>]
    type IntegratedPathRenderInfo =
        { PathRenderInfo: PathRenderInfo 
          ClippingPathInfo: ClippingPathInfo }
    with 
        interface IPathRenderInfo with 
            member x.Value = x.PathRenderInfo

        interface IAbstractRenderInfo with 
            member x.Value = x.PathRenderInfo :> AbstractRenderInfo

        interface IIntegratedRenderInfo with 
            member x.Tag = IntegratedRenderInfoTag.Path
            member x.ClippingPathInfo = x.ClippingPathInfo

    [<Struct>]
    type IntegratedTextRenderInfo =
        { TextRenderInfo: TextRenderInfo 
          ClippingPathInfo: ClippingPathInfo }

    with 
        interface ITextRenderInfo with 
            member x.Value = x.TextRenderInfo

        interface IAbstractRenderInfo with 
            member x.Value = x.TextRenderInfo :> AbstractRenderInfo

        interface IIntegratedRenderInfo with 
            member x.Tag = IntegratedRenderInfoTag.Text

            member x.ClippingPathInfo = x.ClippingPathInfo

    [<RequireQualifiedAccess>]
    type IntegratedRenderInfo =
        | Text of IntegratedTextRenderInfo
        | Path of IntegratedPathRenderInfo

    with 
        member x.ClippingPathInfo =
            match x with 
            | IntegratedRenderInfo.Text info -> info.ClippingPathInfo
            | IntegratedRenderInfo.Path info -> info.ClippingPathInfo

        member x.RenderInfo : AbstractRenderInfo =
            match x with 
            | IntegratedRenderInfo.Text info -> info.TextRenderInfo :> AbstractRenderInfo
            | IntegratedRenderInfo.Path info -> info.PathRenderInfo :> AbstractRenderInfo

[<RequireQualifiedAccess>]
module IIntegratedRenderInfo =
    let (|Text|Path|) (info: IIntegratedRenderInfo) = 
        match info.Tag with 
        | IntegratedRenderInfoTag.Path -> Path (info :?> IntegratedPathRenderInfo)
        | IntegratedRenderInfoTag.Text -> Text (info :?> IntegratedTextRenderInfo)
        | _ -> failwith "Invalid token"

    let asIPathRenderInfo (info: IIntegratedRenderInfo) = 
        match info with
        | Path info -> Some (info)
        | _ -> None 

    let asITextRenderInfo (info: IIntegratedRenderInfo) =
        match info with
        | Text info -> Some (info)
        | _ -> None 

    let isStrokeVisible (info: IIntegratedRenderInfo) = 
        IAbstractRenderInfo.isVisible FillOrStrokeOptions.Stroke info.ClippingPathInfo info

    let isFillVisible (info: IIntegratedRenderInfo) = 
        IAbstractRenderInfo.isVisible FillOrStrokeOptions.Fill info.ClippingPathInfo info

    let tryGetVisibleBound boundGettingOptions (info: IIntegratedRenderInfo) =
        IAbstractRenderInfo.tryGetVisibleBound boundGettingOptions info.ClippingPathInfo info

    let isVisible (info: IIntegratedRenderInfo) =
        isFillVisible info || isStrokeVisible info



[<RequireQualifiedAccess>]
type RenderInfoSelector = 
    | PathOrText of (IIntegratedRenderInfo -> bool)
    | Path of (IntegratedPathRenderInfo -> bool)
    | Text of (IntegratedTextRenderInfo -> bool)
    | Dummy
    | AND of RenderInfoSelector list
    | OR of RenderInfoSelector list

[<RequireQualifiedAccess>]
module RenderInfoSelector =


    let toEventTypes selector =
        let rec loop selector =
            match selector with 
            | RenderInfoSelector.PathOrText _ -> [EventType.RENDER_TEXT; EventType.RENDER_PATH; EventType.CLIP_PATH_CHANGED]
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
                fun (renderInfo: IIntegratedRenderInfo) -> 
                    match renderInfo with 
                    | IIntegratedRenderInfo.Text _ -> false
                    | IIntegratedRenderInfo.Path renderInfo -> prediate renderInfo

            | RenderInfoSelector.PathOrText prediate -> 
                fun (renderInfo: IIntegratedRenderInfo) ->
                    prediate renderInfo

            | RenderInfoSelector.Text prediate ->
                fun (renderInfo: IIntegratedRenderInfo) ->
                    match renderInfo with 
                    | IIntegratedRenderInfo.Text renderInfo -> prediate renderInfo
                    | IIntegratedRenderInfo.Path _ -> false

            | RenderInfoSelector.Dummy _ -> fun _ -> false


            | RenderInfoSelector.AND selectors ->
                fun (renderInfo: IIntegratedRenderInfo) ->
                    selectors |> List.forall (fun selector -> loop selector renderInfo)

            | RenderInfoSelector.OR selectors ->
                fun (renderInfo: IIntegratedRenderInfo) ->
                    selectors |> List.exists (fun selector -> loop selector renderInfo)

        loop selector


[<Struct>]
type SelectorModiferToken = 
    { Index: int 
      Name: string }

module internal Listeners =

    type CurrentRenderInfoStatus =
        | Skiped = 0
        | Selected = 1


    [<AllowNullLiteral>]
    /// a type named FilteredEventListener is already defined in itext7
    /// OR Between selectors of renderInfoSelectorMapping
    type FilteredEventListenerEx(renderInfoSelectorMapping: Map<SelectorModiferToken, RenderInfoSelector>) =
        let prediateMapping = 
            renderInfoSelectorMapping
            |> Map.map (fun token renderInfoSelector -> 
                RenderInfoSelector.toRenderInfoPredication renderInfoSelector
            )

        let mutable currentRenderInfo: IIntegratedRenderInfo option = None
        let mutable currentRenderInfoToken = None
        let mutable currentRenderInfoStatus = CurrentRenderInfoStatus.Selected
        let mutable currentClippingPathInfo = null

        let parsedRenderInfos = List<IIntegratedRenderInfo>()

        member this.CurrentRenderInfo = currentRenderInfo.Value

        member this.CurrentRenderInfoStatus = currentRenderInfoStatus

        member this.CurrentRenderInfoToken = currentRenderInfoToken

        member this.ParsedRenderInfos = parsedRenderInfos :> seq<IIntegratedRenderInfo>


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
                              PathRenderInfo = pathRenderInfo }
                            :> IIntegratedRenderInfo

                        | :? TextRenderInfo as textRenderInfo ->
                            { ClippingPathInfo  = currentClippingPathInfo 
                              TextRenderInfo = textRenderInfo }
                            :> IIntegratedRenderInfo

                        |_ -> failwith "Not implemented"


                    let predicate _ filter =
                        filter renderInfo

                    match Map.tryFindKey predicate prediateMapping with 
                    | Some token ->

                        renderInfo.Value.PreserveGraphicsState()

                        parsedRenderInfos.Add(renderInfo)
                        currentRenderInfoToken <- Some token
                        currentRenderInfo <- Some renderInfo
                        currentRenderInfoStatus <- CurrentRenderInfoStatus.Selected

                    | None -> 
                        currentRenderInfoStatus <- CurrentRenderInfoStatus.Skiped
                        currentRenderInfo <- None

            member this.GetSupportedEvents() = 
                let supportedEvents = 
                    renderInfoSelectorMapping
                    |> Map.toList
                    |> List.map snd
                    |> RenderInfoSelector.OR
                    |> RenderInfoSelector.toEventTypes
                  
                List supportedEvents :> ICollection<_>

    type DummyListener() =
        interface IEventListener with
            member this.EventOccurred(data,tp) = ()
            member this.GetSupportedEvents() = List () :> ICollection<_>




[<RequireQualifiedAccess>]
module PdfDocumentContentParser =
    open Listeners
    let parse (pageNum: int) (renderInfoSelector: RenderInfoSelector) (parser: PdfDocumentContentParser) =

        let et = RenderInfoSelector.toEventTypes renderInfoSelector

        match et with 
        | [] -> [] :> seq<IIntegratedRenderInfo>
        | _ ->
            let renderInfoSelectorMapping = Map.ofList [{Index = 0; Name= "Unknown"}, renderInfoSelector]
            let listener = new FilteredEventListenerEx(renderInfoSelectorMapping)
            parser.ProcessContent(pageNum, listener).ParsedRenderInfos
        
