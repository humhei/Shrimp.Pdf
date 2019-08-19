namespace Shrimp.Pdf.Parser
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Listener
open System.Collections.Generic
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Extensions
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open System.Reflection

[<AutoOpen>]
module private Reflection =
    let private gs_fieldInfo = typeof<AbstractRenderInfo>.GetField("gs", BindingFlags.NonPublic ||| BindingFlags.Instance)
    type AbstractRenderInfo with 
        member this.GetInnerGS() =
            let value = gs_fieldInfo.GetValue(this) 
            value :?> ParserGraphicsState

type ParserGraphicsStatePreservation(source: ParserGraphicsState) =
    inherit CanvasGraphicsState(source)
    let clippingPath =
        match source.GetClippingPath()  with 
        | null -> None
        | path -> Some path

    member this.ClippingPath = clippingPath

type internal PathRenderInfoPreservation(pathRenderInfo: PathRenderInfo) =
    inherit PathRenderInfo(new Stack<_>(pathRenderInfo.GetCanvasTagHierarchy()), pathRenderInfo.GetInnerGS(), pathRenderInfo.GetPath(), pathRenderInfo.GetOperation())
    let mutable graphicsStateIsPreserved = false

    override this.PreserveGraphicsState() =
        this.CheckGraphicsState();
        this.gs <- new ParserGraphicsStatePreservation(this.gs :?> ParserGraphicsState)
        graphicsStateIsPreserved <- true

    override this.GetGraphicsState() =
        this.CheckGraphicsState()
        if graphicsStateIsPreserved 
        then this.gs 
        else (new ParserGraphicsStatePreservation(this.gs :?> ParserGraphicsState)) :> CanvasGraphicsState

    override this.IsGraphicsStatePreserved() = graphicsStateIsPreserved

    override this.ReleaseGraphicsState() =
        if (not graphicsStateIsPreserved) then this.gs <- null

type TextRenderInfoPreservation(textRenderInfo: TextRenderInfo) =
    inherit TextRenderInfo(textRenderInfo.GetPdfString(), textRenderInfo.GetInnerGS(), textRenderInfo.GetTextMatrix(), new Stack<_>(textRenderInfo.GetCanvasTagHierarchy()))

    let mutable graphicsStateIsPreserved = false

    override this.PreserveGraphicsState() =
        this.CheckGraphicsState();
        this.gs <- new ParserGraphicsStatePreservation(this.gs :?> ParserGraphicsState)
        graphicsStateIsPreserved <- true

    override this.GetGraphicsState() =
        this.CheckGraphicsState()
        if graphicsStateIsPreserved 
        then this.gs 
        else (new ParserGraphicsStatePreservation(this.gs :?> ParserGraphicsState)) :> CanvasGraphicsState

    override this.IsGraphicsStatePreserved() = graphicsStateIsPreserved

    override this.ReleaseGraphicsState() = if (not graphicsStateIsPreserved) then this.gs <- null

module internal Listeners =

    type CurrentRenderInfoStatus =
        | Skiped = 0
        | Selected = 1

    [<AllowNullLiteral>]
    /// a type named FilteredEventListener is already defined in itext7
    type FilteredEventListenerEx(supportedEvents: EventType list, filter: AbstractRenderInfo -> bool) =
        let mutable currentRenderInfo = null
        let mutable currentRenderInfoStatus = CurrentRenderInfoStatus.Selected
        let parsedRenderInfos = List<AbstractRenderInfo>()


        member this.CurrentRenderInfo = currentRenderInfo

        member this.CurrentRenderInfoStatus = currentRenderInfoStatus

        member this.ParsedRenderInfos = parsedRenderInfos :> seq<AbstractRenderInfo>


        interface IEventListener with 
            member this.EventOccurred(data, tp) = 
                let renderInfo = 
                    match data with 
                    | :? PathRenderInfo as pathRenderInfo ->
                        new PathRenderInfoPreservation(pathRenderInfo) :> AbstractRenderInfo

                    | :? TextRenderInfo as textRenderInfo ->
                        new TextRenderInfoPreservation(textRenderInfo) :> AbstractRenderInfo
                    | _ -> failwithf "Unspported renderinfo type %s" (data.GetType().FullName) 

                if filter renderInfo then
                    //let parserCanvasState = renderInfo.GetGraphicsState() :?> ParserGraphicsState
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
    | Path of (PathRenderInfo -> bool)
    | Text of (TextRenderInfo -> bool)
    | Dummy
    | AND of RenderInfoSelector list
    | OR of RenderInfoSelector list

[<RequireQualifiedAccess>]
module RenderInfoSelector =
    let toEventTypes selector =
        let rec loop selector =
            match selector with 
            | RenderInfoSelector.Path _ -> [EventType.RENDER_PATH]
            | RenderInfoSelector.Text _ -> [EventType.RENDER_TEXT]
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
                    | :? PathRenderInfo as pathRenderInfo ->
                        prediate pathRenderInfo
                    | _ -> false

            | RenderInfoSelector.Text prediate -> 
                fun (renderInfo: AbstractRenderInfo) ->
                    match renderInfo with 
                    | :? TextRenderInfo as textRenderInfo ->
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
        
