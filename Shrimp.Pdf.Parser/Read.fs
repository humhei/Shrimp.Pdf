namespace Shrimp.Pdf.Parser
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Listener
open System.Collections.Generic
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Extensions

module internal Listeners =

    type CurrentRenderInfoStatus =
        | Skiped = 0
        | Selected = 1

    [<AllowNullLiteral>]
    /// a type named FilteredEventListener is already defined in itext7
    type FilteredEventListenerEx<'T when 'T :> AbstractRenderInfo and 'T: null>(supportedEvents: EventType list, filter: 'T -> bool) =
        let mutable currentRenderInfo = null
        let mutable currentRenderInfoStatus = CurrentRenderInfoStatus.Selected
        let parsedRenderInfos = List<'T>()


        member this.CurrentRenderInfo = currentRenderInfo

        member this.CurrentRenderInfoStatus = currentRenderInfoStatus

        member this.ParsedRenderInfos = parsedRenderInfos :> seq<'T>


        interface IEventListener with 
            member this.EventOccurred(data, tp) = 
                let data = data :?> 'T
                if filter data then
                    data.PreserveGraphicsState()
                    parsedRenderInfos.Add(data)
                    currentRenderInfo <- data
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
    | AND of RenderInfoSelector list
    | OR of RenderInfoSelector list

[<RequireQualifiedAccess>]
module RenderInfoSelector =
    let toEventTypes selector =
        let rec loop selector =
            match selector with 
            | RenderInfoSelector.Path _ -> [EventType.RENDER_PATH]
            | RenderInfoSelector.Text _ -> [EventType.RENDER_TEXT]
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
        let listener = new FilteredEventListenerEx<'T>(et,filter)
        parser.ProcessContent(pageNum, listener).ParsedRenderInfos |> Seq.cast<'T>
        
