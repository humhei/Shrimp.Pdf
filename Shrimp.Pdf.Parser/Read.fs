namespace Shrimp.Pdf.Parser
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Listener
open System.Collections.Generic
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Extensions

module internal Listeners =

    [<AllowNullLiteral>]
    /// a type named FilteredEventListener is already defined in itext7
    type FilteredEventListenerEx<'T when 'T :> AbstractRenderInfo and 'T: null>(supportedEvents: EventType list, filter: 'T -> bool) =
        let mutable currentRenderInfo = null
        let parsedRenderInfos = List<'T>()

        member this.CurrentRenderInfo = currentRenderInfo

        member this.ParsedRenderInfos = parsedRenderInfos :> seq<'T>

        interface IEventListener with 
            member this.EventOccurred(data, tp) = 
                let data = data :?> 'T
                if filter data then
                    data.PreserveGraphicsState()
                    parsedRenderInfos.Add(data)
                    currentRenderInfo <- data

            member this.GetSupportedEvents() = 
                List supportedEvents :> ICollection<_>

    type DummyListener() =
        interface IEventListener with
            member this.EventOccurred(data,tp) = ()
            member this.GetSupportedEvents() =
                    List () :> ICollection<_>

[<RequireQualifiedAccess>]
module PdfDocumentContentParser =
    open Listeners

    let private parse<'T when 'T:> AbstractRenderInfo and 'T: null> et (pageNum: int) filter (parser: PdfDocumentContentParser) =
        let listener = new FilteredEventListenerEx<'T>(et,filter)
        parser.ProcessContent(pageNum, listener).ParsedRenderInfos |> Seq.cast<'T>
        
    let parsePaths (pageNum: int) filter (parser: PdfDocumentContentParser) =
        parse<PathRenderInfo> [EventType.RENDER_PATH] pageNum filter parser
            
    let parseTexts (pageNum: int) filter (parser: PdfDocumentContentParser) =
        parse<TextRenderInfo> [EventType.RENDER_TEXT] pageNum filter parser

    let parseTextAndPaths (pageNum: int) filter (parser: PdfDocumentContentParser) =
        parse<AbstractRenderInfo> [EventType.RENDER_TEXT;EventType.RENDER_PATH] pageNum filter parser

    //let parseTextAndPathsInAllPages (page: PdfPage) filter =
    //    read page filter

