namespace Atrous.Pdfarser
[<RequireQualifiedAccess>]
module Extract =
    open iText.Kernel.Pdf.Canvas.Parser
    open iText.Kernel.Pdf.Canvas.Parser.Data
    open Atrous.Pdf.Parser.Core.Listener
    open Atrous.Pdf.Parser.Operators
    open iText.Kernel.Pdf

    let private extract<'T when 'T:> AbstractRenderInfo> et (pageNum: int) filter (parser: PdfDocumentContentParser) =
        let listener = new DataEventListener<'T>(et,filter)
        parser.ProcessContent(pageNum,listener).Datas |> Seq.cast<'T>
        
    let paths (pageNum: int) filter (parser: PdfDocumentContentParser) =
        extract<PathRenderInfo> [EventType.RENDER_PATH] pageNum filter parser
            
    let texts (pageNum: int) filter (parser: PdfDocumentContentParser) =
        extract<TextRenderInfo> [EventType.RENDER_TEXT] pageNum filter parser

    let all (pageNum: int) filter (parser: PdfDocumentContentParser) =
        extract<AbstractRenderInfo> [EventType.RENDER_TEXT;EventType.RENDER_PATH] pageNum filter parser

    let allWithDetails (page: PdfPage) filter =
        read page filter

