namespace Shrimp.Pdf.Parser

open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Listener
open System.Collections.Generic
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Extensions
open Listeners
open System.IO



type OperatorRange =
    { Operator: PdfLiteral 
      Operands: seq<PdfObject> }

[<RequireQualifiedAccess>]
module PdfCanvas =
    let writeOperatorRange (operatorRange: OperatorRange) (pdfCanvas: PdfCanvas) =
        let outputStream = pdfCanvas.GetContentStream().GetOutputStream()
        for operand in operatorRange.Operands do
            outputStream.Write(operand).WriteSpace()
            |> ignore

        outputStream.Write(operatorRange.Operator).WriteNewLine() |> ignore




type internal CallbackableContentOperator (originalOperator) =
    member this.OriginalOperator: IContentOperator = originalOperator

    interface IContentOperator with 
        member this.Invoke(processor,operator,operands) =
            let processor = processor :?> OperatorRangeCallbackablePdfCanvasProcessor
            let operatorName = operator.ToString()
            if operatorName <> "Do" then 
                try 
                    this.OriginalOperator.Invoke(processor, operator, operands)
                with ex ->
                    if ex.Message = "Dictionary doesn't have supported font data." 
                    then
                        printfn "Skip checking MM font %A" operator
                        let size = (operands.[1]) :?> PdfNumber
                        let size = size.FloatValue()
                        processor.GetGraphicsState().SetFontSize(size)

                    else failwithf "%A" ex

            processor.InvokeOperatorRange({ Operator = operator; Operands = operands})

and internal OperatorRangeCallbackablePdfCanvasProcessor(listener) =
    inherit PdfCanvasProcessor(listener)
    abstract member InvokeOperatorRange: OperatorRange -> unit

    default this.InvokeOperatorRange(operatorRange) = ()

    override this.RegisterContentOperator(operatorString: string , operator: IContentOperator) : IContentOperator =
        let wrapper = new CallbackableContentOperator(operator)
        let formOperator = base.RegisterContentOperator(operatorString, wrapper)
        match formOperator with 
        | :? CallbackableContentOperator as wrapper -> wrapper.OriginalOperator
        | _ -> formOperator


[<RequireQualifiedAccess>]
type Selector = 
    | Path of (PathRenderInfo -> bool)
    | Text of (TextRenderInfo -> bool)
    | AND of Selector list
    | OR of Selector list


[<RequireQualifiedAccess>]
module Selector =
    let toEventTypes selector =
        let rec loop selector =
            match selector with 
            | Selector.Path _ -> [EventType.RENDER_PATH]
            | Selector.Text _ -> [EventType.RENDER_TEXT]
            | Selector.AND selectors ->
                selectors
                |> List.collect(loop)

            | Selector.OR selectors ->
                selectors
                |> List.collect(loop)

        loop selector
        |> List.distinct

    let toRenderInfoPredication (selector) =
        let rec loop selector =
            match selector with 
            | Selector.Path prediate -> 
                fun (renderInfo: AbstractRenderInfo) ->
                    match renderInfo with 
                    | :? PathRenderInfo as pathRenderInfo ->
                        prediate pathRenderInfo
                    | _ -> false

            | Selector.Text prediate -> 
                fun (renderInfo: AbstractRenderInfo) ->
                    match renderInfo with 
                    | :? TextRenderInfo as textRenderInfo ->
                        prediate textRenderInfo
                    | _ -> false

            | Selector.AND selectors ->
                fun (renderInfo: AbstractRenderInfo) ->
                    selectors |> List.forall (fun selector -> loop selector renderInfo)
                

            | Selector.OR selectors ->
                fun (renderInfo: AbstractRenderInfo) ->
                    selectors |> List.exists (fun selector -> loop selector renderInfo)

        loop selector




type FixArgs =
    { RenderInfo: AbstractRenderInfo
      OperatorRange: OperatorRange
      Canvas: PdfCanvas }


[<RequireQualifiedAccess>]
type Modifier =
    | DropColor
    | Fix of (FixArgs -> unit)



type internal PdfCanvasEditor(selector: Selector, modifier: Modifier) =
    inherit OperatorRangeCallbackablePdfCanvasProcessor(FilteredEventListenerEx(Selector.toEventTypes selector, Selector.toRenderInfoPredication selector))
    
    let eventTypes = Selector.toEventTypes selector
    
    let mutable currentPdfCanvas = null
    let mutable eventListener: FilteredEventListenerEx<AbstractRenderInfo> = null

    override this.InvokeOperatorRange (operatorRange: OperatorRange) =
        match operatorRange.Operator.ToString() with 
        | "f" | "F" | "f*" | "S" | "s" | "B" | "B*" | "b" | "b*" 
            when List.contains EventType.RENDER_PATH eventTypes ->
                match modifier with
                | Modifier.DropColor ->
                    let newOperatorRange = 
                        { operatorRange with 
                            Operator = new PdfLiteral("n") }

                    PdfCanvas.writeOperatorRange newOperatorRange currentPdfCanvas

                | Modifier.Fix (fix) ->
                    let pathRenderInfo = eventListener.CurrentRenderInfo

                    let args =
                        { RenderInfo = pathRenderInfo
                          OperatorRange = operatorRange 
                          Canvas = currentPdfCanvas }
                    fix args

        | "Tj" | "TJ" when List.contains EventType.RENDER_TEXT eventTypes ->
                match modifier with
                | Modifier.DropColor ->
                    let newOperatorRange = 
                        { operatorRange with 
                            Operator = new PdfLiteral("n") }

                    PdfCanvas.writeOperatorRange newOperatorRange currentPdfCanvas

                | Modifier.Fix (fix) ->
                    let pathRenderInfo = eventListener.CurrentRenderInfo

                    let args =
                        { RenderInfo = pathRenderInfo
                          OperatorRange = operatorRange 
                          Canvas = currentPdfCanvas }
                    fix args
        | _ -> 
            PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas


    member this.EditContent(pdfCanvas: PdfCanvas, contentBytes: byte [], resources: PdfResources) =
        eventListener <- this.GetEventListener() :?> FilteredEventListenerEx<AbstractRenderInfo>  
        currentPdfCanvas <- pdfCanvas

        base.ProcessContent(contentBytes, resources)

        currentPdfCanvas.GetContentStream()



[<RequireQualifiedAccess>]
module PdfPage =
    let modify (selector: Selector) (modifier: Modifier) (page: PdfPage) =
        let editor = new PdfCanvasEditor(selector, modifier)

        let rec loop (resources: PdfResources) (pdfObject: PdfObject) =
            match pdfObject with 
            | :? PdfStream as stream -> 
                let pdfCanvas = new PdfCanvas(new PdfStream(), resources, page.GetDocument())
                let bytes = stream.GetBytes()
                let fixedStream = editor.EditContent(pdfCanvas, bytes, resources)
                fixedStream

            | :? PdfArray as array ->
                if array |> Seq.forall (fun o -> o :? PdfStream) && Seq.length array > 1 then 
                    let s = new PdfStream()
                    array |> Seq.cast<PdfStream> |> Seq.iter (fun s1 ->
                        s.GetOutputStream().WriteBytes(s1.GetBytes()) |> ignore
                    )
                    loop resources s
                else
                    failwith "No implemented"

            | :? PdfDictionary as map ->
                failwith "No implemented"
            | _ -> failwith "No implemented"

        let pageContents = page.GetPdfObject().Get(PdfName.Contents)

        match pageContents with 
        | null -> ()
        | _ ->
            let resources = page.GetResources()
            let fixedStream = loop resources pageContents
            page.Put(PdfName.Contents, fixedStream)
            |> ignore
