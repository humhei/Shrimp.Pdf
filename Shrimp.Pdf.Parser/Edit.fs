namespace Shrimp.Pdf.Parser

open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Extensions
open Listeners
open System.Collections.Generic
open FParsec.CharParsers
open FParsec
open iText.Kernel.Colors
open iText.Layout
open Shrimp.Pdf


type OperatorRange =
    { Operator: PdfLiteral 
      Operands: IList<PdfObject> }

[<RequireQualifiedAccess>]
module PdfCanvas =
    let writeOperatorRange (operatorRange: OperatorRange) (pdfCanvas: PdfCanvas) =
        let outputStream = pdfCanvas.GetContentStream().GetOutputStream()
        let operands = operatorRange.Operands
        for i = 0 to operands.Count - 1 do
            let operand = operands.[i]

            if i = operands.Count - 1 then 
                outputStream.Write(operand).WriteNewLine()
                |> ignore
            else 
                outputStream.Write(operand).WriteSpace()
                |> ignore

        pdfCanvas

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
type RenderInfoSelector = 
    | Path of (PathRenderInfo -> bool)
    | Text of (TextRenderInfo -> bool)
    | AND of RenderInfoSelector list
    | OR of RenderInfoSelector list


[<RequireQualifiedAccess>]
module Selector =
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


[<Struct>]
type _SelectionModifierAddNewArguments =
    { CurrentRenderInfo: AbstractRenderInfo }

[<RequireQualifiedAccess>]
type SelectionModifier =
    | DropColor
    | AddNew of (_SelectionModifierAddNewArguments -> list<PdfCanvas -> PdfCanvas>)
    | Modify of (list<PdfCanvas -> PdfCanvas>)


type internal PdfCanvasEditor(selector: RenderInfoSelector, modifier: SelectionModifier) =
    inherit OperatorRangeCallbackablePdfCanvasProcessor(FilteredEventListenerEx(Selector.toEventTypes selector, Selector.toRenderInfoPredication selector))
    
    let eventTypes = Selector.toEventTypes selector
    
    let mutable currentPdfCanvas = null
    let mutable eventListener: FilteredEventListenerEx<AbstractRenderInfo> = null

    override this.InvokeOperatorRange (operatorRange: OperatorRange) =
        let operatorName = operatorRange.Operator.ToString()
        let (|Path|_|) operatorName =
            match operatorName with 
            | "f" | "F" | "f*" | "S" | "s" | "B" | "B*" | "b" | "b*" 
                when List.contains EventType.RENDER_PATH eventTypes-> Some ()
            | _ -> None

        let (|Text|_|) operatorName =
            match operatorName with 
            | "Tj" | "TJ" when List.contains EventType.RENDER_TEXT eventTypes ->
                Some ()
            | _ -> None

        let (|PathOrText|_|) operatorName =
            match operatorName with 
            | Path _ -> Some ()
            | Text _ -> Some ()
            | _ -> None

        match operatorName with 
        | Path ->
            match modifier with
            | SelectionModifier.DropColor ->
                let newOperatorRange = 
                    { operatorRange with 
                        Operator = new PdfLiteral("n") }

                PdfCanvas.writeOperatorRange newOperatorRange currentPdfCanvas
                |> ignore
            | _ -> ()

        | Text ->
            match modifier with
            | SelectionModifier.DropColor ->
                PdfCanvas.useCanvas currentPdfCanvas (fun pdfCanvas ->
                    pdfCanvas.SetTextRenderingMode(TextRenderingMode.INVISIBLE) |> ignore
                    PdfCanvas.writeOperatorRange operatorRange pdfCanvas
                )
                |> ignore
            | _ -> ()

        | _ -> ()


        match operatorName with 
        | PathOrText ->
            match modifier with
                | SelectionModifier.Modify (pdfCanvasActions) ->
                    PdfCanvas.useCanvas currentPdfCanvas (fun canvas ->
                        (canvas, (pdfCanvasActions @ [PdfCanvas.writeOperatorRange operatorRange]))
                        ||> List.fold(fun canvas action ->
                            action canvas 
                        )
                    )
                | SelectionModifier.AddNew addNew -> 
                    PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas |> ignore
                    PdfCanvas.useCanvas currentPdfCanvas (fun canvas ->
                        let pdfCanvasActions = addNew { CurrentRenderInfo = eventListener.CurrentRenderInfo }
                        (canvas, (pdfCanvasActions))
                        ||> List.fold(fun canvas action ->
                            action canvas 
                        )
                    )

                    ()
                | _ -> ()
        | _ -> 
            PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
            |> ignore

    member this.EditContent(pdfCanvas: PdfCanvas, contentBytes: byte [], resources: PdfResources) =
        eventListener <- this.GetEventListener() :?> FilteredEventListenerEx<AbstractRenderInfo>  
        currentPdfCanvas <- pdfCanvas

        base.ProcessContent(contentBytes, resources)

        currentPdfCanvas.GetContentStream()


type Begin = Begin of int
type End = End of int

[<RequireQualifiedAccess>]
type PageSelectorExprSinglePage =
    | Begin of Begin
    | End of End

[<RequireQualifiedAccess>]
type PageSelectorExpr = 
    | SinglePage of PageSelectorExprSinglePage
    | Between of Begin * End
    | Compose of PageSelectorExpr list

[<RequireQualifiedAccess>]
module PageSelectorExpr = 
    let private parser = 
        let pBegin = 
            pint32 |>> Begin

        let pEnd = 
            (pstringCI "R") >>. pint32 |>> End

        let pBetween = 
            (pBegin .>> pchar '-' .>>. pEnd)
            |>> PageSelectorExpr.Between

        let pSinglePage = 
            (pBegin |>> (PageSelectorExprSinglePage.Begin >> PageSelectorExpr.SinglePage))
            <|> 
                (pEnd |>> (PageSelectorExprSinglePage.End >> PageSelectorExpr.SinglePage))

        sepBy1 (pBetween <|> pSinglePage) (pchar ',')

    let create (exprText: string) =
        match run parser exprText with 
        | Success (result, _, _) -> result
        | Failure (errorMsg, _, _) -> failwithf "%s" errorMsg

[<RequireQualifiedAccess>]
type PageSelector =
    | Last
    | First
    | All
    | Expr of PageSelectorExpr


[<AutoOpen>]
module Extensions =

    type PdfDocument with
        member pdfDocument.GetPageNumbers(pageSelectorExpr: PageSelectorExpr) =
            match pageSelectorExpr with 
            | PageSelectorExpr.SinglePage singlePage ->
                match singlePage with 
                | PageSelectorExprSinglePage.Begin (Begin i) -> [i]
                | PageSelectorExprSinglePage.End (End i) -> [i]

            | PageSelectorExpr.Between (Begin m, End n) ->
                [m..n]

            | PageSelectorExpr.Compose compose ->
                compose
                |> List.collect (pdfDocument.GetPageNumbers)
                |> List.distinct

        member pdfDocument.GetPageNumbers(pageSelector) =
            let numberOfPages = pdfDocument.GetNumberOfPages()
            match pageSelector with 
            | PageSelector.First -> [1]
            | PageSelector.Last -> [numberOfPages]
            | PageSelector.Expr expr -> pdfDocument.GetPageNumbers(expr)
            | PageSelector.All -> [1..numberOfPages]

[<RequireQualifiedAccess>]
module internal PdfPage =
    let modify (renderInfoSelector: RenderInfoSelector) (modifier: SelectionModifier) (page: PdfPage) =
        let editor = new PdfCanvasEditor(renderInfoSelector, modifier)

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


type IntegralDocument private (reader: string, writer: string) =
    let reader = new PdfReader(reader)

    let writer = new PdfWriter(writer)

    let pdfDocument = new PdfDocumentWithCachedResources(reader, writer)

    member x.Value = pdfDocument

    static member Create(reader, writer) = new IntegralDocument(reader, writer)


[<RequireQualifiedAccess>]
module IntegralDocument =
    let modify (pageSelector: PageSelector) (renderInfoSelector: RenderInfoSelector) (selectionModifier: SelectionModifier) (document: IntegralDocument) =
        let selectedPageNumbers = document.Value.GetPageNumbers(pageSelector) 
        for i = 1 to document.Value.GetNumberOfPages() do
            if List.contains i selectedPageNumbers then
                let page = document.Value.GetPage(i)
                PdfPage.modify renderInfoSelector selectionModifier page



    let addNew (pageSelector: PageSelector) (pageBoxKind: PageBoxKind) (actions: list<Canvas -> Canvas>) (document: IntegralDocument) =

        let selectedPageNumbers = document.Value.GetPageNumbers(pageSelector) 
        for i = 1 to document.Value.GetNumberOfPages() do
            if List.contains i selectedPageNumbers then
                let page = document.Value.GetPage(i)

                let canvas = new Canvas(page, page.GetPageBox(pageBoxKind))
                (canvas, actions)
                ||> List.fold(fun pdfCanvas action ->
                    action pdfCanvas
                ) 
                |> ignore