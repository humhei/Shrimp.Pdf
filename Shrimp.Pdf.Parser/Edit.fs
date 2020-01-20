﻿namespace Shrimp.Pdf.Parser

open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf.Extensions
open System.Collections.Generic
open FParsec.CharParsers
open FParsec
open iText.Kernel.Pdf.Canvas.Parser
open iText.Layout
open Shrimp.Pdf
open Listeners
open System.Threading
open iText.IO.Source
open System.IO
open Shrimp.Pdf.Parser.Helper


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



[<RequireQualifiedAccess>]
type SinglePageSelectorExpr =
    | Begin of int
    | End of int

[<RequireQualifiedAccess>]
type PageSelectorExpr = 
    | SinglePage of SinglePageSelectorExpr
    | Between of SinglePageSelectorExpr * SinglePageSelectorExpr
    | Compose of PageSelectorExpr list

[<RequireQualifiedAccess>]
module PageSelectorExpr = 
    let private parser = 

        let pSinglePage = 
            let pBegin = 
                pint32 |>> (fun i -> 
                    if i > 0 then  SinglePageSelectorExpr.Begin i
                    else failwithf "page num %d should be bigger than 0" i
                )

            let pEnd = 
                (pstringCI "R") >>. pint32 |>> (fun i ->
                    if i > 0 then  SinglePageSelectorExpr.End i
                    else failwithf "page num %d should be bigger than 0" i
                )

            (pEnd)
            <|> (pBegin)

        let pBetween = 
            (pSinglePage .>>? pchar '-' .>>. pSinglePage )
            |>> PageSelectorExpr.Between

        sepBy1 ((pBetween <|> (pSinglePage |>> PageSelectorExpr.SinglePage)) .>> spaces) (pchar ',')

    let create (exprText: string) =
        match run parser exprText with 
        | Success (result, _, _) -> 
            match result with 
            | [expr] -> expr
            | _ -> PageSelectorExpr.Compose result

        | Failure (errorMsg, _, _) -> failwithf "%s" errorMsg

[<RequireQualifiedAccess>]
type PageSelector =
    | Last
    | First
    | All
    | Expr of PageSelectorExpr
    | Numbers of Set<int>



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

                    else raise ex

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

    override this.ProcessPageContent(page) =
        this.ProcessContent(page.GetContentBytes(), page.GetResources());


[<AutoOpen>]
module Extensions =

    type PdfDocument with

        member pdfDocument.GetPageNumber(pageSelectorExpr: SinglePageSelectorExpr) =
            let totalPageNum = pdfDocument.GetNumberOfPages()
            
            match pageSelectorExpr with
            | SinglePageSelectorExpr.Begin (i) -> i
            | SinglePageSelectorExpr.End (i) -> totalPageNum - i + 1


        member pdfDocument.GetPageNumbers(pageSelectorExpr: PageSelectorExpr) =

            match pageSelectorExpr with 
            | PageSelectorExpr.SinglePage singlePage -> [pdfDocument.GetPageNumber singlePage]

            | PageSelectorExpr.Between (beginExpr, endExpr) ->
                [pdfDocument.GetPageNumber beginExpr .. pdfDocument.GetPageNumber endExpr]

            | PageSelectorExpr.Compose compose ->
                compose
                |> List.collect (pdfDocument.GetPageNumbers)
                |> List.distinct

        member pdfDocument.GetPageNumbers(pageSelector) =
            let numberOfPages = pdfDocument.GetNumberOfPages()
            match pageSelector with 
            | PageSelector.First -> [1]
            | PageSelector.Last -> [numberOfPages]
            | PageSelector.Expr expr -> 
                pdfDocument.GetPageNumbers(expr)
            | PageSelector.All -> [1..numberOfPages]
            | PageSelector.Numbers numbers -> 
                let intersectedNumbers =
                    Set.intersect
                        numbers
                        (Set.ofList [1..numberOfPages])
                    |> Set.toList

                intersectedNumbers




[<Struct>]
type _SelectionModifierFixmentArguments =
    { Close: OperatorRange
      CurrentRenderInfo: IIntegratedRenderInfo }

type private Modifier = _SelectionModifierFixmentArguments -> list<PdfCanvas -> PdfCanvas>



and internal PdfCanvasEditor(selectorModifierMapping: Map<SelectorModiferToken, RenderInfoSelector * Modifier>, document: PdfDocument) =
    inherit OperatorRangeCallbackablePdfCanvasProcessor(FilteredEventListenerEx(Map.map (fun _ -> fst) selectorModifierMapping))
    
    let eventTypes = 
        selectorModifierMapping
        |> Map.map (fun _ -> fst)
        |> Map.toList
        |> List.map snd
        |> RenderInfoSelector.OR
        |> RenderInfoSelector.toEventTypes
    
    let mutable eventListener: FilteredEventListenerEx = null
    let pdfCanvasStack = new Stack<CanvasGraphicsStateSettablePdfCanvas>()
    let resourcesStack = new Stack<PdfResources>()

    let (|Path|_|) operatorName =
        match operatorName with 
        | "f" | "F" | "f*" | "S" | "s" | "B" | "B*" | "b" | "b*" 
            when List.contains EventType.RENDER_PATH eventTypes-> Some ()
        | _ -> None

    let (|Text|_|) operatorName =
        match operatorName with 
        | "Tj" | "TJ" when List.contains EventType.RENDER_TEXT eventTypes -> Some ()
        | _ -> None

    let (|PathOrText|_|) operatorName =
        match operatorName with 
        | Path _ -> Some ()
        | Text _ -> Some ()
        | _ -> None

    let (|Form|Image|Others|) pdfName = 
        if pdfName = PdfName.Form 
        then Form
        elif pdfName = PdfName.Image
        then Image
        else Others

    override this.InvokeOperatorRange (operatorRange: OperatorRange) =
        
        let currentPdfCanvas = pdfCanvasStack.Peek()

        let operatorName = operatorRange.Operator.ToString()


        match operatorName with 
        | "Do" ->

            let resources = resourcesStack.Peek()
            let name = operatorRange.Operands.[0] :?> PdfName

            let container = resources.GetResource(PdfName.XObject)
            let xobjectStream = container.Get(name) :?> PdfStream
            let subType = xobjectStream.GetAsName(PdfName.Subtype)
            match subType with 
            | Form ->
                let xobjectResources = 
                    let subResources = xobjectStream.GetAsDictionary(PdfName.Resources)
                    match subResources with 
                    | null -> resources
                    | _ -> PdfResources subResources

                let fixedStream: PdfStream = this.EditContent(xobjectResources, xobjectStream)
                container.Put(name, fixedStream) |> ignore

            | Image -> ()
            | Others -> ()

            PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
            |> ignore


        | PathOrText ->

            match eventListener.CurrentRenderInfoStatus with 
            | CurrentRenderInfoStatus.Skiped -> 
                PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                |> ignore

            | CurrentRenderInfoStatus.Selected ->
                currentPdfCanvas.SetCanvasGraphicsState(this.GetGraphicsState())

                let fix = snd selectorModifierMapping.[eventListener.CurrentRenderInfoToken.Value]

                match operatorName with 
                | PathOrText ->
                    PdfCanvas.useCanvas (currentPdfCanvas :> PdfCanvas) (fun canvas ->
                        let pdfCanvasActions = fix { Close = operatorRange; CurrentRenderInfo = eventListener.CurrentRenderInfo }
                        (canvas, pdfCanvasActions)
                        ||> List.fold(fun canvas action ->
                            action canvas 
                        )
                    )
                    
                | _ -> 
                    PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                    |> ignore
            | _ -> failwith "Invalid token"
        | _ ->
            PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
            |> ignore


    member this.EditContent (resources: PdfResources, pdfObject: PdfObject) =
        match eventListener with 
        | null -> eventListener <- this.GetEventListener() :?> FilteredEventListenerEx
        | _ -> ()

        match pdfObject with 
        | :? PdfStream as stream -> 
            let pdfCanvas = new CanvasGraphicsStateSettablePdfCanvas(new PdfStream(), resources, document)

            let bytes = stream.GetBytes()
            pdfCanvasStack.Push(pdfCanvas)
            resourcesStack.Push(resources)

            base.ProcessContent(bytes, resources)

            let pdfCanvas = pdfCanvasStack.Pop()
            resourcesStack.Pop()|> ignore
            let clonedStream = stream.Clone() :?> PdfStream
            clonedStream.SetData(pdfCanvas.GetContentStream().GetBytes()) |> ignore
            clonedStream

        | :? PdfArray as array ->
            if array |> Seq.forall (fun o -> o :? PdfStream) && Seq.length array > 1 then 
                let stream = new PdfStream()
                array |> Seq.cast<PdfStream> |> Seq.iter (fun s1 ->
                    stream.GetOutputStream().WriteBytes(s1.GetBytes()) |> ignore
                )
                this.EditContent (resources, stream)
            else
                failwith "Not implemented"

        | :? PdfDictionary as map ->
            failwith "Not implemented"
        | _ -> failwith "Not implemented"



[<RequireQualifiedAccess>]
module PdfPage =
    let modify (selectorModifierMapping) (page: PdfPage) =
        let document = page.GetDocument()
        let editor = new PdfCanvasEditor(selectorModifierMapping, document)
        let pageContents = page.GetPdfObject().Get(PdfName.Contents)

        match pageContents with 
        | null -> ()
        | _ ->
            let resources = page.GetResources()
            let fixedStream = editor.EditContent(resources, pageContents)
            page.Put(PdfName.Contents, fixedStream)
            |> ignore





