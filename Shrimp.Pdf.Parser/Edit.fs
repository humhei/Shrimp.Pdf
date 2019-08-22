﻿namespace Shrimp.Pdf.Parser

open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf.Extensions
open System.Collections.Generic
open FParsec.CharParsers
open FParsec
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Extensions
open iText.Layout
open Shrimp.Pdf
open Listeners
open System.Diagnostics

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
            | PageSelector.Expr expr -> 
                pdfDocument.GetPageNumbers(expr)
            | PageSelector.All -> [1..numberOfPages]




[<Struct>]
type _SelectionModifierAddNewArguments =
    { CurrentRenderInfo: IIntegratedRenderInfo }

[<Struct>]
type _SelectionModifierFixmentArguments =
    { Close: OperatorRange }


[<RequireQualifiedAccess>]
type SelectionModifier =
    | DropColor
    | AddNew of (_SelectionModifierAddNewArguments -> list<PdfCanvas -> PdfCanvas>)
    | Fix of (_SelectionModifierFixmentArguments -> list<PdfCanvas -> PdfCanvas>)


and internal PdfCanvasEditor(selector: RenderInfoSelector, modifier: SelectionModifier, document: PdfDocument) =
    inherit OperatorRangeCallbackablePdfCanvasProcessor(FilteredEventListenerEx(RenderInfoSelector.toEventTypes selector, RenderInfoSelector.toRenderInfoPredication selector))
    
    let eventTypes = RenderInfoSelector.toEventTypes selector
    
    let mutable eventListener: FilteredEventListenerEx = null
    let pdfCanvasStack = new Stack<PdfCanvas>()
    let resourcesStack = new Stack<PdfResources>()



    override this.InvokeOperatorRange (operatorRange: OperatorRange) =
        let currentPdfCanvas = pdfCanvasStack.Peek()

        let operatorName = operatorRange.Operator.ToString()
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

        match operatorName with 
        | "Do" ->
            let (|Form|Image|Others|) pdfName = 
                if pdfName = PdfName.Form 
                then Form
                elif pdfName = PdfName.Image
                then Image
                else Others

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
                match operatorName with 
                | Path ->
                    match modifier with
                    | SelectionModifier.DropColor ->
                        let newOperatorRange = 
                            { operatorRange with 
                                Operator = new PdfLiteral("n")
                                Operands = ResizeArray [new PdfLiteral("n") :> PdfObject]}

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
                        | SelectionModifier.Fix (fix) ->
                            PdfCanvas.useCanvas currentPdfCanvas (fun canvas ->
                                let pdfCanvasActions = fix { Close = operatorRange }
                                (canvas, pdfCanvasActions)
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
            let pdfCanvas = new PdfCanvas(new PdfStream(), resources, document)

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
module internal PdfPage =
    let modify (renderInfoSelector: RenderInfoSelector) (modifier: SelectionModifier) (page: PdfPage) =
        let document = page.GetDocument()
        let editor = new PdfCanvasEditor(renderInfoSelector, modifier, document)
        let pageContents = page.GetPdfObject().Get(PdfName.Contents)

        match pageContents with 
        | null -> ()
        | _ ->
            let resources = page.GetResources()
            let fixedStream = editor.EditContent(resources, pageContents)
            page.Put(PdfName.Contents, fixedStream)
            |> ignore


type IntegratedDocument private (reader: string, writer: string) =

    let pdfDocument = new PdfDocumentWithCachedResources(reader, writer)

    member x.ReaderName = reader

    member x.Value = pdfDocument

    static member Create(reader, writer) = new IntegratedDocument(reader, writer)


[<RequireQualifiedAccess>]
module IntegratedDocument =
    let modify name (pageSelector: PageSelector) (renderInfoSelectorFactory: (PageNumber * PdfPage) -> RenderInfoSelector) (selectionModifierFactory: (PageNumber * PdfPage) -> SelectionModifier) (document: IntegratedDocument) =
        let selectedPageNumbers = document.Value.GetPageNumbers(pageSelector) 
        let totalNumberOfPages = document.Value.GetNumberOfPages()
        Logger.info (sprintf "MODIFY: %s" name) 

        for i = 1 to totalNumberOfPages do
            if List.contains i selectedPageNumbers then
                let page = document.Value.GetPage(i)
                let renderInfoSelector = renderInfoSelectorFactory (PageNumber i, page)
                let selectionModifier = selectionModifierFactory (PageNumber i, page)
                PdfPage.modify renderInfoSelector selectionModifier page


