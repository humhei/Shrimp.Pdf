namespace Shrimp.Pdf.Parser
#nowarn "0104"
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
open Shrimp.FSharp.Plus
open Shrimp.Pdf.Parser.Helper
open Constants.Operators

[<Struct>]
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



type CloseOperator= 
    | Open  = 0
    | Close = 1
    | Keep  = 2

[<RequireQualifiedAccess>]
module CloseOperator=
    let apply left right =
        match left, right with 
        | CloseOperator.Keep, _ -> right
        | _ ,CloseOperator.Keep -> left
        | _ -> right

        

type PathCloseOperator =
    { Fill: CloseOperator
      Stroke: CloseOperator}
with 
    static member Keep =
        { Fill = CloseOperator.Keep 
          Stroke = CloseOperator.Keep }

    member x.Apply(originOperator: string) = 
        let operator = originOperator.ToString()
        match x.Fill, x.Stroke with 
        | CloseOperator.Keep, CloseOperator.Keep -> operator
        | CloseOperator.Keep, CloseOperator.Close -> 
            match operator with 
            | EQ b -> f
            | EQ B -> F
            | EQ ``b*`` -> ``f*``
            | EQ ``B*`` -> F
            | operatorName -> 
                match operatorName with 
                | ContainsBy [f; F; ``f*``]  -> operatorName
                | ContainsBy [s; S] -> n
                | _ -> failwithf "Unexcepted operator %s" operatorName

        | CloseOperator.Keep, CloseOperator.Open -> 
            match operator with 
            | EQ f -> b
            | EQ F -> B
            | EQ ``f*`` -> ``b*``
            | operatorName -> 
                match operatorName with 
                | ContainsBy [b; B; ``B*``; ``b*``] 
                | ContainsBy [s; S] -> operatorName
                | _ -> failwithf "Unexcepted operator %s" operatorName


        | CloseOperator.Close, CloseOperator.Keep ->
            match operator with 
            | EQ b -> s
            | EQ B -> S
            | EQ ``b*`` -> s
            | EQ ``B*`` -> S
            | operatorName -> 
                match operatorName with 
                | ContainsBy [s; S] -> operatorName
                | ContainsBy [f; F; ``f*``] -> n
                | _ -> failwithf "Unexcepted operator %s" operatorName

        | CloseOperator.Open, CloseOperator.Keep ->
            match operator with 
            | EQ s -> b
            | EQ S -> B
            | operatorName -> 
                match operatorName with 
                | ContainsBy [b; B; ``B*``;``b*``] 
                | ContainsBy [f; F; ``f*``] -> operatorName
                | _ -> failwithf "Unexcepted operator %s" operatorName

        | CloseOperator.Open, CloseOperator.Open -> 
            match operator with 
            | ContainsBy [b; ``b*``; B; ``B*``] -> operator
            | EqualTo f -> b
            | EqualTo ``f*`` -> ``b*``
            | EqualTo F -> B
            | EqualTo S -> B
            | ContainsBy [s; n] -> b
            | _ -> failwithf "Unexcepted operator %s" operator

        | CloseOperator.Close, CloseOperator.Close -> n
            
            
type TextCloseOperator =
    { Fill: CloseOperator
      Stroke: CloseOperator
      Text: string option }
with 
    static member Keep =
        { Fill = CloseOperator.Keep 
          Stroke = CloseOperator.Keep
          Text = None }

    member x.Apply(textRenderingMode: int) =
        match textRenderingMode with 
        | TextRenderingMode.FILL_STROKE
        | TextRenderingMode.FILL
        | TextRenderingMode.STROKE
        | TextRenderingMode.INVISIBLE ->

            match x.Fill, x.Stroke with 
            | CloseOperator.Keep, CloseOperator.Keep -> textRenderingMode
            | CloseOperator.Keep, CloseOperator.Close -> 
                match textRenderingMode with 
                | PdfCanvasConstants.TextRenderingMode.FILL_STROKE ->TextRenderingMode.FILL
                | PdfCanvasConstants.TextRenderingMode.FILL -> TextRenderingMode.FILL
                | PdfCanvasConstants.TextRenderingMode.STROKE -> TextRenderingMode.INVISIBLE
                | PdfCanvasConstants.TextRenderingMode.INVISIBLE -> TextRenderingMode.INVISIBLE
                | _ -> failwith "Invalid token"

            | CloseOperator.Keep, CloseOperator.Open -> 
                match textRenderingMode with 
                | PdfCanvasConstants.TextRenderingMode.FILL_STROKE -> TextRenderingMode.FILL_STROKE
                | PdfCanvasConstants.TextRenderingMode.FILL -> TextRenderingMode.FILL_STROKE
                | PdfCanvasConstants.TextRenderingMode.STROKE -> TextRenderingMode.STROKE
                | PdfCanvasConstants.TextRenderingMode.INVISIBLE -> TextRenderingMode.STROKE
                | _ -> failwith "Invalid token"


            | CloseOperator.Close, CloseOperator.Keep ->
                match textRenderingMode with
                | TextRenderingMode.FILL_STROKE -> TextRenderingMode.STROKE
                | TextRenderingMode.STROKE -> TextRenderingMode.STROKE
                | TextRenderingMode.FILL -> TextRenderingMode.INVISIBLE
                | TextRenderingMode.INVISIBLE -> TextRenderingMode.INVISIBLE
                | _ -> failwith "Invalid token"

            | CloseOperator.Open, CloseOperator.Keep ->
                match textRenderingMode with 
                | PdfCanvasConstants.TextRenderingMode.FILL_STROKE -> TextRenderingMode.FILL_STROKE
                | PdfCanvasConstants.TextRenderingMode.FILL -> TextRenderingMode.FILL
                | PdfCanvasConstants.TextRenderingMode.STROKE -> TextRenderingMode.FILL_STROKE
                | PdfCanvasConstants.TextRenderingMode.INVISIBLE -> TextRenderingMode.FILL
                | _ -> failwith "Invalid token"

            | CloseOperator.Open, CloseOperator.Open -> TextRenderingMode.FILL_STROKE
                

            | CloseOperator.Close, CloseOperator.Close -> TextRenderingMode.INVISIBLE
                
        | _ -> 
            Logger.unSupportedTextRenderMode textRenderingMode
            textRenderingMode
            


[<RequireQualifiedAccess>]
type CloseOperatorUnion =
    | Text of TextCloseOperator
    | Path of PathCloseOperator
with 
    static member Keep(tag) =
        match tag with 
        | IntegratedRenderInfoTag.Path -> CloseOperatorUnion.Path(PathCloseOperator.Keep)
        | IntegratedRenderInfoTag.Text -> CloseOperatorUnion.Text(TextCloseOperator.Keep)

    static member CreatePath(?fill, ?stroke) =
        { Fill = defaultArg fill CloseOperator.Keep 
          Stroke = defaultArg stroke CloseOperator.Keep  }
        |> CloseOperatorUnion.Path


    static member CreateText(?fill, ?stroke, ?text) =
        { Fill = defaultArg fill CloseOperator.Keep 
          Stroke = defaultArg stroke CloseOperator.Keep 
          Text = text}
        |> CloseOperatorUnion.Text

    static member Create(tag, ?fill, ?stroke) =
        match tag with 
        | IntegratedRenderInfoTag.Path -> 
            CloseOperatorUnion.CreatePath(?fill = fill, ?stroke = stroke)
        | IntegratedRenderInfoTag.Text ->
            CloseOperatorUnion.CreateText(?fill = fill, ?stroke = stroke)

[<RequireQualifiedAccess>]
module private CloseOperatorUnion =
    let concatPath (operators: PathCloseOperator al1List) =
        operators.AsList
        |> List.reduce(fun left right ->
            { Fill = CloseOperator.apply left.Fill right.Fill
              Stroke = CloseOperator.apply left.Stroke right.Stroke }
        )

    let concatText (operators: TextCloseOperator al1List) =
        operators.AsList
        |> List.reduce(fun left right ->
            { Fill = CloseOperator.apply left.Fill right.Fill
              Stroke = CloseOperator.apply left.Stroke right.Stroke
              Text = 
                match right.Text, left.Text with 
                | Some text, _ -> Some text
                | None, leftText -> leftText
              }
        )


    let concat (tag: IntegratedRenderInfoTag) (operators: CloseOperatorUnion al1List) =
        
        match tag with 
        | IntegratedRenderInfoTag.Path ->
            let operator = 
                operators
                |> AtLeastOneList.map(fun m ->
                    match m with 
                    | CloseOperatorUnion.Path path -> path
                    | CloseOperatorUnion.Text _ -> failwith "Current close operator %A should be path Close Operator"
                )
                |> concatPath


            CloseOperatorUnion.Path operator


        | IntegratedRenderInfoTag.Text ->
            let operator = 
                operators
                |> AtLeastOneList.map(fun m ->
                    match m with 
                    | CloseOperatorUnion.Text text -> text 
                    | CloseOperatorUnion.Path _ -> failwith "Current close operator %A should be text Close Operator"
                )
                |> concatText

            CloseOperatorUnion.Text operator


type ModifierPdfCanvasActions =
    { Actions: list<PdfCanvas -> PdfCanvas>
      SuffixActions: list<PdfCanvas -> PdfCanvas>
      Close: CloseOperatorUnion }
with 
    static member Keep(tag: IntegratedRenderInfoTag) =  
        { Actions = [] 
          Close = CloseOperatorUnion.Keep(tag)
          SuffixActions = [] }


    static member CreateCloseOperator(tag, ?fill, ?stroke) =
        { Actions = []
          SuffixActions = []
          Close = CloseOperatorUnion.Create(tag, ?fill = fill, ?stroke = stroke)}

    static member createActions tag (pdfActions) = 
        { Actions = pdfActions
          Close = CloseOperatorUnion.Keep(tag)
          SuffixActions = [] }

    static member createSuffix tag (suffixPdfActions) = 
        { Actions = []
          Close = CloseOperatorUnion.Keep(tag)
          SuffixActions = suffixPdfActions }

    static member ConcatOrKeep (tag) (writers: ModifierPdfCanvasActions list) =
        match AtLeastOneList.TryCreate writers with 
        | Some writers ->
            let actions =
                writers.AsList
                |> List.collect (fun m -> m.Actions)

            let closeOperator =
                writers
                |> AtLeastOneList.map(fun m -> m.Close)
                |> CloseOperatorUnion.concat tag

            let suffixActions = 
                writers.AsList
                |> List.collect (fun m -> m.SuffixActions)


            { Close = closeOperator
              Actions = actions
              SuffixActions = suffixActions }

        | None -> ModifierPdfCanvasActions.Keep(tag)
            
        
    member private x.WriteClose(originCloseOperatorRange: OperatorRange, textRenderingMode) =
        fun (canvas: PdfCanvas) ->
            match x.Close with 
            | CloseOperatorUnion.Text close ->
                let textRenderingMode = close.Apply(textRenderingMode)

                canvas.SetTextRenderingMode(textRenderingMode) |> ignore

                match close.Text with 
                | None -> PdfCanvas.writeOperatorRange originCloseOperatorRange canvas
                | Some text -> PdfCanvas.showText(text) canvas

            | CloseOperatorUnion.Path close ->
                let newOperatorName = close.Apply(originCloseOperatorRange.Operator.Text())
                let newOperator =
                    { Operator = new PdfLiteral(newOperatorName)
                      Operands = ResizeArray [new PdfLiteral(newOperatorName) :> PdfObject]}

                PdfCanvas.writeOperatorRange newOperator canvas

    member x.WriteCloseAndSuffixActions(originCloseOperatorRange: OperatorRange, textRenderingMode) =
        fun (canvas: PdfCanvas) ->
            let pdfActions = 
                [
                    yield x.WriteClose(originCloseOperatorRange, textRenderingMode) 
                    yield! x.SuffixActions
                ]

            (canvas, pdfActions)
            ||> List.fold(fun canvas fPdfCanvas ->
                fPdfCanvas canvas
            )



type internal CallbackableContentOperator (originalOperator) =
    member this.OriginalOperator: IContentOperator = originalOperator

    interface IContentOperator with 
        member this.Invoke(processor,operator,operands) =
            
            let processor = processor :?> OperatorRangeCallbackablePdfCanvasProcessor
            let operatorName = operator.Text()
            match operatorName with 
            | ContainsBy [Tj; TJ] -> 
                processor.Listener.BeginShowText()
            | _ -> ()


            if operatorName <> Do then 
                try 
                    this.OriginalOperator.Invoke(processor, operator, operands)
                with ex ->
                    if ex.Message = "Dictionary doesn't have supported font data." 
                    then
                        Logger.warning (sprintf "Skip checking MM font %A" operator)
                        let size = (operands.[1]) :?> PdfNumber
                        let size = size.FloatValue()
                        processor.GetGraphicsState().SetFontSize(size)

                    else reraise()

            match operatorName with 
            | ContainsBy [Tj; TJ] -> 
                processor.InvokeOperatorRange({ Operator = operator; Operands = operands})
                processor.Listener.EndShoeText()

            | _ -> 
                processor.InvokeOperatorRange({ Operator = operator; Operands = operands})

            

and private OperatorRangeCallbackablePdfCanvasProcessor(listener) =
    inherit NonInitialCallbackablePdfCanvasProcessor(listener)
    abstract member InvokeOperatorRange: OperatorRange -> unit

    member internal x.Listener: FilteredEventListenerEx = listener

    default this.InvokeOperatorRange(operatorRange) = ()

    override this.RegisterContentOperator(operatorString: string , operator: IContentOperator) : IContentOperator =
        let wrapper = new CallbackableContentOperator(operator)
        let formOperator = base.RegisterContentOperator(operatorString, wrapper)
        match formOperator with 
        | :? CallbackableContentOperator as wrapper -> wrapper.OriginalOperator
        | _ -> formOperator







[<Struct>]
type _SelectionModifierFixmentArguments =
    { CurrentRenderInfo: IIntegratedRenderInfoIM
      ConcatedTextInfos: IntegratedTextRenderInfo seq }


type private Modifier = _SelectionModifierFixmentArguments -> ModifierPdfCanvasActions

type internal ModifierPdfCanvas(contentStream, resources, document) =
    inherit CanvasGraphicsStateSettablePdfCanvas(contentStream, resources, document)

    override x.Rectangle(rect) =
        let affineTransform = AffineTransform.ofMatrix(x.GetGraphicsState().GetCtm())
        let rect = affineTransform.InverseTransform(rect)
        base.Rectangle(rect)


and private PdfCanvasEditor(selectorModifierMapping: Map<SelectorModiferToken, RenderInfoSelector * Modifier>, document: PdfDocument) =
    inherit OperatorRangeCallbackablePdfCanvasProcessor(FilteredEventListenerEx(Map.map (fun _ -> fst) selectorModifierMapping))
    
    let eventTypes = 
        selectorModifierMapping
        |> Map.map (fun _ -> fst)
        |> Map.toList
        |> List.map snd
        |> RenderInfoSelector.OR
        |> RenderInfoSelector.toEventTypes
    
    let renderImage =
        List.contains EventType.RENDER_IMAGE eventTypes

    let renderText =
        List.contains EventType.RENDER_TEXT eventTypes

    let renderPath =
        List.contains EventType.RENDER_PATH eventTypes

    let mutable eventListener: FilteredEventListenerEx = null
    let pdfCanvasStack = new Stack<ModifierPdfCanvas>()
    let resourcesStack = new Stack<PdfResources>()

    let (|Path|_|) operatorName =
        match renderPath with 
        | true ->
            match operatorName with 
            | ContainsBy [f; F; ``f*``; S; s; B; ``B*``; b; ``b*``] -> Some ()
            | _ -> None

        | false -> None

    let (|Text|_|) operatorName =
        match renderText with 
        | true -> 
            match operatorName with 
            | ContainsBy [Tj; TJ] -> Some ()
            | _ -> None

        | false -> None

    let (|PathOrText|_|) operatorName =
        match operatorName with 
        | Path _ -> Some ()
        | Text _ -> Some ()
        | _ -> None

    let (|Form|Image|Others|) pdfName = 
        if pdfName = PdfName.Form 
        then Form
        elif renderImage && pdfName = PdfName.Image 
        then Image
        else Others

    override this.InvokeOperatorRange (operatorRange: OperatorRange) =
        
        let currentPdfCanvas = pdfCanvasStack.Peek()
        let operatorName = operatorRange.Operator.Text()

        match operatorName with 
        | Do ->

            let resources = resourcesStack.Peek()
            let name = operatorRange.Operands.[0] :?> PdfName

            let container = resources.GetResource(PdfName.XObject)
            let xobjectStream = 
                let pdfStream = (container.Get(name) :?> PdfStream)
                pdfStream.Clone() :?> PdfStream
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

            | Image -> failwith ""
                
            | Others -> ()

            PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
            |> ignore


        | PathOrText ->
            match eventListener.CurrentRenderInfoStatus with 
            | CurrentRenderInfoStatus.Skiped -> 
                PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                |> ignore

            | CurrentRenderInfoStatus.Selected ->
                let currentGS = this.GetGraphicsState()
                currentPdfCanvas.SetCanvasGraphicsState(currentGS)

                let tag =
                    match operatorName with 
                    | Path -> IntegratedRenderInfoTag.Path
                    | Text -> IntegratedRenderInfoTag.Text
                    | _ -> failwith "Invalid token"

                let modifierPdfCanvasActions =
                    eventListener.CurrentRenderInfoToken.Value
                    |> List.map(fun token -> 
                        let fix = snd selectorModifierMapping.[token]
                        fix { CurrentRenderInfo = eventListener.CurrentRenderInfo
                              ConcatedTextInfos = eventListener.ConcatedTextInfos }    
                    )
                    |> ModifierPdfCanvasActions.ConcatOrKeep tag

                PdfCanvas.useCanvas (currentPdfCanvas :> PdfCanvas) (fun canvas ->
                    (canvas, modifierPdfCanvasActions.Actions)
                    ||> List.fold(fun canvas action ->
                        action canvas 
                    )
                    |> modifierPdfCanvasActions.WriteCloseAndSuffixActions(operatorRange, currentGS.GetTextRenderingMode())
                )
            | _ -> failwith "Invalid token"

        | _ ->
            PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
            |> ignore


    member internal x.ParsedRenderInfos = eventListener.ParsedRenderInfos

    member this.EditContent (resources: PdfResources, pdfObject: PdfObject) =
        match eventListener with 
        | null -> eventListener <- this.GetEventListener() :?> FilteredEventListenerEx
        | _ -> ()

        match pdfObject with 
        | :? PdfStream as stream -> 
            let pdfCanvas = new ModifierPdfCanvas(new PdfStream(), resources, document)
            
            let bytes = stream.GetBytes()
            pdfCanvasStack.Push(pdfCanvas)
            resourcesStack.Push(resources)

            base.ProcessContent(bytes, resources)

            let pdfCanvas = pdfCanvasStack.Pop()
            resourcesStack.Pop()|> ignore
            //let clonedStream = stream.Clone() :?> PdfStream
            stream.SetData(pdfCanvas.GetContentStream().GetBytes()) |> ignore
            stream

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
    let modifyIM (selectorModifierMapping) (page: PdfPage) =
        let document = page.GetDocument()
        let editor = new PdfCanvasEditor(selectorModifierMapping, document)
        let pageContents = page.GetPdfObject().Get(PdfName.Contents).Clone()

        match pageContents with 
        | null -> Seq.empty
        | _ ->
            let resources = page.GetResources()

            editor.InitClippingPath(page)
            let fixedStream = editor.EditContent(resources, pageContents)

            page.Put(PdfName.Contents, fixedStream)
            |> ignore

            editor.ParsedRenderInfos

    let modify (selectorModifierMapping) (page: PdfPage) =
        let renderInfos = 
            selectorModifierMapping
            |> Map.toList
            |> List.map snd
            |> List.map fst

        RenderInfoSelector.checkNonImageSelectorExists renderInfos

        modifyIM selectorModifierMapping page
        |> Seq.map (fun m -> m :?> IIntegratedRenderInfo)

         





