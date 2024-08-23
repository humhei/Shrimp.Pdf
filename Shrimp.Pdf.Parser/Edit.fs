

namespace Shrimp.Pdf.Parser

open iText.IO.Image
open System.Collections.Concurrent
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.Kernel.Colors
open iText.Kernel.Geom

#nowarn "0104"
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Xobject
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Exceptions
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
open Shrimp.Pdf
open Shrimp.Pdf.Parser.Helper

open Constants.Operators

[<RequireQualifiedAccess>]
type XObjectReference =
    | ByRef
    | ByCopied



[<RequireQualifiedAccess>]
type ModifyLayerOptions =
    | RemoveLayerCase of layers: StreamableFsLayers
    | OnlyVisibleLayer
    | InLayerCase of includeTopLevel: bool * layers: StreamableFsLayers
    | AddClippingPath of layers: ShpLayer list
with 
    static member InLayer(layers, ?includeTopLevel) =
        ModifyLayerOptions.InLayerCase( 
            layers = layers,
            includeTopLevel = defaultArg includeTopLevel true)

    static member RemoveLayer(layers) =
        ModifyLayerOptions.RemoveLayerCase(layers = layers)


    //member private x.Layers() =
    //    match x with 
    //    | RemoveLayerCase (layers) 
    //    | InLayerCase (_, layers) -> layers
    //    | AddClippingPath (layers) -> StreamableFsLayers.InStream layers

    member x.ToReaderLayerOptions() =
        match x with 
        | RemoveLayerCase (layers) -> 
            ReaderLayerOptions.SpecificLayers(includeTopLevel = true, layers = layers)


        | InLayerCase (includeTopLevel, layers) -> 
            ReaderLayerOptions.SpecificLayers(includeTopLevel = includeTopLevel, layers = layers)
        
        | AddClippingPath (layers) -> 
            let layers = StreamableFsLayers.InStream layers
            ReaderLayerOptions.SpecificLayers(includeTopLevel = true, layers = layers)


        | OnlyVisibleLayer ->
            ReaderLayerOptions.OnlyVisibleLayers

    static member RemoveShpLayer(shpLayer, ?inStream) =
        let layers =
            match defaultArg inStream true with 
            | true -> StreamableFsLayers.InStream shpLayer
            | false ->
                shpLayer
                |> List.map FsLayer.ShpLayer
                |> StreamableFsLayers.FsLayers


        ModifyLayerOptions.RemoveLayer (layers)

    static member InShpLayer(shpLayer, ?inStream, ?includeToplevel) =
        let layers =
            match defaultArg inStream true with 
            | true -> StreamableFsLayers.InStream shpLayer
            | false ->
                shpLayer
                |> List.map FsLayer.ShpLayer
                |> StreamableFsLayers.FsLayers

        ModifyLayerOptions.InLayer (
            ?includeTopLevel = includeToplevel,
            layers = layers
        )


type PdfModifyOptions =
    { XObjectReference: XObjectReference
      LayerOptions: ModifyLayerOptions option }
with 
    static member ByRef =
        { LayerOptions = None
          XObjectReference = XObjectReference.ByRef }

    static member DefaultValue =
        { XObjectReference = XObjectReference.ByCopied 
          LayerOptions = None }

    member x.InShpLayer(shpLayers, ?includeToplevel) =
        let layerOptions = ModifyLayerOptions.InShpLayer(shpLayers, ?includeToplevel = includeToplevel)
        { x with LayerOptions = Some layerOptions }

    member x.RemoveShpLayer(shpLayers) =
        let layerOptions = ModifyLayerOptions.RemoveShpLayer(shpLayers)
        { x with LayerOptions = Some layerOptions }

type PdfModifyOptions2 =
    { XObjectReference: XObjectReference
      LayerOptions: ModifyLayerOptions option
      AddClippingPathForLayerOperation: option<PdfCanvas -> ParserGraphicsState -> OperatorRange -> unit>
      ParserCache: DocumentParserCache }
with 
    member x.NoCache() =
        { XObjectReference = x.XObjectReference 
          LayerOptions = x.LayerOptions }

    member x.ByRef() =
        { x with XObjectReference = XObjectReference.ByRef }

    static member Create(cache, ?xobjectReference, ?layerOptions, ?addClippingPathForLayerOperation) =
        { XObjectReference = defaultArg xobjectReference XObjectReference.ByCopied 
          LayerOptions = layerOptions
          ParserCache = cache
          AddClippingPathForLayerOperation = addClippingPathForLayerOperation }




type CloseOperator= 
    | Open  = 0
    | Close = 1
    | Keep  = 2

[<RequireQualifiedAccess>]
module CloseOperator=
    let concatenate left right =
        match left, right with 
        | CloseOperator.Keep, _ -> right
        | _ ,CloseOperator.Keep -> left
        | _ -> right


type PageOrXObject =
    | Page = 0 
    | XObject = 1

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

        | CloseOperator.Close, CloseOperator.Close -> n
        | CloseOperator.Close, CloseOperator.Open -> 
            match operator with 
            | EQ b -> s
            | EQ B -> S
            | EQ ``b*`` -> s
            | EQ ``B*`` -> S
            | operatorName -> 
                match operatorName with 
                | ContainsBy [s; S] -> operatorName
                | ContainsBy [f; ``f*``] -> s
                | EQ F -> S
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

        | CloseOperator.Open, CloseOperator.Close ->
            match operator with 
            | EQ b -> f
            | EQ B -> F
            | EQ ``b*`` -> ``f*``
            | EQ ``B*`` -> F
            | operatorName -> 
                match operatorName with 
                | ContainsBy [f; F; ``f*``]  -> operatorName
                | EQ s -> f
                | EQ S -> F
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


type TextCloseOperator =
    { Fill: CloseOperator
      Stroke: CloseOperator
      Text: PdfConcatedWord option }
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

            | CloseOperator.Close, CloseOperator.Close -> TextRenderingMode.INVISIBLE
            | CloseOperator.Close, CloseOperator.Open -> TextRenderingMode.STROKE

            | CloseOperator.Open, CloseOperator.Close -> TextRenderingMode.FILL
            | CloseOperator.Open, CloseOperator.Keep ->
                match textRenderingMode with 
                | PdfCanvasConstants.TextRenderingMode.FILL_STROKE -> TextRenderingMode.FILL_STROKE
                | PdfCanvasConstants.TextRenderingMode.FILL -> TextRenderingMode.FILL
                | PdfCanvasConstants.TextRenderingMode.STROKE -> TextRenderingMode.FILL_STROKE
                | PdfCanvasConstants.TextRenderingMode.INVISIBLE -> TextRenderingMode.FILL
                | _ -> failwith "Invalid token"

            | CloseOperator.Open, CloseOperator.Open -> TextRenderingMode.FILL_STROKE
                
        | _ -> 
            PdfLogger.unSupportedTextRenderMode textRenderingMode
            textRenderingMode

type ImageData_ForWrite =
    { ID: SpawnablePdfObjectID
      ImageData: ImageData
      Bytes: option<byte []>
      SoftMask: FsSoftMask_For_Write option }

[<RequireQualifiedAccess>]
type ImageDataOrImageXObject =
    | ImageData of  ImageData_ForWrite * (PdfCanvas ->  ImageData_ForWrite -> PdfXObject)
    | ImageXObject of PdfImageXObject
    | Inline       of PdfStream
    | MaskColor    of Color
with 
    member x.AsSpawned() =
        match x with 
        | ImageXObject _ -> x
        | ImageData (data, factory) ->
            ImageData({data with ID.IsSpawned = true}, factory)

        | MaskColor _ -> x
        | Inline _ -> x

[<RequireQualifiedAccess>]
type ImageCloseOperator =
    | Keep
    | New of ctm: AffineTransformRecord * ImageDataOrImageXObject
    | Remove

[<RequireQualifiedAccess>]
type CloseOperatorUnion =
    | Text of TextCloseOperator
    | Path of PathCloseOperator
    | Image of ImageCloseOperator
with 
    static member Keep(tag) =
        match tag with 
        | IntegratedRenderInfoTag.Path -> CloseOperatorUnion.Path(PathCloseOperator.Keep)
        | IntegratedRenderInfoTag.Text -> CloseOperatorUnion.Text(TextCloseOperator.Keep)

    static member KeepAll(tag) =
        match tag with 
        | IntegratedRenderInfoTagIM.Path -> CloseOperatorUnion.Path(PathCloseOperator.Keep)
        | IntegratedRenderInfoTagIM.Text -> CloseOperatorUnion.Text(TextCloseOperator.Keep)
        | IntegratedRenderInfoTagIM.Image -> CloseOperatorUnion.Image(ImageCloseOperator.Keep)


    static member CreatePath(?fill, ?stroke) =
        { Fill = defaultArg fill CloseOperator.Keep 
          Stroke = defaultArg stroke CloseOperator.Keep  }
        |> CloseOperatorUnion.Path


    static member CreateText(?fill, ?stroke, ?text) =
        { Fill = defaultArg fill CloseOperator.Keep 
          Stroke = defaultArg stroke CloseOperator.Keep 
          Text = text}
        |> CloseOperatorUnion.Text

    static member Create(tag, ?fill, ?stroke, ?text) =
        match tag with 
        | IntegratedRenderInfoTag.Path -> 
            CloseOperatorUnion.CreatePath(?fill = fill, ?stroke = stroke)
        | IntegratedRenderInfoTag.Text ->
            CloseOperatorUnion.CreateText(?fill = fill, ?stroke = stroke, ?text = text)

[<RequireQualifiedAccess>]
module private CloseOperatorUnion =
    let concatPath (operators: PathCloseOperator al1List) =
        operators.AsList
        |> List.reduce(fun left right ->
            { Fill = CloseOperator.concatenate left.Fill right.Fill
              Stroke = CloseOperator.concatenate left.Stroke right.Stroke }
        )

    let concatText (operators: TextCloseOperator al1List) =
        operators.AsList
        |> List.reduce(fun left right ->
            { Fill = CloseOperator.concatenate left.Fill right.Fill
              Stroke = CloseOperator.concatenate left.Stroke right.Stroke
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
                    | CloseOperatorUnion.Text _
                    | CloseOperatorUnion.Image _ -> failwith "Current close operator %A should be path Close Operator"
                )
                |> concatPath


            CloseOperatorUnion.Path operator


        | IntegratedRenderInfoTag.Text ->
            let operator = 
                operators
                |> AtLeastOneList.map(fun m ->
                    match m with 
                    | CloseOperatorUnion.Text text -> text 
                    | CloseOperatorUnion.Path _ 
                    | CloseOperatorUnion.Image _ -> failwith "Current close operator %A should be text Close Operator"
                )
                |> concatText

            CloseOperatorUnion.Text operator


type ModifierPdfCanvasActions =
    { Actions: list<PdfCanvas -> PdfCanvas>
      SuffixActions: list<PdfCanvas -> PdfCanvas>
      Close: CloseOperatorUnion }
with 
    member internal x.AllActionsLength =
        x.Actions.Length + x.SuffixActions.Length

    member internal x.UseCanvas (canvas0: #PdfCanvas) (operatorRange: OperatorRange) action =
            match operatorRange.Operator.Text() with 
            | "'" -> 
                let canvas = canvas0 :> PdfCanvas
                canvas.NewlineText()
                |> ignore

                let partOperatorRange =
                    { operatorRange with 
                        Operator = PdfLiteral(Tj)
                        Operands = [|operatorRange.Operands.[0]; PdfLiteral(Tj)|]
                    }

                PdfCanvas.useCanvas canvas0 (action partOperatorRange) 

            | "\"" ->
                let canvas = canvas0 :> PdfCanvas
                let wordSpacing = operatorRange.Operands.[0] :?> PdfNumber
                let characterSpacing = operatorRange.Operands.[1] :?> PdfNumber
                canvas
                    .SetWordSpacing(wordSpacing.FloatValue())
                    .SetCharacterSpacing(characterSpacing.FloatValue())
                    .NewlineText()
                    |> ignore

                let partOperatorRange =
                    { Operator = PdfLiteral(Tj)
                      Operands =
                        [|operatorRange.Operands.[2]; PdfLiteral(Tj)|]
                      }

                PdfCanvas.useCanvas canvas0 (action partOperatorRange) 

            | _ -> PdfCanvas.useCanvas canvas0 (action operatorRange)

    member x.AddActions(actions) =
        { x with Actions = x.Actions @ actions}

    static member Keep(tag: IntegratedRenderInfoTag) =  
        { Actions = [] 
          Close = CloseOperatorUnion.Keep(tag)
          SuffixActions = [] }

    static member KeepAll(tag: IntegratedRenderInfoTagIM) =  
        { Actions = [] 
          Close = CloseOperatorUnion.KeepAll(tag)
          SuffixActions = [] }

    static member KeepImage() =  
        { Actions = [] 
          Close = CloseOperatorUnion.Image (ImageCloseOperator.Keep)
          SuffixActions = [] }

    static member NewImage(originCtm, image) =  
        { Actions = [] 
          Close = CloseOperatorUnion.Image (ImageCloseOperator.New (originCtm, image))
          SuffixActions = [] }

    static member RemoveImage() =  
        { Actions = [] 
          Close = CloseOperatorUnion.Image (ImageCloseOperator.Remove)
          SuffixActions = [] }

    static member CreateCloseOperator(tag, ?fill, ?stroke, ?text) =
        { Actions = []
          SuffixActions = []
          Close = CloseOperatorUnion.Create(tag, ?fill = fill, ?stroke = stroke, ?text = text)}

    static member CreateActions tag (pdfActions) = 
        { Actions = pdfActions
          Close = CloseOperatorUnion.Keep(tag)
          SuffixActions = [] }

    static member CreateActions_Image (actions) = 
        { Actions = actions
          Close = CloseOperatorUnion.Image (ImageCloseOperator.Keep)
          SuffixActions = [] }

    static member CreateActions_All tag (actions) = 
        { Actions = actions
          Close = CloseOperatorUnion.KeepAll (tag)
          SuffixActions = [] }

    static member CreateSuffix_Image (suffixPdfActions) = 
        { Actions = []
          Close = CloseOperatorUnion.Image (ImageCloseOperator.Keep)
          SuffixActions = suffixPdfActions }

    static member CreateSuffix tag (suffixPdfActions) = 
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
                | Some text -> 
                    match text.FollowedWords with 
                    | [] -> PdfCanvas.showText(text.HeadWord) canvas
                    | followedTexts ->
                        let document = canvas.GetDocument()
                        let currentGs = canvas.GetGraphicsState()
                        let contentStream = canvas.GetContentStream()
                        document.CheckIsoConformance(currentGs, IsoKey.FONT_GLYPHS, null, contentStream)
                        match currentGs.GetFont() with 
                        | null -> 
                            let ex = 
                                (KernelExceptionMessageConstant.FONT_AND_SIZE_MUST_BE_SET_BEFORE_WRITING_ANY_TEXT, currentGs)
                                |> PdfException
                            raise ex 

                        | font -> 
                            let outputStream = contentStream.GetOutputStream()

                            let writeString (text: string) =
                                outputStream.WriteString(text) |> ignore
                                
                            let writeSpace() =
                                outputStream.WriteSpace() |> ignore
                                

                            writeString "["
                            font.WriteText(text.HeadWord, outputStream)
                            writeSpace()

                            followedTexts
                            |> List.iteri(fun i followedText ->
                                match followedText.Space with 
                                | Some space -> 
                                    outputStream
                                        .WriteFloat(float32 space)
                                        .WriteSpace()
                                        |> ignore

                                    font.WriteText(followedText.Text, outputStream)

                                | None -> font.WriteText(followedText.Text, outputStream)

                                match i = followedTexts.Length - 1 with 
                                | true -> 
                                    writeString("]")
                                    writeString(TJ)
                                    outputStream.WriteNewLine() |> ignore
                                | false -> writeSpace()
                            )

                            canvas

            | CloseOperatorUnion.Path close ->
                let newOperatorName = close.Apply(originCloseOperatorRange.Operator.Text())
                let newOperator =
                    { Operator = new PdfLiteral(newOperatorName)
                      Operands = ResizeArray [new PdfLiteral(newOperatorName) :> PdfObject]}

                PdfCanvas.writeOperatorRange newOperator canvas

            | CloseOperatorUnion.Image close -> 
                match close with 
                | ImageCloseOperator.Keep ->
                    PdfCanvas.writeOperatorRange originCloseOperatorRange canvas
                    
                | ImageCloseOperator.New (ctm, image) ->
                    match image with 
                    | ImageDataOrImageXObject.ImageData (data, imageFactory) ->
                        let xobject = imageFactory canvas (data)
                        canvas.AddXObject(xobject, ctm) 

                    | ImageDataOrImageXObject.ImageXObject image ->
                        canvas.AddXObject(image, ctm) 

                    | ImageDataOrImageXObject.Inline pdfStream ->
                        let operatorRange =
                            { Operator = PdfLiteral(EI)
                              Operands = 
                                [|
                                    pdfStream :> PdfObject
                                    PdfLiteral(EI) :> PdfObject
                                |]
                              }
                        canvas
                        |> PdfCanvas.writeOperatorRange operatorRange
                        
                    | ImageDataOrImageXObject.MaskColor color ->
                        canvas.SetFillColor(color) |> ignore
                        PdfCanvas.writeOperatorRange originCloseOperatorRange canvas

                | ImageCloseOperator.Remove ->
                    canvas


                    

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




type internal FsPdfResources(pdfResources: PdfResources) =
    let removableXObjectName = HashSet()
    let fixedStreamObjNums = new ConcurrentDictionary<FsPdfObjectID, PdfName option * PdfStream>()

    /// prior > than removableXObjectName
    let keepingXObjectNames = HashSet()

    member x.FixedStreamObjNums = fixedStreamObjNums

    member x.AddRemovableXObjectName(xobjectName: PdfName) = 
        removableXObjectName.Add(xobjectName)
        |> ignore<bool>

    member x.AddKeepingXObjectName(xobjectName: PdfName) =
        keepingXObjectNames.Add(xobjectName)
        |> ignore<bool>

    member x.DeleteRemovableXObject(ops: PdfModifyOptions) =
        //match ops.XObjectReference with 
        //| XObjectReference.ByRef -> ()
        //| XObjectReference.ByCopied ->

            for xobjectName in removableXObjectName do
                match keepingXObjectNames.Contains xobjectName with 
                | true -> ()
                | false ->
                    let __remove_resources_xobject = 
                        let container = pdfResources.GetResource(PdfName.XObject)
                        container.Remove(xobjectName)
                        |> ignore<PdfObject>
                    
                    ()

            removableXObjectName.Clear()






    member x.PdfResources = pdfResources

    member x.GetResource(resType) = pdfResources.GetResource(resType)

    member x.AddForm(form: PdfStream) = pdfResources.AddForm(form)

    member x.GetProperties(name) = pdfResources.GetProperties(name)

type FsPdfDocumentEditorResources() =
    let fixedStreamObjNums = new ConcurrentDictionary<FsPdfObjectID, PdfName option * PdfStream>()
    let fsResources  = Stack<FsPdfResources>()


    member internal x.FixedStreamObjNums = fixedStreamObjNums
    member internal x.FsPdfResources = fsResources

    member x.DeleteRemovableXObject(ops) =
        for item in fsResources.ToArray() do 
            item.DeleteRemovableXObject(ops)

type IFsPdfDocumentEditor =
    abstract member Resources: FsPdfDocumentEditorResources



type internal CallbackableContentOperator (originalOperator) =
    let listenterConnectedContentOperator = 
        RenderInfoAccumulatableContentOperator(originalOperator, false ,(fun processor ->
            let processor = processor :?> OperatorRangeCallbackablePdfCanvasProcessor
            processor.CurrentResource().PdfResources
        ))

    member this.OriginalOperator: IContentOperator = originalOperator

    interface IContentOperator with 
        member this.Invoke(processor,operator,operands) =
            let processor = processor :?> OperatorRangeCallbackablePdfCanvasProcessor
            (listenterConnectedContentOperator).Invoke(processor,operator,operands, fun () ->
                processor.InvokeOperatorRange({ Operator = operator; Operands = operands})
            )

            

and private OperatorRangeCallbackablePdfCanvasProcessor(ocProperties, listener, ?readerLayerOptions, ?noLayerOperation, ?inLayerOperationOverride) =
    inherit InStreamLayerStackablePdfCanvasProcessor(ocProperties, listener, ?readerLayerOptions = readerLayerOptions, ?noLayerOperation = noLayerOperation, ?inLayerOperationOverride = inLayerOperationOverride)
    abstract member InvokeOperatorRange: OperatorRange -> unit
    abstract member CurrentResource: unit -> FsPdfResources

    member internal x.Listener: FilteredEventListenerEx = listener

    default this.CurrentResource() = failwithf "Invalid token"

    default this.InvokeOperatorRange(operatorRange) = ()

    override this.RegisterContentOperator(operatorString: string, operator: IContentOperator) : IContentOperator =
        let wrapper = new CallbackableContentOperator(operator)
        let formOperator = base.RegisterContentOperator(operatorString, wrapper)
        
        match formOperator with 
        | :? CallbackableContentOperator as wrapper -> wrapper.OriginalOperator
        | _ -> formOperator

[<AbstractClass>]
type private LayerStackableOperatorRangeCallbackablePdfCanvasProcessor
    (ocProperties, listener, ?modifyLayerOptions: ModifyLayerOptions) as this =
    inherit OperatorRangeCallbackablePdfCanvasProcessor(
        ocProperties,
        listener,
        ?readerLayerOptions = (modifyLayerOptions |> Option.map (fun m -> m.ToReaderLayerOptions())),
        noLayerOperation = (fun operatorRange ->
            this.InvokeOperatorRange_NoLayer(operatorRange)
        ),
        inLayerOperationOverride = (fun (isInLayerChoice) ->
            this.InvokeOperatorRange_InLayer_Override(isInLayerChoice)
        )
    )

    abstract member InvokeOperatorRange_InLayer_Override: IsInLayerChoice -> InLayerOperationOverride
    abstract member InvokeOperatorRange_NoLayer: OperatorRange -> unit


type ShadableKind =
    | Normal = 0
    | SHShading = 1
    | ClippingPathShading = 2

[<Struct>]
type _SelectionModifierFixmentArguments =
    { CurrentRenderInfo: IIntegratedRenderInfoIM }


type private Modifier = _SelectionModifierFixmentArguments -> ModifierPdfCanvasActions

type ShadableModifier =
    { 
        SHShadingModifier: option<IntegratedPathRenderInfo -> OperatorRange -> PdfCanvas -> unit>
        ClippingPathShadingModifier: option<IntegratedPathRenderInfo -> OperatorRange -> PdfCanvas -> unit>
        CommonModifier: _SelectionModifierFixmentArguments -> ModifierPdfCanvasActions
    }
with 
    static member Create(commonModifier) =
        { SHShadingModifier = None 
          ClippingPathShadingModifier = None
          CommonModifier = commonModifier }
    
    static member CreateSHShadingModifier(commonModifier, shModifier) =
        { SHShadingModifier = shModifier
          ClippingPathShadingModifier = None
          CommonModifier = commonModifier }

    static member CreateClippingShadingModifier(commonModifier, clippingShadingModifier) =
        { SHShadingModifier = None
          ClippingPathShadingModifier = clippingShadingModifier
          CommonModifier = commonModifier }



[<RequireQualifiedAccess>]
type ModifierUnion =
    | Modifier of (_SelectionModifierFixmentArguments -> ModifierPdfCanvasActions)
    | ShadingModifier of ShadableModifier
with 
    member x.CommonModifier =
        match x with 
        | Modifier vv -> vv
        | ShadingModifier vv -> vv.CommonModifier



type internal ModifierPdfCanvas(contentStream, resources: FsPdfResources, document) =
    inherit CanvasGraphicsStateSettablePdfCanvas(contentStream, resources.PdfResources, document)

    do 
        let outputStream = contentStream.GetOutputStream()
        outputStream.SetLocalHighPrecision(true)

    override x.Rectangle(rect) =
        let affineTransform = AffineTransform.ofMatrix(x.GetGraphicsState().GetCtm())
        let rect = affineTransform.InverseTransform(rect)
        base.Rectangle(rect)


and private PdfCanvasEditor(ocProperties, selectorModifierMapping: Map<SelectorModiferToken, RenderInfoSelector * ModifierUnion>, ops: PdfModifyOptions2, page: PdfPage, document: PdfDocument) =
    inherit LayerStackableOperatorRangeCallbackablePdfCanvasProcessor
        (ocProperties, 
         FilteredEventListenerEx(ops.ParserCache, page, Map.map (fun _ -> fst) selectorModifierMapping),
         ?modifyLayerOptions = ops.LayerOptions)
    let fsDocumentResources = (box document :?> IFsPdfDocumentEditor).Resources
    let layerOptions = ops.LayerOptions
    let exists_ClippingPath_Shading_Modifier =
        selectorModifierMapping
        |> Map.exists(fun _ (_, modifier) ->
            match modifier with 
            | ModifierUnion.ShadingModifier vv -> vv.ClippingPathShadingModifier.IsSome
            | _ -> false
        )

    let exists_SH_Shading_Modifier =
        selectorModifierMapping
        |> Map.exists(fun _ (_, modifier) ->
            match modifier with 
            | ModifierUnion.ShadingModifier vv -> vv.SHShadingModifier.IsSome
            | _ -> false
        )


    let eventTypes = 
        selectorModifierMapping
        |> Map.map (fun _ -> fst)
        |> Map.toList
        |> List.map snd
        |> RenderInfoSelector.OR
        |> RenderInfoSelector.toEventTypes
    
    let renderImage =
        List.contains EventType.RENDER_IMAGE eventTypes

    let __checkModifierValid =
        match renderImage, selectorModifierMapping.Count with 
        | true, 1 -> ()
        | true, _ -> 
            let keys = selectorModifierMapping |> Map.toList |> List.map fst
            failwithf "Multiple modifiers %A are not supported when image is selectable" keys
        | _ -> ()

    let renderText =
        List.contains EventType.RENDER_TEXT eventTypes

    let renderPath =
        List.contains EventType.RENDER_PATH eventTypes





    let mutable eventListener: FilteredEventListenerEx = null
    let pdfCanvasStack = new Stack<ModifierPdfCanvas>()
    let resourcesStack = new Stack<FsPdfResources>()

    let (|PathOrText|_|) operatorName =
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
                | ContainsBy showTextOperators -> Some ()
                | _ -> None

            | false -> None

        match operatorName with 
        | Path _ -> Some IntegratedRenderInfoTag.Path
        | Text _ -> Some IntegratedRenderInfoTag.Text
        | _ -> None

    let (|Form|Image|Others|) pdfName = 
        if pdfName = PdfName.Form 
        then Form
        elif renderImage && pdfName = PdfName.Image 
        then Image
        else Others

    override this.CurrentResource() = resourcesStack.Peek()

    override this.InvokeOperatorRange_InLayer_Override (isInLayerChoice) =
        
        match layerOptions with 
        | Some (ModifyLayerOptions.RemoveLayerCase _) -> 
            InLayerOperationOverride.Override(fun operatorRange ->
                match isInLayerChoice with 
                | IsInLayerChoice.InTopLayer -> 
                    let currentPdfCanvas = pdfCanvasStack.Peek()
                    PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                    |> ignore

                | IsInLayerChoice.InSelectedLayer -> ()
            )
        | Some (ModifyLayerOptions.AddClippingPath _) -> 
            match isInLayerChoice with
            | IsInLayerChoice.InTopLayer -> InLayerOperationOverride.InvokeOperator
            | IsInLayerChoice.InSelectedLayer ->
                let canvasGs = this.GetGraphicsState()
                match ops.AddClippingPathForLayerOperation with 
                | None -> failwithf "AddClippingPathForLayerOperation is not defined, while modify layer options is ModifyLayerOptions.AddClippingPath"
                | Some operation -> 
                    InLayerOperationOverride.Override(fun operatorRange ->
                        let currentPdfCanvas = pdfCanvasStack.Peek()
                        operation currentPdfCanvas canvasGs operatorRange
                    )

        | _ -> InLayerOperationOverride.InvokeOperator

    override this.InvokeOperatorRange_NoLayer (operatorRange: OperatorRange) =
        let currentPdfCanvas = pdfCanvasStack.Peek()
        PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
        |> ignore

        //match layerOptions with 
        //| Some (ModifyLayerOptions.RemoveLayer _) -> 
        //    let currentPdfCanvas = pdfCanvasStack.Peek()
        //    PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
        //    |> ignore
        //| _ ->
        //    let currentPdfCanvas = pdfCanvasStack.Peek()
        //    PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
        //    |> ignore
        
    override this.InvokeOperatorRange (operatorRange: OperatorRange) =
        
        let currentPdfCanvas = pdfCanvasStack.Peek()
        let operatorName = operatorRange.Operator.Text()

        let getImageClose() =
            match eventListener.CurrentRenderInfoStatus with 
            | CurrentRenderInfoStatus.Skiped -> 
                Choice1Of2 operatorRange

            | CurrentRenderInfoStatus.Selected ->
                let modifierPdfCanvasActions =
                    match eventListener.CurrentRenderInfoToken.Value with 
                    | [token] ->
                        let fix = snd selectorModifierMapping.[token]
                        fix.CommonModifier { CurrentRenderInfo = eventListener.CurrentRenderInfo }    
                    | _ -> 
                        let keys = eventListener.CurrentRenderInfoToken.Value
                        failwithf "Multiple modifiers %A are not supported  when image is selectable" keys

                modifierPdfCanvasActions
                |> Choice2Of2

        let writeImage() =  
            match getImageClose() with 
            | Choice1Of2 operatorRange ->
                PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                |> ignore

                None
            
            | Choice2Of2 modifierPdfCanvasActions ->
                let currentGS = this.GetGraphicsState()
                currentPdfCanvas.SetCanvasGraphicsState(currentGS)

                PdfCanvas.useCanvas (currentPdfCanvas :> PdfCanvas) (fun canvas ->
                    (canvas, modifierPdfCanvasActions.Actions)
                    ||> List.fold(fun canvas action ->
                        action canvas 
                    )
                    |> modifierPdfCanvasActions.WriteCloseAndSuffixActions(operatorRange, currentGS.GetTextRenderingMode())
                )

                Some modifierPdfCanvasActions


        match operatorName with 
        | EI -> writeImage() |> ignore
        | Do ->
        
            let resources = resourcesStack.Peek()
            let name = operatorRange.Operands.[0] :?> PdfName
            let container = resources.GetResource(PdfName.XObject)
            let xobjectStream = 
                let value =  container.Get(name)
                let pdfStream = (value :?> PdfStream)
                pdfStream

            match isNull xobjectStream with 
            | true -> 
                let stack = this.Listener.InfoContainerIDStack()
                failwithf "Null xobject stream name %A" (stack, name)
            | false -> ()


            let subType = xobjectStream.GetAsName(PdfName.Subtype)

            //let hash = hashNumberOfPdfIndirectReference (xobjectStream.GetIndirectReference())
                

            let getXObjectResources(xobjectStream: PdfStream) =
                let subResources = xobjectStream.GetAsDictionary(PdfName.Resources)
                match subResources with 
                | null -> resources
                | _ -> 
                    subResources
                    |> PdfResources 
                    |> FsPdfResources

            let fixXObjectStream() = 


                match subType with 
                | Form ->
                    let hash = 
                        hashNumberOfPdfIndirectReference (xobjectStream.GetIndirectReference())
                        |> InfoContainerID.XObject

                    let xobjectStream = xobjectStream.Clone() :?> PdfStream
                        //match ops.XObjectReference with 
                        //| XObjectReference.ByCopied -> 
                        //    match subType with 
                        //    | Form -> 
                        //    | _ -> xobjectStream

                        //| XObjectReference.ByRef -> xobjectStream
                    this.Listener.InfoContainerIDStack_Push(hash)
                    this.Listener.SaveGS_XObject(this.GetGraphicsState())
                    
                    resources.AddRemovableXObjectName(name)

                    let xobjectResources = getXObjectResources(xobjectStream)

                    let fixedStream: PdfStream = this.EditContent(xobjectResources, xobjectStream, PageOrXObject.XObject) :?> PdfStream
                    let name = resources.AddForm(fixedStream)
                    //container.Put(name, fixedStream) |> ignore
                    //let operatorRange =
                    //    { Operator = operatorRange.Operator 
                    //      Operands = [| name :> PdfObject; PdfLiteral("Do") :> PdfObject |] }
        
                    //PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                    //|> ignore

                    currentPdfCanvas.WriteLiteral(name.ToString() + " Do\n") |> ignore

                    this.Listener.RestoreGS_XObject()
                    this.Listener.InfoContainerIDStack_Pop()

                    Some name, fixedStream

                | Image -> 
                    match writeImage() with 
                    | None -> ()
                    | Some modifierPdfCanvasActions ->
                        match modifierPdfCanvasActions.Close with 
                        | CloseOperatorUnion.Image close ->
                            let name = operatorRange.Operands.[0] :?> PdfName
                            match close with 
                            | ImageCloseOperator.Remove 
                            | ImageCloseOperator.New _   -> resources.AddRemovableXObjectName(name)
                            | ImageCloseOperator.Keep -> resources.AddKeepingXObjectName(name)

                        | _ -> failwith "Invalid token"
                    None, xobjectStream

                | Others ->

                    PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                    |> ignore

                    None, xobjectStream

            match ops.XObjectReference with 
            | XObjectReference.ByCopied -> fixXObjectStream() |> ignore
            | XObjectReference.ByRef ->
 
                let xobjectStreamID = xobjectStream.GetIndirectReference() |> hashNumberOfPdfIndirectReference
                match fsDocumentResources.FixedStreamObjNums.TryGetValue xobjectStreamID with 
                | true, (fixedName, fixedStream) -> 
                    match subType with 
                    | Form ->
                        let fixedName = 
                            resources.FixedStreamObjNums.GetOrAdd(xobjectStreamID, valueFactory = fun _ ->
                                let name = resources.AddForm(fixedStream)
                                Some name, fixedStream
                            )
                            |> fst
                            |> Option.get


                        resources.AddRemovableXObjectName(name)
                        //let xobjectResources = getXObjectResources(xobjectStream)
                        let text = (fixedName.ToString() + " Do\n")
                        currentPdfCanvas.WriteLiteral text |> ignore
                        //PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                        //|> ignore   

                    | Image ->
                        match (getImageClose()) with 
                        | Choice1Of2 operatorRange ->
                            PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                            |> ignore

                        | Choice2Of2 modifierPdfCanvasActions ->
                            match modifierPdfCanvasActions.Close with 
                            | CloseOperatorUnion.Image close ->
                                match close with 
                                | ImageCloseOperator.Keep ->
                                    PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                                    |> ignore

                                | ImageCloseOperator.Remove -> ()
                                | ImageCloseOperator.New _ -> 
                                    fixXObjectStream() |> ignore
                                    //failwithf "Not implemented for (XObjectRef,ImageCloseOperator.New)"

                            | _ -> failwith "Invalid token"
                            
                    | Others -> 
                        PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                        |> ignore

                | false, _ ->
                    let fixXObjectStream_Cached() =
                        fsDocumentResources.FixedStreamObjNums.GetOrAdd(xobjectStreamID, valueFactory = fun _ ->
                            resources.FixedStreamObjNums.GetOrAdd(xobjectStreamID, valueFactory = fun _ ->
                               fixXObjectStream()
                            )
                        )
                        |> ignore

                    match subType with 
                    | Image -> 
                        let imageClose = getImageClose()
                        match imageClose with 
                        | Choice.Choice1Of2 operatorRange ->
                            PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                            |> ignore

                        | Choice.Choice2Of2 modifierPdfCanvasActions ->
                            match modifierPdfCanvasActions.Close with 
                            | CloseOperatorUnion.Image close ->
                                match modifierPdfCanvasActions.Actions.Length + modifierPdfCanvasActions.SuffixActions.Length with 
                                | 0 -> 
                                    match close with 
                                    | ImageCloseOperator.Keep ->
                                        PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                                        |> ignore

                                    | ImageCloseOperator.Remove -> 
                                        fsDocumentResources.FixedStreamObjNums.GetOrAdd(xobjectStreamID, valueFactory = fun _ ->
                                            resources.FixedStreamObjNums.GetOrAdd(xobjectStreamID, valueFactory = fun _ ->
                                               resources.AddRemovableXObjectName(name)
                                               None, xobjectStream
                                            )
                                        )
                                        |> ignore

                                    | ImageCloseOperator.New _ ->  
                                        fixXObjectStream_Cached()
                                        //failwithf "Not implemented for (XObjectRef,ImageCloseOperator.New)"

                                | _ ->
                                    match close with 
                                    | ImageCloseOperator.Keep ->
                                        fixXObjectStream()
                                        |> ignore
                                    | _ -> failwithf "Not implemented"


                            | _ -> failwith "Invalid token"
                            
                    | _ -> fixXObjectStream_Cached()


        | PathOrText tag ->
            match eventListener.CurrentRenderInfoStatus with 
            | CurrentRenderInfoStatus.Skiped -> 
                match eventListener.OffsetedTextMatrix with 
                | None ->
                    PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                    |> ignore

                | Some textMatrix ->
                    match tag with 
                    | IntegratedRenderInfoTag.Text ->
                        currentPdfCanvas.SetTextMatrix(AffineTransform.ofMatrix textMatrix)
                        |> ignore

                    | IntegratedRenderInfoTag.Path -> ()

                    PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                    |> ignore

            | CurrentRenderInfoStatus.Selected ->
                let isShading = 
                    match eventListener.CurrentRenderInfo with 
                    | IIntegratedRenderInfoIM.Path (pathRenderInfo) -> 
                        match pathRenderInfo.IsShading with 
                        | true -> Some pathRenderInfo
                        | false -> None

                    | _ -> None

                match isShading with 
                | Some shadingInfo -> 
                    match exists_ClippingPath_Shading_Modifier with 
                    | false -> 
                        PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                        |> ignore

                    | true ->
                        let shadingModifiers = 
                            eventListener.CurrentRenderInfoToken.Value
                            |> List.choose(fun token -> 
                                let fix = snd selectorModifierMapping.[token]
                                match fix with 
                                | ModifierUnion.ShadingModifier vv -> vv.ClippingPathShadingModifier
                                | _ -> None
                            )

                        match shadingModifiers with 
                        | [] ->failwithf "Invalid token"
                        | [shadingModifier] ->
                            shadingModifier shadingInfo operatorRange currentPdfCanvas

                        | _ -> failwithf "Multiple clippingPath shading %d operators currently not supported" shadingModifiers.Length

                        

                | None ->
                    let currentGS = this.GetGraphicsState()
                    currentPdfCanvas.SetCanvasGraphicsState(currentGS)

                    let modifierPdfCanvasActions =
                        eventListener.CurrentRenderInfoToken.Value
                        |> List.map(fun token -> 
                            let fix = snd selectorModifierMapping.[token]
                            fix.CommonModifier { CurrentRenderInfo = eventListener.CurrentRenderInfo }    
                        )
                        |> ModifierPdfCanvasActions.ConcatOrKeep tag

                    let setTextMatrix() =
                        match tag with 
                        | IntegratedRenderInfoTag.Text ->   [
                            fun (canvas: PdfCanvas) -> 
                                let x = eventListener.CurrentRenderInfo :?> IntegratedTextRenderInfo
                                let textMatrix = x.ConcatedTextInfo.HeadWordInfo.GetTextMatrix()
                                let transform = AffineTransform.ofMatrix textMatrix
                                canvas.SetTextMatrix(transform)
                            ]

                        | IntegratedRenderInfoTag.Path -> []


                    modifierPdfCanvasActions.UseCanvas (currentPdfCanvas :> PdfCanvas) operatorRange (fun operatorRange canvas ->
                        (canvas, setTextMatrix() @ modifierPdfCanvasActions.Actions)
                        ||> List.fold(fun canvas action ->
                            action canvas 
                        )
                        |> modifierPdfCanvasActions.WriteCloseAndSuffixActions(operatorRange, currentGS.GetTextRenderingMode())
                    )

                    //let onlyWriteClose = 
                    //    match tag with 
                    //    | IntegratedRenderInfoTag.Text ->
                    //        match modifierPdfCanvasActions.AllActionsLength with 
                    //        | 0 -> 
                    //            match modifierPdfCanvasActions.Close with 
                    //            | CloseOperatorUnion.Text textClose ->
                    //                match textClose.Fill, textClose.Stroke with 
                    //                | CloseOperator.Keep, CloseOperator.Keep -> true
                    //                | _ -> false
                    //            | _ -> failwith "Invalid token"
                    //        | _ -> false
                    //    | _ -> false

                    //match onlyWriteClose with 
                    //| true -> 
                    //    (currentPdfCanvas :> PdfCanvas)
                    //    |> modifierPdfCanvasActions.WriteCloseAndSuffixActions(operatorRange, currentGS.GetTextRenderingMode())
                    //    |> ignore

                    //| false ->
                    //    PdfCanvas.useCanvas (currentPdfCanvas :> PdfCanvas) (fun canvas ->
                    //        (canvas, modifierPdfCanvasActions.Actions)
                    //        ||> List.fold(fun canvas action ->
                    //            action canvas 
                    //        )
                    //        |> modifierPdfCanvasActions.WriteCloseAndSuffixActions(operatorRange, currentGS.GetTextRenderingMode())
                    //    )



            | _ -> failwith "Invalid token"

        | _ ->
            
            let writeOperatorRange() =
                match exists_SH_Shading_Modifier with 
                | false -> 
                    PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                    |> ignore

                | true ->
                    match operatorName with 
                    | EQ sh -> 
                        let shadingModifiers = 
                            eventListener.CurrentRenderInfoToken.Value
                            |> List.choose(fun token -> 
                                let fix = snd selectorModifierMapping.[token]
                                match fix with 
                                | ModifierUnion.ShadingModifier vv -> vv.SHShadingModifier
                                | _ -> None
                            )

                        match shadingModifiers with 
                        | [] ->failwithf "Invalid token"
                        | [shadingModifier] ->
                            let shadingInfo = eventListener.CurrentRenderInfo :?> IntegratedPathRenderInfo
                            shadingModifier shadingInfo operatorRange currentPdfCanvas

                        | _ -> failwithf "Multiple sh shading %d  operators currently not supported" shadingModifiers.Length


                    | _ -> 
                        PdfCanvas.writeOperatorRange operatorRange currentPdfCanvas
                        |> ignore

            writeOperatorRange()




    member internal x.ParsedRenderInfos = eventListener.ParsedRenderInfos

    override this.ProcessContent(contentBytes, resources) =
        
        base.ProcessContent(contentBytes, resources)

    member this.EditContent (resources: FsPdfResources, pdfObject: PdfObject, pageOrXObject: PageOrXObject): PdfObject =
        match eventListener with 
        | null -> eventListener <- this.GetEventListener() :?> FilteredEventListenerEx
        | _ -> ()

        match pdfObject with 
        | :? PdfStream as stream -> 
            let editStream() =
                let pdfCanvas = new ModifierPdfCanvas(new PdfStream(), resources, document)
            
                let bytes = stream.GetBytes()
                pdfCanvasStack.Push(pdfCanvas)
                resourcesStack.Push(resources)

                this.ProcessContent(bytes, resources.PdfResources)
                let pdfCanvas = pdfCanvasStack.Pop()
                let resources_pop = resourcesStack.Pop()
                fsDocumentResources.FsPdfResources.Push(resources_pop)
                //let stream = stream.Clone() :?> PdfStream
                let bytes = pdfCanvas.GetContentStream().GetBytes()
                stream.SetData(bytes) |> ignore
                stream

            match ops.XObjectReference with 
            | XObjectReference.ByCopied -> editStream()

            | XObjectReference.ByRef -> 
                match pageOrXObject with 
                | PageOrXObject.XObject -> editStream()
                | PageOrXObject.Page -> editStream()
                    //let objRef = stream.GetIndirectReference() |> hashNumberOfPdfIndirectReference
                    //let r = 
                    //    fsDocumentResources.FixedStreamObjNums.GetOrAdd(objRef, valueFactory = fun _ ->
                    //        None, editStream()
                    //    )
                    
                    //(snd r) :> PdfObject

        | :? PdfArray as array ->
            if array |> Seq.forall (fun o -> o :? PdfStream) && Seq.length array > 1 then 
                match ops.XObjectReference with 
                | XObjectReference.ByCopied ->
                    let stream = new PdfStream()
                    array |> Seq.cast<PdfStream> |> Seq.iter (fun s1 ->
                        stream.GetOutputStream().WriteBytes(s1.GetBytes()) |> ignore
                    )
                    this.EditContent (resources, stream, pageOrXObject)

                | XObjectReference.ByRef ->
                    let streams = array |> Seq.cast<PdfStream>
                    for stream in streams do
                        this.EditContent (resources, stream, pageOrXObject)
                        |> ignore
                    array


            else
                failwith "Not implemented"

        | :? PdfDictionary as map ->
            failwith "Not implemented"
        | _ -> failwith "Not implemented"



[<RequireQualifiedAccess>]
module PdfPage =
    let modifyIM (ops: PdfModifyOptions2) (selectorModifierMapping) (page: PdfPage) =

        let pageContents = page.GetPdfObject().Get(PdfName.Contents)
        let modify() =
            let document = page.GetDocument()
            let ocProps = ops.ParserCache.OCPropertiesCache.GetOrAdd(true, valueFactory = fun _ ->
                document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.OCProperties)
                |> FsOCProperties.Create
            )
            let editor = new PdfCanvasEditor(ocProps, selectorModifierMapping, ops, page, document)
            editor.Listener.SaveGS(editor.GetGraphicsState())


            let pageContents = 
                match pageContents with 
                | :? PdfArray as array ->
                    array 
                    |> Seq.cast<PdfStream>
                    |> Seq.map(fun stream ->
                        stream.Clone()
                    )
                    |> ResizeArray
                    |> PdfArray
                    |> fun array -> array :> PdfObject

                | :? PdfStream as stream -> stream.Clone()
                | _ -> failwithf "Invalid PageContents type %A" (pageContents.GetType())
                    



            let r = 
                match pageContents with 
                | null -> Seq.empty
                | _ ->
                    let resources = 
                        page.GetResources()
                        |> FsPdfResources

                    editor.InitClippingPath(page)
                    let fixedStream = editor.EditContent(resources, pageContents, PageOrXObject.Page)

                    page.Put(PdfName.Contents, fixedStream)
                    |> ignore

                    editor.ParsedRenderInfos

            editor.Listener.RestoreGS()

            r

        match ops.XObjectReference with 
        | XObjectReference.ByCopied -> modify()
        | XObjectReference.ByRef -> 
            let hash =  
                match pageContents with 
                | :? PdfArray as array ->
                    array 
                    |> Seq.cast<PdfStream>
                    |> Seq.map(fun stream ->
                        hashNumberOfPdfIndirectReference <| stream.GetIndirectReference() 
                    )
                    |> List.ofSeq

                | :? PdfStream as stream -> stream.GetIndirectReference() |> hashNumberOfPdfIndirectReference |> List.singleton
                | _ -> failwithf "Invalid PageContents type %A" (pageContents.GetType())

            let newContents =  
                ops.ParserCache.PageModifyCache.GetOrAddEx(hash, valueFactory = fun _ ->
                    modify()
                    |> ignore
                    let newContents = page.GetPdfObject().Get(PdfName.Contents)
                    let resources = page.GetResources()
                    newContents, resources
                )

            match newContents with 
            | GetOrAddResult.Get (newContents, resources) -> 
                page.Put(PdfName.Contents, newContents) |> ignore
                page.Put(PdfName.Resources, resources.GetPdfObject()) |> ignore
                Seq.empty

            | GetOrAddResult.Add _ -> Seq.empty



    let createClippingPathForLayer (ops: PdfModifyOptions2) layerName (page: PdfPage) operation =
        let ops =   
            { ops with 
                LayerOptions = Some (ModifyLayerOptions.AddClippingPath layerName)
                AddClippingPathForLayerOperation = Some operation }
        modifyIM ops Map.empty page

    let removeLayer (ops: PdfModifyOptions2) layerName (page: PdfPage) =
        let ops =   
            { ops with LayerOptions = Some (ModifyLayerOptions.RemoveLayer layerName) }
        modifyIM ops Map.empty  page


    let onlyVisibleLayer (ops: PdfModifyOptions2) (page: PdfPage) =
        let ops =   
            { ops with LayerOptions = Some (ModifyLayerOptions.OnlyVisibleLayer) }
        modifyIM ops Map.empty  page

    let modify ops (selectorModifierMapping) (page: PdfPage) =
        let renderInfos = 
            selectorModifierMapping
            |> Map.toList
            |> List.map snd
            |> List.map fst

        RenderInfoSelector.checkNonImageSelectorExists renderInfos

        modifyIM ops selectorModifierMapping page
        |> Seq.map (fun m -> m :?> IIntegratedRenderInfo)




         





