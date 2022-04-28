namespace Shrimp.Pdf.DSL

open Shrimp.Pdf.Colors
open Newtonsoft.Json
open iText.Kernel.Pdf.Xobject
open iText.IO.Image

#nowarn "0104"
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Parser
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf
open System.IO
open iText.Kernel.Pdf
open Shrimp.FSharp.Plus
open Shrimp.FSharp.Plus.Text
open iText.Kernel.Geom

type StrokeWidthIncrenment =
    { Value: ``ufloat>0`` 
      LineJoinStyle: int }
with 
    static member Create(value, ?lineJoinStyle) =
        { Value = ``ufloat>0`` value 
          LineJoinStyle = defaultArg lineJoinStyle LineJoinStyle.MITER }

type FillOrStrokeModifingOptions =
    | Stroke = 0
    | Fill = 1
    | FillAndStroke = 2

type _SelectionModifierAddNewArguments<'userState> =
    { CurrentRenderInfo: IIntegratedRenderInfo  
      PageModifingArguments: PageModifingArguments<'userState> }

with 
    member x.PageNum = x.PageModifingArguments.PageNum

    member x.UserState = x.PageModifingArguments.UserState

    member x.Page = x.PageModifingArguments.Page

type _SelectionModifierFixmentArgumentsIM<'userState> =
    { CurrentRenderInfoIM: IIntegratedRenderInfoIM 
      PageModifingArguments: PageModifingArguments<'userState>
      ConcatedTextInfos: seq<IntegratedTextRenderInfo> }
with 
    member x.PageNum = x.PageModifingArguments.PageNum

    member x.UserState = x.PageModifingArguments.UserState

    member x.Page = x.PageModifingArguments.Page

type _SelectionModifierFixmentArguments<'userState> =
    { CurrentRenderInfo: IIntegratedRenderInfo 
      PageModifingArguments: PageModifingArguments<'userState>
      ConcatedTextInfos: seq<IntegratedTextRenderInfo>}

with 
    member internal x.Tag = x.CurrentRenderInfo.Tag

    member x.PageNum = x.PageModifingArguments.PageNum

    member x.UserState = x.PageModifingArguments.UserState

    member x.Page = x.PageModifingArguments.Page


type PageInfosValidation = PageInfosValidation of (PageNumber -> seq<IIntegratedRenderInfo> -> unit)
with 
    member x.Value =
        let (PageInfosValidation value) = x
        value

[<RequireQualifiedAccess>]
module PageInfosValidation =
    let atLeastOne =
        fun (pageNumber) infos ->
            let length = (Seq.length infos)
            if length = 0 then failwith "Didn't selector any page infos"

        |> PageInfosValidation

    let ignore = 
        fun _ _ -> ()
        |> PageInfosValidation

type PageInfosValidationIM = PageInfosValidationIM of (PageNumber -> seq<IIntegratedRenderInfoIM> -> unit)
with 
    member x.Value =
        let (PageInfosValidationIM value) = x
        value

[<RequireQualifiedAccess>]
module PageInfosValidationIM =
    let atLeastOne =
        fun (pageNumber) infos ->
            let length = (Seq.length infos)
            if length = 0 then failwith "Didn't selector any page infos"

        |> PageInfosValidationIM

    let ignore = 
        fun _ _ -> ()
        |> PageInfosValidationIM

    let internal ofPageInfosValidation (pageInfosValidation: PageInfosValidation) =
        PageInfosValidationIM(fun pageNumber infos ->
            let infos = infos |> Seq.map(fun info -> info :?> IIntegratedRenderInfo)
            pageInfosValidation.Value pageNumber infos
            
        )



type Modifier<'userState> = _SelectionModifierFixmentArguments<'userState> -> ModifierPdfCanvasActions
type ModifierIM<'userState> = _SelectionModifierFixmentArgumentsIM<'userState> -> ModifierPdfCanvasActions
 
[<RequireQualifiedAccess>]
module private Modifier =
    let toIM (modifier: Modifier<_>): ModifierIM<_> =
        fun (args: _SelectionModifierFixmentArgumentsIM<'userState>) ->
            { CurrentRenderInfo = args.CurrentRenderInfoIM :?> IIntegratedRenderInfo
              PageModifingArguments = args.PageModifingArguments
              ConcatedTextInfos = args.ConcatedTextInfos }
            |> modifier


[<RequireQualifiedAccess>]
module private Modifiers =
    let private toSelectionModifierCommon (pageModifingArguments: PageModifingArguments<_>) (modifiers: _ list) fArgs  =
        fun (args: _SelectionModifierFixmentArguments) ->
            let actions = 
                match args.CurrentRenderInfo with 
                | IIntegratedRenderInfoIM.Vector vectorRenderInfo ->
                    let args  = fArgs args

                    modifiers
                    |> List.mapi (fun i factory -> 
                        let actions: ModifierPdfCanvasActions = factory args
                        actions
                    )
                    |> ModifierPdfCanvasActions.ConcatOrKeep (vectorRenderInfo.Tag)
                
                | IIntegratedRenderInfoIM.Pixel pixelRenderInfo  ->
                    match modifiers with 
                    | [modifier] ->
                        let args  = fArgs args
                        modifier args
                    | _ -> failwithf "Multiple modifiers are not supported when render info tag is %A" args.CurrentRenderInfo.TagIM

            actions

    let toSelectionModifier (pageModifingArguments: PageModifingArguments<_>) (modifiers: Modifier<_> list) =
        toSelectionModifierCommon pageModifingArguments modifiers (fun args ->
            { PageModifingArguments = pageModifingArguments
              CurrentRenderInfo = args.CurrentRenderInfo :?> IIntegratedRenderInfo
              ConcatedTextInfos = args.ConcatedTextInfos }
        )
      

    let toSelectionModifierIM (pageModifingArguments: PageModifingArguments<_>) (modifiers: ModifierIM<_> list) =
        toSelectionModifierCommon pageModifingArguments modifiers (fun args ->
            { PageModifingArguments = pageModifingArguments
              CurrentRenderInfoIM = args.CurrentRenderInfo
              ConcatedTextInfos = args.ConcatedTextInfos }
        )
       

open Constants.Operators

[<StructuredFormatDisplay("{LoggingText}")>]
type ColorMapping =
    { OriginColors: AlternativeFsColor al1List 
      NewColor: NullablePdfCanvasColor }
        
with 
    static member WhiteTo(newColor) =
        { OriginColors = AtLeastOneList.Create AlternativeFsColor.Whites
          NewColor =  NullablePdfCanvasColor.OfPdfCanvasColor newColor }

    member x.LoggingText =
        let colors = x.OriginColors
        let loggingText =
            match colors with 
            | AtLeastOneList.One color -> color.LoggingText
            | AtLeastOneList.Many strokeColors -> 
                let colorsText =
                    strokeColors.AsList
                    |> List.map (fun m -> ValidFileName.Create m.LoggingText )
                    |> AtLeastOneList.Create
                    |> ConcactedText

                sprintf "multiple %d[%s]" 
                    strokeColors.Length 
                    (
                        colorsText.RangeText(50)
                    )
        sprintf "OriginColors: %s, NewColor:%O" loggingText x.NewColor
        

    override x.ToString() = x.LoggingText
        

type ColorMappings = ColorMappings of ColorMapping al1List
with 
    member x.Add(newColorMapping) =
        let (ColorMappings colorMapping) = x
        colorMapping.Add [newColorMapping]
        |> ColorMappings

    static member WhiteTo(newColor) =
        ColorMapping.WhiteTo(newColor)
        |> List.singleton
        |> AtLeastOneList.Create
        |> ColorMappings


    member internal x.AsPicker() =
        let (ColorMappings colorMappings) = x
        fun (fsColor: FsColor) ->
            colorMappings.AsList
            |> List.tryPick(fun colorMapping ->
                let colors = List.map FsColor.OfAlternativeFsColor colorMapping.OriginColors.AsList
                match FsColors.contains fsColor colors with 
                | true -> Some colorMapping.NewColor
                    
                | false -> None
            )
        



type Modifier =

    static member SetFontAndSize(font, size: float, ?alignment: XEffect) : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            let alignment = defaultArg alignment XEffect.Left
            match args.Tag with 
            | IntegratedRenderInfoTag.Path ->
                ModifierPdfCanvasActions.Keep(args.Tag)

            | IntegratedRenderInfoTag.Text ->
                let info = args.CurrentRenderInfo :?> IntegratedTextRenderInfo
                let transformedFontSize = ITextRenderInfo.toTransformedFontSize size info 
                let text = ITextRenderInfo.getText info
                let difference() =
                    let originWidth = ITextRenderInfo.getWidth info
                    let currentWidth =
                        let widthUnits = PdfFont.calcLineWidthUnits text font
                        List.max widthUnits * size
                    currentWidth - originWidth

                let originTransform() = 
                    info.TextRenderInfo.GetTextMatrix()
                    |> AffineTransform.ofMatrix

                { Actions =
                    [
                        match alignment with 
                        | XEffect.Left -> ()
                        | XEffect.Middle ->
                            let offset = -(difference() / 2.) * transformedFontSize / size
                            let transform = originTransform()
                            transform.Translate(offset, 0.)
                            PdfCanvas.setTextMatrixByTransform(AffineTransformRecord.ofAffineTransform transform)

                        | XEffect.Right ->
                            let offset = difference()
                            let transform = 
                                { AffineTransformRecord.DefaultValue with 
                                    TranslateX = offset }
                            PdfCanvas.setTextMatrixByTransform(transform)



                        PdfCanvas.setFontAndSize(font, float32 transformedFontSize)
                        //PdfCanvas.writeOperatorRange args.Close
                    ]
                    
                  Close = 
                    CloseOperatorUnion.Text(
                        { Fill = CloseOperator.Keep 
                          Stroke = CloseOperator.Keep
                          Text = Some text }
                    )
                  SuffixActions = []
                }


    static member SetFontAndSize(font, size: float, ?alignment) : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            let doc = args.Page.GetDocument() :?> PdfDocumentWithCachedResources
            let font = doc.GetOrCreatePdfFont(font)
            Modifier.SetFontAndSize(font, size, ?alignment = alignment) args

    static member CancelFillAndStroke() : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            ModifierPdfCanvasActions.CreateCloseOperator(
                tag = args.Tag,
                fill = CloseOperator.Close,
                stroke = CloseOperator.Close
            )
         
         

    static member OpenFill() : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            ModifierPdfCanvasActions.CreateCloseOperator(
                tag = args.Tag,
                fill = CloseOperator.Open
            )
         

    static member OpenStroke() : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            ModifierPdfCanvasActions.CreateCloseOperator(
                tag = args.Tag,
                stroke = CloseOperator.Open
            )
        

    static member CancelStroke() : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            ModifierPdfCanvasActions.CreateCloseOperator(
                tag = args.Tag,
                stroke = CloseOperator.Close
            )
       


    static member CancelFill() : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            ModifierPdfCanvasActions.CreateCloseOperator(
                tag = args.Tag,
                fill = CloseOperator.Close
            )
        


    static member internal ReplaceColorCommon(picker: (_ -> FsColor -> Choice<Color, NullablePdfCanvasColor> option), ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) : Modifier<'userState> =
        let fillOrStrokeModifyingOptions = defaultArg fillOrStrokeModifyingOptions FillOrStrokeModifingOptions.FillAndStroke

        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            let getOrCreateColor (color: Choice<Color, NullablePdfCanvasColor>) =
                match color with 
                | Choice.Choice1Of2 color -> Some color
                | Choice.Choice2Of2 color ->
                    let doc = args.Page.GetDocument() :?> PdfDocumentWithCachedResources
                    match color with 
                    | NullablePdfCanvasColor.Non -> None
                    | NullablePdfCanvasColor.PdfCanvasColor color ->
                        doc.GetOrCreateColor(color)
                        |> Some

            let cancelOperation1 = 
                if IAbstractRenderInfo.hasFill args.CurrentRenderInfo 
                then 
                    match fillOrStrokeModifyingOptions, picker args (args.CurrentRenderInfo.Value.GetFillColor() |> FsColor.OfItextColor) with
                    | FillOrStrokeModifingOptions.Stroke, _ 
                    | _, None -> ModifierPdfCanvasActions.Keep(args.Tag)
                    | FillOrStrokeModifingOptions.Fill, Some (newColor) 
                    | FillOrStrokeModifingOptions.FillAndStroke, Some (newColor) ->
                        match getOrCreateColor newColor with 
                        | Some newColor ->
                            [PdfCanvas.setFillColor(newColor)]
                            |> ModifierPdfCanvasActions.createActions args.Tag
                        | None ->
                            ModifierPdfCanvasActions.CreateCloseOperator(args.Tag, fill = CloseOperator.Close)
                else ModifierPdfCanvasActions.Keep(args.Tag)
                         
            let cancelOperation2 = 

                if IAbstractRenderInfo.hasStroke args.CurrentRenderInfo 
                then 
                    match fillOrStrokeModifyingOptions, picker args (args.CurrentRenderInfo.Value.GetStrokeColor() |> FsColor.OfItextColor) with
                    | FillOrStrokeModifingOptions.Fill, _ 
                    | _, None -> ModifierPdfCanvasActions.Keep(args.Tag)
                    | FillOrStrokeModifingOptions.Stroke, Some (newColor) 
                    | FillOrStrokeModifingOptions.FillAndStroke, Some (newColor) ->
                        match getOrCreateColor newColor with 
                        | Some newColor ->
                            [PdfCanvas.setStrokeColor(newColor)]
                            |> ModifierPdfCanvasActions.createActions args.Tag

                        | None -> 
                            ModifierPdfCanvasActions.CreateCloseOperator(args.Tag, stroke = CloseOperator.Close)
                else ModifierPdfCanvasActions.Keep(args.Tag)

            ModifierPdfCanvasActions.ConcatOrKeep args.Tag [cancelOperation1; cancelOperation2]

    static member private ReplaceColorEx(picker: (_ -> FsColor -> Color option), ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) : Modifier<'userState> =
        Modifier.ReplaceColorCommon((fun args color -> picker args color |> Option.map Choice1Of2), ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions)

    static member ReplaceColorNullable(picker: (_ -> FsColor -> NullablePdfCanvasColor option), ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) : Modifier<'userState> =
        Modifier.ReplaceColorCommon(
            picker = (fun info color -> picker info color |> Option.map Choice2Of2),
            ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions
        )


    static member ReplaceColor(picker: (FsColor -> Color option), ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) : Modifier<'userState> =
        Modifier.ReplaceColorCommon(
            picker = (fun info color -> picker color |> Option.map Choice1Of2),
            ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions
        )

    static member ReplaceColors(colorMappings: ColorMappings, ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) =
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            let doc = args.Page.GetDocument() :?> PdfDocumentWithCachedResources
            let picker = 
                colorMappings.AsPicker() >> Option.map(fun color ->
                color
            )

            Modifier.ReplaceColorNullable((fun args color -> picker color), ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions)
            

    static member ReplaceAlternativeColor(picker: (AlternativeFsColor -> Color option), ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) =
        let picker (fsColor: FsColor) =
            match fsColor.AsAlternativeFsColor with 
            | Some color -> picker color
            | None -> None

        Modifier.ReplaceColor(picker, ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions)

    static member ReplaceColor(colorKeyValuePairs: list<Color * Color>, ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) : Modifier<'userState> =
        let originColors = 
            colorKeyValuePairs 
            |> List.map fst
            |> List.map (FsColor.OfItextColor)

        let newColors = 
            colorKeyValuePairs 
            |> List.map snd


        Modifier.ReplaceColor(
            ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
            picker =
                fun color ->
                    let color = color
                    match FsColors.tryFindIndex color originColors with 
                    | Some index -> Some newColors.[index]
                    | None -> None
        )

    static member InvertColors(?fillOrStrokeModifyingOptions) =
        Modifier.ReplaceColor(
            ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
            picker = 
                fun color ->
                    match color.AlterColor with 
                    | None -> None
                    | Some color ->
                        (color)
                        |> FsValueColor.Invert
                        |> FsValueColor.ToItextColor
                        |> Some
        )
    
    static member SetColor(color: Color, ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) : Modifier<'userState> =
        Modifier.ReplaceColor(
            ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
            picker = fun _ -> Some color
        )

    static member SetFillColor(color: Color) : Modifier<'userState> =
        Modifier.ReplaceColor(
            fillOrStrokeModifyingOptions = FillOrStrokeModifingOptions.Fill,
            picker = fun _ -> Some color
        )

    static member SetStrokeColor(color: Color) : Modifier<'userState> =
        Modifier.ReplaceColor(
            fillOrStrokeModifyingOptions = FillOrStrokeModifingOptions.Stroke,
            picker = fun _ -> Some color
        )



    static member AddRectangleToBound(mapping) : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            let border = IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithStrokeWidth args.CurrentRenderInfo

            ModifierPdfCanvasActions.createSuffix 
                args.Tag 
                [
                    PdfCanvas.addRectangle border mapping
                ]
                

    static member ExpandStrokeWidth(targetColor: PdfCanvasColor, width: ``ufloat>0``, ?lineJoinStyle, ?overprint) =
        let width = width.Value
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            let info =  args.CurrentRenderInfo

            let setStroke(width) =
                [
                    PdfCanvas.setLineWidth width
                    PdfCanvas.SetStrokeColor (NullablePdfCanvasColor.OfPdfCanvasColor targetColor)
                    match defaultArg overprint false with 
                    | true -> PdfCanvas.setExtGState FsExtGState.StrokeOverprint
                    | false -> ()
                    match lineJoinStyle with 
                    | None -> ()
                    | Some lineJoinStyle -> PdfCanvas.setLineJoinStyle lineJoinStyle
                ]

            match info with 
            | If IIntegratedRenderInfo.isStrokeVisible ->
                match info with 
                | IIntegratedRenderInfo.Path pathInfo ->
                    let strokeWidth = pathInfo.PathRenderInfo.GetLineWidth()
                    setStroke (float strokeWidth +  width)

                | IIntegratedRenderInfo.Text textInfo ->
                    let strokeWidth = textInfo.TextRenderInfo.GetGraphicsState().GetLineWidth()
                    setStroke (float strokeWidth +  width)

                |> ModifierPdfCanvasActions.createActions args.Tag  

            | If IIntegratedRenderInfo.isFillVisible ->
                let addPrefix prefixActions (modifierPdfCanvasActions: ModifierPdfCanvasActions) = 
                    { modifierPdfCanvasActions with 
                        Actions = prefixActions @ modifierPdfCanvasActions.Actions
                    }

                match info with 
                | IIntegratedRenderInfo.Path pathInfo ->
                    addPrefix (setStroke (width)) (Modifier.OpenStroke() args)
                | IIntegratedRenderInfo.Text textInfo ->
                    addPrefix (setStroke (width)) (Modifier.OpenStroke() args)

            | _ -> 
                ModifierPdfCanvasActions.Keep(args.Tag)


    static member AddVarnish(varnish: FsSeparation, width) =
        Modifier.ExpandStrokeWidth(
            PdfCanvasColor.Separation varnish,
            width,
            overprint = true,
            lineJoinStyle = LineJoinStyle.ROUND
        )


type SelectorAndModifiersRecordIM<'userState> =
    { Name: string 
      Selector: Selector<'userState> 
      Modifiers: ModifierIM<'userState> list }

type SelectorAndModifiersRecord<'userState> =
    { Name: string 
      Selector: Selector<'userState> 
      Modifiers: Modifier<'userState> list }


type SelectorAndModifiersRecordEx<'userState> =
    { Name: string 
      Selector: Selector<'userState> 
      Modifiers: Modifier<'userState> list
      PageInfosValidation: PageInfosValidation 
      Parameters: list<string * string>}

type SelectorAndModifiers<'userState>(name, selector: Selector<'userState>, modifiers: Modifier<'userState> list, ?parameters: list<string * string>, ?pageInfosValidation) =
    let pageInfosValidation = defaultArg pageInfosValidation PageInfosValidation.ignore
    
    member x.Name = name

    member x.Selector = selector

    member x.Modifiers = modifiers

    member x.Parameters = parameters

    member x.PageInfosValidation = pageInfosValidation

type SelectorAndModifiersIM<'userState>(name, selector: Selector<'userState>, modifiers: ModifierIM<'userState> list, ?parameters: list<string * string>, ?pageInfosValidation) =
    let pageInfosValidation = defaultArg pageInfosValidation PageInfosValidationIM.ignore

    member x.Name = name

    member x.Selector = selector

    member x.Modifiers = modifiers

    member x.Parameters = parameters

    member x.PageInfosValidation = pageInfosValidation

type SelectorAndModifiers<'userState> with 
    member internal x.ToIM() =
        let modifiers =
            x.Modifiers
            |> List.map(Modifier.toIM)

        let pageInfosValidation =
            PageInfosValidationIM.ofPageInfosValidation x.PageInfosValidation

        SelectorAndModifiersIM(x.Name, x.Selector, modifiers, ?parameters = x.Parameters, pageInfosValidation = pageInfosValidation)


[<AutoOpen>]
module ModifyOperators =

    [<RequireQualifiedAccess>]
    type ModifyingAsyncWorker =
        | PageNumberEveryWorker of int
        | Sync

    [<RequireQualifiedAccess>]
    module IntegratedDocument =
        type private Modifier = _SelectionModifierFixmentArguments -> ModifierPdfCanvasActions
        
        let private modifyCommon (pageSelector: PageSelector) (selectorModifierPageInfosValidationMappingFactory) modify (document: IntegratedDocument) =
            let selectedPageNumbers = document.Value.GetPageNumbers(pageSelector) 
            let totalNumberOfPages = document.Value.GetNumberOfPages()
            for i = 1 to totalNumberOfPages do
                if List.contains i selectedPageNumbers then
                    let page = document.Value.GetPage(i)
                    let selectorModifierMapping: Map<_, _> = selectorModifierPageInfosValidationMappingFactory (PageNumber i, page)
                    for pair in selectorModifierMapping do
                        let renderInfoSelector, modifier, pageInfosValidation = pair.Value
                        pageInfosValidation (PageNumber i) (modify (Map.ofList[pair.Key, (renderInfoSelector, modifier)]) page)
    

        let modify (pageSelector: PageSelector) (selectorModifierPageInfosValidationMappingFactory: (PageNumber * PdfPage) -> Map<SelectorModiferToken, RenderInfoSelector * Modifier * (PageNumber -> IIntegratedRenderInfo seq -> unit)>) (document: IntegratedDocument) =
            modifyCommon pageSelector selectorModifierPageInfosValidationMappingFactory PdfPage.modify document

        let modifyIM (pageSelector: PageSelector) (selectorModifierPageInfosValidationMappingFactory: (PageNumber * PdfPage) -> Map<SelectorModiferToken, RenderInfoSelector * Modifier * (PageNumber -> IIntegratedRenderInfoIM seq -> unit)>) (document: IntegratedDocument) =
            modifyCommon pageSelector selectorModifierPageInfosValidationMappingFactory PdfPage.modifyIM document
            


    [<RequireQualifiedAccess>]
    module private Manipulate =
        let runInAsync (modifyingAsyncWorker: ModifyingAsyncWorker) f  =
            fun (flowModel: FlowModel<_>) (document: IntegratedDocument) ->
                let totalNumberOfPages = document.Value.GetNumberOfPages()
                match modifyingAsyncWorker, totalNumberOfPages with 
                | ModifyingAsyncWorker.Sync, _ 
                | _ , 1 -> f totalNumberOfPages id flowModel document
                | ModifyingAsyncWorker.PageNumberEveryWorker i, _ when i < 1 -> failwith "Async worker number should bigger than 1"
                | ModifyingAsyncWorker.PageNumberEveryWorker i, j when i > 0 && j > 1 ->
                    failwithf "Async worker is disabled now"
                    let splitedFlowModels = 
                        runWithFlowModel flowModel (Flow.FileOperation (FileOperations.splitDocumentToMany (fun args -> { args with Override = true; ChunkSize = i})))


                    let flowModels = 
                        splitedFlowModels
                        |> List.chunkBySize i
                        |> List.mapi (fun groupIndex flowModels -> 
                            async {
                                return
                                    flowModels
                                    |> List.mapi (fun memberIndex flowModel ->
                                        let pageNum = groupIndex * i + (memberIndex + 1)
                                        let manipuate = (Manipulate (f totalNumberOfPages (fun _ -> PageNumber pageNum)))
                                        runWithFlowModel flowModel (Flow.Manipulate manipuate)
                                    )
                                    |> List.concat
                            }
                        )
                        |> Async.Parallel
                        |> Async.RunSynchronously
                        |> List.concat

                    let mergeFlow = 
                        Flow.FileOperation 
                            (FileOperations.mergeDocumentsInternal flowModel.File (document.Value))


                    runManyWithFlowModels flowModels mergeFlow
                    |> ignore

                    for flowModel in flowModels do
                        File.Delete(flowModel.File)

                    flowModels.[0].UserState


                | _ -> failwith "Invalid token"
            |> Manipulate



    /// async may doesn't make any sense for cpu bound computation?
    let private modifyIM (modifyingAsyncWorker, pageSelector, (selectorAndModifiersList: list<SelectorAndModifiersIM<'userState>>)) =
        let names = 
            selectorAndModifiersList
            |> List.map (fun selectorAndModifier -> selectorAndModifier.Name)

        if names.Length <> (List.distinct names).Length then failwithf "Duplicated keys in SelectorAndModifiers %A" selectorAndModifiersList

        let asyncManiputation (selectorAndModifiersList: SelectorAndModifiersIM<_> list) = 
            fun (totalNumberOfPages) (transformPageNum: PageNumber -> PageNumber) (flowModel: FlowModel<_>) (document: IntegratedDocument) ->
                IntegratedDocument.modifyIM
                    pageSelector 
                    (
                        fun (pageNum, pdfPage) ->
                            selectorAndModifiersList 
                            |> List.mapi (fun i (selectorAndModifiers) ->
                                let pageModifingArguments =
                                    { PageNum = (transformPageNum pageNum).Value
                                      UserState = flowModel.UserState
                                      Page = pdfPage
                                      TotalNumberOfPages = totalNumberOfPages }
                                ( { Name = selectorAndModifiers.Name }, 
                                    ( Selector.toRenderInfoSelector pageModifingArguments selectorAndModifiers.Selector,
                                      Modifiers.toSelectionModifierIM pageModifingArguments selectorAndModifiers.Modifiers,
                                      selectorAndModifiers.PageInfosValidation.Value
                                    )
                                )
                            )
                            |> Map.ofList
                    ) document

        selectorAndModifiersList
        |> List.map (fun modifier ->
            let parameters =
                match modifier.Parameters with 
                | None ->
                    [
                        "pageSelctor" => pageSelector.ToString()
                        "selector" => modifier.Selector.ToString()
                    ]

                | Some parameters ->
                    (
                        [
                            "pageSelctor" => pageSelector.ToString()
                            "selector" => modifier.Selector.ToString()
                        ]
                        @ parameters
                    )
                    |> List.distinctBy(fun (key, _) -> key.Trim().ToLower())


            Manipulate.runInAsync modifyingAsyncWorker (asyncManiputation [modifier])
            |> Manipulate.rename 
                modifier.Name
                parameters
        )
        |> Manipulate.Batch()
        ||>> ignore
        
    /// async may doesn't make any sense for cpu bound computation?
    let private modify (modifyingAsyncWorker, pageSelector, (selectorAndModifiersList: list<SelectorAndModifiers<'userState>>)) =
        modifyIM (modifyingAsyncWorker, pageSelector, selectorAndModifiersList |> List.map (fun m -> m.ToIM()))


    type Modify =
        static member Create(pageSelector, selectorAndModifiersList: SelectorAndModifiers<'userState> list, ?modifyingAsyncWorker) =
            let modifyingAsyncWorker = defaultArg modifyingAsyncWorker ModifyingAsyncWorker.Sync

            modify(modifyingAsyncWorker, pageSelector, selectorAndModifiersList)

        static member CreateIM(pageSelector, selectorAndModifiersList: SelectorAndModifiersIM<'userState> list, ?modifyingAsyncWorker) =
            let modifyingAsyncWorker = defaultArg modifyingAsyncWorker ModifyingAsyncWorker.Sync

            modifyIM(modifyingAsyncWorker, pageSelector, selectorAndModifiersList)


        static member Create (pageSelector, selectorAndModifiersList: SelectorAndModifiersRecord<'userState> list, ?modifyingAsyncWorker) =
            let modifyingAsyncWorker = defaultArg modifyingAsyncWorker ModifyingAsyncWorker.Sync
            let selectorAndModifiersList =
                selectorAndModifiersList
                |> List.map (fun m ->
                    SelectorAndModifiers(m.Name, m.Selector, m.Modifiers)
                )
            modify(modifyingAsyncWorker, pageSelector, selectorAndModifiersList)

        static member Create_Record (pageSelector, selectorAndModifiersList: SelectorAndModifiersRecord<'userState> list, ?modifyingAsyncWorker) =
            Modify.Create(pageSelector, selectorAndModifiersList, ?modifyingAsyncWorker = modifyingAsyncWorker)

        static member Create_RecordIM (pageSelector, selectorAndModifiersList: SelectorAndModifiersRecordIM<'userState> list, ?modifyingAsyncWorker) =
            let modifyingAsyncWorker = defaultArg modifyingAsyncWorker ModifyingAsyncWorker.Sync
            let selectorAndModifiersList =
                selectorAndModifiersList
                |> List.map (fun m ->
                    SelectorAndModifiersIM(m.Name, m.Selector, m.Modifiers)
                )
            modifyIM(modifyingAsyncWorker, pageSelector, selectorAndModifiersList)


        static member Create_RecordEx (pageSelector, selectorAndModifiersList: SelectorAndModifiersRecordEx<'userState> list, ?modifyingAsyncWorker) =
            let modifyingAsyncWorker = defaultArg modifyingAsyncWorker ModifyingAsyncWorker.Sync
            let selectorAndModifiersList =
                selectorAndModifiersList
                |> List.map (fun m ->
                    SelectorAndModifiers(m.Name, m.Selector, m.Modifiers, m.Parameters, m.PageInfosValidation)
                )
            modify(modifyingAsyncWorker, pageSelector, selectorAndModifiersList)

type FontAndSizeQuery [<JsonConstructor>] (?fontName, ?fontSize, ?fillColor, ?info_BoundIs_Args, ?textPattern) =
    inherit POCOBaseEquatable<string option * float option * FsColor option * Info_BoundIs_Args option * TextMatchingPattern option>(fontName, fontSize, fillColor, info_BoundIs_Args, textPattern)

    [<JsonProperty>]
    member x.FontName = fontName

    [<JsonProperty>]
    member x.FontSize = fontSize

    [<JsonProperty>]
    member x.FillColor = fillColor

    [<JsonProperty>]
    member x.Info_BoundIs_Args = info_BoundIs_Args

    [<JsonProperty>]
    member x.TextPattern = textPattern

    member x.LoggingText =
        let fontSize = 
            match fontSize with 
            | Some fontSize -> Some (fontSize.ToString())
            | None -> None

        let fillColor =
            match fillColor with 
            | Some fillColor -> Some fillColor.LoggingText
            | None -> None

        let textPattern = textPattern |> Option.map (fun m -> m.ToString())
        let info_BoundIs_Args = info_BoundIs_Args |> Option.map(fun m -> m.ToString())

            

        [ fontName 
          fontSize 
          fillColor
          textPattern
          info_BoundIs_Args ]
        |> List.choose id
        |> List.filter(fun m -> m <> "")
        |> String.concat " "

    member x.AsSelector() = 
        Selector.Text(fun args textInfo ->
            match textPattern with 
            | Some textPattern -> 
                let text = ITextRenderInfo.getText textInfo
                textPattern.Predicate text
            | None -> true
            &&
            match fontName, fontSize with 
            | Some fontName, Some fontSize -> TextInfo.FontNameAndSizeIs(fontName, fontSize) args textInfo
            | Some fontName, None -> TextInfo.FontNameIs(fontName) args textInfo
            | None, Some fontSize -> TextInfo.FontSizeIs(fontSize) args textInfo
            | None, None -> true 
            && ( match fillColor with 
                  | Some fillColor -> 
                     let fillColor' = textInfo.TextRenderInfo.GetFillColor() |> FsColor.OfItextColor
                     FsColor.equal (fillColor) fillColor'
                  | None -> true
               )
            &&
            match info_BoundIs_Args with 
            | Some args2 ->
                let args2 = args2 
                Info.DenseBoundIs(
                    args2.RelativePosition,
                    args2.AreaGettingOptions,
                    args2.BoundGettingStrokeOptions
                ) args textInfo
            | None -> true

        )


type NewFontAndSize [<JsonConstructor>] (?font: FsPdfFontFactory, ?fontSize: float, ?alignment) = 
    inherit POCOBase<FsPdfFontFactory option * float option * XEffect option>(font, fontSize, alignment)
    let __checkArgsValid =
        match font, fontSize with 
        | None, None -> failwith "Cannot Create NewFontAndSize, both font and fontSize are empty"
        | _ -> ()

    [<JsonProperty>]
    member x.Font = font

    [<JsonProperty>]
    member x.FontSize = fontSize

    [<JsonProperty>]
    member x.Alignment = alignment

    member x.LoggingText =
        let font =
            match font with 
            | Some font -> font.LoggingText |> Some 
            | None -> None

        let fontSize = 
            match fontSize with 
            | Some fontSize -> fontSize.ToString() |> Some 
            | None -> None

        let alignment = 
            match alignment with 
            | Some alignment -> alignment.ToString() |> Some 
            | None -> None

        [ font 
          fontSize
          alignment ]
        |> List.choose id
        |> String.concat " "

    member x.AsModifier(): Modifier<_> =
        fun args ->
            let font =
                match font with 
                | Some font -> 
                    let doc = args.Page.GetDocument() :?> PdfDocumentWithCachedResources
                    doc.GetOrCreatePdfFont(font)

                | None ->
                    let info = args.CurrentRenderInfo :?> IntegratedTextRenderInfo
                    info.TextRenderInfo.GetFont()

            let size =
                match fontSize with 
                | Some fontSize -> fontSize
                | None ->
                    let info = args.CurrentRenderInfo :?> IntegratedTextRenderInfo
                    ITextRenderInfo.getActualFontSize info

            Modifier.SetFontAndSize(font, size, ?alignment = alignment) args


type Modify_ReplaceColors_Options = 
    { FillOrStrokeOptions: FillOrStrokeOptions 
      PageInfosValidation: PageInfosValidation 
      SelectorTag: SelectorTag
      Info_BoundIs_Args: Info_BoundIs_Args option
      PageSelector: PageSelector }
with 
    static member DefaultValue =
        { FillOrStrokeOptions = FillOrStrokeOptions.FillOrStroke 
          PageInfosValidation = PageInfosValidation.ignore
          SelectorTag = SelectorTag.PathOrText 
          Info_BoundIs_Args = None
          PageSelector = PageSelector.All }

type Modify =
    static member ExpandStrokeWidth(colors, width: ``ufloat>0``, targetColor, ?lineJoinStyle, ?overprint) =
        Modify.Create(
            PageSelector.All,
            [
                SelectorAndModifiers(
                    name = sprintf "Expand stroke width %g" width.Value,
                    selector = (
                        Selector.PathOrText(
                            (Info.IsStrokeVisible() <&&> Info.StrokeColorIsOneOf colors)
                            <||>
                            (Info.IsFillVisible() <&&> Info.FillColorIsOneOf colors)
                        )
                    ),
                    modifiers = [
                        Modifier.ExpandStrokeWidth(targetColor, width, ?lineJoinStyle =lineJoinStyle, ?overprint = overprint)
                    ]
                )
            ]
        )

    static member AddVarnish(selector, varnish: FsSeparation, width: ``ufloat>0``) =
        Modify.Create(
            PageSelector.All,
            [
                SelectorAndModifiers(
                    name = sprintf "add varnish '%s' width %g" varnish.LoggingText width.Value,
                    selector = selector,
                    modifiers = [
                        Modifier.AddVarnish(varnish, width)
                    ]
                )
            ]
        )

    static member ReplaceColorsCommon (picker, ?options: Modify_ReplaceColors_Options, ?nameAndParameters: NameAndParameters) =
        let options = defaultArg options Modify_ReplaceColors_Options.DefaultValue
        let fillOrStrokeOptions = options.FillOrStrokeOptions

        let selectorTag = options.SelectorTag

        let pageInfosValidation = options.PageInfosValidation

        let fillOrStrokeModifyingOptions =
            match fillOrStrokeOptions with 
            | FillOrStrokeOptions.FillAndStroke 
            | FillOrStrokeOptions.FillOrStroke -> FillOrStrokeModifingOptions.FillAndStroke
            | FillOrStrokeOptions.Fill -> FillOrStrokeModifingOptions.Fill
            | FillOrStrokeOptions.Stroke -> FillOrStrokeModifingOptions.Stroke
        
        let nameAndParameters =
            defaultArg 
                nameAndParameters 
                { Name = "ReplaceColors"
                  Parameters = [ "Modify_ReplaceColors_Options" => options.ToString()] }

        Modify.Create(
            options.PageSelector,
            [
                SelectorAndModifiers(
                    name = nameAndParameters.Name,
                    selector =
                        (
                            let colorInfo = 
                                fun args info ->
                                    Info.ColorIs(fillOrStrokeOptions, fun color -> 
                                        match picker (args, info) color with 
                                        | Some _ -> true
                                        | None -> false
                                    ) args info

                            let info =
                                match options.Info_BoundIs_Args with 
                                | None -> colorInfo
                                | Some info_boundIs_args -> Info.BoundIs(info_boundIs_args) <&&> colorInfo

                            match selectorTag with 
                            | SelectorTag.PathOrText -> 
                                PathOrText info
                            | SelectorTag.Path -> Selector.Path info
                            | SelectorTag.Text -> Text info
                        ),
                    modifiers = [
                        Modifier.ReplaceColorCommon(
                            fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
                            picker = (fun args color ->
                                picker (args.PageModifingArguments, args.CurrentRenderInfo) color
                            )
                        )
                    ],
                    pageInfosValidation = pageInfosValidation,
                    parameters = nameAndParameters.Parameters
                )
            ]
        )
    static member ReplaceColorsEx (picker, ?options: Modify_ReplaceColors_Options, ?nameAndParameters: NameAndParameters) =
        Modify.ReplaceColorsCommon(picker = (fun info color -> picker info color |> Option.map Choice1Of2), ?options = options, ?nameAndParameters = nameAndParameters)

    static member ReplaceColors (picker, ?options: Modify_ReplaceColors_Options, ?nameAndParameters: NameAndParameters) =
        Modify.ReplaceColorsEx(picker = (fun info color -> picker color), ?options = options, ?nameAndParameters = nameAndParameters)
    
    static member ReplaceAlternativeColors (picker, ?options: Modify_ReplaceColors_Options, ?nameAndParameters: NameAndParameters) =
        let picker (fsColor: FsColor) =
            match fsColor.AsAlternativeFsColor with 
            | Some color -> picker color
            | None -> None

        Modify.ReplaceColors(picker, ?options = options, ?nameAndParameters = nameAndParameters)

    static member ReplaceColors (originColors: Color list, targetColor: Color, ?options: Modify_ReplaceColors_Options) =
        let originColors = 
            originColors
            |> List.map FsColor.OfItextColor
            
        let options = defaultArg options Modify_ReplaceColors_Options.DefaultValue
        let nameAndParameters =
            { Name = "ReplaceColors"
              Parameters = 
                ["originColors" => originColors.ToString() 
                 "targetColor" => targetColor.ToString() 
                 "options" => options.ToString() ]
            }
        

        Modify.ReplaceColors(
            options = options,
            nameAndParameters = nameAndParameters,
            picker = fun color ->
                if FsColors.contains color originColors
                then Some targetColor
                else None
        ) :> Manipulate<_, _>


    static member ReplaceColors (originColors: Color list, targetColor: FsSeparation, ?options: Modify_ReplaceColors_Options) =
        Manipulate.Factory(fun flowModel splitDocument ->
            let seperationColor = splitDocument.Value.GetOrCreateColor(ResourceColor.CustomSeparation targetColor)
            Modify.ReplaceColors(originColors, seperationColor, ?options = options)
        )

    static member ReplaceColors (colorMapping: ColorMappings, ?options: Modify_ReplaceColors_Options) =
        Manipulate.Factory(fun flowModel splitDocument ->
            let picker = 
                colorMapping.AsPicker() 
                
            let options = defaultArg options Modify_ReplaceColors_Options.DefaultValue
            let nameAndParameters =
                { Name = "ReplaceColors"
                  Parameters = 
                    ["colorMapping" => colorMapping.ToString()
                     "options" => options.ToString() ]
                }


            Modify.ReplaceColorsCommon((fun args color -> picker color |> Option.map Choice2Of2), options = options, nameAndParameters = nameAndParameters)
        )

    static member ReplaceColors1 (colorMapping: ColorMappings, ?options: Modify_ReplaceColors_Options) =
        let options =
            options 
            |> Option.map (fun options ->
                { options with  
                    PageInfosValidation = PageInfosValidation.atLeastOne }
            )

        Modify.ReplaceColors(colorMapping, ?options = options)

    static member ReplaceColors1 (originColors: Color list, targetColor: Color, ?options: Modify_ReplaceColors_Options) =
        let options =
            options 
            |> Option.map (fun options ->
                { options with  
                    PageInfosValidation = PageInfosValidation.atLeastOne }
            )

        Modify.ReplaceColors(originColors, targetColor, ?options = options)

    static member ReplaceColors1 (originColors: Color list, targetColor: FsSeparation, ?options: Modify_ReplaceColors_Options) =
        let options =
            options 
            |> Option.map (fun options ->
                { options with  
                    PageInfosValidation = PageInfosValidation.atLeastOne }
            )

        Modify.ReplaceColors(originColors, targetColor, ?options = options)


    static member InvertColors(?predicate: FsColor -> bool, ?options: Modify_ReplaceColors_Options) =
        let options = defaultArg options Modify_ReplaceColors_Options.DefaultValue

        let nameAndParameters =
            { Name = "InvertColors" 
              Parameters = 
                ["predicate" => predicate.ToString()
                 "options" => options.ToString() ]
            }

        let predicate = defaultArg predicate (fun _ -> true)

        Modify.ReplaceColors(
            options = options,
            picker = (
                fun color ->
                    if predicate color 
                    then
                        (color.AlterColor)
                        |> Option.map(
                            FsValueColor.Invert
                            >> FsValueColor.ToItextColor
                        )

                    else None
            ),
            nameAndParameters = nameAndParameters
        )

    static member MapFontAndSizeF(fOldFontAndSize: PageModifingArguments<_> -> FontAndSizeQuery, newFontAndSize: NewFontAndSize) =
        Modify.Create_RecordEx(
            PageSelector.All,
            selectorAndModifiersList = [
                { Name = "MapFonts"
                  Selector = Selector.Factory(fun args -> (fOldFontAndSize args).AsSelector())
                  Modifiers = 
                    [
                        newFontAndSize.AsModifier()
                    ]
                  PageInfosValidation = PageInfosValidation.ignore
                  Parameters = 
                    ["oldFontAndSize" => fOldFontAndSize.ToString()
                     "newFontAndSize" => newFontAndSize.ToString() ]
                }
            ]
        )

    static member MapFontAndSize(oldFontAndSize: FontAndSizeQuery, newFontAndSize: NewFontAndSize) =
        Modify.Create_RecordEx(
            PageSelector.All,
            selectorAndModifiersList = [
                { Name = "MapFonts"
                  Selector = Selector.Factory(fun args -> oldFontAndSize.AsSelector())
                  Modifiers = 
                    [
                        newFontAndSize.AsModifier()
                    ]
                  PageInfosValidation = PageInfosValidation.ignore
                  Parameters = 
                    ["oldFontAndSize" => oldFontAndSize.LoggingText
                     "newFontAndSize" => newFontAndSize.ToString() ]
                }
            ]
        )



    static member SplitTextLineToWords() =
        Modify.Create_Record(
            PageSelector.All,
            selectorAndModifiersList = [
                { SelectorAndModifiersRecord.Name = "split text line to words"
                  Selector = Text(fun _ _ -> true)
                  Modifiers = [
                    (fun args ->
                        let textInfos = List.ofSeq args.ConcatedTextInfos
                        let lastTextInfo = List.last textInfos
                        let actions =
                            match textInfos with 
                            | [textInfo] -> 
                                let matrix = textInfo.TextRenderInfo.GetTextMatrix()
                                [PdfCanvas.setTextMatrix matrix]   
                            | _ ->
                                textInfos
                                |> List.mapi (fun i textInfo ->
                                    let matrix = textInfo.TextRenderInfo.GetTextMatrix()
                                    let lastIndex = (textInfos.Length - 1)
                                    match i with 
                                    | 0 -> 
                                        [
                                            PdfCanvas.setTextMatrix matrix
                                            PdfCanvas.showText(ITextRenderInfo.getText textInfo)
                                        ]
                                    | EqualTo lastIndex -> 
                                        [
                                            PdfCanvas.setTextMatrix(matrix)
                                        ]
                                    | _ ->
                                        [
                                            PdfCanvas.setTextMatrix matrix
                                            PdfCanvas.showText(ITextRenderInfo.getText textInfo)
                                        ]
                                )
                                |> List.concat

                        { ModifierPdfCanvasActions.Actions = actions 
                          SuffixActions = []
                          Close = CloseOperatorUnion.CreateText(text = ITextRenderInfo.getText lastTextInfo)
                        }
                    )
                  ]
                  }
            ]
        )