namespace Shrimp.Pdf.DSL

open Shrimp.Pdf.Colors

#nowarn "0104"
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Parser
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf
open System.IO
open iText.Kernel.Pdf

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

type _SelectionModifierFixmentArguments<'userState> =
    { Close: OperatorRange
      CurrentRenderInfo: IIntegratedRenderInfo 
      PageModifingArguments: PageModifingArguments<'userState> }

with 
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

type Modifier<'userState> = _SelectionModifierFixmentArguments<'userState> -> list<PdfCanvas -> PdfCanvas>
 
[<RequireQualifiedAccess>]
module private Modifiers =
    let toSelectionModifier (pageModifingArguments: PageModifingArguments<_>) (modifiers: Modifier<_> list) =
        fun (args: _SelectionModifierFixmentArguments) ->
            let actions = 
                let args  = 
                    { Close = args.Close
                      PageModifingArguments = pageModifingArguments
                      CurrentRenderInfo = args.CurrentRenderInfo }
                
                modifiers
                |> List.collect (fun factory -> factory args)
                
            actions


type Modifier =
    static member CancelFillAndStroke() : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            match args.CurrentRenderInfo.Tag with 
            | IntegratedRenderInfoTag.Path ->
                let newClose = 
                    { args.Close with 
                        Operator = new PdfLiteral("n")
                        Operands = ResizeArray [new PdfLiteral("n") :> PdfObject]}
                [
                    PdfCanvas.writeOperatorRange newClose
                ]

            | IntegratedRenderInfoTag.Text ->
                [
                    PdfCanvas.setTextRenderingMode(TextRenderingMode.INVISIBLE) 
                    PdfCanvas.writeOperatorRange args.Close
                ]

    static member CancelStroke() : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            match args.CurrentRenderInfo.Tag with 
            | IntegratedRenderInfoTag.Path ->
                let newOperatorName =
                    match args.Close.Operator.ToString() with 
                    | "b" -> "f"
                    | "B" -> "F"
                    | "b*" -> "f*"
                    | "B*" -> "f*"
                    | operatorName -> 
                        match operatorName with 
                        | "f" | "F" | "f*" -> operatorName
                        | "s" | "S" -> "n"
                        | _ -> failwithf "Unexcepted opetor %s" operatorName


                let newClose = 
                    { args.Close with 
                        Operator = new PdfLiteral(newOperatorName)
                        Operands = ResizeArray [new PdfLiteral(newOperatorName) :> PdfObject]}
                [
                    PdfCanvas.writeOperatorRange newClose
                ]

            | IntegratedRenderInfoTag.Text ->
                let newTextRenderingMode =
                    match args.CurrentRenderInfo.Value.GetGraphicsState().GetTextRenderingMode() with 
                    | PdfCanvasConstants.TextRenderingMode.FILL_STROKE -> PdfCanvasConstants.TextRenderingMode.FILL
                    | PdfCanvasConstants.TextRenderingMode.FILL -> PdfCanvasConstants.TextRenderingMode.FILL
                    | PdfCanvasConstants.TextRenderingMode.STROKE -> TextRenderingMode.INVISIBLE
                    | textRenderMode -> 
                        Logger.unSupportedTextRenderMode textRenderMode
                        TextRenderingMode.INVISIBLE

                [
                    PdfCanvas.setTextRenderingMode(newTextRenderingMode) 
                    PdfCanvas.writeOperatorRange args.Close
                ]


    static member CancelFill() : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            match args.CurrentRenderInfo.Tag with 
            | IntegratedRenderInfoTag.Path ->
                let newOperatorName =
                    match args.Close.Operator.ToString() with 
                    | "b" -> "s"
                    | "B" -> "S"
                    | "b*" -> "s"
                    | "B*" -> "S"
                    | operatorName -> 
                        match operatorName with 
                        | "s" | "S" -> operatorName
                        | "f" | "F" | "f*" -> "n"
                        | _ -> failwithf "Unexcepted opetor %s" operatorName


                let newClose = 
                    { args.Close with 
                        Operator = new PdfLiteral(newOperatorName)
                        Operands = ResizeArray [new PdfLiteral(newOperatorName) :> PdfObject]}
                [
                    PdfCanvas.writeOperatorRange newClose
                ]

            | IntegratedRenderInfoTag.Text ->
                let newTextRenderingMode =
                    match args.CurrentRenderInfo.Value.GetGraphicsState().GetTextRenderingMode() with 
                    | TextRenderingMode.FILL_STROKE -> TextRenderingMode.STROKE
                    | TextRenderingMode.STROKE -> TextRenderingMode.STROKE
                    | TextRenderingMode.FILL -> TextRenderingMode.INVISIBLE
                    | textRenderMode -> 
                        Logger.unSupportedTextRenderMode textRenderMode
                        TextRenderingMode.INVISIBLE

                [
                    PdfCanvas.setTextRenderingMode(newTextRenderingMode) 
                    PdfCanvas.writeOperatorRange args.Close
                ]


    static member ReplaceColor(picker: (Color -> Color option), ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) : Modifier<'userState> =
        let fillOrStrokeModifyingOptions = defaultArg fillOrStrokeModifyingOptions FillOrStrokeModifingOptions.FillAndStroke

        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            [
                if IAbstractRenderInfo.hasFill args.CurrentRenderInfo 
                then 
                    match fillOrStrokeModifyingOptions, picker (args.CurrentRenderInfo.Value.GetFillColor()) with
                    | FillOrStrokeModifingOptions.Stroke, _ 
                    | _, None -> ()
                    | FillOrStrokeModifingOptions.Fill, Some (newColor) 
                    | FillOrStrokeModifingOptions.FillAndStroke, Some (newColor) ->
                        PdfCanvas.setFillColor(newColor)


                if IAbstractRenderInfo.hasStroke args.CurrentRenderInfo 
                then 
                    match fillOrStrokeModifyingOptions, picker (args.CurrentRenderInfo.Value.GetStrokeColor()) with
                    | FillOrStrokeModifingOptions.Fill, _ 
                    | _, None -> ()
                    | FillOrStrokeModifingOptions.Stroke, Some (newColor) 
                    | FillOrStrokeModifingOptions.FillAndStroke, Some (newColor) ->
                        PdfCanvas.setStrokeColor(newColor)

                PdfCanvas.writeOperatorRange args.Close
            ]


    static member ReplaceColor(colorKeyValuePairs: list<Color * Color>, ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) : Modifier<'userState> =
        let originColors = colorKeyValuePairs |> List.map fst
        let newColors = colorKeyValuePairs |> List.map snd
        Modifier.ReplaceColor(
            ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
            picker =
                fun color ->
                    match Colors.tryFindIndex color originColors with 
                    | Some index -> Some newColors.[index]
                    | None -> None
        )

    static member InvertColors(?fillOrStrokeModifyingOptions) =
        Modifier.ReplaceColor(
            ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
            picker = 
                fun color ->
                    (FsValueColor.OfItextColor color)
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
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            let border = IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithStrokeWidth args.CurrentRenderInfo
            [
                PdfCanvas.writeOperatorRange args.Close
                PdfCanvas.addRectangle border mapping
            ]

type SelectorAndModifiersRecord<'userState> =
    { Name: string 
      Selector: Selector<'userState> 
      Modifiers: Modifier<'userState> list }

type SelectorAndModifiersRecordEx<'userState> =
    { Name: string 
      Selector: Selector<'userState> 
      Modifiers: Modifier<'userState> list
      PageInfosValidation: PageInfosValidation 
      Paramters: list<string * string>}

type SelectorAndModifiers<'userState>(name, selector: Selector<'userState>, modifiers: Modifier<'userState> list, ?parameters: list<string * string>, ?pageInfosValidation) =
    let pageInfosValidation = defaultArg pageInfosValidation PageInfosValidation.ignore
    
    member x.Name = name

    member x.Selector = selector

    member x.Modifiers = modifiers

    member x.Paramters = parameters

    member x.PageInfosValidation = pageInfosValidation

[<AutoOpen>]
module ModifyOperators =

    [<RequireQualifiedAccess>]
    type ModifyingAsyncWorker =
        | PageNumberEveryWorker of int
        | Sync

    [<RequireQualifiedAccess>]
    module IntegratedDocument =
        type private Modifier = _SelectionModifierFixmentArguments -> list<PdfCanvas -> PdfCanvas>
        
        let modify (pageSelector: PageSelector) (selectorModifierPageInfosValidationMappingFactory: (PageNumber * PdfPage) -> Map<SelectorModiferToken, RenderInfoSelector * Modifier * (PageNumber -> IIntegratedRenderInfo seq -> unit)>) (document: IntegratedDocument) =
            let selectedPageNumbers = document.Value.GetPageNumbers(pageSelector) 
            let totalNumberOfPages = document.Value.GetNumberOfPages()
            for i = 1 to totalNumberOfPages do
                if List.contains i selectedPageNumbers then
                    let page = document.Value.GetPage(i)
                    let selectorModifierMapping = selectorModifierPageInfosValidationMappingFactory (PageNumber i, page)
                    for pair in selectorModifierMapping do
                        let renderInfoSelector, modifier, pageInfosValidation = pair.Value
                        pageInfosValidation (PageNumber i) (PdfPage.modify (Map.ofList[pair.Key, (renderInfoSelector, modifier)]) page)
    

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
    let private modify (modifyingAsyncWorker, pageSelector, (selectorAndModifiersList: list<SelectorAndModifiers<'userState>>)) =
        let names = 
            selectorAndModifiersList
            |> List.map (fun selectorAndModifier -> selectorAndModifier.Name)

        if names.Length <> (List.distinct names).Length then failwithf "Duplicated keys in SelectorAndModifiers %A" selectorAndModifiersList

        let asyncManiputation (selectorAndModifiersList: SelectorAndModifiers<_> list) = 
            fun (totalNumberOfPages) (transformPageNum: PageNumber -> PageNumber) (flowModel: FlowModel<_>) (document: IntegratedDocument) ->
                IntegratedDocument.modify
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
                                      Modifiers.toSelectionModifier pageModifingArguments selectorAndModifiers.Modifiers,
                                      selectorAndModifiers.PageInfosValidation.Value
                                    )
                                )
                            )
                            |> Map.ofList
                    ) document

        selectorAndModifiersList
        |> List.map (fun modifier ->
            let parameters =
                match modifier.Paramters with 
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
        



    type Modify =
        static member Create(pageSelector, selectorAndModifiersList: SelectorAndModifiers<'userState> list, ?modifyingAsyncWorker) =
            let modifyingAsyncWorker = defaultArg modifyingAsyncWorker ModifyingAsyncWorker.Sync

            modify(modifyingAsyncWorker, pageSelector, selectorAndModifiersList)

        static member Create (pageSelector, selectorAndModifiersList: SelectorAndModifiersRecord<'userState> list, ?modifyingAsyncWorker) =
            let modifyingAsyncWorker = defaultArg modifyingAsyncWorker ModifyingAsyncWorker.Sync
            let selectorAndModifiersList =
                selectorAndModifiersList
                |> List.map (fun m ->
                    SelectorAndModifiers(m.Name, m.Selector, m.Modifiers)
                )
            modify(modifyingAsyncWorker, pageSelector, selectorAndModifiersList)

        static member Create (pageSelector, selectorAndModifiersList: SelectorAndModifiersRecordEx<'userState> list, ?modifyingAsyncWorker) =
            let modifyingAsyncWorker = defaultArg modifyingAsyncWorker ModifyingAsyncWorker.Sync
            let selectorAndModifiersList =
                selectorAndModifiersList
                |> List.map (fun m ->
                    SelectorAndModifiers(m.Name, m.Selector, m.Modifiers, m.Paramters, m.PageInfosValidation)
                )
            modify(modifyingAsyncWorker, pageSelector, selectorAndModifiersList)



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
    static member ReplaceColors (picker, ?options: Modify_ReplaceColors_Options, ?nameAndParamters: NameAndParamters) =
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
        
        let nameAndParamters =
            defaultArg 
                nameAndParamters 
                { Name = "ReplaceColors"
                  Parameters = [ "Modify_ReplaceColors_Options" => options.ToString()] }

        Modify.Create(
            options.PageSelector,
            [
                SelectorAndModifiers(
                    name = nameAndParamters.Name,
                    selector =
                        (
                            let colorInfo = Info.ColorIs(fillOrStrokeOptions, fun color -> 
                                match picker color with 
                                | Some _ -> true
                                | None -> false
                            )

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
                        Modifier.ReplaceColor(
                            fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
                            picker = picker
                        )
                    ],
                    pageInfosValidation = pageInfosValidation,
                    parameters = nameAndParamters.Parameters
                )
            ]
        )


    static member ReplaceColors (originColors: Color list, targetColor: Color, ?options: Modify_ReplaceColors_Options) =
        let options = defaultArg options Modify_ReplaceColors_Options.DefaultValue
        let nameAndParamters =
            { Name = "ReplaceColors"
              Parameters = 
                ["originColors" => originColors.ToString() 
                 "targetColor" => targetColor.ToString() 
                 "options" => options.ToString() ]
            }
        

        Modify.ReplaceColors(
            options = options,
            nameAndParamters = nameAndParamters,
            picker = fun color ->
                if Colors.contains color originColors
                then Some targetColor
                else None
        ) :> Manipulate<_, _>


    static member ReplaceColors (originColors: Color list, targetColor: FsSeparation, ?options: Modify_ReplaceColors_Options) =
        Manipulate.Factory(fun flowModel splitDocument ->
            let seperationColor = splitDocument.Value.GetOrCreateColor(ResourceColor.CustomSeparation targetColor)
            Modify.ReplaceColors(originColors, seperationColor, ?options = options)
        )

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


    static member InvertColors(?predicate: Color -> bool, ?options: Modify_ReplaceColors_Options) =
        let options = defaultArg options Modify_ReplaceColors_Options.DefaultValue

        let nameAndParamters =
            { Name = "InvertColors" 
              Parameters = 
                ["predicate" => predicate.ToString()
                 "options" => options.ToString() ]
            }

        let predicate = defaultArg predicate (fun _ -> true)

        Modify.ReplaceColors(
            options = options,
            picker = 
                fun color ->
                    if predicate color 
                    then
                        (FsValueColor.OfItextColor color)
                        |> FsValueColor.Invert
                        |> FsValueColor.ToItextColor
                        |> Some
                    else None
        )