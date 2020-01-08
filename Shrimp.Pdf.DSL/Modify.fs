namespace Shrimp.Pdf.DSL
#nowarn "0104"
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Parser
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf
open Shrimp.Pdf.FileOperations
open System.IO
open iText.Kernel.Pdf

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

    static member SetStrokeColor(color: Color) : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            [
                PdfCanvas.setStrokeColor (color)
                PdfCanvas.writeOperatorRange args.Close
            ]

    static member SetFillColor(color: Color) : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            [
                PdfCanvas.setFillColor (color)
                PdfCanvas.writeOperatorRange args.Close
            ]

    static member SetFillAndStrokeColor(color: Color) : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            [
                PdfCanvas.setFillColor (color)
                PdfCanvas.setStrokeColor (color)
                PdfCanvas.writeOperatorRange args.Close
            ]

    static member AddRectangleToBound(mapping) : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            let border = IAbstractRenderInfo.getBound BoundGettingOptions.WithStrokeWidth args.CurrentRenderInfo
            [
                PdfCanvas.writeOperatorRange args.Close
                PdfCanvas.addRectangle border mapping
            ]

type _SelectorAndModifiers<'userState> =
    { Selector: Selector<'userState> 
      Modifiers: Modifier<'userState> list
      Name: string }



[<AutoOpen>]
module ModifyOperators =

    [<RequireQualifiedAccess>]
    type ModifyingAsyncWorker =
        | PageNumberEveryWorker of int
        | Sync

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
                        run flowModel (Flow.FileOperation (splitDocumentToMany (fun args -> { args with Override = true; ChunkSize = i})))


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
                                        run flowModel (Flow.Manipulate manipuate)
                                    )
                                    |> List.concat
                            }
                        )
                        |> Async.Parallel
                        |> Async.RunSynchronously
                        |> List.concat

                    let mergeFlow = 
                        Flow.FileOperation 
                            (mergeDocumentsInternal flowModel.File (document.Value))


                    runMany flowModels mergeFlow
                    |> ignore

                    for flowModel in flowModels do
                        File.Delete(flowModel.File)

                    flowModels.[0].UserState


                | _ -> failwith "Invalid token"
                    

            |> Manipulate

    /// async may doesn't make any sense for cpu bound computation?
    let modifyAsync (modifyingAsyncWorker, pageSelector, (modifiers: list<_SelectorAndModifiers<'userState>>)) =
        let names = 
            modifiers
            |> List.map (fun selectorAndModifier -> selectorAndModifier.Name)

        if names.Length <> (List.distinct names).Length then failwithf "Duplicated keys in SelectorAndModifiers %A" modifiers

        let asyncManiputation = 
            fun (totalNumberOfPages) (transformPageNum: PageNumber -> PageNumber) (flowModel: FlowModel<_>) (document: IntegratedDocument) ->
                IntegratedDocument.modify
                    (String.concat "\n" names)
                    pageSelector 
                    (
                        fun (pageNum, pdfPage) ->
                            modifiers 
                            |> List.mapi (fun i (selectAndModify) ->
                                let pageModifingArguments =
                                    { PageNum = (transformPageNum pageNum).Value
                                      UserState = flowModel.UserState
                                      Page = pdfPage
                                      TotalNumberOfPages = totalNumberOfPages }
                                ( {Index = i; Name = selectAndModify.Name }, 
                                    ( Selector.toRenderInfoSelector pageModifingArguments selectAndModify.Selector,
                                        Modifiers.toSelectionModifier pageModifingArguments selectAndModify.Modifiers)
                                )
                            )
                            |> Map.ofList
                    ) document

                
            
        Manipulate.runInAsync modifyingAsyncWorker asyncManiputation

    let modify (pageSelector, (modifiers: list<_SelectorAndModifiers<'userState>>)) =
        modifyAsync (ModifyingAsyncWorker.Sync, pageSelector, modifiers)