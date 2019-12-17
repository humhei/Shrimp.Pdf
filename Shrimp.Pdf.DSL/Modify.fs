namespace Shrimp.Pdf.DSL
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Parser
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf
open Shrimp.Pdf.FileOperations
open System.IO

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




type Modifier<'userState> =
    | DropColor 
    | AddNew of list<_SelectionModifierAddNewArguments<'userState> -> list<PdfCanvas -> PdfCanvas>>
    | Fix of list<_SelectionModifierFixmentArguments<'userState> -> list<PdfCanvas -> PdfCanvas>>

 

[<RequireQualifiedAccess>]
module private Modifier =
    let toSelectionModifier (pageModifingArguments: PageModifingArguments<_>) (modifier: Modifier<_>) =
        match modifier with
        | Modifier.AddNew factorys ->
            fun (args: _SelectionModifierAddNewArguments) ->
                let actions = 
                    let args = 
                        { CurrentRenderInfo = args.CurrentRenderInfo
                          PageModifingArguments = pageModifingArguments }

                    factorys
                    |> List.collect (fun factory -> factory args)

                actions
            |> SelectionModifier.AddNew

        | Modifier.Fix factorys ->
            fun (args: _SelectionModifierFixmentArguments) ->
                let actions = 
                    let args  = 
                        { Close = args.Close
                          PageModifingArguments = pageModifingArguments
                          CurrentRenderInfo = args.CurrentRenderInfo }
                    factorys
                    |> List.collect (fun factory -> factory args)
                actions
            |> SelectionModifier.Fix

        | Modifier.DropColor -> SelectionModifier.DropColor

type Modify =
    static member SetStrokeColor(color: Color) =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            [
                PdfCanvas.setStrokeColor (color)
                PdfCanvas.writeOperatorRange args.Close
            ]

    static member SetFillColor(color: Color) =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            [
                PdfCanvas.setFillColor (color)
                PdfCanvas.writeOperatorRange args.Close
            ]

    static member SetFillAndStrokeColor(color: Color) =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            [
                PdfCanvas.setFillColor (color)
                PdfCanvas.setStrokeColor (color)
                PdfCanvas.writeOperatorRange args.Close
            ]

    static member AddRectangleToBound(mapping) =
        fun (args: _SelectionModifierAddNewArguments<'userState>)  ->
            let border = IAbstractRenderInfo.getBound BoundGettingOptions.WithStrokeWidth args.CurrentRenderInfo
            [
                PdfCanvas.addRectangle border mapping
            ]

type _SelectorAndModifier<'userState> =
    { Selector: Selector<'userState> 
      Modifier: Modifier<'userState>
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

    let modifyAsync (modifyingAsyncWorker, pageSelector, (operators: list<_SelectorAndModifier<'userState>>)) =
        let names = 
            operators
            |> List.map (fun selectorAndModifier -> selectorAndModifier.Name)

        if names.Length <> (List.distinct names).Length then failwithf "Duplicated keys in SelectorAndModifiers %A" operators

        let asyncManiputation = 
            fun (totalNumberOfPages) (transformPageNum: PageNumber -> PageNumber) (flowModel: FlowModel<_>) (document: IntegratedDocument) ->
                IntegratedDocument.modify
                    (String.concat "\n" names)
                    pageSelector 
                    (
                        fun (pageNum, pdfPage) ->
                            operators 
                            |> List.mapi (fun i (selectAndModify) ->
                                let pageModifingArguments =
                                    { PageNum = (transformPageNum pageNum).Value
                                      UserState = flowModel.UserState
                                      Page = pdfPage
                                      TotalNumberOfPages = totalNumberOfPages }
                                ( {Index = i; Name = selectAndModify.Name }, 
                                    ( Selector.toRenderInfoSelector pageModifingArguments selectAndModify.Selector,
                                        Modifier.toSelectionModifier pageModifingArguments selectAndModify.Modifier)
                                )
                            )
                            |> Map.ofList
                    ) document

                flowModel.UserState
            
        Manipulate.runInAsync modifyingAsyncWorker asyncManiputation

    let modify (pageSelector, (operators: list<_SelectorAndModifier<'userState>>)) =
        modifyAsync (ModifyingAsyncWorker.Sync, pageSelector, operators)