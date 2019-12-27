namespace Shrimp.Pdf
open iText.Kernel.Font
open Shrimp.Pdf.Parser
open Fake.IO
open System.IO
open System.Collections.Concurrent
open Shrimp.Pdf.FontExtensions



type _FlowModel<'userState> =
    { File: string
      UserState: 'userState }

type FlowModel<'userState> =
    { File: string
      UserState: 'userState
      PdfDocumentCache: PdfDocumentCache }

[<RequireQualifiedAccess>]
module FlowModel =
    let create (flowModel: _FlowModel<_>) =
        { File = flowModel.File 
          UserState = flowModel.UserState 
          PdfDocumentCache = new PdfDocumentCache() }

    let mapM mapping flowModel =
        { File = flowModel.File 
          UserState = mapping flowModel.UserState
          PdfDocumentCache = flowModel.PdfDocumentCache }

type Reuse<'oldUserState, 'newUserState> = Reuse of (FlowModel<'oldUserState> -> SplitDocument -> 'newUserState)
with 
    member x.Value =
        let (Reuse value) = x
        value

    member private x.InvokeAndReOpenDocument =
        fun flowModel (document: SplitDocument) ->
            let userState = x.Value flowModel document
            document.ReOpen()
            userState

    /// internal use
    /// using ||>> instead
    member x.TransformNewUserState (mapping) =
        fun flowModel document ->
            x.Value flowModel document
            |> mapping
        |> Reuse

    /// internal use
    /// using <<|| instead
    member x.TransformOldUserStateBack (mapping) =
        fun flowModel document ->
            let flowModel = FlowModel.mapM mapping flowModel
            x.Value flowModel document
        |> Reuse

    member x.RedirectUserState (mapping) =
        fun flowModel document ->
            let newUserState = x.Value flowModel document
            mapping (flowModel.UserState, newUserState)
        |> Reuse


    /// internal use
    /// using <+> instead
    static member Bind (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        fun flowModel (document: SplitDocument) ->
            let middleUserState = reuse1.InvokeAndReOpenDocument flowModel document
            reuse2.InvokeAndReOpenDocument (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document
        |> Reuse

    /// internal use
    /// using <++> instead
    static member Bind_TupleUserState (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        fun flowModel (document: SplitDocument) ->
            let middleUserState = reuse1.InvokeAndReOpenDocument flowModel document
            reuse2.InvokeAndReOpenDocument (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document |> ignore
            middleUserState
        |> Reuse

    /// internal use
    /// using <.+> instead
    static member Bind_FstUserState (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        fun flowModel (document: SplitDocument) ->
            let middleUserState = reuse1.InvokeAndReOpenDocument flowModel document
            middleUserState, reuse2.InvokeAndReOpenDocument (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document
        |> Reuse



type Manipulate<'oldUserState, 'newUserState> = 
    Manipulate of (FlowModel<'oldUserState> -> IntegratedDocument -> 'newUserState)
with 
    member x.Value =
        let (Manipulate (value)) = x
        value

    member private x.InvokeAndReOpenDocument =
        let (Manipulate (value)) = x
        fun flowModel (document: IntegratedDocument) ->
            let userState = value flowModel document
            document.ReOpen()
            userState

    /// internal use
    /// using ||>> instead
    member x.TransformNewUserState (mapping) =
        fun flowModel document ->
            x.Value flowModel document
            |> mapping
        |> Manipulate

    /// internal use
    /// using <<|| instead
    member x.TransformOldUserStateBack (mapping) =
        fun flowModel document ->
            let flowModel = FlowModel.mapM mapping flowModel
            x.Value flowModel document
        |> Manipulate

    member x.RedirectUserState (mapping) =
        fun flowModel document ->
            let newUserState = x.Value flowModel document
            mapping (flowModel.UserState, newUserState)
        |> Manipulate

    /// internal use
    /// using <+> instead
    static member Bind (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        fun flowModel (document: IntegratedDocument) ->
            let middleUserState = manipulate1.InvokeAndReOpenDocument flowModel document
            manipulate2.InvokeAndReOpenDocument (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document
        |> Manipulate

    /// internal use
    /// using <++> instead
    static member Bind_TupleUserState (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        fun flowModel (document: IntegratedDocument) ->
            let middleUserState: 'middleUserState = manipulate1.InvokeAndReOpenDocument flowModel document
            middleUserState, manipulate2.InvokeAndReOpenDocument (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document
        |> Manipulate

    /// internal use
    /// using <.+> instead
    static member Bind_FstUserState (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        fun flowModel (document: IntegratedDocument) ->
            let middleUserState: 'middleUserState = manipulate1.InvokeAndReOpenDocument flowModel document
            manipulate2.InvokeAndReOpenDocument (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document |> ignore
            middleUserState
        |> Manipulate



[<RequireQualifiedAccess>]
module Manipulate =
    /// followed by <++> or <.+> to restore userState
    let dummy = Manipulate(fun model _ -> model.UserState)

    let ofConstraint (manipulateFactory: 'oldUserState -> Manipulate<'oldUserState,'newUserState>) =
        fun (flowModel: FlowModel<'oldUserState>) ->
            (manipulateFactory flowModel.UserState).Value flowModel
        |> Manipulate


type FileOperation<'oldUserState, 'newUserState> = 
    FileOperation of (FlowModel<'oldUserState> list -> FlowModel<'newUserState> list)
with 
    member x.Value =
        let (FileOperation value) = x
        value

[<RequireQualifiedAccess>]
type Flow<'oldUserState, 'newUserState> =
    | Manipulate of (Manipulate<'oldUserState, 'newUserState>)
    | Reuse of (Reuse<'oldUserState, 'newUserState>)
    | FileOperation of FileOperation<'oldUserState, 'newUserState>
    | Transform of (FlowModel<'oldUserState> -> FlowModel<'newUserState> list)
with 
    static member internal Run(flowModel, flow) =
        let file = flowModel.File
        let writerFile = Path.changeExtension ".writer.pdf" file

        let draft() =
            File.Delete(file)
            File.Move(writerFile, file)

        match flow with 
        | Flow.Manipulate (manipulate) ->
            let pdfDocument = IntegratedDocument.Create(file, writerFile, flowModel.PdfDocumentCache)
            let newUserState = manipulate.Value flowModel pdfDocument
            pdfDocument.Value.Close()
            draft()
            [(flowModel |> FlowModel.mapM(fun _ -> newUserState))]

        | Flow.Reuse (reuse) ->

            let pdfDocument = SplitDocument.Create(file, writerFile, flowModel.PdfDocumentCache)
            let newUserState = reuse.Value flowModel pdfDocument
            pdfDocument.Reader.Close()
            pdfDocument.Writer.Close()
            draft()
            [(flowModel |> FlowModel.mapM(fun _ -> newUserState))]

        | Flow.FileOperation fileOperation -> fileOperation.Value [flowModel]

        | Flow.Transform transform ->
            transform flowModel



    static member (<+>) (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'modifiedUserState>) =
        fun flowModel ->
            let middleFlowModels = Flow<'originUserState, 'middleUserState>.Run (flowModel, flow1) 

            let newFlowModels = 
                match flow2 with 
                | Flow.FileOperation fileOperation ->
                    fileOperation.Value middleFlowModels
                | _ ->
                    middleFlowModels
                    |> List.collect (fun middleFlowModel ->
                        Flow<'middleUserState, 'newUserState>.Run (middleFlowModel, flow2) 
                    )
            newFlowModels

        |> Flow.Transform


    /// internal use
    /// using <++> instead
    static member (<++>) (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'modifiedUserState>) =
        fun flowModel ->
            let middleFlowModels = Flow<'originUserState, 'middleUserState>.Run (flowModel, flow1) 

            match flow2 with 
            | Flow.FileOperation fileOperation -> failwith "file operation only support <+> only"
            | _ -> 
                let newFlowModels = 
                    middleFlowModels
                    |> List.collect (fun middleFlowModel ->
                        Flow<'middleUserState, 'modifiedUserState>.Run (middleFlowModel, flow2) 
                        |> List.map (FlowModel.mapM(fun modifiedUserState -> middleFlowModel.UserState, modifiedUserState))
                    )
                newFlowModels

        |> Flow.Transform


    /// internal use
    /// using <.+> instead
    static member Bind_FstUserState (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'modifiedUserState>) =
        fun flowModel ->
            let middleFlowModels = Flow<'originUserState, 'middleUserState>.Run (flowModel, flow1) 

            match flow2 with 
            | Flow.FileOperation _ -> failwith "file operation only support <+> only"
            | _ ->
                let newFlowModels = 
                    middleFlowModels
                    |> List.collect (fun middleFlowModel ->
                        Flow<'middleUserState, 'modifiedUserState>.Run (middleFlowModel, flow2) 
                        |> List.map (FlowModel.mapM(fun _ -> middleFlowModel.UserState))
                    )
                newFlowModels
        |> Flow.Transform

    /// internal use
    /// using ||>> instead
    member x.TransformNewUserState mapping  =
        fun flowModel ->
            Flow<_, _>.Run(flowModel, x) 
            |> List.map (FlowModel.mapM mapping)
        |> Flow.Transform 

    /// internal use
    /// using <<|| instead
    member x.TransformOldUserStateBack mapping  =
        fun flowModel ->
            let flowModel = FlowModel.mapM mapping flowModel
            Flow<_, _>.Run(flowModel, x) 
         
        |> Flow.Transform 

    member x.RedirectUserState (mapping) =
        fun flowModel ->
            let filwModels = Flow<_, _>.Run (flowModel, x) 
            filwModels
            |> List.map (FlowModel.mapM (fun newUserState -> mapping (flowModel.UserState, newUserState)))
       
        |> Flow.Transform
       

[<AutoOpen>]
module Operators =

    let inline (||>>) (flow : ^a) (mapping: ^b) =
        (^a: (member TransformNewUserState : ^b -> ^c) (flow, mapping))

    let inline (<<||) (mapping: ^b) (flow : ^a)  =
        (^a: (member TransformOldUserStateBack : ^b -> ^c) (flow, mapping))

    let inline (<+>) (flow1: ^a) (flow2: ^b) = 
        ((^a or ^b): (static member Bind: ^a * ^b -> ^c) (flow1, flow2))


    let inline (<++>) (flow1: ^a) (flow2: ^b) = 
        ((^a or ^b): (static member Bind_TupleUserState: ^a * ^b -> ^c) (flow1, flow2))

    let inline (<.+>) (flow1: ^a) (flow2: ^b) = 
        ((^a or ^b): (static member Bind_FstUserState: ^a * ^b -> ^c) (flow1, flow2))


    let inline redirect (mapping : ^b) (flow: ^a) =
        (^a: (member RedirectUserState : ^b -> ^c) (flow, mapping))


    let runWithReuseOldPdfDocumentCache (flowModel: FlowModel<'userState>) flow = 
        Logger.infoWithStopWatch(sprintf "RUN: %s" flowModel.File) (fun _ ->
            Flow<_, _>.Run(flowModel, flow)
        )

    let run (flowModel: _FlowModel<'userState>) flow = 
        runWithReuseOldPdfDocumentCache (FlowModel.create flowModel) flow

    let runManyWithReuseOldPdfDocumentCache (flowModels: FlowModel<_> list) flow = 
        ( Flow.Transform (fun _ -> flowModels )
          <+>
          flow )
        |> run {File = ""; UserState = ()}



    let runMany (flowModels: _FlowModel<_> list) flow = 
        ( Flow.Transform (fun flowModel -> flowModels |> List.map (fun m ->
            { File = m.File 
              UserState = m.UserState 
              PdfDocumentCache = flowModel.PdfDocumentCache } ))
          <+>
          flow )
        |> run {File = ""; UserState = ()}

