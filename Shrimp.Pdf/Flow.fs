namespace Shrimp.Pdf
open Shrimp.Pdf.Parser
open Fake.IO
open System.IO


type FlowModel<'userState> =
    { File: string
      UserState: 'userState }

[<RequireQualifiedAccess>]
module FlowModel =
    let mapM mapping flowModel =
        { File = flowModel.File 
          UserState = mapping flowModel.UserState }

type Reuse<'oldUserState, 'newUserState> = Reuse of (FlowModel<'oldUserState> -> SplitDocument -> 'newUserState)
with 
    member x.Value =
        let (Reuse value) = x
        value

    member x.TransformUserState (mapping) =
        fun userState document ->
            x.Value userState document
            |> mapping
        |> Reuse

type Manipulate<'oldUserState, 'newUserState> = 
    Manipulate of (FlowModel<'oldUserState> -> IntegratedDocument -> 'newUserState)
with 
    member x.Value =
        let (Manipulate (value)) = x
        value

    /// internal use
    /// using <+> instead
    static member Bind (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        fun flowModel (document: IntegratedDocument) ->
            let middleUserState = manipulate1.Value flowModel document
            manipulate2.Value {File = flowModel.File; UserState = middleUserState} document
        |> Manipulate

    /// internal use
    /// using <++> instead
    static member Bind_TupleUserState (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        fun flowModel (document: IntegratedDocument) ->
            let middleUserState: 'middleUserState = manipulate1.Value flowModel document
            middleUserState, manipulate2.Value {File = flowModel.File; UserState = middleUserState} document
        |> Manipulate

    /// internal use
    /// using <.+> instead
    static member Bind_FstUserState (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        fun flowModel (document: IntegratedDocument) ->
            let middleUserState: 'middleUserState = manipulate1.Value flowModel document
            manipulate2.Value {File = flowModel.File; UserState = middleUserState} document |> ignore
            middleUserState
        |> Manipulate

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




[<RequireQualifiedAccess>]
module Manipulate =
    /// followed by <++> or <.+> to rediscover userState
    let dummy = Manipulate(fun model _ -> model.UserState)

    let ofConstraint (manipulateFactory: 'oldUserState -> Manipulate<'oldUserState,'newUserState>) =
        fun (flowModel: FlowModel<'oldUserState>) ->
            (manipulateFactory flowModel.UserState).Value flowModel
        |> Manipulate


type FileOperation<'oldUserState, 'newUserState> = 
    FileOperation of (FlowModel<'oldUserState> -> ReaderDocument -> FlowModel<'newUserState> list)
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

            let pdfDocument = IntegratedDocument.Create(file, writerFile)
            let newUserState = manipulate.Value flowModel pdfDocument
            pdfDocument.Value.Close()
            draft()
            [{ File = file; UserState = newUserState }]

        | Flow.Reuse (reuse) ->

            let pdfDocument = SplitDocument.Create(file, writerFile)
            let newUserState = reuse.Value flowModel pdfDocument
            pdfDocument.Reader.Close()
            pdfDocument.Writer.Close()
            draft()
            [{ File = file; UserState = newUserState }]

        | Flow.FileOperation fileOperation ->
            let readerDocument = new ReaderDocument(file)
            let newModels = fileOperation.Value flowModel readerDocument
            newModels

        | Flow.Transform transform ->
            transform flowModel



    /// internal use
    /// using <+> instead
    static member Bind (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'modifiedUserState>) =
        fun flowModel ->
            let middleFlowModels = Flow<'originUserState, 'middleUserState>.Run (flowModel, flow1) 

            let newFlowModels = 
                middleFlowModels
                |> List.collect (fun middleFlowModel ->
                    Flow<'middleUserState, 'newUserState>.Run (middleFlowModel, flow2) 
                )
            newFlowModels
        |> Flow.Transform


    /// internal use
    /// using <++> instead
    static member Bind_TuplingUserState (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'modifiedUserState>) =
        fun flowModel ->
            let middleFlowModels = Flow<'originUserState, 'middleUserState>.Run (flowModel, flow1) 

            let newFlowModels = 
                middleFlowModels
                |> List.collect (fun middleFlowModel ->
                    Flow<'middleUserState, 'modifiedUserState>.Run (middleFlowModel, flow2) 
                    |> List.map (fun flowModel ->
                        { File = flowModel.File; UserState = middleFlowModel.UserState, flowModel.UserState }
                    )
                )
            newFlowModels
        |> Flow.Transform


    /// internal use
    /// using <.+> instead
    static member Bind_FstUserState (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'modifiedUserState>) =
        fun flowModel ->
            let middleFlowModels = Flow<'originUserState, 'middleUserState>.Run (flowModel, flow1) 

            let newFlowModels = 
                middleFlowModels
                |> List.collect (fun middleFlowModel ->
                    Flow<'middleUserState, 'modifiedUserState>.Run (middleFlowModel, flow2) 
                    |> List.map (fun flowModel ->
                        { File = flowModel.File; UserState = middleFlowModel.UserState }
                    )
                )
            newFlowModels
        |> Flow.Transform

    /// internal use
    /// using ||>> instead
    member x.TransformNewUserState mapping  =
        fun flowModel ->
            Flow<_, _>.Run(flowModel, x) 
            |> List.map (fun newFlowModel ->
                { File = newFlowModel.File; UserState = mapping newFlowModel.UserState}
            )
        |> Flow.Transform 

    /// internal use
    /// using <<|| instead
    member x.TransformOldUserStateBack mapping  =
        fun flowModel ->
            let flowModel = FlowModel.mapM mapping flowModel
            Flow<_, _>.Run(flowModel, x) 
         
        |> Flow.Transform 

[<AutoOpen>]
module Operators =

    let run flowModel flow = 
        Logger.infoWithStopWatch(sprintf "RUN: %s" flowModel.File) (fun _ ->
            Flow<_, _>.Run(flowModel, flow)
        )

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