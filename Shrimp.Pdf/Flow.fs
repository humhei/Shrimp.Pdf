namespace Shrimp.Pdf
open Fake.IO
open System.IO



type FlowModel<'userState> =
    { File: string
      UserState: 'userState }


[<RequireQualifiedAccess>]
module FlowModel =

    let mapM mapping (flowModel: FlowModel<_>) =
        { File = flowModel.File 
          UserState = mapping flowModel.UserState }


type Reuse<'oldUserState, 'newUserState> = Reuse of (FlowModel<'oldUserState> -> SplitDocument -> 'newUserState)
with 
    member x.Value =
        let (Reuse value) = x
        value

    member private x.Invoke =
        fun flowModel (document: SplitDocument) ->
            Logger.infoWithStopWatch (sprintf "%A" x) (fun _ ->
                let userState = x.Value flowModel document
                userState
            )

    member private x.InvokeAndReOpenDocument =
        fun flowModel (document: SplitDocument) ->
            Logger.infoWithStopWatch (sprintf "%A" x) (fun _ ->
                let userState = x.Value flowModel document
                document.ReOpen()
                userState
            )




    member x.RedirectUserState (mapping) =
        fun flowModel document ->
            let newUserState = x.Value flowModel document
            mapping (flowModel.UserState, newUserState)
        |> Reuse

    static member (||>>) (reuse: Reuse<_, _>, mapping) =
        fun flowModel document ->
            reuse.Value flowModel document
            |> mapping
        |> Reuse

    static member (<<||) (mapping, reuse: Reuse<_, _>) =
        fun flowModel document ->
            let flowModel = FlowModel.mapM mapping flowModel
            reuse.Value flowModel document
        |> Reuse

    static member (<+>) (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        fun flowModel (document: SplitDocument) ->
            let middleUserState = reuse1.InvokeAndReOpenDocument flowModel document
            reuse2.Invoke (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document
        |> Reuse

    static member (<++>) (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        fun flowModel (document: SplitDocument) ->
            let middleUserState = reuse1.InvokeAndReOpenDocument flowModel document
            middleUserState, reuse2.Invoke (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document
        |> Reuse

    static member (<.+>) (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        fun flowModel (document: SplitDocument) ->
            let middleUserState = reuse1.InvokeAndReOpenDocument flowModel document
            middleUserState, reuse2.Invoke (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document
        |> Reuse

[<RequireQualifiedAccess>]
module Reuse =
    let dummy = 
        Reuse(fun flowModel splitDocument -> 
            splitDocument.Reader.CopyPagesTo(1, splitDocument.Reader.GetNumberOfPages(), splitDocument.Writer)
            |> ignore

            flowModel.UserState 
        )

type Manipulate<'oldUserState, 'newUserState> = Manipulate of (FlowModel<'oldUserState> -> IntegratedDocument -> 'newUserState)
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

    static member (||>>) (manipulate: Manipulate<_, _>, mapping) =
        fun flowModel document ->
            manipulate.Value flowModel document
            |> mapping
        |> Manipulate

    static member (<<||) (mapping, manipulate: Manipulate<_, _>) =
        fun flowModel document ->
            let flowModel = FlowModel.mapM mapping flowModel
            manipulate.Value flowModel document
        |> Manipulate

    member x.RedirectUserState (mapping) =
        fun flowModel document ->
            let newUserState = x.Value flowModel document
            mapping (flowModel.UserState, newUserState)
        |> Manipulate

    static member (<+>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        fun flowModel (document: IntegratedDocument) ->
            let middleUserState = manipulate1.InvokeAndReOpenDocument flowModel document
            manipulate2.Value (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document
        |> Manipulate

    static member (<++>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        fun flowModel (document: IntegratedDocument) ->
            let middleUserState: 'middleUserState = manipulate1.InvokeAndReOpenDocument flowModel document
            middleUserState, manipulate2.Value (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document
        |> Manipulate

    static member (<.+>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        fun flowModel (document: IntegratedDocument) ->
            let middleUserState: 'middleUserState = manipulate1.InvokeAndReOpenDocument flowModel document
            manipulate2.Value (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document |> ignore
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
    | Factory of (FlowModel<'oldUserState> -> Flow<'oldUserState, 'newUserState>)
with 
    static member internal Run(flowModel: FlowModel<_>, flow) =
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
            [(flowModel |> FlowModel.mapM(fun _ -> newUserState))]


        | Flow.Reuse (reuse) ->
            Logger.infoWithStopWatch (sprintf "%A" reuse) (fun _ ->
                let pdfDocument = SplitDocument.Create(file, writerFile)
                let newUserState = reuse.Value flowModel pdfDocument
                pdfDocument.Reader.Close()
                pdfDocument.Writer.Close()
                draft()
                [ (flowModel |> FlowModel.mapM(fun _ -> newUserState)) ]
            )

        | Flow.FileOperation fileOperation -> fileOperation.Value [flowModel]

        | Flow.Transform transform ->
            transform flowModel

        | Flow.Factory (factory) ->
            let flow = factory flowModel
            Flow<_, _>.Run(flowModel, flow)


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


    static member (<.+>) (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'modifiedUserState>) =
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

    static member (<<||) (mapping, flow: Flow<_, _>) =
        fun flowModel ->
            let flowModel = FlowModel.mapM mapping flowModel
            Flow<_, _>.Run(flowModel, flow) 
         
        |> Flow.Transform 



    /// internal use
    /// using ||>> instead
    static member (||>>) (flow, mapping)  =
        fun flowModel ->
            Flow<_, _>.Run(flowModel, flow) 
            |> List.map (FlowModel.mapM mapping)
        |> Flow.Transform 

    member x.RedirectUserState (mapping) =
        fun flowModel ->
            let filwModels = Flow<_, _>.Run (flowModel, x) 
            filwModels
            |> List.map (FlowModel.mapM (fun newUserState -> mapping (flowModel.UserState, newUserState)))
       
        |> Flow.Transform
    
[<RequireQualifiedAccess>]
module Flow =
    let dummy = Flow.Transform (fun flowModel -> [flowModel] )


[<AutoOpen>]
module Operators =

    let run (flowModel: FlowModel<'userState>) flow = 
        Logger.infoWithStopWatch(sprintf "RUN: %s" flowModel.File) (fun _ ->
            Flow<_, _>.Run(flowModel, flow)
        )
 
    let runMany (flowModels: FlowModel<_> list) flow = 
        ( Flow.Transform (fun _ -> flowModels )
          <+>
          flow )
        |> run {File = ""; UserState = ()}


