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

    member internal x.Invoke =
        fun flowModel (document: SplitDocument) ->
            Logger.infoWithStopWatch (sprintf "%A" x) (fun _ ->
                let userState = x.Value flowModel document
                userState
            )

    member internal x.InvokeAndReOpenDocument =
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

    let batch (reuses: seq<Reuse<'originUserState,'newUserState>>) =
        fun flowModel (document: SplitDocument) ->
            let reuses = List.ofSeq reuses
            reuses
            |> List.mapi(fun i reuse ->
                if i = reuses.Length - 1 
                then reuse.Invoke flowModel document
                else reuse.InvokeAndReOpenDocument flowModel document
            )
        |> Reuse

type Manipulate<'oldUserState, 'newUserState> = Manipulate of (FlowModel<'oldUserState> -> IntegratedDocument -> 'newUserState)
with 
    member x.Value =
        let (Manipulate (value)) = x
        value

    member internal x.InvokeAndReOpenDocument =
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

    let batch (manipulates: seq<Manipulate<'originUserState,'newUserState>>) =
        fun flowModel (document: IntegratedDocument) ->
            let manipulates = List.ofSeq manipulates
            manipulates
            |> List.mapi(fun i manipulate ->
                if i = manipulates.Length - 1 
                then manipulate.Value flowModel document
                else manipulate.InvokeAndReOpenDocument flowModel document
            )
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
    | TransformList of (FlowModel<'oldUserState> list -> FlowModel<'newUserState> list)
    | Factory of (FlowModel<'oldUserState> -> Flow<'oldUserState, 'newUserState>)
with 
    static member internal Run(flowModels: FlowModel<_> list, flow) =
        match flow with 
        | Flow.FileOperation flow -> 
            flow.Value flowModels
        | Flow.TransformList flow ->
            flow flowModels
        | _ ->
            flowModels
            |> List.collect(fun flowModel ->
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

                | Flow.Transform transform ->
                    transform flowModel

                | Flow.Factory (factory) ->
                    let flow = factory flowModel
                    Flow<_, _>.Run([flowModel], flow)

                | Flow.FileOperation fileOperation -> failwith "Invalid token"
                | Flow.TransformList _ -> failwith "Invalid token"
            )



    static member (<+>) (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'modifiedUserState>) =
        fun flowModels ->
            let middleFlowModels = Flow<'originUserState, 'middleUserState>.Run (flowModels, flow1) 

            let newFlowModels = Flow<'middleUserState, 'newUserState>.Run (middleFlowModels, flow2) 
          
            newFlowModels

        |> Flow.TransformList


    static member (<++>) (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'modifiedUserState>) =
        fun flowModels ->
            let middleFlowModels = Flow<'originUserState, 'middleUserState>.Run (flowModels, flow1) 
            let newFlowModels = Flow<'middleUserState, 'newUserState>.Run (middleFlowModels, flow2) 

            let middleFiles = middleFlowModels |> List.map (fun m -> m.File)
            let newFiles = newFlowModels |> List.map (fun m -> m.File)

            if middleFiles = newFiles 
            then 
                (middleFlowModels, newFlowModels)
                ||> List.map2 (fun middleFlowModel newFlowModel ->
                    FlowModel.mapM (fun modifiedUserState -> (middleFlowModel.UserState, modifiedUserState)) newFlowModel
                )
            else 
                failwithf "<++> is not supported when middleFiles %A are different to new files %A" middleFiles newFiles

        |> Flow.TransformList


    static member (<.+>) (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'modifiedUserState>) =
        fun flowModel ->
            let middleFlowModels = Flow<'originUserState, 'middleUserState>.Run (flowModel, flow1) 

            let newFlowModels = Flow<'middleUserState, 'newUserState>.Run (middleFlowModels, flow2) 

            let middleFiles = middleFlowModels |> List.map (fun m -> m.File)
            let newFiles = newFlowModels |> List.map (fun m -> m.File)

            if middleFiles = newFiles 
            then 
                (middleFlowModels, newFlowModels)
                ||> List.map2 (fun middleFlowModel newFlowModel ->
                    FlowModel.mapM (fun modifiedUserState -> middleFlowModel.UserState) newFlowModel
                )
            else 
                failwithf "<.+> is not supported when middleFiles %A are different to new files %A" middleFiles newFiles

        |> Flow.TransformList

    static member (<<||) (mapping, flow: Flow<_, _>) =
        fun flowModels ->

            let flowModels = 
                flowModels
                |> List.map (FlowModel.mapM mapping)

            Flow<_, _>.Run(flowModels, flow) 
         
        |> Flow.TransformList



    /// internal use
    /// using ||>> instead
    static member (||>>) (flow, mapping)  =
        fun flowModels ->
            Flow<_, _>.Run(flowModels, flow) 
            |> List.map (FlowModel.mapM mapping)
        |> Flow.TransformList 


    
[<RequireQualifiedAccess>]
module Flow =
    let dummy = Flow.Transform (fun flowModel -> [flowModel] )

    let batch (flows: seq<Flow<'originUserState,'newUserState>>) =
        fun flowModels ->
            let flows = List.ofSeq flows
            let newFlowModels = 
                flows
                |> List.collect(fun flow ->
                    Flow<_, _>.Run (flowModels, flow)
                )

            let files = flowModels |> List.map (fun m -> m.File)
            let newFiles = newFlowModels |> List.map (fun m -> m.File)

            if files = newFiles 
            then 
                flowModels
                |> List.map (FlowModel.mapM(fun originUserState -> 
                    newFlowModels
                    |> List.map (fun flow -> flow.UserState)
                ))
            else failwithf "Batch is not supported when origin files %A are different to new files %A" files newFiles

        |> Flow.TransformList
[<AutoOpen>]
module Operators =


    let runMany (flowModels: FlowModel<_> list) flow = 
        let filesListText =
            flowModels 
            |> List.map (fun m -> m.File)
            |> String.concat "\n"

        Logger.infoWithStopWatch(sprintf "RUN: %s" filesListText) (fun _ ->
            Flow<_, _>.Run(flowModels, flow)
        )

    let run (flowModel: FlowModel<'userState>) flow = 
        runMany [flowModel] flow
 
    let runWithBackup backupPath path flow =
        File.Copy(path, backupPath, true)
        run { UserState = (); File = backupPath } flow