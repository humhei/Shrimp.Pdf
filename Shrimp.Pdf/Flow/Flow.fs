namespace  Shrimp.Pdf
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO


type FileOperation<'oldUserState, 'newUserState> = 
    FileOperation of (FlowModel<'oldUserState> list -> FlowModel<'newUserState> list)
with 
    member x.Value =
        let (FileOperation value) = x
        value
[<AutoOpen>]
module rec _FlowMutualTypes =

    type ITupledFlow<'oldUserState, 'newUserState> =
        interface end


    type TupledFlow<'oldUserState, 'middleUserState, 'newUserState, 'finalUserState> =
        { Flow1: Flow<'oldUserState, 'newUserState>
          Flow2: Flow<'middleUserState, 'newUserState>
          FMonadState: 'newUserState -> 'finalUserState }

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
        // let rec fixFlowNameAndFlowIndex flow =
        //     match flow with 
        //     | Flow.FileOperation _ -> flow
        //     | Flow.TransformList flow ->
        //         flow flowModels

        //     //| Flow.Manipulate (manipulate) ->
            


        //     //| Flow.Reuse (reuse) ->
         

        //     //| Flow.Transform transform ->
               

        //     //| Flow.Factory (factory) ->
          



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