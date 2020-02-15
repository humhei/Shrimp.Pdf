namespace  Shrimp.Pdf
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO

[<AutoOpen>]
module _FileOperation =
    type FileOperation<'oldUserState, 'newUserState> = 
        FileOperation of (FlowModel<'oldUserState> list -> FlowModel<'newUserState> list)
    with 
        member x.Value =
            let (FileOperation value) = x
            value

[<AutoOpen>]
module rec _FlowMutualTypes =


    [<RequireQualifiedAccess>]
    type Flow<'oldUserState, 'newUserState> =
        | Manipulate of (Manipulate<'oldUserState, 'newUserState>)
        | Reuse of (Reuse<'oldUserState, 'newUserState>)
        | FileOperation of FileOperation<'oldUserState, 'newUserState>
        | TupledFlow of ITupledFlow<'oldUserState, 'newUserState>
        | Factory of ('oldUserState -> Flow<'oldUserState, 'newUserState>)
        | NamedFlow of (IFlowName * Flow<'oldUserState, 'newUserState>)
    with 
        static member internal Run(flowModels: FlowModel<'oldUserState> list, flow): FlowModel<'newUserState> list =
            match flow with 
            | Flow.FileOperation flow -> 
                flow.Value flowModels
            | Flow.TupledFlow flow ->
                flow.Run flowModels
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


                    | Flow.Factory (factory) ->
                        let flow = factory flowModel.UserState
                        Flow<_, _>.Run([flowModel], flow)

                    | Flow.NamedFlow (iFlowName, flow) ->
                        Logger.tryInfoWithFlowName iFlowName.Value iFlowName.FlowNameIndexes (fun _ ->
                            let flowModels = Flow<_, _>.Run([flowModel], flow)
                            flowModel.TryBackupFile(iFlowName.Value, iFlowName.FlowNameIndexes)
                            flowModels
                        )
                        

                    | Flow.FileOperation fileOperation -> failwith "Invalid token"
                    | Flow.TupledFlow _ ->  failwith "Invalid token"
                )



        static member (<+>) (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'modifiedUserState>) =
            { Flow1 = flow1 
              Flow2 = flow2 
              FMonadStateBack = id
              FMonadState = snd } :> ITupledFlow<_, _>
            |> Flow.TupledFlow
          

        static member (<++>) (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'modifiedUserState>) =
          { Flow1 = flow1 
            Flow2 = flow2 
            FMonadStateBack = id
            FMonadState = (fun (middleFlowModels, newFlowModels) ->
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

            ) } :> ITupledFlow<_, _>
          |> Flow.TupledFlow



        static member (<.+>) (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'modifiedUserState>) =
            
            { Flow1 = flow1 
              Flow2 = flow2 
              FMonadStateBack = id
              FMonadState = (fun (middleFlowModels, newFlowModels) ->
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

              ) } :> ITupledFlow<_, _>
            |> Flow.TupledFlow
         

        static member (<<||) (mapping, flow: Flow<_, _>) =
            let rec loop flow = 
                match flow with 
                | Flow.Reuse reuse -> 
                    mapping <<|| reuse
                    |> Flow.Reuse

                | Flow.Manipulate manipulate -> 
                    mapping <<|| manipulate
                    |> Flow.Manipulate

                | Flow.TupledFlow (tupledFlow) ->
                    tupledFlow.MapStateBack (mapping)
                    |> Flow.TupledFlow

                | Flow.Factory factory ->
                    fun userState ->
                        userState
                        |> mapping
                        |> factory 
                        |> loop
                    |> Flow.Factory

                | Flow.NamedFlow (iFlowName, flow) ->
                    (iFlowName, loop flow)
                    |> Flow.NamedFlow

                | Flow.FileOperation fileOperation ->
                    _FileOperation.FileOperation(
                        fun flowModels ->
                            flowModels
                            |> List.map (FlowModel.mapM mapping)
                            |> fileOperation.Value 
                    )
                    |> Flow.FileOperation
        
            loop flow



        static member (||>>) (flow, mapping) =
            let rec loop flow = 
                match flow with 
                | Flow.Reuse reuse -> 
                    reuse ||>> mapping
                    |> Flow.Reuse

                | Flow.Manipulate manipulate -> 
                    manipulate ||>> mapping
                    |> Flow.Manipulate

                | Flow.TupledFlow (tupledFlow) ->
                    tupledFlow.MapState (mapping)
                    |> Flow.TupledFlow

                | Flow.Factory factory ->
                    fun userState ->
                        factory userState
                        |> loop
                    |> Flow.Factory
                
                | Flow.NamedFlow (iFlowName, flow) ->
                    (iFlowName, loop flow)
                    |> Flow.NamedFlow

                | Flow.FileOperation fileOperation ->
                    _FileOperation.FileOperation(
                        fun flowModels ->
                            fileOperation.Value flowModels
                            |> List.map (FlowModel.mapM mapping)
                    )
                    |> Flow.FileOperation
        
            loop flow
    
    type ITupledFlow<'oldUserState, 'newUserState> =
        abstract member Run: FlowModel<'oldUserState> list -> FlowModel<'newUserState> list
            
        abstract member MapState: ('newUserState -> 'a) -> ITupledFlow<'oldUserState, 'a>

        abstract member MapStateBack: ('a -> 'oldUserState) -> ITupledFlow<'a, 'newUserState>


    type internal TupledFlow<'a, 'oldUserState, 'middleUserState, 'newUserState, 'finalUserState> =
        { Flow1: Flow<'oldUserState, 'middleUserState>
          Flow2: Flow<'middleUserState, 'newUserState>
          FMonadStateBack : 'a -> 'oldUserState
          FMonadState: FlowModel<'middleUserState> list * FlowModel<'newUserState> list -> FlowModel<'finalUserState> list }
    with 
        interface ITupledFlow<'a, 'finalUserState> with 
            member x.Run(flowModels: FlowModel<_> list) =
                let flowModels = flowModels |> List.map (FlowModel.mapM x.FMonadStateBack)

                let middleUserModels  = Flow<_, _>.Run(flowModels, x.Flow1)

                let newUserModels = Flow<_, _>.Run(middleUserModels, x.Flow2)
                x.FMonadState(middleUserModels, newUserModels)

            member x.MapState(mapping) =
                { Flow1 = x.Flow1 
                  Flow2 = x.Flow2 
                  FMonadStateBack = x.FMonadStateBack
                  FMonadState = 
                    fun (flowModels1, flowModels2) ->
                        x.FMonadState (flowModels1, flowModels2)
                        |> List.map (FlowModel.mapM mapping)

                } :> ITupledFlow<_, _>

            member x.MapStateBack(mapping) =
                { Flow1 = x.Flow1 
                  Flow2 = x.Flow2 
                  FMonadStateBack = mapping >> x.FMonadStateBack
                  FMonadState = x.FMonadState
                } :> ITupledFlow<_, _>
       


    [<RequireQualifiedAccess>]
    module Flow =
        let dummy() = 
            _FileOperation.FileOperation id
            |> Flow.FileOperation 


        let batch (flowName: FlowName) (flows: seq<Flow<'originUserState,'newUserState>>) =
            match List.ofSeq flows with 
            | [] -> 
                Flow.NamedFlow (IFlowName.OfFlowName flowName, Flow.dummy() ||>> fun _ -> [])

            | flows ->
                Flow.Factory(fun userState ->
                    let rec loop (flowAccum: Flow<'originUserState, 'newUserState list> option) (flows: Flow<'originUserState, 'newUserState> list) =
                        match flows with 
                        | [] -> flowAccum.Value

                        | flow :: flows ->
                            match flowAccum with 
                            | None -> 
                                let flowAccum = flow ||>> List.singleton
                                loop (Some flowAccum) flows

                            | Some flowAccum -> 

                                let flowAccum = flowAccum ||>> (fun newUserStates -> userState, newUserStates)
                                

                                let flow = fst <<|| flow
                        
                                let flowAccum = flowAccum <++> flow ||>> (fun (a, b) -> snd a @ [b])

                                loop (Some flowAccum) flows

                    loop None flows
                )




        

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