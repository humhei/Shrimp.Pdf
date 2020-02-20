namespace  Shrimp.Pdf
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO

[<AutoOpen>]
module _FileOperation =
    type FileOperation<'oldUserState, 'newUserState> = 
        | FileOperation of (FlowModel<'oldUserState> list -> FlowModel<'newUserState> list)

    with 
        member x.Value =
            let (FileOperation value) = x
            value



[<AutoOpen>]
module rec _FlowMutualTypes =
    module PublicFlowModel = FlowModel

    module FlowModel = InternalFlowModel

    [<RequireQualifiedAccess>]
    type Flow<'oldUserState, 'newUserState> =
        | Manipulate of (Manipulate<'oldUserState, 'newUserState>)
        | Reuse of (Reuse<'oldUserState, 'newUserState>)
        | FileOperation of FileOperation<'oldUserState, 'newUserState>
        | TupledFlow of ITupledFlow<'oldUserState, 'newUserState>
        | Factory of ('oldUserState -> Flow<'oldUserState, 'newUserState>)
        | FactoryByFlowModel of (FlowModel<'oldUserState> -> Flow<'oldUserState, 'newUserState>)
        | NamedFlow of (FlowName * Flow<'oldUserState, 'newUserState>)
    with 
        static member internal Run(flowModels: InternalFlowModel<'oldUserState> list, flow): InternalFlowModel<'newUserState> list =
            match flow with 
            | Flow.FileOperation flow -> 
                flowModels
                |> List.map InternalFlowModel.toFlowModel
                |> flow.Value
                |> List.map (fun flowModel ->
                    { File = flowModel.File 
                      FlowName = None 
                      UserState = flowModel.UserState 
                      FlowNameTupleBindedStatus = FlowNameTupleBindedStatus.None }
                )

            | Flow.TupledFlow flow ->
                flow.Invoke (flowModels |> List.map InternalFlowModelWrapper)
                |> List.map (fun m -> m.Value)

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
                        let newFlowModel = manipulate.Invoke flowModel pdfDocument
                        pdfDocument.Value.Close()
                        draft()
                        newFlowModel.TryBackupFile()
                        [newFlowModel]


                    | Flow.Reuse (reuse) ->
                        let pdfDocument = SplitDocument.Create(file, writerFile)
                        let newFlowModel = reuse.Invoke flowModel pdfDocument
                        pdfDocument.Reader.Close()
                        pdfDocument.Writer.Close()
                        draft()
                        newFlowModel.TryBackupFile()
                        [newFlowModel]


                    | Flow.Factory (factory) ->
                        let flow = factory flowModel.UserState
                        Flow<_, _>.Run([flowModel], flow)

                    | Flow.FactoryByFlowModel (factory) ->
                        let flow = 
                            flowModel
                            |> InternalFlowModel.toFlowModel
                            |> factory

                        Flow<_, _>.Run([flowModel], flow)

                    | Flow.NamedFlow (flowName1, flow) ->
                        let flowName0 = flowModel.FlowName

                        let flowName =
                            match flowModel.FlowNameTupleBindedStatus with 
                            | FlowNameTupleBindedStatus.Binding previous ->
                                FlowName.tupledFlow_Bind_FlowName previous flowName0 flowName1
                            | FlowNameTupleBindedStatus.None ->
                                match flowName0 with 
                                | None -> flowName1
                                | Some flowName0 -> flowName0.Bind(flowName1)

                        let flowModel =
                            { flowModel with 
                                FlowName = Some flowName
                                FlowNameTupleBindedStatus = FlowNameTupleBindedStatus.None }

                        //flowModel.CleanBackupDirectoryWhenFlowName_FileName_Index_IsZero()

                        Logger.TryInfoWithFlowModel (flowModel, (fun _ ->
                            let flowModels = Flow<_, _>.Run([flowModel], flow)
                            flowModel.TryBackupFile()
                            flowModels
                        ))

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

                | Flow.FactoryByFlowModel factory ->
                    fun flowModel ->
                        PublicFlowModel.mapM mapping flowModel
                        |> factory
                        |> loop
                    |> Flow.FactoryByFlowModel

                | Flow.NamedFlow (flowName, flow) ->
                    (flowName, loop flow)
                    |> Flow.NamedFlow

                | Flow.FileOperation fileOperation ->
                    _FileOperation.FileOperation(
                        fun flowModels ->
                            flowModels
                            |> List.map (PublicFlowModel.mapM mapping)
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
                
                | Flow.FactoryByFlowModel factory ->
                    fun flowModel ->
                        factory flowModel
                        |> loop
                    |> Flow.FactoryByFlowModel

                | Flow.NamedFlow (iFlowName, flow) ->
                    (iFlowName, loop flow)
                    |> Flow.NamedFlow

                | Flow.FileOperation fileOperation ->
                    _FileOperation.FileOperation(
                        fun flowModels ->
                            fileOperation.Value flowModels
                            |> List.map (PublicFlowModel.mapM mapping)
                    )
                    |> Flow.FileOperation
        
            loop flow
    
    type ITupledFlow<'oldUserState, 'newUserState> =
        abstract member Invoke: InternalFlowModelWrapper<'oldUserState> list -> InternalFlowModelWrapper<'newUserState> list
            
        abstract member MapState: ('newUserState -> 'a) -> ITupledFlow<'oldUserState, 'a>

        abstract member MapStateBack: ('a -> 'oldUserState) -> ITupledFlow<'a, 'newUserState>


    type internal TupledFlow<'a, 'oldUserState, 'middleUserState, 'newUserState, 'finalUserState> =
        { Flow1: Flow<'oldUserState, 'middleUserState>
          Flow2: Flow<'middleUserState, 'newUserState>
          FMonadStateBack : 'a -> 'oldUserState
          FMonadState: InternalFlowModel<'middleUserState> list * InternalFlowModel<'newUserState> list -> InternalFlowModel<'finalUserState> list }
    with 
        interface ITupledFlow<'a, 'finalUserState> with 
            member x.Invoke(flowModels: InternalFlowModelWrapper<_> list) =
                let flowModels = flowModels |> List.map (fun m -> m.Value)
                let flowModels = flowModels |> List.map (FlowModel.mapM x.FMonadStateBack)

                let middleFlowModels = 
                    let middleFlowModels = Flow<_, _>.Run(flowModels, x.Flow1)
                    let files = flowModels |> List.map (fun m -> m.File)
                    let middleFiles = middleFlowModels |> List.map (fun m -> m.File)
                    if files = middleFiles 
                    then 
                        List.map2 (
                            fun flowModel middleFlowModel ->
                                { middleFlowModel with 
                                    FlowNameTupleBindedStatus = FlowNameTupleBindedStatus.Binding flowModel.FlowName }

                        ) flowModels middleFlowModels
                    else
                        middleFlowModels

                let newUserModels = Flow<_, _>.Run(middleFlowModels, x.Flow2)
                x.FMonadState(middleFlowModels, newUserModels)
                |> List.map InternalFlowModelWrapper

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


    [<AutoOpen>]
    module FlowDSL =
        type Flow =
            static member Batch(?flowName: FlowName) =
                fun (flows: seq<Flow<'originUserState,'newUserState>>) ->
                    let flow = 
                        match List.ofSeq flows with 
                        | [] ->  Flow.dummy() ||>> fun _ -> []

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

                    match flowName with 
                    | None -> flow
                    | Some flowName ->
                        Flow.NamedFlow(flowName, flow)


            static member Rename (name, ?parameters) =

                let flowName = 
                    let parameters = defaultArg parameters []
                    FlowName.Override(name, parameters)

                fun (flow: Flow<_, _>) ->
                    Flow.NamedFlow(
                        flowName,
                        flow
                    )


        

[<AutoOpen>]
module Operators =
    let (=>) a b = a,b

    let runManyWithFlowModels (flowModels: FlowModel<_> list) flow = 
        let filesListText =
            flowModels 
            |> List.map (fun m -> m.File)
            |> String.concat "\n"

        let flowModels =
            flowModels
            |> List.map (fun m -> 
                { File = m.File 
                  UserState = m.UserState 
                  FlowName = None 
                  FlowNameTupleBindedStatus = FlowNameTupleBindedStatus.None }
            )

        Logger.infoWithStopWatch(sprintf "RUN: %s" filesListText) (fun _ ->
            Flow<_, _>.Run(flowModels, flow)
            |> List.map InternalFlowModel.toFlowModel
        )

    let runMany (files: string list) flow = 

        let flowModels =
            files 
            |> List.map (fun file ->
                { File = file 
                  UserState = () }
            )

        runManyWithFlowModels flowModels flow

    let run (file: string) flow = 
        runMany [file] flow

    let runWithFlowModel (flowModel: FlowModel<_>) flow = 
        runManyWithFlowModels [flowModel] flow
 
    let runWithBackup backupPath file flow =
        File.Copy(file, backupPath, true)
        run backupPath flow

