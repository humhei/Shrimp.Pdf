namespace  Shrimp.Pdf
#nowarn "0104"
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO
open Shrimp.FSharp.Plus

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
        | Factory of (FlowModel<'oldUserState> -> Flow<'oldUserState, 'newUserState>)
        | NamedFlow of (FlowName * Flow<'oldUserState, 'newUserState>)
    with 
        static member internal Run(flowModels: InternalFlowModel<'oldUserState> list, flow): InternalFlowModel<'newUserState> list =
            match flow with 
            | Flow.FileOperation flow -> 
                let inputFiles = 
                    flowModels
                    |> List.map (fun m -> m.File)

                let newFlowModels = 
                    flowModels
                    |> List.map InternalFlowModel.toFlowModel
                    |> flow.Value

                let newFiles =
                    newFlowModels
                    |> List.map (fun m -> m.File)

                if inputFiles = newFiles 
                then
                    ( flowModels, newFlowModels )
                    ||> List.map2 (fun flowModel newFlowModel ->
                        { File = flowModel.File 
                          FlowName = flowModel.FlowName
                          UserState = newFlowModel.UserState
                          OperatedFlowNames = flowModel.OperatedFlowNames
                          Configuration =flowModel.Configuration }
                    )

                else 
                    newFlowModels
                    |> List.map (fun flowModel ->
                        { File = flowModel.File 
                          FlowName = None 
                          UserState = flowModel.UserState
                          OperatedFlowNames = []
                          Configuration =flowModel.Configuration }
                    )

            | Flow.TupledFlow flow ->
                flow.Invoke (flowModels |> List.map InternalFlowModelWrapper)
                |> List.map (fun m -> m.Value)

            | _ ->
                flowModels
                |> List.collect(fun flowModel ->
                    let file = flowModel.File
                    let writerFile = Path.changeExtension ".writer.pdf" file



                    match flow with 
                    | Flow.Manipulate (manipulate) ->
                        let pdfDocument = IntegratedDocument.Create(file, writerFile)
                        let newFlowModel = manipulate.Invoke flowModel pdfDocument
                        [newFlowModel]


                    | Flow.Reuse (reuse) ->
                        let pdfDocument = SplitDocument.Create(file, writerFile)
                        let newFlowModel = reuse.Invoke flowModel pdfDocument
                        [newFlowModel]



                    | Flow.Factory (factory) ->
                        let flow = 
                            flowModel
                            |> InternalFlowModel.toFlowModel
                            |> factory

                        Flow<_, _>.Run([flowModel], flow)

                    | Flow.NamedFlow (flowName0, flow) ->
                        let index =
                            flowModel.OperatedFlowNames
                            |> List.filter(fun m -> m.RelativeDirectory = flowName0.RelativeDirectory)
                            |> List.length

                        let flowModel = { flowModel with FlowName = Some flowName0; OperatedFlowNames = flowName0 :: flowModel.OperatedFlowNames }

                        PdfLogger.TryInfoWithFlowModel(index, flowModel, fun _ ->
                            let flowModels = Flow<_, _>.Run([flowModel], flow)

                            flowModel.TryBackupFile(index)

                            flowModels
                        )
            

                    | Flow.FileOperation fileOperation -> failwith "Invalid token"
                    | Flow.TupledFlow _ ->  failwith "Invalid token"
                )


        static member internal SetParentFlowName(parentFlowName, flow: Flow<_, _>) =
            let rec loop parentFlowName flow = 
                match flow with 
                | Flow.Reuse reuse -> 
                    reuse.SetParentFlowName(parentFlowName)
                    |> Flow.Reuse

                | Flow.Manipulate manipulate -> 
                    manipulate.SetParentFlowName(parentFlowName)
                    |> Flow.Manipulate

                | Flow.TupledFlow (tupledFlow) ->
                    tupledFlow.SetParentFlowName(parentFlowName)
                    |> Flow.TupledFlow
                
                | Flow.Factory factory ->
                    fun flowModel ->
                        factory flowModel
                        |> loop parentFlowName
                    |> Flow.Factory

                | Flow.NamedFlow (flowName, flow) ->
                    let flowName = 
                        match parentFlowName with 
                        | Some parentFlowName ->
                            match parentFlowName with 
                            | FlowName.New _ ->
                                flowName.SetParentFlowName(parentFlowName)
                            | FlowName.Disable _ ->
                                FlowName.Disable

                            | FlowName.Override _ -> FlowName.Disable
                        | None -> flowName

                    (flowName, loop (Some flowName) flow)
                    |> Flow.NamedFlow


                | Flow.FileOperation _ -> flow
        
            loop parentFlowName flow

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
                    fun flowModel ->
                        PublicFlowModel.mapM mapping flowModel
                        |> factory
                        |> loop
                    |> Flow.Factory

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
                    fun flowModel ->
                        factory flowModel
                        |> loop
                    |> Flow.Factory

                | Flow.NamedFlow (flowName, flow) ->
                    (flowName, loop flow)
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

        abstract member SetParentFlowName: (FlowName option) -> ITupledFlow<'oldUserState, 'newUserState>


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

                let middleFlowModels = Flow<_, _>.Run(flowModels, x.Flow1)

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
       
            member x.SetParentFlowName(parentFlowName) =
                { Flow1 = Flow<_, _>.SetParentFlowName(parentFlowName, x.Flow1)
                  Flow2 = Flow<_, _>.SetParentFlowName(parentFlowName, x.Flow2)
                  FMonadStateBack = x.FMonadStateBack
                  FMonadState = x.FMonadState
                } :> ITupledFlow<_, _>

    [<RequireQualifiedAccess>]
    module Flow =
        let dummy() = 
            _FileOperation.FileOperation id
            |> Flow.FileOperation 





        
    [<AutoOpen>]
    module FlowDSL =

        [<RequireQualifiedAccess>]
        type FlowResponse<'originUserState, 'newUserState> =
            | UserState of 'newUserState
            | Flow of Flow<'originUserState, 'newUserState>

        type Flow =
            static member Batch(?flowName: FlowName) =
                fun (flows: seq<Flow<'originUserState,'newUserState>>) ->
                    let flow = 
                        match List.ofSeq flows with 
                        | [] ->  Flow.dummy() ||>> fun _ -> []

                        | flows ->
                            Flow.Factory(fun flowModel ->
                                let userState = flowModel.UserState
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

            static member Func(factory: 'originUserState -> FlowResponse<'originUserState, 'newUserState>) =
                fun (flowModel: FlowModel<_>) ->
                    let response = factory flowModel.UserState
                    match response with 
                    | FlowResponse.UserState userState -> Flow.dummy() ||>> (fun _ -> userState)
                    | FlowResponse.Flow flow -> flow
                |> Flow.Factory

            static member Func(factory: 'originUserState -> Flow<'originUserState, 'newUserState>) =
                fun (flowModel: FlowModel<_>) ->
                    factory flowModel.UserState
                |> Flow.Factory



[<AutoOpen>]
module Operators =
    let (=>) a b = a,b


    let runManyWithFlowModels (config: Configuration) (flowModels: FlowModel<_> list) flow = 
        match flowModels with 
        | [] -> []
        | _ ->
            let flow = Flow.SetParentFlowName(None, flow)

            let flowModels =
                flowModels
                |> List.map (fun m -> 
                    { File = m.PdfFile.Path 
                      UserState = m.UserState 
                      FlowName = None
                      OperatedFlowNames = []
                      Configuration =m.Configuration }
                )

            match config.LoggerLevel with 
            | PdfLoggerLevel.Info ->
                let filesListText =
                    flowModels 
                    |> List.map (fun m -> m.File)
                    |> String.concat "\n"

                PdfLogger.infoWithStopWatch(sprintf "RUN: %s" filesListText) (fun _ ->
                    Flow<_, _>.Run(flowModels, flow)
                    |> List.map InternalFlowModel.toFlowModel
                )

            | PdfLoggerLevel.Slient ->
                Flow<_, _>.Run(flowModels, flow)
                |> List.map InternalFlowModel.toFlowModel

    let runMany config (files: string list) flow = 

        let flowModels =
            files 
            |> List.map (fun file ->
                { PdfFile = PdfFile file 
                  UserState = ()
                  Configuration = config
                  }
            )

        runManyWithFlowModels config flowModels flow

    let runWith config (file: string) flow = 
        runMany config [file] flow

    let run file flow = runWith Configuration.DefaultValue file flow

    let runWithFlowModel (flowModel: FlowModel<_>) flow = 
        runManyWithFlowModels flowModel.Configuration [flowModel] flow
 
    let runWithBackup backupPath file flow =
        File.Copy(file, backupPath, true)
        run backupPath flow

    let runWithBackupAndConfiguration config backupPath file flow =
        File.Copy(file, backupPath, true)
        runWith config backupPath flow




