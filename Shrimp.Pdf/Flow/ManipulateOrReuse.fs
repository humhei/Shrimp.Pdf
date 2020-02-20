﻿namespace Shrimp.Pdf
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO


module internal rec ManipulateOrReuse =

    [<RequireQualifiedAccess>]
    type SplitOrIntegratedDocument =
        | SplitDocument of SplitDocument
        | IntegratedDocument of IntegratedDocument
    with 
        member x.ReOpen() =
            match x with 
            | SplitOrIntegratedDocument.SplitDocument document -> document.ReOpen()
            | SplitOrIntegratedDocument.IntegratedDocument document -> document.ReOpen()
    

    type FlowModel<'userState> =
        { File: string 
          Document: SplitOrIntegratedDocument 
          FlowName: FlowName option
          FlowNameTupleBindedStatus: FlowNameTupleBindedStatus
          UserState: 'userState }
    with 
        member flowModel.ToPublicFlowModel() =
            { File = flowModel.File 
              UserState = flowModel.UserState }

        member flowModel.ToInternalFlowModel() =
            { File = flowModel.File 
              UserState = flowModel.UserState 
              FlowName = flowModel.FlowName 
              FlowNameTupleBindedStatus = flowModel.FlowNameTupleBindedStatus }

        member flowModel.TryBackupFile() =
            flowModel.ToInternalFlowModel().TryBackupFile()

        member flowModel.CleanBackupDirectoryWhenFlowName_FileName_Index_IsZero() =
            flowModel.ToInternalFlowModel().CleanBackupDirectoryWhenFlowName_FileName_Index_IsZero()
         


    [<RequireQualifiedAccess>]
    module FlowModel =
        let mapM mapping (flowModel: FlowModel<_>) =
            { File = flowModel.File 
              UserState = mapping flowModel.UserState
              FlowName = flowModel.FlowName
              Document = flowModel.Document
              FlowNameTupleBindedStatus = flowModel.FlowNameTupleBindedStatus }

        let mapTo userState (flowModel: FlowModel<_>) =
            { File = flowModel.File 
              UserState = userState
              FlowName = flowModel.FlowName
              Document = flowModel.Document 
              FlowNameTupleBindedStatus = flowModel.FlowNameTupleBindedStatus}

    type Logger with
        static member TryInfoWithFlowModel (flowModel: FlowModel<_>, f) =
            Logger.TryInfoWithFlowModel(
                flowModel.ToInternalFlowModel(),
                f = f
            )


    [<RequireQualifiedAccess>]
    type Flow<'oldUserState, 'newUserState> =
        | ManipulateOrReuse of (FlowModel<'oldUserState> -> 'newUserState)
        | Factory of ('oldUserState -> Flow<'oldUserState, 'newUserState>)
        | FactoryByFlowModel of (FlowModel<'oldUserState> -> Flow<'oldUserState, 'newUserState>)
        | NamedFlow of FlowName * Flow<'oldUserState, 'newUserState>
        | TupledFlow of ITupledFlow<'oldUserState, 'newUserState>
        | AppendedFlow of (SplitOrIntegratedDocument -> unit) * Flow<'oldUserState, 'newUserState>
    with 
        static member internal Invoke(flowModel: FlowModel<'userState>) (flow) : FlowModel<'newUserState> =
            let rec loop (flowModel: FlowModel<'userState>) flow =
                match flow with 
                | Flow.ManipulateOrReuse manipulateOrReuse -> 
                    let newUserState = (manipulateOrReuse flowModel)
                    FlowModel.mapTo newUserState flowModel

                | Flow.TupledFlow flow -> flow.Invoke flowModel
                
                | Flow.Factory (factory) ->
                    let flow = factory flowModel.UserState
                    loop flowModel flow

                | Flow.FactoryByFlowModel (factory) ->
                    let flow = factory flowModel
                    loop flowModel flow

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
                        let userState = loop flowModel flow
                        userState
                    ))

                | Flow.AppendedFlow (appendix, flow) ->
                    let newUserState = loop flowModel flow
                    appendix flowModel.Document
                    newUserState

            loop flowModel flow


        static member Bind 
            (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'modifiedUserState>, fMonadState) =
            { Flow1 = flow1 
              Flow2 = flow2 
              FMonadStateBack = id
              FMonadState = fMonadState } :> ITupledFlow<_, _>
            |> Flow.TupledFlow
      
         
        static member MapStateBack (mapping, flow: Flow<_, _>) =
            let rec loop flow = 
                match flow with 
                | Flow.ManipulateOrReuse transform -> 
                    fun flowModel ->
                        FlowModel.mapM mapping flowModel
                        |> transform
                    |> Flow.ManipulateOrReuse

                | Flow.Factory factory ->
                    fun userState ->
                        userState
                        |> mapping
                        |> factory
                        |> loop

                    |> Flow.Factory

                | Flow.FactoryByFlowModel factory ->
                    fun flowModel ->
                        FlowModel.mapM mapping flowModel
                        |> factory
                        |> loop
                    |> Flow.FactoryByFlowModel

                | Flow.TupledFlow (tupledFlow) ->
                    tupledFlow.MapStateBack (mapping)
                    |> Flow.TupledFlow

                | Flow.NamedFlow (flowName, flow) ->
                    (flowName, loop flow)
                    |> Flow.NamedFlow

                | Flow.AppendedFlow (appendix, flow) ->
                    (appendix, loop flow)
                    |> Flow.AppendedFlow
            
            loop flow

        static member MapState (flow, mapping) =
            let rec loop flow = 
                match flow with 
                | Flow.ManipulateOrReuse transform -> 
                    fun flowModel ->
                        flowModel
                        |> transform
                        |> mapping
                    |> Flow.ManipulateOrReuse

                | Flow.Factory factory ->
                    fun userState ->
                        userState
                        |> factory
                        |> loop

                    |> Flow.Factory
                    
                | Flow.FactoryByFlowModel factory ->
                    fun flowModel ->
                        flowModel
                        |> factory
                        |> loop

                    |> Flow.FactoryByFlowModel
                | Flow.TupledFlow (tupledFlow) ->
                    tupledFlow.MapState (mapping)
                    |> Flow.TupledFlow

                | Flow.NamedFlow (flowName, flow) ->
                    (flowName, loop flow)
                    |> Flow.NamedFlow

                | Flow.AppendedFlow (appendix, flow) ->
                    (appendix, loop flow)
                    |> Flow.AppendedFlow
            

            loop flow

        static member Dummy() =
            Flow.ManipulateOrReuse(
               fun flowModel -> 
                   match flowModel.Document with 
                   | SplitOrIntegratedDocument.SplitDocument splitDocument ->
                       splitDocument.Reader.CopyPagesTo(1, splitDocument.Reader.GetNumberOfPages(), splitDocument.Writer)
                       |> ignore

                       flowModel.UserState 

                   | SplitOrIntegratedDocument.IntegratedDocument _ ->
                       flowModel.UserState
            )

        static member Batch (?flowName: FlowName) =
            fun flows ->
                let flow = 
                    Flow.Factory(fun userState ->
                        match flows with 
                        | [] -> Flow<_, _>.MapState(Flow<_, _>.Dummy(), fun _ -> [])

                        | flows ->
                            let rec loop (flowAccum: Flow<'originUserState, 'newUserState list> option) (flows: Flow<'originUserState, 'newUserState> list) =
                                match flows with 
                                | [] -> flowAccum.Value

                                | flow :: flows ->
                                    match flowAccum with 
                                    | None -> 
                                        let flowAccum = Flow<_, _>.MapState(flow, List.singleton)
                                        loop (Some flowAccum) flows

                                    | Some flowAccum -> 

                                        let flowAccum =
                                            Flow<_, _>.MapState(flowAccum, (fun a -> userState, a))

                                        let flow = 
                                            Flow<_, _>.MapStateBack(fst, flow)
                    
                                        let flowAccum = 
                                            Flow<_, _>.Bind(flowAccum, flow, fun (a, b) -> snd a @ [b])

                                        loop (Some flowAccum) flows

                            loop None flows
                    )

                match flowName with 
                | None -> flow
                | Some flowName ->
                    Flow.NamedFlow (flowName, flow)



    type ITupledFlow<'oldUserState, 'newUserState> =
        abstract member Invoke: FlowModel<'oldUserState> -> FlowModel<'newUserState>
            
        abstract member MapState: ('newUserState -> 'a) -> ITupledFlow<'oldUserState, 'a>
    
        abstract member MapStateBack: ('a -> 'oldUserState) -> ITupledFlow<'a, 'newUserState>
    
    type TupledFlow<'a, 'oldUserState, 'middleUserState, 'newUserState, 'finalUserState> =
        { Flow1: Flow<'oldUserState, 'middleUserState>
          Flow2: Flow<'middleUserState, 'newUserState>
          FMonadStateBack : 'a -> 'oldUserState
          FMonadState: 'middleUserState * 'newUserState -> 'finalUserState }
    with 
        interface ITupledFlow<'a, 'finalUserState> with 
            member x.Invoke(flowModel: FlowModel<'a>) =

                let flowModel = FlowModel.mapM x.FMonadStateBack flowModel

                let middleFlowModel =

                    let middleFlowModel = Flow<_, _>.Invoke flowModel x.Flow1
                    
                    { middleFlowModel with 
                        FlowNameTupleBindedStatus = FlowNameTupleBindedStatus.Binding flowModel.FlowName
                    }

                middleFlowModel.Document.ReOpen()
                middleFlowModel.TryBackupFile()

                let newFlowModel = Flow<_, _>.Invoke middleFlowModel x.Flow2
               
                let finalUserState = x.FMonadState (middleFlowModel.UserState, newFlowModel.UserState)

                FlowModel.mapTo finalUserState newFlowModel


            member x.MapState(mapping) =
                { Flow1 = x.Flow1 
                  Flow2 = x.Flow2 
                  FMonadStateBack = x.FMonadStateBack
                  FMonadState = 
                    fun (middleUserState, newUserState) -> 
                        x.FMonadState (middleUserState, newUserState)
                        |> mapping

                } :> ITupledFlow<_, _>

            member x.MapStateBack(mapping) =
                { Flow1 = x.Flow1 
                  Flow2 = x.Flow2 
                  FMonadStateBack = mapping >> x.FMonadStateBack
                  FMonadState = x.FMonadState
                } :> ITupledFlow<_, _>



open ManipulateOrReuse

type Reuse<'oldUserState, 'newUserState> internal 
    (flow: Flow<'oldUserState, 'newUserState>) =

    member x.Append(appendix) =
        let appendix = 
            fun document ->
                match document with
                | SplitOrIntegratedDocument.SplitDocument document -> appendix document
                | SplitOrIntegratedDocument.IntegratedDocument _ -> failwith "Invalid token"

        Flow.AppendedFlow(appendix, x.Flow)
        |> Reuse

    member internal x.Flow = flow

    member internal x.Invoke (flowModel: Shrimp.Pdf.InternalFlowModel<_>) (document: SplitDocument) = 
        flow 
        |> Flow<_, _>.Invoke(
            { File = flowModel.File
              Document = SplitOrIntegratedDocument.SplitDocument document
              UserState = flowModel.UserState
              FlowName = flowModel.FlowName
              FlowNameTupleBindedStatus = flowModel.FlowNameTupleBindedStatus }
        )
        |> fun flowModel ->
            flowModel.ToInternalFlowModel()
       
  

    static member (||>>) (reuse: Reuse<'oldUserState, 'newUserState>, mapping: 'newUserState -> 'a) =
        Flow<_, _>.MapState(reuse.Flow, mapping)
        |> Reuse

    static member (<<||) (mapping, reuse: Reuse<_, _>) =
        Flow<_, _>.MapStateBack(mapping, reuse.Flow)
        |> Reuse
    
    static member (<+>) (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Bind(reuse1.Flow, reuse2.Flow, snd)
        |> Reuse

    static member (<++>) (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Bind(reuse1.Flow, reuse2.Flow, id)
        |> Reuse

    static member (<.+>) (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Bind(reuse1.Flow, reuse2.Flow, fst)
        |> Reuse

     
    new (f: Shrimp.Pdf.FlowModel<'oldUserState> -> SplitDocument -> 'newUserState, ?flowName: FlowName) =
        let flow =
            Flow.ManipulateOrReuse (fun flowModel ->
                match flowModel.Document with 
                | SplitOrIntegratedDocument.SplitDocument document ->
                    f 
                        (flowModel.ToPublicFlowModel())
                        document

                | SplitOrIntegratedDocument.IntegratedDocument _ ->
                    failwith "Invalid token"
            )
        match flowName with 
        | None -> Reuse(flow)

        | Some flowName ->

            Reuse(
                Flow.NamedFlow(
                    flowName,
                    flow
                )
            )

    new (flowName, reuse: Reuse<_, _>) =
        Reuse(
            Flow.NamedFlow(
                flowName,
                reuse.Flow
            )
        )
    



[<RequireQualifiedAccess>]
module Reuse =
    let dummy() = 
        Reuse(fun flowModel splitDocument -> 
            splitDocument.Reader.CopyPagesTo(1, splitDocument.Reader.GetNumberOfPages(), splitDocument.Writer)
            |> ignore

            flowModel.UserState 
        )

    let rename (name: string) (paramters: list<string * string>) =
        fun (reuse: Reuse<_, _>) ->
            Reuse(
                Flow.NamedFlow(
                    FlowName.Override(name, paramters),
                    reuse.Flow
                )
            )


type Manipulate<'oldUserState, 'newUserState> internal (flow: Flow<'oldUserState, 'newUserState>) =
    member internal x.Flow = flow

    member internal x.Invoke (flowModel: Shrimp.Pdf.InternalFlowModel<_>) (document: IntegratedDocument) = 
        flow 
        |> Flow<_, _>.Invoke(
            { File = flowModel.File
              Document = SplitOrIntegratedDocument.IntegratedDocument document
              UserState = flowModel.UserState
              FlowName = flowModel.FlowName
              FlowNameTupleBindedStatus = flowModel.FlowNameTupleBindedStatus }
        ) 
        |> fun flowModel ->
            flowModel.ToInternalFlowModel()


  

    static member (||>>) (manipulate: Manipulate<'oldUserState, 'newUserState>, mapping: 'newUserState -> 'a) =
        Flow<_, _>.MapState(manipulate.Flow, mapping)
        |> Manipulate

    static member (<<||) (mapping, manipulate: Manipulate<_, _>) =
        Flow<_, _>.MapStateBack(mapping, manipulate.Flow)
        |> Manipulate
    
    static member (<+>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Bind(manipulate1.Flow, manipulate2.Flow, snd)
        |> Manipulate

    static member (<++>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Bind(manipulate1.Flow, manipulate2.Flow, id)
        |> Manipulate

    static member (<.+>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Bind(manipulate1.Flow, manipulate2.Flow, fst)
        |> Manipulate


    new (f: Shrimp.Pdf.FlowModel<'oldUserState> -> IntegratedDocument -> 'newUserState, ?flowName: FlowName) =
        
        let flow = 
            Flow.ManipulateOrReuse (fun flowModel ->
                match flowModel.Document with 
                | SplitOrIntegratedDocument.SplitDocument document ->
                    failwith "Invalid token"

                | SplitOrIntegratedDocument.IntegratedDocument document ->
                    f 
                        (flowModel.ToPublicFlowModel())
                        document
            )

        match flowName with 
        | Some flowName ->
            Manipulate(
                Flow.NamedFlow(
                    flowName,
                    flow
                )
            )

        | None -> Manipulate flow

    new (flowName, manipulate: Manipulate<_, _>) =
        Manipulate(
            Flow.NamedFlow(
                flowName,
                manipulate.Flow
            )
        )
     



[<RequireQualifiedAccess>]
module Manipulate =
    let dummy() = 
        Manipulate(fun flowModel _ -> flowModel.UserState)

    let rename (name: string) (paramters: list<string * string>) =
        fun (reuse: Manipulate<_, _>) ->
            Manipulate(
                Flow.NamedFlow(
                    FlowName.Override(name, paramters),
                    reuse.Flow
                )
            )

[<AutoOpen>]
module ManipulateOrReuseDSL =
    type Reuse =
     
        static member Rename (name, ?parameters) =
            let flowName = 
                let parameters = defaultArg parameters []
                FlowName.Override(name, parameters)

            fun (reuse: Reuse<_, _>) ->
                Reuse(
                    Flow.NamedFlow(
                        flowName,
                        reuse.Flow
                    )
                )



        static member Batch (?flowName: FlowName) =
            fun (reuses: list<Reuse<_, _>>) ->
                reuses
                |> List.map (fun reuse ->
                    reuse.Flow
                )
                |> Flow<_, _>.Batch(?flowName = flowName)
                |> Reuse


        static member Factory (factory: Shrimp.Pdf.FlowModel<_> -> SplitDocument -> Reuse<_, _>) =
            Flow.FactoryByFlowModel(fun flowModel ->
                let document =
                    match flowModel.Document with 
                    | SplitOrIntegratedDocument.SplitDocument document -> document
                    | SplitOrIntegratedDocument.IntegratedDocument _ -> failwith "Invalid token"
                let reuse = 
                    factory 
                        (flowModel.ToPublicFlowModel())
                        document
                reuse.Flow
            )
            |> Reuse
          
        static member Append (appendix: SplitDocument -> unit) (reuse: Reuse<_, _>) =
            reuse.Append(appendix)


    type Manipulate =
    

        static member Rename (name, ?parameters) =
            let flowName = 
                let parameters = defaultArg parameters []
                FlowName.Override(name, parameters)

            fun (manipulate: Manipulate<_, _>) ->
                Manipulate(
                    Flow.NamedFlow(
                        flowName,
                        manipulate.Flow
                    )
                )

        static member Batch (?flowName: FlowName) =
           fun (manipulates: list<Manipulate<_, _>>) ->
               manipulates
               |> List.map (fun manipulate ->
                   manipulate.Flow
               )
               |> Flow<_, _>.Batch(?flowName = flowName)
               |> Manipulate




        static member Factory<'oldUserState, 'newUserState> (factory: Shrimp.Pdf.FlowModel<'oldUserState> -> IntegratedDocument -> Manipulate<'oldUserState, 'newUserState>) =
            Flow.FactoryByFlowModel(fun flowModel ->
                let document =
                    match flowModel.Document with 
                    | SplitOrIntegratedDocument.SplitDocument document -> failwith "Invalid token"
                    | SplitOrIntegratedDocument.IntegratedDocument document -> document

                let mainpulate = 
                    (factory 
                        (flowModel.ToPublicFlowModel())
                        document) :> Manipulate<_, _>
                mainpulate.Flow
            )
            |> Manipulate