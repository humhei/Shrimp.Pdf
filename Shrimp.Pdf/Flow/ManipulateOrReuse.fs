namespace Shrimp.Pdf
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO


module internal ManipulateOrReuse =
    [<RequireQualifiedAccess>]
    type SplitOrIntegratedDocument =
        | SplitDocument of SplitDocument
        | IntegratedDocument of IntegratedDocument
    with 
        member x.ReOpen() =
            match x with 
            | SplitOrIntegratedDocument.SplitDocument document -> document.ReOpen()
            | SplitOrIntegratedDocument.IntegratedDocument document -> document.ReOpen()
    
        member x.ReaderPath =
            match x with 
            | SplitOrIntegratedDocument.SplitDocument document -> document.ReaderPath
            | SplitOrIntegratedDocument.IntegratedDocument document -> document.ReaderPath
    
    type internal INamedFlow<'oldUserState, 'newUserState> =
        abstract member FlowName: FlowName 
        abstract member Value: FlowModel<'oldUserState> -> SplitOrIntegratedDocument -> 'newUserState
        abstract member FlowNameIndexes: FlowNameIndex list
        abstract member MapState: ('oldUserState * 'newUserState -> 'a) -> INamedFlow<'oldUserState, 'a>
        abstract member MapStateBack: ('a -> 'oldUserState) -> INamedFlow<'a, 'newUserState>

    type SingletonNamedFlow<'oldUserState, 'newUserState> =
        { FlowName: FlowName 
          Value: FlowModel<'oldUserState> -> SplitOrIntegratedDocument -> 'newUserState 
          FlowNameIndexes: FlowNameIndex list }


    with
        interface INamedFlow<'oldUserState, 'newUserState> with 
            member x.FlowName = x.FlowName

            member x.FlowNameIndexes = x.FlowNameIndexes
    
            member x.Value flowModel document = 
                Logger.tryInfoWithFlowName x.FlowName x.FlowNameIndexes (fun _ ->
                    let userState = x.Value flowModel document
                    flowModel.TryBackupFile(x.FlowName, x.FlowNameIndexes)
                    userState
                )
                

            member x.MapState mapping = 
                { FlowName = x.FlowName 
                  Value = 
                    fun flowModel document ->
                        let newState = (x :> INamedFlow<_, _>).Value flowModel document
                        mapping(flowModel.UserState, newState)
                  FlowNameIndexes  = x.FlowNameIndexes
                } :> INamedFlow<_, _>

            member x.MapStateBack mapping = 
                { FlowName = x.FlowName 
                  Value = 
                    fun flowModel document ->
                        let flowModel = FlowModel.mapM mapping flowModel
                        (x :> INamedFlow<_, _>).Value flowModel document
                  FlowNameIndexes  = x.FlowNameIndexes
                } :> INamedFlow<_, _>

    type IFMonadState<'originUserState, 'newUserState> =
        abstract member Value: 'originUserState -> 'newUserState

    type TupledNamedFlow<'a, 'oldUserState, 'middleUserState, 'newUserState, 'finalUserState> =
        { Flow1: INamedFlow<'oldUserState, 'middleUserState>
          Flow2: INamedFlow<'middleUserState, 'newUserState>
          FlowName: FlowName
          FlowNameIndexes: FlowNameIndex list
          FMonadStateBack: 'a -> 'oldUserState
          FMonadState: obj -> ('middleUserState * 'newUserState) -> 'finalUserState }

    with
        member private x.TypedFMonadState (a, b, c, d) =
            x.FMonadState (box (a, b)) (c, d)

        interface INamedFlow<'a, 'finalUserState> with 
            member x.FlowName = x.FlowName

            member x.FlowNameIndexes = x.FlowNameIndexes

            member x.Value flowModel (document: SplitOrIntegratedDocument) = 
                let a = flowModel.UserState

                let flowModel = FlowModel.mapM x.FMonadStateBack flowModel

                let middleUserState = x.Flow1.Value flowModel document
                document.ReOpen()

                let newUserState = x.Flow2.Value (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document
                x.TypedFMonadState (a, flowModel.UserState, middleUserState, newUserState)

            member x.MapState mapping = 
                { Flow1 = x.Flow1
                  Flow2 = x.Flow2
                  FlowName = x.FlowName
                  FlowNameIndexes = x.FlowNameIndexes
                  FMonadStateBack = x.FMonadStateBack
                  FMonadState = 
                    fun (boxedValue) (c, d) ->
                        let (a, b) = unbox boxedValue
                        let e = x.TypedFMonadState(a, b, c, d)
                        mapping (a, e)

                } :> INamedFlow<_, _>

            member x.MapStateBack mapping = 
                { Flow1 = x.Flow1
                  Flow2 = x.Flow2
                  FlowName = x.FlowName
                  FlowNameIndexes = x.FlowNameIndexes
                  FMonadStateBack = mapping >> x.FMonadStateBack
                  FMonadState = x.FMonadState
                } :> INamedFlow<_, _>


    type Flow<'oldUserState, 'newUserState> (namedFlow: INamedFlow<'oldUserState, 'newUserState>, ?flowName: FlowName) =
        let flowName = defaultArg flowName FlowName.Default
    
        member private x.Spawn(namedFlow) =
            new Flow<_, _>(
                namedFlow = namedFlow,
                flowName = flowName
            )

        member internal x.AsNamedFlow = namedFlow
    
        static member Add(i, b) = i + b

        static member TupleState(flow: Flow<'oldUserState, 'newUserState>) =
            flow.AsNamedFlow.MapState id
            |> flow.Spawn

        static member MapState (flow: Flow<'oldUserState, 'newUserState>, mapping: 'newUserState -> 'a) =
            flow.AsNamedFlow.MapState (snd >> mapping)
            |> flow.Spawn
          
        static member MapStateBack (mapping, flow: Flow<_, _>) =
            flow.AsNamedFlow.MapStateBack(mapping)
            |> flow.Spawn
    
        static member Apply (flow1: Flow<'originUserState,'middleUserState>, flow2: Flow<'middleUserState,'modifiedUserState>,  fMonadState) =
            Flow(
                namedFlow = 
                    { Flow1 = flow1.AsNamedFlow 
                      Flow2 = flow2.AsNamedFlow 
                      FlowName = FlowName.Default
                      FlowNameIndexes = []
                      FMonadStateBack = id
                      FMonadState = 
                        fun _ (c, d) ->
                            fMonadState (c, d)
                    },
                flowName = FlowName.Default
            )
    
        new (f: FlowModel<'oldUserState> -> SplitOrIntegratedDocument -> 'newUserState, ?flowName: FlowName) =
    
            Flow(
                { FlowName = defaultArg flowName FlowName.Default
                  FlowNameIndexes = []
                  Value = f
                },
                FlowName.Default
            )
    
        new (f: FlowModel<'oldUserState> -> SplitOrIntegratedDocument -> 'newUserState, ?name: string) =
            new Flow<_, _>(
                f = f,
                flowName = 
                    match name with 
                    | None -> FlowName.Default
                    | Some name -> FlowName.Override name
            )
     
    [<RequireQualifiedAccess>]
    module Flow =
        let dummy() = 
            Flow<_, _>(
                ?name = None,
                f = 
                    fun flowModel document -> 
                    match document with 
                    | SplitOrIntegratedDocument.SplitDocument splitDocument ->
                        splitDocument.Reader.CopyPagesTo(1, splitDocument.Reader.GetNumberOfPages(), splitDocument.Writer)
                        |> ignore

                        flowModel.UserState 

                    | SplitOrIntegratedDocument.IntegratedDocument _ ->
                        flowModel.UserState
            )


        let batch flowName (flows: seq<Flow<'originUserState,'newUserState>>) =
            let rec loop (flowAccum: Flow<'originUserState, 'newUserState list> option) (flows: Flow<'originUserState, 'newUserState> list) =
                match flows with 
                | [] -> 
                    match flowAccum with 
                    | Some flow -> flow
                    | None -> 
                        Flow.MapState(
                            Flow(
                                namedFlow = dummy().AsNamedFlow,
                                flowName = flowName
                            ),
                            fun _ -> []
                        )

                | flow :: flows ->
                    match flowAccum with 
                    | None -> 
                        let flowAccum = Flow<_, _>.MapState(flow, List.singleton)
                        loop (Some flowAccum) flows

                    | Some flowAccum -> 

                        let flowAccum =
                            Flow<_, _>.TupleState(flowAccum)

                        let flow = 
                            Flow<_, _>.MapStateBack(fst, flow)
                        
                        let flowAccum = 
                            Flow<_, _>.Apply(flowAccum, flow, fun (a, b) -> snd a @ [b])

                        loop (Some flowAccum) flows

            loop None (List.ofSeq flows)


            
    

open ManipulateOrReuse

type Reuse<'oldUserState, 'newUserState> private (flow: Flow<'oldUserState, 'newUserState>) =
    member private x.Flow = flow

    member internal x.Value = 
        fun flowModel document ->
            x.Flow.AsNamedFlow.Value flowModel (SplitOrIntegratedDocument.SplitDocument document)
        

    static member (||>>) (reuse: Reuse<'oldUserState, 'newUserState>, mapping: 'newUserState -> 'a) =
        Flow<'oldUserState, 'newUserState>.MapState(reuse.Flow, mapping)
        |> Reuse

    static member (<<||) (mapping, reuse: Reuse<_, _>) =
        Flow<_, _>.MapStateBack(mapping, reuse.Flow)
        |> Reuse
    
    static member (<+>) (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Apply(reuse1.Flow, reuse2.Flow, snd)
        |> Reuse

    static member (<++>) (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Apply(reuse1.Flow, reuse2.Flow, id)
        |> Reuse

    static member (<.+>) (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Apply(reuse1.Flow, reuse2.Flow, fst)
        |> Reuse

    static member Batch flowName (reuses: seq<Reuse<_, _>>) = 
        reuses
        |> Seq.map (fun reuse ->
            reuse.Flow
        )
        |> Flow.batch flowName
        |> Reuse

    new (f: FlowModel<'oldUserState> -> SplitDocument -> 'newUserState, flowName: FlowName) =
        Reuse(
            new Flow<_, _>(
                flowName = flowName,
                f = fun flowModel document ->
                    match document with 
                    | SplitOrIntegratedDocument.SplitDocument splitDocument ->
                        f flowModel splitDocument

                    | SplitOrIntegratedDocument.IntegratedDocument _ ->
                        failwith "Invalid token"
            )
        )

    new (f: FlowModel<'oldUserState> -> SplitDocument -> 'newUserState, ?name: string) =
        Reuse(
            f = f,
            flowName = 
                match name with 
                | None -> FlowName.Default
                | Some name ->  FlowName.Override name
        )

    new (reuse: Reuse<'oldUserState, 'newUserState>, flowName: FlowName) =
        Reuse(
            Flow<_, _>(
                namedFlow = reuse.Flow.AsNamedFlow,
                flowName = flowName
            )
        )

    new (reuse: Reuse<'oldUserState, 'newUserState>, ?name: string) =
        Reuse(
            Flow<_, _>(
                namedFlow = reuse.Flow.AsNamedFlow,
                ?flowName = Option.map FlowName.Override name 
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




type Manipulate<'oldUserState, 'newUserState> private (flow: Flow<'oldUserState, 'newUserState>) =
    member private x.Flow = flow

    member internal x.Value = 
        fun flowModel document ->
            x.Flow.AsNamedFlow.Value flowModel (SplitOrIntegratedDocument.IntegratedDocument document)
        

    static member (||>>) (manipulate: Manipulate<'oldUserState, 'newUserState>, mapping: 'newUserState -> 'a) =
        Flow<'oldUserState, 'newUserState>.MapState(manipulate.Flow, mapping)
        |> Manipulate

    static member (<<||) (mapping, manipulate: Manipulate<_, _>) =
        Flow<_, _>.MapStateBack(mapping, manipulate.Flow)
        |> Manipulate
    
    static member (<+>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Apply(manipulate1.Flow, manipulate2.Flow, snd)
        |> Manipulate

    static member (<++>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Apply(manipulate1.Flow, manipulate2.Flow, id)
        |> Manipulate

    static member (<.+>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Apply(manipulate1.Flow, manipulate2.Flow, fst)
        |> Manipulate

    static member Batch flowName (manipulates: seq<Manipulate<_, _>>) = 
        manipulates
        |> Seq.map (fun manipulate ->
            manipulate.Flow
        )
        |> Flow.batch flowName
        |> Manipulate

    new (f: FlowModel<'oldUserState> -> IntegratedDocument -> 'newUserState, flowName: FlowName) =
        Manipulate(
            new Flow<_, _>(
                flowName = flowName,
                f = fun flowModel document ->
                    match document with 
                    | SplitOrIntegratedDocument.SplitDocument splitDocument ->
                        failwith "Invalid token"

                    | SplitOrIntegratedDocument.IntegratedDocument document ->
                        f flowModel document
            )
        )

    new (f: FlowModel<'oldUserState> -> IntegratedDocument -> 'newUserState, ?name: string) =
        Manipulate(
            f = f,
            flowName = 
                match name with 
                | None -> FlowName.Default
                | Some name ->  FlowName.Override name
        )

    new (manipulate: Manipulate<'oldUserState, 'newUserState>, ?flowName: FlowName) =
        Manipulate(
            Flow<_, _>(
                namedFlow = manipulate.Flow.AsNamedFlow,
                ?flowName = flowName
            )
        )

    new (manipulate: Manipulate<'oldUserState, 'newUserState>, ?name: string) =
        Manipulate(
            Flow<_, _>(
                namedFlow = manipulate.Flow.AsNamedFlow,
                ?flowName = Option.map FlowName.Override name 
            )
        )

[<RequireQualifiedAccess>]
module Manipualte =
    let dummy() = Manipulate(fun model _ -> model.UserState)