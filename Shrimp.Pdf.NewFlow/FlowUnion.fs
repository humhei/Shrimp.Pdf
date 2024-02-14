namespace Shrimp.Pdf.SlimFlow
#nowarn "0104"
open Shrimp.Pdf
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL

[<AutoOpen>]
module rec _SlimFlowUnion =

    type ISlimTupleFlow<'userState, 'newUserState> =
        abstract member Invoke: SlimFlowFunc<'userState, 'newUserState>
        abstract member SetParentFlowName: FlowName -> ISlimTupleFlow<'userState, 'newUserState>

    [<RequireQualifiedAccess>]
    type SlimFlowUnion<'userState, 'newUserState> =
        | Flow of SlimFlow<'userState, 'newUserState>
        | TupledFlow of ISlimTupleFlow<'userState, 'newUserState>
        | Func of (PageModifingArguments<'userState> -> RenewableInfos -> SlimFlowUnion<'userState, 'newUserState>)
    with 
        member x.SetParentFlowName(flowName: FlowName) =
            match x with 
            | Flow flow -> flow.SetParentFlowName(flowName) |> Flow
            | TupledFlow tupleFlow -> tupleFlow.SetParentFlowName(flowName) |> TupledFlow
            | Func (f) ->
                Func(
                    fun args infos ->
                        (f args infos).SetParentFlowName(flowName)
                )

        member x.Invoke: SlimFlowFunc<'userState, 'newUserState> =
            match x with 
            | Flow v -> v.Invoke
            | TupledFlow v -> v.Invoke
            | Func f -> 
                fun flowModel args infos ->
                    let flow = f args infos
                    flow.Invoke flowModel args infos

        static member (<+>) (flow1: SlimFlowUnion<_, _>, flow2: SlimFlowUnion<_, _>) =
            let r = SlimTupledFlow(flow1, flow2)
            r :> ISlimTupleFlow<_, _>
            |> SlimFlowUnion.TupledFlow

        static member (<.+>) (flow1: SlimFlowUnion<_, _>, flow2: SlimFlowUnion<_, _>) =
            let r = 
                SlimTupledFlow(flow1, flow2)
                |> SlimTupledFlow_FstUserState
            r :> ISlimTupleFlow<_, _>
            |> SlimFlowUnion.TupledFlow

        static member (<++>) (flow1: SlimFlowUnion<_, _>, flow2: SlimFlowUnion<_, _>) =
            let r = 
                SlimTupledFlow(flow1, flow2)
                |> SlimTupledFlow_TupleUserState
            r :> ISlimTupleFlow<_, _>
            |> SlimFlowUnion.TupledFlow

    type SlimTupledFlow<'userState, 'middleUserState, 'newUserState>(flow1: SlimFlowUnion<'userState, 'middleUserState>, flow2: SlimFlowUnion<'middleUserState, 'newUserState>, ?flowName) =
        let flow1 = 
            match flowName with 
            | None -> flow1
            | Some flowName -> flow1.SetParentFlowName(flowName)

        let flow2 = 
            match flowName with 
            | None -> flow2
            | Some flowName -> flow2.SetParentFlowName(flowName)

        member x.FlowName = flowName

        member x.SetFlowName(flowName) =
            SlimTupledFlow(flow1, flow2, flowName)

        member x.SetParentFlowName(parentFlowName) = 
            let newFlowName =
                match x.FlowName with 
                | None -> None
                | Some flowName -> 
                    flowName.SetParentFlowName(parentFlowName)
                    |> Some

            match newFlowName with 
            | None -> x
            | Some newFlowName -> x.SetFlowName(newFlowName)

        member x.Flow1 = flow1
        member x.Flow2 = flow2
        member internal x.Invoke(f) =
            fun flowModel args infos pageSetter ->
                let middleResult = flow1.Invoke flowModel args infos pageSetter
                let result =
                    let newFlowModel = flowModel.MapUserState(fun _ -> middleResult.UserState)
                    let newArgs = args.MapUserState(fun _ -> middleResult.UserState)

                    flow2.Invoke newFlowModel newArgs middleResult.Infos middleResult.WriterPageSetter

                f middleResult result


        interface ISlimTupleFlow<'userState, 'newUserState> with
            member x.Invoke = 
                x.Invoke(fun r1 r2 ->
                    r2
                )

            member x.SetParentFlowName(parentFlowName) = x.SetParentFlowName(parentFlowName)

            
    type SlimTupledFlow_TupleUserState<'userState, 'middleUserState, 'newUserState>(innerFlow: SlimTupledFlow<'userState, 'middleUserState, 'newUserState>) =

        interface ISlimTupleFlow<'userState, 'middleUserState * 'newUserState> with
            member x.Invoke = 
                innerFlow.Invoke(fun r1 r2 ->
                    r2.MapUserState(fun userState2 ->
                        r1.UserState, userState2
                    )
                )

            member x.SetParentFlowName(parentFlowName) =    
                let result = 
                    innerFlow.SetParentFlowName(parentFlowName)
                    |> SlimTupledFlow_TupleUserState

                let result =
                    result 
                    |> unbox

                result

    type SlimTupledFlow_FstUserState<'userState, 'middleUserState, 'newUserState>(innerFlow: SlimTupledFlow<'userState, 'middleUserState, 'newUserState>) =

        interface ISlimTupleFlow<'userState, 'middleUserState> with
            member x.Invoke = 
                innerFlow.Invoke(fun r1 r2 ->
                    r2.MapUserState(fun userState2 ->
                        r1.UserState
                    )
                )

            member x.SetParentFlowName(parentFlowName) =    
                let result = 
                    innerFlow.SetParentFlowName(parentFlowName)
                    |> SlimTupledFlow_TupleUserState

                let result =
                    result 
                    |> unbox

                result

