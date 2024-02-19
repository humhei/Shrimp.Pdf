namespace Shrimp.Pdf.SlimFlow
#nowarn "0104"
open Shrimp.Pdf
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL

[<AutoOpen>]
module _SlimFlowUnion =

    type ISlimTupleFlow<'userState, 'newUserState> =
        abstract member Invoke: SlimFlowFunc<'userState, 'newUserState>
        abstract member SetParentFlowName: FlowName -> ISlimTupleFlow<'userState, 'newUserState>

    [<RequireQualifiedAccess>]
    type SlimFlowUnion<'userState, 'newUserState> =
        | Flow of SlimFlow<'userState, 'newUserState>
        | TupledFlow of ISlimTupleFlow<'userState, 'newUserState>
        | Func2 of (SlimFlowModel<'userState> -> PageModifingArguments<'userState> -> RenewableInfos -> SlimFlowUnion<'userState, 'newUserState>)
    with 

        member x.SetParentFlowName(flowName: FlowName) =
            match x with 
            | Flow flow -> flow.SetParentFlowName(flowName) |> Flow
            | TupledFlow tupleFlow -> tupleFlow.SetParentFlowName(flowName) |> TupledFlow
            | Func2 (f) ->
                Func2(
                    fun flowModel args infos ->
                        (f flowModel args infos).SetParentFlowName(flowName)
                )

        static member Func(f: PageModifingArguments<'userState> -> RenewableInfos -> SlimFlowUnion<'userState, 'newUserState>) =
            SlimFlowUnion.Func2(fun flowModel args infos ->
                f args infos
            )

        member x.Invoke: SlimFlowFunc<'userState, 'newUserState> =
            match x with 
            | Flow v -> v.Invoke
            | TupledFlow v -> v.Invoke
            | Func2 f -> 
                fun flowModel args infos ->
                    let flow = f flowModel args infos
                    flow.Invoke flowModel args infos


            

 
    type SlimTupledFlow<'userState, 'middleUserState, 'newUserState> =
        { Flow1: SlimFlowUnion<'userState, 'middleUserState>
          Flow2: SlimFlowUnion<'middleUserState, 'newUserState>
          FlowName: FlowName option }
    with 
        member internal x.Invoke(f) =
            fun flowModel args infos pageSetter ->
                let middleResult = x.Flow1.Invoke flowModel args infos pageSetter
                let result =
                    let newFlowModel = flowModel.MapUserState(fun _ -> middleResult.UserState)
                    let newArgs = args.MapUserState(fun _ -> middleResult.UserState)

                    x.Flow2.Invoke newFlowModel newArgs middleResult.Infos middleResult.WriterPageSetter

                f middleResult result

 

        static member Create(flow1: SlimFlowUnion<_, _>, flow2: SlimFlowUnion<_, _>, ?flowName) =
            let flow1 = 
                match flowName with 
                | None -> flow1
                | Some flowName -> flow1.SetParentFlowName(flowName)

            let flow2 = 
                match flowName with 
                | None -> flow2
                | Some flowName -> flow2.SetParentFlowName(flowName)

            { Flow1 = flow1 
              Flow2 = flow2
              FlowName = flowName }

        member x.SetFlowName(flowName) =
            SlimTupledFlow<_, _, _>.Create(x.Flow1, x.Flow2, flowName)

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

        interface ISlimTupleFlow<'userState, 'newUserState> with

            member x.Invoke = 
                x.Invoke(fun r1 r2 ->
                    r2
                )

            member x.SetParentFlowName(parentFlowName) =    
                x.SetParentFlowName(parentFlowName)
           
 

    type SlimTupledFlow_TupleUserState<'userState, 'middleUserState, 'newUserState> =
        { InnerFlow: SlimTupledFlow<'userState, 'middleUserState, 'newUserState> } 
    with
        static member Create(flow) =
            { InnerFlow = flow }


        interface ISlimTupleFlow<'userState, 'middleUserState * 'newUserState> with
            member x.Invoke = 
                x.InnerFlow.Invoke(fun r1 r2 ->
                    r2.MapUserState(fun userState2 ->
                        r1.UserState, userState2
                    )
                )

            member x.SetParentFlowName(parentFlowName) =    
                let result = 
                    x.InnerFlow.SetParentFlowName(parentFlowName)
                    |> SlimTupledFlow_TupleUserState<_, _, _>.Create

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
                    |> SlimTupledFlow_TupleUserState<_, _, _>.Create

                let result =
                    result 
                    |> unbox

                result



    type SlimFlowUnion<'userState, 'newUserState> with
        static member (<+>) (flow1: SlimFlowUnion<_, _>, flow2: SlimFlowUnion<_, _>) =
            let r = SlimTupledFlow<_, _, _>.Create(flow1, flow2)
            r :> ISlimTupleFlow<_, _>
            |> SlimFlowUnion.TupledFlow

        static member (<.+>) (flow1: SlimFlowUnion<_, _>, flow2: SlimFlowUnion<_, _>) =
            let r = 
                SlimTupledFlow<_, _, _>.Create(flow1, flow2)
                |> SlimTupledFlow_FstUserState
            r :> ISlimTupleFlow<_, _>
            |> SlimFlowUnion.TupledFlow

        static member (<++>) (flow1: SlimFlowUnion<_, _>, flow2: SlimFlowUnion<_, _>) =
            let r = 
                SlimTupledFlow<_, _, _>.Create(flow1, flow2)
                |> SlimTupledFlow_TupleUserState<_, _, _>.Create
            r :> ISlimTupleFlow<_, _>
            |> SlimFlowUnion.TupledFlow

        static member (||>>) (flow: SlimFlowUnion<_, _>, mapping) =
            let dummy =
                SlimFlow(fun flowModel args infos pageSetter ->
                    { Infos = infos 
                      UserState = mapping flowModel.UserState
                      WriterPageSetter = pageSetter
                    }
                )

            let dummy = SlimFlowUnion.Flow (dummy)
            flow 
            <+> dummy
