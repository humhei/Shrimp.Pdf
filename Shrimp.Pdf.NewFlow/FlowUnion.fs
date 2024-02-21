namespace Shrimp.Pdf.SlimFlow
#nowarn "0104"
open iText.Kernel.Pdf
open Shrimp.Pdf
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL

[<AutoOpen>]
module _SlimFlowUnion =
    
    type ISlimTupleFlow<'oldUserState, 'newUserState> =
        abstract member Invoke: SlimFlowFunc<'oldUserState, 'newUserState>
            
        abstract member MapState: ('newUserState -> 'a) -> ISlimTupleFlow<'oldUserState, 'a>
    
        abstract member MapStateBack: ('a -> 'oldUserState) -> ISlimTupleFlow<'a, 'newUserState>
    
        abstract member SetParentFlowName: FlowName -> ISlimTupleFlow<'oldUserState, 'newUserState>

        abstract member InPage: PageSelector -> ISlimTupleFlow<'oldUserState, 'newUserState option>


    [<RequireQualifiedAccess>]
    type SlimFlowUnion<'oldUserState, 'newUserState> =
        | Flow of SlimFlow<'oldUserState, 'newUserState>
        | TupledFlow of ISlimTupleFlow<'oldUserState, 'newUserState>
        | Func2 of (SlimFlowModel<'oldUserState> -> PageModifingArguments<'oldUserState> -> RenewableInfos -> SlimFlowUnion<'oldUserState, 'newUserState>)
    with 

                
        member x.MapState(mapping) =
            match x with 
            | Flow flow ->
                SlimFlow.mapState mapping flow
                |> Flow

            | TupledFlow flow -> flow.MapState(mapping) |> TupledFlow
            | Func2 f -> 
                Func2(
                    fun flowModel args infos ->
                        let flow = (f flowModel args infos)
                        flow.MapState(mapping)
                )

        member x.MapStateBack(mapping) =
            match x with 
            | Flow flow ->
                SlimFlow.mapStateBack mapping flow
                |> Flow

            | TupledFlow flow -> flow.MapStateBack(mapping) |> TupledFlow
            | Func2 f -> 
                Func2(
                    fun flowModel args infos ->
                        let args = args.MapUserState mapping
                        let flowModel = flowModel.MapUserState mapping
                        let flow = (f flowModel args infos)
                        flow.MapStateBack mapping
                )

        member x.InPage(pageSelector: PageSelector) =
            match x with 
            | Flow flow ->
                SlimFlow.applyPageSelector pageSelector flow
                |> Flow

            | TupledFlow flow -> 
                flow.InPage(pageSelector)
                |> TupledFlow

            | Func2 f -> 
                Func2(
                    fun flowModel args infos ->
                        match PageModifingArguments.isCurrentPageSelected pageSelector args with 
                        | true ->
                            let flow = (f flowModel args infos)
                            flow.MapState(Some)

                        | false -> 
                            let flow =  
                                SlimFlow.dummy()
                                |> SlimFlowUnion.Flow

                            flow.MapState(fun _ -> None)
                )

        member x.SetParentFlowName(flowName: FlowName) =
            match x with 
            | Flow flow -> flow.SetParentFlowName(flowName) |> Flow
            | TupledFlow tupleFlow -> tupleFlow.SetParentFlowName(flowName) |> TupledFlow
            | Func2 (f) ->
                Func2(
                    fun flowModel args infos ->
                        (f flowModel args infos).SetParentFlowName(flowName)
                )

        static member Func(f: PageModifingArguments<'oldUserState> -> RenewableInfos -> SlimFlowUnion<'oldUserState, 'newUserState>) =
            SlimFlowUnion.Func2(fun flowModel args infos ->
                f args infos
            )

        member x.Invoke: SlimFlowFunc<'oldUserState, 'newUserState> =
            match x with 
            | Flow v -> v.Invoke
            | TupledFlow v -> v.Invoke
            | Func2 f -> 
                fun flowModel args infos ->
                    let flow = f flowModel args infos
                    flow.Invoke flowModel args infos


    type internal SlimTupleFlow<'a, 'oldUserState, 'middleUserState, 'newUserState, 'finalUserState> =
        { Flow1: SlimFlowUnion<'oldUserState, 'middleUserState>
          Flow2: SlimFlowUnion<'middleUserState, 'newUserState>
          FMonadStateBack : 'a -> 'oldUserState
          FMonadState: 'middleUserState * 'newUserState -> 'finalUserState }
    with 
        interface ISlimTupleFlow<'a, 'finalUserState> with 
            member x.Invoke =
                fun (flowModel) (args: PageModifingArguments<_>) infos pageSetter ->

                    let flowModel = flowModel.MapUserState x.FMonadStateBack 
                    let args      = args.MapUserState x.FMonadStateBack

                    let middleSlimFlowResult = x.Flow1.Invoke flowModel args infos pageSetter

                    let middleFlowModel = flowModel.MapUserState(fun _ -> middleSlimFlowResult.UserState)
                    let middleArgs = args.MapUserState(fun _ -> middleSlimFlowResult.UserState)

                    let finalSlimFlowResult = x.Flow2.Invoke middleFlowModel middleArgs middleSlimFlowResult.Infos middleSlimFlowResult.WriterPageSetter
                    let finalUserState = x.FMonadState (middleFlowModel.UserState, finalSlimFlowResult.UserState)

                    finalSlimFlowResult.MapUserState(fun _ -> finalUserState)
                    


            member x.MapState(mapping) =
                { Flow1 = x.Flow1 
                  Flow2 = x.Flow2 
                  FMonadStateBack = x.FMonadStateBack
                  FMonadState = 
                    fun (middleUserState, newUserState) -> 
                        x.FMonadState (middleUserState, newUserState)
                        |> mapping

                } :> ISlimTupleFlow<_, _>

            member x.MapStateBack(mapping) =
                { Flow1 = x.Flow1 
                  Flow2 = x.Flow2 
                  FMonadStateBack = mapping >> x.FMonadStateBack
                  FMonadState = x.FMonadState
                } :> ISlimTupleFlow<_, _>


            member x.SetParentFlowName(parentFlowName) =
                { Flow1 = x.Flow1.SetParentFlowName(parentFlowName)
                  Flow2 = x.Flow2.SetParentFlowName(parentFlowName)
                  FMonadStateBack = x.FMonadStateBack
                  FMonadState = x.FMonadState
                } :> ISlimTupleFlow<_, _>


            member x.InPage(pageSelector) =
                let r = 
                    { Flow1 = x.Flow1.InPage(pageSelector)
                      Flow2 = 
                        SlimFlowUnion.Func(fun args infos ->
                            match args.UserState with 
                            | Some _ -> 
                                let flow2 = x.Flow2.MapStateBack(fun (m: option<_>) ->
                                    m.Value
                                )
                                flow2.InPage(pageSelector)

                            | None -> 
                                let flow = 
                                    SlimFlow.dummy()
                                    |> SlimFlowUnion.Flow
                                flow.MapState(fun _ -> None)
                        )
                
                      FMonadStateBack = x.FMonadStateBack
                      FMonadState = 
                        fun (a, b) -> 
                            match a, b with 
                            | None, None -> None
                            | Some a, Some b -> Some (x.FMonadState (a, b))
                            | _ -> failwithf "Invalid userState %A" (a, b)

                    } :> ISlimTupleFlow<_, _>

                r

    type SlimFlowUnion<'oldUserState, 'newUserState> with
        static member (<+>) (flow1: SlimFlowUnion<_, _>, flow2: SlimFlowUnion<_, _>) =
            { Flow1 = flow1 
              Flow2 = flow2 
              FMonadStateBack = id
              FMonadState = snd } :> ISlimTupleFlow<_, _>
            |> SlimFlowUnion.TupledFlow

        static member (<++>) (flow1: SlimFlowUnion<'originUserState, 'middleUserState>, flow2: SlimFlowUnion<'middleUserState, 'modifiedUserState>) =
          { Flow1 = flow1 
            Flow2 = flow2 
            FMonadStateBack = id
            FMonadState = id

          } :> ISlimTupleFlow<_, _>
            |> SlimFlowUnion.TupledFlow


        static member (<.+>) (flow1: SlimFlowUnion<'originUserState, 'middleUserState>, flow2: SlimFlowUnion<'middleUserState, 'modifiedUserState>) =
            
            { Flow1 = flow1 
              Flow2 = flow2 
              FMonadStateBack = id
              FMonadState = fst } :> ISlimTupleFlow<_, _>
            |> SlimFlowUnion.TupledFlow


        static member (||>>) (flow: SlimFlowUnion<'originUserState, 'newUserState>, mapping: 'newUserState -> 'a) =
            flow.MapState(mapping)
            
        static member (<<||) (flow: SlimFlowUnion<'originUserState, 'newUserState>, mapping) =
            flow.MapStateBack(mapping)

