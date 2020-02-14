namespace Shrimp.Pdf
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO


type internal INamedManipulate<'oldUserState, 'newUserState> =
    abstract member FlowName: FlowName 
    abstract member Value: FlowModel<'oldUserState> -> IntegratedDocument -> 'newUserState
    abstract member FlowIndex: int option
    abstract member MapState: ('newUserState -> 'a) -> INamedManipulate<'oldUserState, 'a>
    abstract member MapStateBack: ('a -> 'oldUserState) -> INamedManipulate<'a, 'newUserState>


type internal SingletonNamedManipulate<'oldUserState, 'newUserState> =
    { FlowName: FlowName 
      Value: FlowModel<'oldUserState> -> IntegratedDocument -> 'newUserState 
      FlowIndex: int option}

with
    interface INamedManipulate<'oldUserState, 'newUserState> with 
        member x.FlowName = x.FlowName

        member x.FlowIndex = x.FlowIndex
        
        member x.Value flowModel document = x.Value flowModel document

        member x.MapState mapping = 
            { FlowName = x.FlowName 
              Value = 
                fun flowModel document ->
                    (x :> INamedManipulate<_, _>).Value flowModel document
                    |> mapping
              FlowIndex  = x.FlowIndex
            } :> INamedManipulate<_, _>

        member x.MapStateBack mapping = 
            { FlowName = x.FlowName 
              Value = 
                fun flowModel document ->
                    let flowModel = FlowModel.mapM mapping flowModel
                    (x :> INamedManipulate<_, _>).Value flowModel document
              FlowIndex  = x.FlowIndex
            } :> INamedManipulate<_, _>


type internal TupledNamedManipulate<'oldUserState, 'middleUserState, 'newUserState, 'finalUserState> =
    { Manipulate1: INamedManipulate<'oldUserState, 'middleUserState>
      Manipulate2: INamedManipulate<'middleUserState, 'newUserState>
      FlowName: FlowName
      FlowIndex: int option
      FMonadState: ('middleUserState * 'newUserState) -> 'finalUserState }

with
    interface INamedManipulate<'oldUserState, 'finalUserState> with 
        member x.FlowName = x.FlowName

        member x.FlowIndex = x.FlowIndex

        member x.Value flowModel (document: IntegratedDocument) = 
            let middleUserState = x.Manipulate1.Value flowModel document
            document.ReOpen()
            
            let newUserState = x.Manipulate2.Value (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document
            x.FMonadState (middleUserState, newUserState)

        member x.MapState mapping = 
            { FlowName = x.FlowName 
              Value = 
                fun flowModel document ->
                    (x :> INamedManipulate<_, _>).Value flowModel document
                    |> mapping
              FlowIndex  = x.FlowIndex
            } :> INamedManipulate<_, _>

        member x.MapStateBack mapping = 
            { FlowName = x.FlowName 
              Value = 
                fun flowModel document ->
                    let flowModel = FlowModel.mapM mapping flowModel
                    (x :> INamedManipulate<_, _>).Value flowModel document
              FlowIndex  = x.FlowIndex
            } :> INamedManipulate<_, _>


type Manipulate<'oldUserState, 'newUserState> internal (namedManipulate: INamedManipulate<'oldUserState, 'newUserState>) =
    
    member private x.AsNamedManipulate = namedManipulate

    member x.Value = namedManipulate.Value
        
    static member (||>>) (manipulate: Manipulate<_, _>, mapping) =
        manipulate.AsNamedManipulate.MapState (mapping)
        |> Manipulate
      

    static member (<<||) (mapping, manipulate: Manipulate<_, _>) =
        manipulate.AsNamedManipulate.MapStateBack(mapping)
        |> Manipulate

    static member (<+>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        { Manipulate1 = manipulate1.AsNamedManipulate 
          Manipulate2 = manipulate2.AsNamedManipulate 
          FlowName = FlowName.None
          FlowIndex = None 
          FMonadState = snd }
        |> Manipulate

    static member (<++>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        { Manipulate1 = manipulate1.AsNamedManipulate 
          Manipulate2 = manipulate2.AsNamedManipulate 
          FlowName = FlowName.None
          FlowIndex = None 
          FMonadState = id }
        |> Manipulate

    static member (<.+>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        { Manipulate1 = manipulate1.AsNamedManipulate 
          Manipulate2 = manipulate2.AsNamedManipulate 
          FlowName = FlowName.None
          FlowIndex = None 
          FMonadState = fst }
        |> Manipulate

    new (f: FlowModel<'oldUserState> -> IntegratedDocument -> 'newUserState, ?name: string) =
         Manipulate(
            { FlowName = 
                match name with 
                | None -> FlowName.None
                | Some name -> FlowName.Usable name
              FlowIndex = None
              Value = f
            }
         )




[<RequireQualifiedAccess>]
module Manipulate =
    /// followed by <++> or <.+> to restore userState
    let dummy() = Manipulate(fun model _ -> model.UserState)

    let batch flowName (manipulates: seq<Manipulate<'originUserState,'newUserState>>) =
        let value = 
            fun flowModel (document: IntegratedDocument) ->
                let rec loop (manipulateAccum: Manipulate<'originUserState, 'newUserState list> option) (manipulates: Manipulate<'originUserState, 'newUserState> list) =
                    match manipulates with 
                    | [] -> 
                        match manipulateAccum with 
                        | Some manipulate -> manipulate.Value flowModel document
                        | None -> []

                    | manipulate :: manipulates ->
                        match manipulateAccum with 
                        | None -> 
                            let manipulateAccum = manipulate ||>> List.singleton
                            loop (Some manipulateAccum) manipulates

                        | Some manipulateAccum -> 
                            let manipulate = (fun _ -> flowModel.UserState) <<|| manipulate 
                            let manipulateAccum = 
                                (manipulateAccum <++> manipulate)
                                ||>> (fun (a, b) -> a @ [b])

                            loop (Some manipulateAccum) manipulates

                loop None (List.ofSeq manipulates)

        
        Manipulate(value, flowName)

