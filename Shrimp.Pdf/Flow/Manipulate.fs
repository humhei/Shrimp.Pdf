namespace Shrimp.Pdf
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO


type internal INamedManipulate<'oldUserState, 'newUserState> =
    abstract member FlowName: FlowName 
    abstract member Value: FlowModel<'oldUserState> -> IntegratedDocument -> 'newUserState
    abstract member FlowNameIndexes: FlowNameIndex list
    abstract member MapState: ('newUserState -> 'a) -> INamedManipulate<'oldUserState, 'a>
    abstract member MapStateBack: ('a -> 'oldUserState) -> INamedManipulate<'a, 'newUserState>


type internal SingletonNamedManipulate<'oldUserState, 'newUserState> =
    { FlowName: FlowName 
      Value: FlowModel<'oldUserState> -> IntegratedDocument -> 'newUserState 
      FlowNameIndexes: FlowNameIndex list}

with
    interface INamedManipulate<'oldUserState, 'newUserState> with 
        member x.FlowName = x.FlowName

        member x.FlowNameIndexes = x.FlowNameIndexes
        
        member x.Value flowModel document = x.Value flowModel document

        member x.MapState mapping = 
            { FlowName = x.FlowName 
              Value = 
                fun flowModel document ->
                    (x :> INamedManipulate<_, _>).Value flowModel document
                    |> mapping
              FlowNameIndexes  = x.FlowNameIndexes
            } :> INamedManipulate<_, _>

        member x.MapStateBack mapping = 
            { FlowName = x.FlowName 
              Value = 
                fun flowModel document ->
                    let flowModel = FlowModel.mapM mapping flowModel
                    (x :> INamedManipulate<_, _>).Value flowModel document
              FlowNameIndexes  = x.FlowNameIndexes
            } :> INamedManipulate<_, _>


type internal TupledNamedManipulate<'a, 'oldUserState, 'middleUserState, 'newUserState, 'finalUserState> =
    { Manipulate1: INamedManipulate<'oldUserState, 'middleUserState>
      Manipulate2: INamedManipulate<'middleUserState, 'newUserState>
      FlowName: FlowName
      FlowNameIndexes: FlowNameIndex list
      FMonadStateBack: 'a -> 'oldUserState
      FMonadState: ('middleUserState * 'newUserState) -> 'finalUserState }

with
    interface INamedManipulate<'a, 'finalUserState> with 
        member x.FlowName = x.FlowName

        member x.FlowNameIndexes = x.FlowNameIndexes

        member x.Value flowModel (document: IntegratedDocument) = 
            let flowModel = FlowModel.mapM x.FMonadStateBack flowModel
            let middleUserState = x.Manipulate1.Value flowModel document
            document.ReOpen()
            match x.FlowName with 
            | FlowName.Override name 
            | FlowName.New name ->
                match x.FlowNameIndexes with 
                | _ :: _ ->
                    let headerIndexes = x.FlowNameIndexes.[0 .. x.FlowNameIndexes.Length - 2]

                    let directory =
                        let rootDir = Path.getDirectory flowModel.File </> ".shrimp.pdf"
                        Directory.ensure rootDir
                        match headerIndexes with 
                        | [] -> rootDir
                        | _ :: _ ->
                            let headerDirNames =
                                headerIndexes 
                                      |> List.map (fun flowNameIndex ->
                                          sprintf "%d_%s" flowNameIndex.Index flowNameIndex.Name
                                      )

                            let directory = 
                                (rootDir :: headerDirNames)
                                |> List.reduce ((</>))
                            
                            Directory.ensure directory
                            
                            directory

                    let lastIndex = List.last x.FlowNameIndexes 

                    let targetPath = 
                        directory </> sprintf "%d_%s.pdf" lastIndex.Index lastIndex.Name

                    File.Copy(document.ReaderPath, targetPath)

                | [] -> failwithf "FlowName is setted while flow index is empty"

            | FlowName.Disable _
            | FlowName.Default _ -> ()
            let newUserState = x.Manipulate2.Value (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document
            x.FMonadState (middleUserState, newUserState)

        member x.MapState mapping = 
            { Manipulate1 = x.Manipulate1
              Manipulate2 = x.Manipulate2
              FlowName = x.FlowName
              FlowNameIndexes = x.FlowNameIndexes
              FMonadStateBack = x.FMonadStateBack
              FMonadState = 
                fun (a, b) ->
                    x.FMonadState (a, b)
                    |> mapping
            } :> INamedManipulate<_, _>

        member x.MapStateBack mapping = 
            { Manipulate1 = x.Manipulate1
              Manipulate2 = x.Manipulate2
              FlowName = x.FlowName
              FlowNameIndexes = x.FlowNameIndexes
              FMonadStateBack = mapping >> x.FMonadStateBack
              FMonadState = x.FMonadState
            } :> INamedManipulate<_, _>


type Manipulate<'oldUserState, 'newUserState> (namedManipulate: INamedManipulate<'oldUserState, 'newUserState>, ?flowName: FlowName) =
    let flowName = defaultArg flowName FlowName.Default

    member private x.Spawn(namedManipulate) =
        new Manipulate<_, _>(namedManipulate, flowName)

    member private x.AsNamedManipulate = namedManipulate

    member internal x.Value = namedManipulate.Value
        
    static member (||>>) (manipulate: Manipulate<_, _>, mapping) =
        manipulate.AsNamedManipulate.MapState (mapping)
        |> manipulate.Spawn
      

    static member (<<||) (mapping, manipulate: Manipulate<_, _>) =
        manipulate.AsNamedManipulate.MapStateBack(mapping)
        |> manipulate.Spawn

    static member (<+>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        { Manipulate1 = manipulate1.AsNamedManipulate 
          Manipulate2 = manipulate2.AsNamedManipulate 
          FlowName = FlowName.Default
          FlowNameIndexes = []
          FMonadStateBack = id
          FMonadState = snd }
        |> Manipulate

    static member (<++>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        { Manipulate1 = manipulate1.AsNamedManipulate 
          Manipulate2 = manipulate2.AsNamedManipulate 
          FlowName = FlowName.Default
          FMonadStateBack = id
          FlowNameIndexes = []
          FMonadState = id }
        |> Manipulate

    static member (<.+>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        { Manipulate1 = manipulate1.AsNamedManipulate 
          Manipulate2 = manipulate2.AsNamedManipulate 
          FlowName = FlowName.Default
          FlowNameIndexes = []
          FMonadStateBack = id
          FMonadState = fst }
        |> Manipulate

    private new (namedManipulate: INamedManipulate<'oldUserState, 'newUserState>) =
        Manipulate(
            namedManipulate,
            FlowName.Default
        )

    new (f: FlowModel<'oldUserState> -> IntegratedDocument -> 'newUserState, ?name: string) =

        Manipulate(
            { FlowName = 
                match name with 
                | None -> FlowName.Default
                | Some name -> FlowName.Override name
              FlowNameIndexes = []
              Value = f
            },
            FlowName.Default
        )





[<RequireQualifiedAccess>]
module Manipulate =
    /// followed by <++> or <.+> to restore userState
    let dummy() = Manipulate(fun model _ -> model.UserState)

    let batch flowName (manipulates: seq<Manipulate<'originUserState,'newUserState>>) =
        match List.ofSeq manipulates with 
        | [] -> dummy()
        | manipulates ->

        //let value = 
        //    fun flowModel (document: IntegratedDocument) ->
        //        let rec loop (manipulateAccum: Manipulate<'originUserState, 'newUserState list> option) (manipulates: Manipulate<'originUserState, 'newUserState> list) =
        //            match manipulates with 
        //            | [] -> 
        //                match manipulateAccum with 
        //                | Some manipulate -> manipulate.Value flowModel document
        //                | None -> []

        //            | manipulate :: manipulates ->
        //                match manipulateAccum with 
        //                | None -> 
        //                    let manipulateAccum = manipulate ||>> List.singleton
        //                    loop (Some manipulateAccum) manipulates

        //                | Some manipulateAccum -> 
        //                    let manipulate = (fun _ -> flowModel.UserState) <<|| manipulate 
        //                    let manipulateAccum = 
        //                        (manipulateAccum <++> manipulate)
        //                        ||>> (fun (a, b) -> a @ [b])

        //                    loop (Some manipulateAccum) manipulates

        //        loop None (List.ofSeq manipulates)

        

