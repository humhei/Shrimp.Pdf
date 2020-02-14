namespace Shrimp.Pdf
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO


type internal INamedReuse<'oldUserState, 'newUserState> =
    abstract member FlowName: FlowName 
    abstract member Value: FlowModel<'oldUserState> -> SplitDocument -> 'newUserState
    abstract member FlowIndex: int option
    abstract member MapState: ('newUserState -> 'a) -> INamedReuse<'oldUserState, 'a>
    abstract member MapStateBack: ('a -> 'oldUserState) -> INamedReuse<'a, 'newUserState>


type internal SingletonNamedReuse<'oldUserState, 'newUserState> =
    { FlowName: FlowName 
      Value: FlowModel<'oldUserState> -> SplitDocument -> 'newUserState 
      FlowIndex: int option}

with
    interface INamedReuse<'oldUserState, 'newUserState> with 
        member x.FlowName = x.FlowName

        member x.FlowIndex = x.FlowIndex
        
        member x.Value flowModel document = x.Value flowModel document

        member x.MapState mapping = 
            { FlowName = x.FlowName 
              Value = 
                fun flowModel document ->
                    (x :> INamedReuse<_, _>).Value flowModel document
                    |> mapping
              FlowIndex  = x.FlowIndex
            } :> INamedReuse<_, _>

        member x.MapStateBack mapping = 
            { FlowName = x.FlowName 
              Value = 
                fun flowModel document ->
                    let flowModel = FlowModel.mapM mapping flowModel
                    (x :> INamedReuse<_, _>).Value flowModel document
              FlowIndex  = x.FlowIndex
            } :> INamedReuse<_, _>


type internal TupledNamedReuse<'oldUserState, 'middleUserState, 'newUserState, 'finalUserState> =
    { Reuse1: INamedReuse<'oldUserState, 'middleUserState>
      Reuse2: INamedReuse<'middleUserState, 'newUserState>
      FlowName: FlowName
      FlowIndex: int option
      FMonadState: ('middleUserState * 'newUserState) -> 'finalUserState }

with
    interface INamedReuse<'oldUserState, 'finalUserState> with 
        member x.FlowName = x.FlowName

        member x.FlowIndex = x.FlowIndex

        member x.Value flowModel (document: SplitDocument) = 
            let middleUserState = x.Reuse1.Value flowModel document
            document.ReOpen()
            
            match x.FlowName with 
            | FlowName.Usable name ->
                match x.FlowIndex with 
                | Some index ->
                    let targetPath = 
                        let directory = Path.getDirectory flowModel.File </> ".shrimp.pdf"
                        Directory.ensure directory

                        directory </> sprintf "%d_%s.pdf" index name

                    File.Copy(document.ReaderPath, targetPath)

                | None -> failwithf "FlowName is setted while flow index is empty"

            | FlowName.Overrided _
            | FlowName.None _ -> ()

            let newUserState = x.Reuse2.Value (flowModel |> FlowModel.mapM(fun _ -> middleUserState)) document
            x.FMonadState (middleUserState, newUserState)

        member x.MapState mapping = 
            { FlowName = x.FlowName 
              Value = 
                fun flowModel document ->
                    (x :> INamedReuse<_, _>).Value flowModel document
                    |> mapping
              FlowIndex  = x.FlowIndex
            } :> INamedReuse<_, _>

        member x.MapStateBack mapping = 
            { FlowName = x.FlowName 
              Value = 
                fun flowModel document ->
                    let flowModel = FlowModel.mapM mapping flowModel
                    (x :> INamedReuse<_, _>).Value flowModel document
              FlowIndex  = x.FlowIndex
            } :> INamedReuse<_, _>

type Reuse<'oldUserState, 'newUserState> internal (namedReuse: INamedReuse<'oldUserState, 'newUserState>) =
    member private x.AsNamedReuse = namedReuse

    member x.Value = namedReuse.Value
        
    static member (||>>) (reuse: Reuse<_, _>, mapping) =
        reuse.AsNamedReuse.MapState (mapping)
        |> Reuse

    static member (<<||) (mapping, reuse: Reuse<_, _>) =
        reuse.AsNamedReuse.MapStateBack(mapping)
        |> Reuse

    static member (<+>) (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        { Reuse1 = reuse1.AsNamedReuse 
          Reuse2 = reuse2.AsNamedReuse 
          FlowName = FlowName.None
          FlowIndex = None 
          FMonadState = snd }
        |> Reuse

    static member (<++>) (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        { Reuse1 = reuse1.AsNamedReuse 
          Reuse2 = reuse2.AsNamedReuse 
          FlowName = FlowName.None
          FlowIndex = None 
          FMonadState = id }
        |> Reuse

    static member (<.+>) (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        { Reuse1 = reuse1.AsNamedReuse 
          Reuse2 = reuse2.AsNamedReuse 
          FlowName = FlowName.None
          FlowIndex = None 
          FMonadState = fst }
        |> Reuse

    new (f: FlowModel<'oldUserState> -> SplitDocument -> 'newUserState, ?name: string) =
         Reuse(
            { FlowName = 
                match name with 
                | None -> FlowName.None
                | Some name -> FlowName.Usable name
              FlowIndex = None
              Value = f
            }
         )



[<RequireQualifiedAccess>]
module Reuse =

    let dummy() = 
        Reuse(fun flowModel splitDocument -> 
            splitDocument.Reader.CopyPagesTo(1, splitDocument.Reader.GetNumberOfPages(), splitDocument.Writer)
            |> ignore

            flowModel.UserState 
        )

    let batch flowName (reuses: seq<Reuse<'originUserState,'newUserState>>) =
        let value = 
            fun flowModel (document: SplitDocument) ->
                let rec loop (reuseAccum: Reuse<'originUserState, 'newUserState list> option) (reuses: Reuse<'originUserState, 'newUserState> list) =
                    match reuses with 
                    | [] -> 
                        match reuseAccum with 
                        | Some reuse -> reuse.Value flowModel document
                        | None -> []

                    | reuse :: reuses ->
                        match reuseAccum with 
                        | None -> 
                            let reuseAccum = reuse ||>> List.singleton
                            loop (Some reuseAccum) reuses

                        | Some reuseAccum -> 
                            let reuse = (fun _ -> flowModel.UserState) <<|| reuse 
                            let reuseAccum = 
                                (reuseAccum <++> reuse)
                                ||>> (fun (a, b) -> a @ [b])

                            loop (Some reuseAccum) reuses

                loop None (List.ofSeq reuses)

        
        Reuse(value, flowName)