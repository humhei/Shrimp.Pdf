﻿namespace Shrimp.Pdf
#nowarn "0104"
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO


type NameAndParamters =
    { Name: string 
      Parameters: list<string * string>}

type FlowModel<'userState> =
    { File: string 
      UserState: 'userState }

[<RequireQualifiedAccess>]
module FlowModel =
    let mapM mapping flowModel =
        { File = flowModel.File 
          UserState = mapping flowModel.UserState }

    let mapTo userState flowModel =
        { File = flowModel.File 
          UserState = userState }



[<RequireQualifiedAccess>]
type internal FlowNameKind =
    | Override of string * list<string * string>
    | New of string * list<string * string>
    | Disable
with 
    member x.Name =
        match x with 
        | FlowNameKind.New (name, _) 
        | FlowNameKind.Override (name, _) -> Some name
        | FlowNameKind.Disable -> None

    member x.NameAndParameters =
        match x with 
        | FlowNameKind.New (name, parameters) 
        | FlowNameKind.Override (name, parameters) -> Some (name, parameters)
        | FlowNameKind.Disable -> None


[<Sealed>]
type FlowName internal (flowNameKind: FlowNameKind, ?parentFlowName) =

    let parentFlowName: FlowName option = parentFlowName

    member internal x.FlowNameKind = flowNameKind

    member private x.ParentFlowName = parentFlowName

    member private x.RelativeDirectoryNamesOp =
        match x.FlowNameKind with 
        | FlowNameKind.New _ 
        | FlowNameKind.Override _ ->
            let rec loop names flowName =
                match flowName with 
                | None -> Some names
                | Some (flowName: FlowName) -> 
                    let name = flowName.FlowNameKind.Name
                    match name with 
                    | Some name ->
                        loop (name :: names) flowName.ParentFlowName

                    | None -> None

            loop [] x.ParentFlowName

        | FlowNameKind.Disable _ -> None

    member internal x.RelativeDirectoryNames =
        match x.RelativeDirectoryNamesOp with 
        | Some names -> names
        | None -> []

    member internal x.RelativeDirectory =
        match x.RelativeDirectoryNamesOp with 
        | Some names ->
            (names)
            |> String.concat (string System.IO.Path.DirectorySeparatorChar)
            |> Some

        | None -> None
     
    static member Override (name: string, ?paramters) = 
        let paramters = defaultArg paramters []
        if name.IndexOfAny(Path.GetInvalidPathChars()) > 0 then failwithf "Invalid path %s" name
        if name.Length > 60 then failwithf "flowName %s 's length is bigger 60" name
        (name, paramters)
        |> FlowNameKind.Override
        |> FlowName

    member internal x.SetParentFlowName(parentFlowName) = 
        FlowName(x.FlowNameKind, parentFlowName)

    static member New (name: string, ?paramters) = 
        let paramters = defaultArg paramters []
        if name.IndexOfAny(Path.GetInvalidPathChars()) > 0 then failwithf "Invalid path %s" name
        if name.Length > 60 then failwithf "flowName %s 's length is bigger 60" name
        (name, paramters)
        |> FlowNameKind.New
        |> FlowName

    static member Disable: FlowName = FlowName(FlowNameKind.Disable)

[<RequireQualifiedAccess>]
module internal FlowName =
    let (|Override|New|Disable|) (flowName: FlowName) =
        match flowName.FlowNameKind with 
        | FlowNameKind.New _ -> New
        | FlowNameKind.Override _ -> Override
        | FlowNameKind.Disable _ -> Disable


    let isDisable (flowName: FlowName) =
        match flowName with 
        | Disable -> true
        | _ -> false



[<RequireQualifiedAccess>]
type internal FlowNameTupleBindedStatus =
    | Binding of previousFlowName: FlowName option
    | None





type internal InternalFlowModel<'userState> =
    { File: string 
      UserState: 'userState 
      FlowName: FlowName option
      OperatedFlowNames: FlowName list }

with

    member internal flowModel.TryGetBackupDirectory() =
        match flowModel.FlowName with 
        | None -> None
        | Some flowName ->
            match flowName.RelativeDirectory with 
            | Some relativeDir ->
                let dir = 
                    Path.getDirectory flowModel.File 
                    </> ".shrimp.pdf" 
                    </> (Path.GetFileNameWithoutExtension flowModel.File) 
                    </> relativeDir 

                Directory.ensure dir

                Some dir
                

            | None -> None


    member internal flowModel.TryBackupFile(index: int) =
        match flowModel.TryGetBackupDirectory() with 
        | Some directory ->
            match flowModel.FlowName with 
            | Some flowName ->
                match flowName.FlowNameKind.Name with 
                | Some fileName ->
                    let targetPath = 
                        directory </> sprintf "%d_%s.pdf" index fileName

                    File.Copy(flowModel.File, targetPath, true)

                | None -> ()

            | None -> ()

        | None -> ()




[<RequireQualifiedAccess>]
module internal InternalFlowModel =
    let mapM mapping (flowModel: InternalFlowModel<_>) =
        { File = flowModel.File 
          UserState = mapping flowModel.UserState 
          FlowName = flowModel.FlowName
          OperatedFlowNames = flowModel.OperatedFlowNames }

    let toFlowModel (internalFlowModel: InternalFlowModel<_>): FlowModel<_> =
        { File = internalFlowModel.File 
          UserState = internalFlowModel.UserState }


    let mapTo userState (flowModel: InternalFlowModel<_>) =
        mapM (fun _ -> userState) flowModel


type InternalFlowModelWrapper<'userState> internal (internalFlowModel: InternalFlowModel<'userState>) =
    member internal x.Value = internalFlowModel

[<AutoOpen>]
module internal Logger_FlowModel =
    type Logger =
        static member TryInfoWithFlowModel (flowNameIndex, flowModel: InternalFlowModel<_>, f) =
            match flowModel.FlowName with 
            | Some flowName ->
                match flowName.FlowNameKind.NameAndParameters with 
                | Some (name, paramters) ->
                    let indentsCount = flowName.RelativeDirectoryNames.Length

                    let indentText =
                        List.replicate indentsCount "    "
                        |> String.concat ""

                    let beginMessage =
                        let splitLine = 
                            if indentsCount = 0
                            then
                                sprintf "[%d]------------------------------------------------------------------\n" flowNameIndex
                            else ""


                        let message =
                            [ 
                          
                              sprintf "Name: %s" name
                              "Paramters:" ]
                            @ (
                                paramters
                                |> List.map (fun (paramter, paramterValue) ->
                                    sprintf "    %s: %s" paramter paramterValue
                                )
                            )
                            |> List.map (sprintf "%s    %s" indentText)
                            |> String.concat "\n"

                        sprintf "%s\n%sBEGIN\n%s" splitLine indentText message

                    let endMessage (elapsed: System.TimeSpan) =



                        let message = sprintf "\n%sEND %s" indentText name
                        sprintf "%s in %O \n" message elapsed

                    let (result, message) = Logger.infoWithStopWatchAndReturnFinalMessage (beginMessage) endMessage f
                    
                    match flowModel.TryGetBackupDirectory() with 
                    | Some directory ->
                        let file = directory </> "Log.txt"
                        match flowNameIndex with 
                        | 0 -> File.delete file
                        | _ -> ()
                        File.AppendAllText(file, message + "\n", System.Text.Encoding.UTF8)
                    
                    | None -> ()

                    result

                | None -> f ()
            | None -> f()
