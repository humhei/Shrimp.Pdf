namespace Shrimp.Pdf
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO




[<RequireQualifiedAccess>]
type FlowName =
    | Default
    | Override of string
    | New of string
    | Disable

type FlowNameIndex =
    { Index: int 
      Name: string }





type FlowModel<'userState> =
    { File: string
      UserState: 'userState }

with 
    member internal flowModel.TryBackupFile(flowName: FlowName, flowNameIndexes: FlowNameIndex list) =
        match flowName with 
        | FlowName.Override name 
        | FlowName.New name ->
            match flowNameIndexes with 
            | _ :: _ ->
                let headerIndexes = flowNameIndexes.[0 .. flowNameIndexes.Length - 2]

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

                let lastIndex = List.last flowNameIndexes 

                let targetPath = 
                    directory </> sprintf "%d_%s.pdf" lastIndex.Index lastIndex.Name

                File.Copy(flowModel.File, targetPath)

            | [] -> failwithf "FlowName is setted while flow index is empty"

        | FlowName.Disable _
        | FlowName.Default _ -> ()


[<RequireQualifiedAccess>]
module FlowModel =

    let mapM mapping (flowModel: FlowModel<_>) =
        { File = flowModel.File 
          UserState = mapping flowModel.UserState }

[<AutoOpen>]
module internal Logger_FlowName =
    module Logger =
        let tryInfoWithFlowName (flowName: FlowName) (flowNameIndexes: FlowNameIndex list) f =
            match flowName with 
            | FlowName.Override name 
            | FlowName.New name ->
                let indentsCount = 
                    match flowNameIndexes with 
                    | _ :: _ ->
                        flowNameIndexes.Length - 1
                 
                    | [] -> 0

                let indentText =
                    List.replicate indentsCount "    "
                    |> String.concat ""

                Logger.infoWithStopWatch (indentText + name) f

            | FlowName.Disable _
            | FlowName.Default _ -> f ()

