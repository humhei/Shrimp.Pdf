namespace Shrimp.Pdf
#nowarn "0104"
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO


type internal FlowNameIndex =
    { Name: string 
      Index: int }

type internal FlowNameIndexes =
    { DirectoryNames: FlowNameIndex list 
      FileName: FlowNameIndex }
with 
    member x.FileNameIndex_Add_IndexValue (index: int) =
        { x with 
            FileName = 
                { x.FileName with   
                    Index = x.FileName.Index + index }
        }

[<RequireQualifiedAccess>]
type internal FlowNameKind =
    | Override of FlowNameIndexes
    | New of FlowNameIndexes
    | Disable


[<Sealed>]
type FlowName internal (flowNameKind: FlowNameKind) =
    member internal x.FlowNameKind = flowNameKind

    member internal x.Bind(flowName: FlowName) =
        match x.FlowNameKind, flowName.FlowNameKind with 
        | FlowNameKind.Disable, _ -> FlowName.Disable
        | FlowNameKind.Override _, _ -> FlowName.Disable
        | FlowNameKind.New flowNameIndexes1, FlowNameKind.New flowNameIndexes2 ->
            { DirectoryNames = 
                flowNameIndexes1.DirectoryNames 
                @ [flowNameIndexes1.FileName]
                @ flowNameIndexes2.DirectoryNames
              FileName = flowNameIndexes2.FileName
            }
            |> FlowNameKind.New
            |> FlowName

        | FlowNameKind.New flowNameIndexes1, FlowNameKind.Override flowNameIndexes2 -> 
            { DirectoryNames = 
                flowNameIndexes1.DirectoryNames 
                @ [flowNameIndexes1.FileName]
                @ flowNameIndexes2.DirectoryNames
              FileName = flowNameIndexes2.FileName
            }
            |> FlowNameKind.Override
            |> FlowName

        | FlowNameKind.New _, FlowNameKind.Disable -> FlowName.Disable


    static member Override name = 
        { DirectoryNames = []
          FileName = 
            { Name = name 
              Index = 0 }
        }
        |> FlowNameKind.Override
        |> FlowName


    static member New name = 
        { DirectoryNames = []
          FileName = 
            { Name = name 
              Index = 0 }
        }
        |> FlowNameKind.New
        |> FlowName

    static member Disable = FlowName(FlowNameKind.Disable)

[<RequireQualifiedAccess>]
module FlowName =
    let internal asFlowNameIndexes (flowName: FlowName) = 
        match flowName.FlowNameKind with 
        | FlowNameKind.Override flowNameIndexes | FlowNameKind.New flowNameIndexes -> Some flowNameIndexes
        | FlowNameKind.Disable _ -> None

    let (|Override|New|Disable|) (flowName: FlowName) =
        match flowName.FlowNameKind with 
        | FlowNameKind.New _ -> New
        | FlowNameKind.Override _ -> Override
        | FlowNameKind.Disable _ -> Disable

type FlowModel<'userState> =
    { File: string
      UserState: 'userState }

with 
    member internal flowModel.TryBackupFile(flowName: FlowName) =
        let flowNameKind = flowName.FlowNameKind

        match flowNameKind with 
        | FlowNameKind.Override flowNameIndexes
        | FlowNameKind.New flowNameIndexes ->
            let directory =
                let rootDir = Path.getDirectory flowModel.File </> ".shrimp.pdf"
                Directory.ensure rootDir
                match flowNameIndexes.DirectoryNames with 
                | [] -> rootDir
                | flowNameIndexes ->
                    let headerDirNames =
                        flowNameIndexes
                            |> List.map(fun flowNameIndex ->
                                sprintf "%d_%s" flowNameIndex.Index flowNameIndex.Name
                            )


                    let directory = 
                        (rootDir :: headerDirNames)
                        |> List.reduce ((</>))
                
                    Directory.ensure directory
                
                    directory



            let fileFlowNameIndex = flowNameIndexes.FileName

            let targetPath = 
                directory </> sprintf "%d_%s.pdf" fileFlowNameIndex.Index fileFlowNameIndex.Name

            File.Copy(flowModel.File, targetPath)


        | FlowNameKind.Disable _ -> ()

[<RequireQualifiedAccess>]
module FlowModel =

    let mapM mapping (flowModel: FlowModel<_>) =
        { File = flowModel.File 
          UserState = mapping flowModel.UserState }

    let mapTo userState (flowModel: FlowModel<_>) =
        { File = flowModel.File 
          UserState = userState }


[<AutoOpen>]
module internal Logger_FlowName =
    module Logger =
        let tryInfoWithFlowName (flowName: FlowName) f =
            match flowName.FlowNameKind with 
            | FlowNameKind.Override flowNameIndexes
            | FlowNameKind.New flowNameIndexes ->
                let indentsCount = flowNameIndexes.DirectoryNames.Length

                let indentText =
                    List.replicate indentsCount "    "
                    |> String.concat ""

                Logger.infoWithStopWatch (indentText + flowNameIndexes.FileName.Name) f

            | FlowNameKind.Disable _ -> f ()

