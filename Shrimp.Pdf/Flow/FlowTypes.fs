namespace Shrimp.Pdf
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

type internal FlowNameIndex =
    { Index: int
      Name: string 
      ParamterMessages: list<string * string> }

type internal FlowNameIndexes =
    { FileName: FlowNameIndex
      DirectoryNames: FlowNameIndex list }
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
    | Disable of previous: FlowNameIndexes option * verticalIndex: int option

[<Sealed>]
type FlowName internal (flowNameKind: FlowNameKind) =
    member internal x.FlowNameKind = flowNameKind

    member internal x.Bind(flowName: FlowName) =
        match x.FlowNameKind, flowName.FlowNameKind with 
        | FlowNameKind.Disable (previous, verticalIndex), _ -> 
            FlowNameKind.Disable (previous, verticalIndex)
            |> FlowName

        | FlowNameKind.Override flowNameIndexes, _ -> 
            FlowNameKind.Disable (Some flowNameIndexes, None)
            |> FlowName

        | FlowNameKind.New flowNameIndexes1, FlowNameKind.New flowNameIndexes2 ->
            let directoryNames = 
                flowNameIndexes1.DirectoryNames 
                @ [flowNameIndexes1.FileName]
                @ flowNameIndexes2.DirectoryNames
            { DirectoryNames = directoryNames

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

        | FlowNameKind.New flowNameIndexes, FlowNameKind.Disable _ -> 
            (Some flowNameIndexes, None)
            |> FlowNameKind.Disable 
            |> FlowName

    static member Override (name: string, ?paramters) = 
        let paramters = defaultArg paramters []
        if name.IndexOfAny(Path.GetInvalidPathChars()) > 0 then failwithf "Invalid path %s" name
        if name.Length > 60 then failwithf "flowName %s 's length is bigger 60" name
        { DirectoryNames = []
          FileName = 
            { Name = name 
              Index = 0
              ParamterMessages = paramters }
        }
        |> FlowNameKind.Override
        |> FlowName


    static member New (name: string, ?paramters) = 
        let paramters = defaultArg paramters []
        if name.IndexOfAny(Path.GetInvalidPathChars()) > 0 then failwithf "Invalid path %s" name
        if name.Length > 60 then failwithf "flowName %s 's length is bigger 60" name
        { DirectoryNames = []
          FileName = 
            { Name = name 
              Index = 0
              ParamterMessages = paramters }
        }
        |> FlowNameKind.New
        |> FlowName

    static member Disable: FlowName = FlowName(FlowNameKind.Disable (None, None))

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

    let internal tupledFlow_Bind_FlowName (leftFlowName: FlowName option) (topFlowName: FlowName option) (rightFlowName: FlowName) =
        
        let (|Non|New|Disable|Override|) (flowName: FlowName option) =
            match flowName with 
            | None -> Non
            | Some flowName ->
                match flowName.FlowNameKind with 
                | FlowNameKind.New flowNameIndexes -> New flowNameIndexes
                | FlowNameKind.Disable (previous, verticalIndex) -> Disable(previous, verticalIndex)
                | FlowNameKind.Override (flowNameIndexes) -> Override flowNameIndexes


        match leftFlowName with 
        | Non | New _ ->
            let leftFlowNameIndexes =
                match leftFlowName with 
                | Some leftFlowName ->
                    match leftFlowName.FlowNameKind with 
                    | FlowNameKind.New flowNameIndexes -> Some flowNameIndexes
                    | _ -> failwith "Invalid token"

                | None -> None

            let leftDirectoryNames =
                match leftFlowNameIndexes with 
                | Some leftFlowNameIndexes -> leftFlowNameIndexes.DirectoryNames
                | None -> []

            let leftDirectoryAndFileNames =
                match leftFlowNameIndexes with 
                | Some leftFlowNameIndexes -> leftFlowNameIndexes.DirectoryNames @ [leftFlowNameIndexes.FileName]
                | None -> []

            let index =
                match topFlowName with 
                | Some topFlowName ->
                    let dirLength = leftDirectoryNames.Length
                    match topFlowName.FlowNameKind with 
                    | FlowNameKind.New flowNameIndexes
                    | FlowNameKind.Override flowNameIndexes -> 
                        (flowNameIndexes.DirectoryNames @ [flowNameIndexes.FileName]).[dirLength].Index + 1
                    | FlowNameKind.Disable (Some flowNameIndexes, verticalIndex) -> 
                        if flowNameIndexes.DirectoryNames.Length = dirLength
                        then 
                            match verticalIndex with 
                            | Some verticalIndex -> verticalIndex
                            | None -> 0
                        elif flowNameIndexes.DirectoryNames.Length > dirLength
                        then 
                            (flowNameIndexes.DirectoryNames @ [flowNameIndexes.FileName]).[dirLength].Index + 1
                        else failwith "Invalid token"

                    | FlowNameKind.Disable (None, Some verticalIndex) -> verticalIndex

                    | FlowNameKind.Disable (None, None) -> 0
                    
                | None -> 0


            match rightFlowName.FlowNameKind with 
            | FlowNameKind.Disable _ -> 
                FlowNameKind.Disable (leftFlowNameIndexes, Some index)
                |> FlowName

            | FlowNameKind.Override flowNameIndexes ->
                { FileName = 
                    { flowNameIndexes.FileName with 
                        Index = index
                    }
                  DirectoryNames = leftDirectoryAndFileNames
                }
                |> FlowNameKind.Override 
                |> FlowName

            | FlowNameKind.New flowNameIndexes ->
                { FileName = 
                    { flowNameIndexes.FileName with 
                        Index = index
                    }
                  DirectoryNames = leftDirectoryAndFileNames
                }
                |> FlowNameKind.New
                |> FlowName

        | Override flowNameIndexes ->
            FlowNameKind.Disable (Some flowNameIndexes, None)
            |> FlowName

        | Disable _ -> leftFlowName.Value




[<RequireQualifiedAccess>]
type internal FlowNameTupleBindedStatus =
    | Binding of previousFlowName: FlowName option
    | None





type internal InternalFlowModel<'userState> =
    { File: string 
      UserState: 'userState 
      FlowNameTupleBindedStatus: FlowNameTupleBindedStatus
      FlowName: FlowName option }

with

    member internal flowModel.TryGetBackupDirectory() =
        match flowModel.FlowName with 
        | None -> None
        | Some flowName ->
            let flowNameKind = flowName.FlowNameKind
            match flowNameKind with 
            | FlowNameKind.Override flowNameIndexes
            | FlowNameKind.New flowNameIndexes ->
                let rootDir = Path.getDirectory flowModel.File </> ".shrimp.pdf" </> Path.GetFileNameWithoutExtension flowModel.File
                Directory.ensure rootDir
                match flowNameIndexes.DirectoryNames with 
                | [] -> Some rootDir
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
                
                    Some directory

            | _ -> None

    member internal flowModel.CleanBackupDirectoryWhenFlowName_FileName_Index_IsZero() =
        match flowModel.FlowName with 
        | Some flowName ->
            let flowNameKind = flowName.FlowNameKind
            match flowNameKind with 
            | FlowNameKind.Override flowNameIndexes
            | FlowNameKind.New flowNameIndexes ->
                if flowNameIndexes.FileName.Index = 0
                then 
                    match flowModel.TryGetBackupDirectory() with 
                    | Some directory -> 
                        try
                            Shell.cleanDir directory
                        with ex ->
                            Logger.warning (ex.Message)
                    | None -> ()

            | FlowNameKind.Disable _ -> ()
        | None -> ()



    member internal flowModel.TryBackupFile() =
        match flowModel.FlowName with 
        | None -> ()
        | Some flowName ->
            let flowNameKind = flowName.FlowNameKind

            match flowNameKind with 
            | FlowNameKind.Override flowNameIndexes
            | FlowNameKind.New flowNameIndexes ->
                match flowModel.TryGetBackupDirectory() with 
                | Some directory ->

                    let fileFlowNameIndex = flowNameIndexes.FileName

                    let targetPath = 
                        directory </> sprintf "%d_%s.pdf" fileFlowNameIndex.Index fileFlowNameIndex.Name

                    File.Copy(flowModel.File, targetPath, true)

                | None -> ()

            | FlowNameKind.Disable _ -> ()

[<RequireQualifiedAccess>]
module internal InternalFlowModel =
    let mapM mapping (flowModel: InternalFlowModel<_>) =
        { File = flowModel.File 
          UserState = mapping flowModel.UserState 
          FlowName = flowModel.FlowName 
          FlowNameTupleBindedStatus = flowModel.FlowNameTupleBindedStatus }

    let toFlowModel (internalFlowModel: InternalFlowModel<_>): FlowModel<_> =
        { File = internalFlowModel.File 
          UserState = internalFlowModel.UserState }


    let mapTo userState (flowModel: InternalFlowModel<_>) =
        { File = flowModel.File 
          UserState = userState
          FlowName = flowModel.FlowName 
          FlowNameTupleBindedStatus = flowModel.FlowNameTupleBindedStatus }


type InternalFlowModelWrapper<'userState> internal (internalFlowModel: InternalFlowModel<'userState>) =
    member internal x.Value = internalFlowModel


[<AutoOpen>]
module internal Logger_FlowModel =
    type Logger =
        static member TryInfoWithFlowModel (flowModel: InternalFlowModel<_>, f) =
            match flowModel.FlowName with 
            | Some flowName ->
                match flowName.FlowNameKind with 
                | FlowNameKind.Override flowNameIndexes
                | FlowNameKind.New flowNameIndexes ->
                    let indentsCount = flowNameIndexes.DirectoryNames.Length

                    let indentText =
                        List.replicate indentsCount "    "
                        |> String.concat ""

                    let beginMessage =
                        let splitLine = 
                            if indentsCount = 0
                            then
                                sprintf "[%d]------------------------------------------------------------------\n" flowNameIndexes.FileName.Index
                            else ""

                        let message =
                            [ 
                          
                              sprintf "Name: %s" flowNameIndexes.FileName.Name
                              "Paramters:" ]
                            @ (
                                flowNameIndexes.FileName.ParamterMessages
                                |> List.map (fun (paramter, paramterValue) ->
                                    sprintf "    %s: %s" paramter paramterValue
                                )
                            )
                            |> List.map (sprintf "%s    %s" indentText)
                            |> String.concat "\n"

                        sprintf "%s\n%sBEGIN\n%s" splitLine indentText message

                    let endMessage (elapsed: System.TimeSpan) =



                        let message = sprintf "\n%sEND %s" indentText flowNameIndexes.FileName.Name
                        sprintf "%s in %O \n" message elapsed

                    let (result, message) = Logger.infoWithStopWatchAndReturnFinalMessage (beginMessage) endMessage f
                    
                    match flowModel.TryGetBackupDirectory() with 
                    | Some directory ->
                        let file = directory </> "Log.txt"
                        match flowNameIndexes.FileName.Index with 
                        | 0 -> File.delete file
                        | _ -> ()
                        File.AppendAllText(file, message + "\n", System.Text.Encoding.UTF8)
                    
                    | None -> ()

                    result

                | FlowNameKind.Disable _ -> f ()
            | None -> f()