﻿namespace Shrimp.Pdf
#nowarn "0104"
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO
open Shrimp.FSharp.Plus
open Shrimp.Pdf.Parser

type NameAndParameters =
    { Name: string 
      Parameters: list<string * string>}


[<RequireQualifiedAccess>]
type SlimableFlowLoggingOptions =
    | SlimParent 
    | Slim
    | Normal

type Configuration =
    { LoggerLevel: PdfLoggerLevel
      PdfModifyOptions: PdfModifyOptions option
      SlimableFlowLoggingOptions: SlimableFlowLoggingOptions
      }
with 
    static member DefaultValue = { LoggerLevel = PdfLoggerLevel.Info; PdfModifyOptions = None; SlimableFlowLoggingOptions = SlimableFlowLoggingOptions.Normal }

type PdfConfiguration = Configuration

type FlowModel<'userState> =
    { PdfFile: PdfFile 
      UserState: 'userState
      Configuration: Configuration }
    with 
        member x.File = x.PdfFile.Path

        member x.MapUserState(fUserState) =
            { PdfFile = x.PdfFile
              UserState = fUserState x.UserState
              Configuration = x.Configuration }

[<RequireQualifiedAccess>]
module FlowModel =
    let mapM mapping flowModel =
        { PdfFile = flowModel.PdfFile 
          UserState = mapping flowModel.UserState
          Configuration = flowModel.Configuration }

    let mapTo userState flowModel =
        { PdfFile = flowModel.PdfFile 
          UserState = userState
          Configuration =flowModel.Configuration }



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
type FlowName private (flowNameKind: FlowNameKind, ?parentFlowName) =

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
     
    static member Override (name: string, ?parameters) = 
        let parameters = defaultArg parameters []
        //let name = ValidFileName.Apply(name)
        if name.IndexOfAny(Path.GetInvalidFileNameChars()) > 0 then failwithf "Invalid flowName path %s" name
        if name.Length > 100 then failwithf "flowName %s 's length is bigger than 100" name
        (name, parameters)
        |> FlowNameKind.Override
        |> FlowName

    member internal x.SetParentFlowName(parentFlowName) = 
        FlowName(x.FlowNameKind, parentFlowName)

    static member New (name: string, ?parameters) = 
        let parameters = defaultArg parameters []
        //let name = ValidFileName.Apply(name)
        if name.IndexOfAny(Path.GetInvalidFileNameChars()) > 0 then failwithf "Invalid flowName %s" name
        if name.Length > 100 then failwithf "flowName %s 's length is bigger 100" name
        (name, parameters)
        |> FlowNameKind.New
        |> FlowName

    static member Disable: FlowName = FlowName(FlowNameKind.Disable)

    member x.IsEmpty = flowNameKind.Name.IsNone

    override x.ToString() =     
        match flowNameKind.NameAndParameters with 
        | None -> "<null>"
        | Some v -> v.ToString()



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





type InternalFlowModel<'userState> =
    { File: string 
      UserState: 'userState 
      FlowName: FlowName option
      OperatedFlowNames: FlowName list
      Configuration: Configuration }

with
    member x.LoggerLevel = x.Configuration.LoggerLevel

    member x.MapUserState(f) =
        { File = x.File 
          UserState = f x.UserState
          FlowName = x.FlowName
          OperatedFlowNames = x.OperatedFlowNames
          Configuration = x.Configuration }

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

                    //let path = 
                    //    @"D:\Users\Jia\Documents\MyData\Docs\2017\仙峰\THEFASHIONSUPPLY\素材\袋子贴标拼好版\.extract\袋子贴标2022-6-16.withTemplate.tiled.HasContents\X002R8GFTT_Emma Shoes Slip On Perfor...da Sneaker(Black_White, 5)_New.pdf"

                    //let dest = @"D:\Users\Jia\Documents\MyData\Docs\2017\仙峰\THEFASHIONSUPPLY\素材\袋子贴标拼好版\.extract\袋子贴标2022-6-16.withTemplate.tiled.HasContents\.shrimp.pdf\X002R8GFTT_Emma Shoes Slip On Perfor...da Sneaker(Black_White, 5)_New\0_RemoveColorsRGB 247 147 30 RGB BLUE PdfExtractorTempTextColor.pdf"
                    //if File.exists path 
                    //then    
                    
                    //    let dir = Path.getDirectory dest
                    //    let dirInfo = DirectoryInfo(dir)
                    //    for i in [1..100] do
                    //        File.Copy(path, dest, true)

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
          OperatedFlowNames = flowModel.OperatedFlowNames
          Configuration =flowModel.Configuration }

    let toFlowModel (internalFlowModel: InternalFlowModel<_>): FlowModel<_> =
        { PdfFile = PdfFile internalFlowModel.File 
          UserState = internalFlowModel.UserState
          Configuration = internalFlowModel.Configuration }


    let mapTo userState (flowModel: InternalFlowModel<_>) =
        mapM (fun _ -> userState) flowModel


type InternalFlowModelWrapper<'userState> internal (internalFlowModel: InternalFlowModel<'userState>) =
    member internal x.Value = internalFlowModel

[<AutoOpen>]
module internal Logger_FlowModel =  



    type PdfLogger =
        static member TryInfoWithFlowModel (flowNameIndex, flowModel: InternalFlowModel<_>, f) =
            let loggingOptions = flowModel.Configuration.SlimableFlowLoggingOptions
            match flowModel.FlowName with 
            | Some flowName ->
                match flowModel.LoggerLevel with 
                | PdfLoggerLevel.Info ->
                    match flowName.FlowNameKind.NameAndParameters with 
                    | Some (name, parameters) ->
                        
                        let indentsCount = 
                            match loggingOptions with
                            | SlimableFlowLoggingOptions.SlimParent 
                            | SlimableFlowLoggingOptions.Normal -> flowName.RelativeDirectoryNames.Length 
                            | SlimableFlowLoggingOptions.Slim -> flowName.RelativeDirectoryNames.Length + 1

                        let indentText =
                            List.replicate indentsCount "    "
                            |> String.concat ""

                        let beginMessage =
                            let splitLine = 
                                if indentsCount = 0 
                                then
                                    sprintf "[%d]------------------------------------------------------------------\n" flowNameIndex
                                elif (indentsCount = 1 && loggingOptions = SlimableFlowLoggingOptions.Slim)
                                then sprintf "    \n\n    [%d]----------------" flowNameIndex
                                else ""


                            let message =
                                [ 
                          
                                  sprintf "Name: %s" name
                                  "Parameters:" ]
                                @ (
                                    parameters
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

                    
                        match flowModel.TryGetBackupDirectory() with 
                        | Some directory ->
                            let file = directory </> "Log.txt"
                            match flowNameIndex with 
                            | 0 -> File.delete file
                            | _ -> ()

                            match loggingOptions with 
                            | SlimableFlowLoggingOptions.SlimParent ->
                                File.AppendAllText(file, beginMessage + "\n", System.Text.Encoding.UTF8)
                                let (result, beginMessage, endMessage) = PdfLogger.infoWithStopWatchAndReturnFinalMessage (beginMessage) endMessage f
                                File.AppendAllText(file, endMessage + "\n", System.Text.Encoding.UTF8)
                                result

                            | SlimableFlowLoggingOptions.Slim 
                            | SlimableFlowLoggingOptions.Normal ->
                                match name with 
                                | "Slim Flows" ->
                                    File.AppendAllText(file, beginMessage + "\n", System.Text.Encoding.UTF8)
                                    let (result, beginMessage, endMessage) = PdfLogger.infoWithStopWatchAndReturnFinalMessage (beginMessage) endMessage f
                                    File.AppendAllText(file, endMessage + "\n", System.Text.Encoding.UTF8)
                                    result

                                | _ ->
                                
                                    let (result, beginMessage, endMessage) = PdfLogger.infoWithStopWatchAndReturnFinalMessage (beginMessage) endMessage f
                                    let message = beginMessage + "\n" + endMessage
                                    File.AppendAllText(file, message + "\n", System.Text.Encoding.UTF8)
                                    result

                        | None -> 
                            let (result, beginMessage, endMessage) = PdfLogger.infoWithStopWatchAndReturnFinalMessage (beginMessage) endMessage f
                            result

                    | None -> f ()

                | PdfLoggerLevel.Slient -> f ()

            | None -> f()
