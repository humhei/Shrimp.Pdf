namespace Shrimp.Pdf

open System.Diagnostics
open System.IO
open System.Collections.Generic


type PdfLoggerLevel =
    | Slient = 0 
    | Info = 1

[<RequireQualifiedAccess>]
module PdfLogger =

    let private logger: NLog.Logger = 
        (NLog.LogManager.GetCurrentClassLogger())
      
    let private configuration =
        match NLog.LogManager.Configuration with 
        | null -> None
        | config -> Some config

    let warning (message: string) =
        match configuration with 
        | Some (_) -> 
            logger.Warn message
        | None -> printfn "WARNING: %s" message

    let error (message: string) =
        match configuration with 
        | Some (_) -> 
            logger.Error message
        | None -> printfn "ERROR: %s" message

    let info (message: string) =
        
        match configuration with 
        | Some (_) -> 
            logger.Info message
        | None -> printfn "%s" message

    let info_alwaysPrintingInConsole (message: string) =
        
        match configuration with 
        | Some (_) -> 
            logger.Info message
            printfn "%s" message

        | None -> printfn "%s" message

    let private textRenderModes: HashSet<int> = new HashSet<_>()
    let private locker = new obj()

    let unSupportedTextRenderMode(textRendingMode: int) =
        lock locker (fun () ->
            match textRenderModes.Contains textRendingMode with 
            | true -> ()
            | false -> 
                warning (sprintf "Unsupported text render mode %d" textRendingMode) (*(stackTrace.ToString()))*)
                textRenderModes.Add(textRendingMode)
                |> ignore
        )


    let infoWithStopWatch message f =
        let stopWatch = Stopwatch.StartNew()
        let result = f()
        stopWatch.Stop()
        info (sprintf "%s in %O " message stopWatch.Elapsed)
        result

    let infoWithStopWatchAndReturnFinalMessage beginMessage endMessage f =
        let stopWatch = Stopwatch.StartNew()
        info (beginMessage) 


        let result = f()
        stopWatch.Stop()
        let endMessage =  endMessage stopWatch.Elapsed
        info endMessage
        result, beginMessage, endMessage
        