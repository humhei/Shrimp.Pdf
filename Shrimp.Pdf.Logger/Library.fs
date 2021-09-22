namespace Shrimp.Pdf

open System.Diagnostics
open System.IO

[<RequireQualifiedAccess>]
module Logger =
    let private logger: Lazy<NLog.Logger> = 
        lazy (NLog.LogManager.GetCurrentClassLogger())
      
    let private configuration =
        lazy
            match NLog.LogManager.Configuration with 
            | null -> None
            | conf -> Some conf

    let warning (message: string) =
        match configuration.Value with 
        | Some (_) -> 
            logger.Value.Warn message
        | None -> printfn "WARNING: %s" message

    let error (message: string) =
        match configuration.Value with 
        | Some (_) -> 
            logger.Value.Error message
        | None -> printfn "ERROR: %s" message

    let info (message: string) =
        match configuration.Value with 
        | Some (_) -> 
            logger.Value.Info message
        | None -> printfn "%s" message

    let unSupportedTextRenderMode(textRendingMode: int) =
        //let stackTrace = new System.Diagnostics.StackTrace();
        //()
        warning (sprintf "Unsupported text render mode %d" textRendingMode) (*(stackTrace.ToString()))*)

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
        result, beginMessage + "\n" + endMessage
        