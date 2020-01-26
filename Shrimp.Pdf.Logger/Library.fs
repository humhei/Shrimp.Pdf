namespace Shrimp.Pdf

open System.Diagnostics
open System.IO

[<RequireQualifiedAccess>]
module Logger =
    let logger: Lazy<NLog.Logger option> = 
        lazy 
            match NLog.LogManager.GetCurrentClassLogger() with 
            | null -> None
            | logger -> Some logger

    let warning (message: string) =
        match logger.Value with 
        | Some (logger: NLog.Logger) -> logger.Warn message
        | None -> printfn "WARNING: %s" message

    let info (message: string) =
        match logger.Value with 
        | Some (logger: NLog.Logger) -> logger.Info message
        | None -> printfn "%s" message

    let unSupportedTextRenderMode(textRendingMode: int) =
        let stackTrace = new System.Diagnostics.StackTrace();
        warning (sprintf "Unsupported text render mode %d \n %s" textRendingMode (stackTrace.ToString()))

    let infoWithStopWatch message f =
        let stopWatch = Stopwatch.StartNew()
        let result = f()
        stopWatch.Stop()
        info (sprintf "%s in %O " message stopWatch.Elapsed)
        result