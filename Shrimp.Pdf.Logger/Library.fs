namespace Shrimp.Pdf

open System.Diagnostics
open System.IO

[<RequireQualifiedAccess>]
module Logger =
    let mutable logger: NLog.Logger option = None
    let unSupportedTextRenderMode(textRendingMode: int) =
        let stackTrace = new System.Diagnostics.StackTrace();
        printfn "Unsupported text render mode %d \n %s" textRendingMode (stackTrace.ToString())


    let info (message: string) =
        match logger with 
        | Some (logger: NLog.Logger) -> logger.Info message
        | None -> printfn "%s" message

    let writer = 
        File.AppendText(@"C:\Users\Jia\Desktop\hello.txt.txt")


    let infoWithStopWatch message f =
        let stopWatch = Stopwatch.StartNew()
        let result = f()
        stopWatch.Stop()
        //writer.WriteLine(sprintf "%s in %O " message stopWatch.Elapsed)
        info (sprintf "%s in %O " message stopWatch.Elapsed)
        result