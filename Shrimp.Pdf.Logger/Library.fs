namespace Shrimp.Pdf

[<RequireQualifiedAccess>]
module Logger =
    let unSupportedTextRenderMode(textRendingMode: int) =
        let stackTrace = new System.Diagnostics.StackTrace();
        printfn "Unsupported text render mode %d \n %s" textRendingMode (stackTrace.ToString())
