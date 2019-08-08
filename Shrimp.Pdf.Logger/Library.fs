namespace Atrous.Pdf.Logger

[<RequireQualifiedAccess>]
module Logger = 
    open System
    open Deedle

    let notSupportedPathRendingMode others =
        printfn "Warning: Not supported path rendering mode %A" others

    let notSupportedTextRendingMode others =
        printfn "Warning: Not supported text rendering mode %A" others