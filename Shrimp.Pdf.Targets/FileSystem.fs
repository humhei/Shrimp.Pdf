
namespace Shrimp.Pdf.Targets

open System.IO
open Fake.IO

module FileSystem =
    open BlackFox.ColoredPrintf

    let ensureDir dir = 
        if not <| Directory.Exists dir then 
            colorprintfn "created directory $cyan[%s]" dir
            Directory.create dir