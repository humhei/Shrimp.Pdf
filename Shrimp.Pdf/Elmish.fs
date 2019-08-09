namespace Shrimp.Pdflmish
[<RequireQualifiedAccess>]
module Program =
    open System.Diagnostics
    open System
    open Shrimp.Elmish
    open BlackFox.ColoredPrintf
    open Shrimp.Pdf.Types


    let inline watch (f:'a -> 'b -> 'c) (p: TimeSpan -> 'a -> 'c -> unit) =
        fun a b ->
            let stopWatch = Stopwatch.StartNew()
            let r = f a b
            stopWatch.Stop()
            let time = stopWatch.Elapsed
            p time a r
            r

    let inline watch4 (f:'a -> 'b -> 'c -> 'd) (p: TimeSpan -> 'a -> 'd -> unit) =
        fun a b c ->
            let stopWatch = Stopwatch.StartNew()
            let r = f a b c
            stopWatch.Stop()
            let time = stopWatch.Elapsed
            p time a r
            r

    let inline watch5 (f:'a -> 'b -> 'c -> 'd -> 'f) (p: TimeSpan -> 'a -> 'f -> unit) =
        fun a b c d ->
            let stopWatch = Stopwatch.StartNew()
            let r = f a b c d
            stopWatch.Stop()
            let time = stopWatch.Elapsed
            p time a r
            r

    let withConsoleTrace (program: Program<FlowModel<'flowState> list,Flow<_>>) =
        let p (time: TimeSpan) f (models: FlowModel<'flowState> list) =
            let srcs = models |> List.map (fun m -> m.Path )
            colorprintfn "$cyan[%A]\n$blue[%A]\t$green[%A]" srcs f time
        { program with 
            update = watch program.update p }