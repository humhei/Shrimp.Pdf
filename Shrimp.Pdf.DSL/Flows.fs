namespace Shrimp.Pdf
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open Shrimp.FSharp.Plus
open iText.Kernel.Pdf
open System.IO

[<AutoOpen>]
module _Flows =
    type Flows =
        static member FilterPages(pageFilter: Selector<_>) =    
            let flow = 
                Flow.Manipulate(
                    ModifyPage.Create(
                        "FilterPages",
                        PageSelector.All,
                        pageFilter,
                        (fun args infos ->
                            match Seq.length infos > 0 with 
                            | true -> Some args.PageNum
                            | false -> None
                        )
                    )
                    ||>> (List.choose id >> PageNumSequence.Create)
                )
                <+>Flow.Func(fun (sequence: PageNumSequence) ->
                    Flow.Reuse(Reuses.SequencePages sequence)
                )

            flow

