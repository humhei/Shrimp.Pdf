﻿namespace Shrimp.Pdf

open Fake.IO

type FlowModel =
    { File: string }

type Flow =
    | Reuse of (FlowModel -> FlowModel list)

[<AutoOpen>]
module Operators =
    let run (file: string) (flows: Flow list) =

        ([ {File = file} ], flows) 
        ||> List.fold(fun models flow ->
            models
            |> List.map (fun model -> 
                async {
                    match flow with 
                    | Flow.Reuse reuse ->
                        return reuse model
                }
            )
            |> Async.Parallel
            |> Async.RunSynchronously
            |> List.ofArray
            |> List.concat
        )