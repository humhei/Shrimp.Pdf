namespace Shrimp.Pdf
open Imposing
open System
open Fake.IO

module Reuses =
    open Shrimp.Pdf.Extensions
    
    let impose (imposingArguments: ImposingArguments) =
        fun (model: FlowModel) (splitDocument: SplitDocument) ->
            let imposingDocument = new ImposingDocument(splitDocument, imposingArguments)
            imposingDocument.Build()

            imposingDocument.Draw()

            [model]

        |> Flow.Reuse