namespace Shrimp.Pdf
open Imposing
open System
open Fake.IO

module Reuses =
    open Shrimp.Pdf.Extensions
    
    let impose (imposingArgument: ImposingArguments) =
        fun (model: FlowModel) ->
            let splitDocument =
                SplitDocument.Create(model.File, Path.changeExtension "writer.pdf" model.File)
            
            let imposingDocument = new ImposingDocument(splitDocument, imposingArgument)

            imposingDocument.Build()

            imposingDocument.Draw()

            splitDocument.Reader.Close()
            splitDocument.Writer.Close()
            [model]

        |> Flow.Reuse