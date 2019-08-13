namespace Shrimp.Pdf
open iText.Kernel.Pdf
open Shrimp.Pdf.Parser
open Fake.IO
open System.IO

type FlowModel =
    { File: string }

[<RequireQualifiedAccess>]
type Flow =
    | Reuse of (FlowModel -> SplitDocument -> FlowModel list)
    | Manipulates of (FlowModel -> IntegralDocument -> FlowModel) list

[<AutoOpen>]
module Operators =
    let run (file: string) (flows: Flow list) =

        ([ {File = file} ], flows) 
        ||> List.fold(fun models flow ->
            models
            |> List.map (fun model -> 
                async {
                    let readerFile = model.File
                    let writerFile = Path.changeExtension ".writer.pdf" readerFile

                    let draft() =
                        File.Delete(readerFile)
                        File.Move(writerFile, readerFile)

                    match flow with 
                    | Flow.Reuse reuse ->
                        let splitDocument = SplitDocument.Create(readerFile, writerFile)
                        let newModel = reuse model splitDocument
                        splitDocument.Reader.Close()
                        splitDocument.Writer.Close()
                        draft()
                        return newModel

                    | Flow.Manipulates manipulates ->
                        let pdfDocument = IntegralDocument.Create(readerFile, writerFile)

                        let newModel = 
                            (model, manipulates)
                            ||> List.fold(fun model manipulate ->
                                manipulate model pdfDocument
                            )

                        pdfDocument.Value.Close()
                        draft()
                        return [newModel]
                }
            )
            |> Async.Parallel
            |> Async.RunSynchronously
            |> List.ofArray
            |> List.concat
        )