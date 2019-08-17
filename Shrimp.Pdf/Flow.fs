namespace Shrimp.Pdf
open iText.Kernel.Pdf
open Shrimp.Pdf.Parser
open Fake.IO
open System.IO
open iText.Layout


type FlowModel<'userState> =
    { File: string
      UserState: 'userState }

type Reuse<'oldUserState, 'newUserState> = Reuse of (FlowModel<'oldUserState> -> SplitDocument -> 'newUserState)
with 
    member x.Value =
        let (Reuse value) = x
        value

    member x.TransformUserState (mapping) =
        fun userState document ->
            x.Value userState document
            |> mapping
        |> Reuse

type Manipulate<'oldUserState, 'newUserState> = 
    Manipulate of (FlowModel<'oldUserState> -> IntegralDocument -> 'newUserState)
with 
    member x.Value =
        let (Manipulate value) = x
        value

    /// internal use
    /// using <+> instead
    static member Bind (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'newUserState2>) =
        fun flowModel (document: IntegralDocument) ->
            let middleUserState = manipulate1.Value flowModel document
            manipulate2.Value {File = flowModel.File; UserState = middleUserState} document
        |> Manipulate

    member x.TransformUserState (mapping) =
        fun userState document ->
            x.Value userState document
            |> mapping
        |> Manipulate


type FileOperation<'oldUserState, 'newUserState> = 
    FileOperation of (FlowModel<'oldUserState> -> ReaderDocument -> FlowModel<'newUserState> list)
with 
    member x.Value =
        let (FileOperation value) = x
        value

[<RequireQualifiedAccess>]
type Flow<'oldUserState, 'newUserState> =
    | Manipulate of (Manipulate<'oldUserState, 'newUserState>)
    | Reuse of (Reuse<'oldUserState, 'newUserState>)
    | FileOperation of FileOperation<'oldUserState, 'newUserState>
    | Transform of (FlowModel<'oldUserState> -> FlowModel<'newUserState> list)
with 
    static member internal Run(flowModel, flow) =
        let file = flowModel.File
        let writerFile = Path.changeExtension ".writer.pdf" file

        let draft() =
            File.Delete(file)
            File.Move(writerFile, file)

        match flow with 
        | Flow.Manipulate (manipulate) ->

            let pdfDocument = IntegralDocument.Create(file, writerFile)
            let newUserState = manipulate.Value flowModel pdfDocument
            pdfDocument.Value.Close()
            draft()
            [{ File = file; UserState = newUserState }]

        | Flow.Reuse (reuse) ->

            let pdfDocument = SplitDocument.Create(file, writerFile)
            let newUserState = reuse.Value flowModel pdfDocument
            pdfDocument.Reader.Close()
            pdfDocument.Writer.Close()
            draft()
            [{ File = file; UserState = newUserState }]

        | Flow.FileOperation fileOperation ->
            let readerDocument = new ReaderDocument(file)
            let newModels = fileOperation.Value flowModel readerDocument
            newModels

        | Flow.Transform transform ->
            transform flowModel



    /// internal use
    /// using <+> instead
    static member Bind (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'newUserState2>) =
        fun flowModel ->
            let middleFlowModels = Flow<'originUserState, 'middleUserState>.Run (flowModel, flow1) 

            let newFlowModels = 
                middleFlowModels
                |> List.collect (fun middleFlowModel ->
                    Flow<'middleUserState, 'newUserState2>.Run (middleFlowModel, flow2) 
                )
            newFlowModels
        |> Flow.Transform

    member x.TransformUserState mapping  =
        fun flowModel ->
                Flow<_, _>.Run(flowModel, x) 
                |> List.map (fun newFlowModel ->
                    { File = newFlowModel.File; UserState = mapping flowModel.UserState}
                )
        |> Flow.Transform 

[<AutoOpen>]
module Operators =


    let run flowModel flow = Flow<_, _>.Run(flowModel, flow)

    let inline (||>>) (flow : ^a) (mapping: ^b) =
        (^a: (member TransformUserState : ^b -> ^c) (flow, mapping))

    let inline (<+>) (flow1: ^a) (flow2: ^b) = 
        ((^a or ^b): (static member Bind: ^a * ^b -> ^c) (flow1, flow2))


