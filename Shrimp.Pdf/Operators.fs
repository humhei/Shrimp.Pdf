namespace Atrous.Pdf
#nowarn "0064"

module Operators = 
    open BlackFox.ColoredPrintf
    open System.Diagnostics
    open iText.Kernel.Pdf
    open iText.Kernel.Pdf.Canvas
    open Atrous.Pdf.Types
    open Fake.IO
    open iText.Kernel.Pdf.Canvas.Parser.Data
    open Atrous.Pdf.Select
    open Atrous.Pdf.Extensions
    open Atrous.Utils

    let private select (selectWithArgs: ManipulateArgument<_> -> AbstractRenderInfo -> bool) (manipulate: ManipulateArgument<_> -> FlowState<_>) =
        fun arg ->
            manipulate { arg with Select = selectWithArgs arg }

    let withRegion margin regionSelect (pageBoxKind: PageBoxKind) =
        select (fun arg ->
            let pageBox = PdfPage.getPageBox pageBoxKind arg.Page |> Rectangle.applyMargin margin
            regionSelect pageBox 
        )

    let inRegionWith margin pageBoxKind =
        withRegion margin Select.inRegion pageBoxKind

    let inRegion pageBoxKind =
        inRegionWith Margin.empty pageBoxKind

    let runLiteralCommand (command: string) (p: PdfPage) =
        let doc = p.GetDocument()
        let before = sprintf "\nq %s n\nq\n" command
        let after = "\nQ\nQ\n"
        (new PdfCanvas(p.NewContentStreamBefore(),new PdfResources(),doc)).WriteLiteral(before) |> ignore
        (new PdfCanvas(p.NewContentStreamAfter(),new PdfResources(),doc)).WriteLiteral(after) |> ignore

    let inline (@+) f1 f2 = 
        float f1 + float f2

    let rec private update (fw: Flow<_>) (models: (FlowModel<_>) list) = 
        match fw with 
        | Flow.Rename map ->
            models |> List.map (fun md ->
                { md with 
                    Path = 
                        
                        let newPath = (md.FlowState.UserState,md.Path) |> map 
                        File.delete newPath
                        System.IO.File.Move(md.Path,newPath)
                        newPath
                }
            )
        | Flow.TransformUserState transform ->
            models |> List.map (fun md ->
                { md with 
                    FlowState = { md.FlowState with UserState = transform md.FlowState.UserState }
                }
            )

        | Flow.Read f ->
            models |> List.map (fun md ->
                let src = md.Path
                let doc = new PdfDocument(new PdfReader(src))

                let flowState = 
                    let flowState = md.FlowState
                    { flowState with 
                        UserState =
                            f doc flowState.UserState }

                doc.Close()
                { md with FlowState = flowState }
            )

        | Flow.Manipulate m ->
            models |> List.map (fun md ->
                let src = md.Path
                let dest = tmp src 
                let doc = PdfDocumentWithResources.create src dest
                let state = doc |> PdfDocument.manipulateForEachPageWithFoldingStates m md.FlowState
                doc.Close()
                draft src dest
                { md with FlowState = state}
            )

        | Flow.Manipulates (ms) ->
            models |> List.map (fun md ->
                let src = md.Path
                let dest = tmp src 
                let doc = PdfDocumentWithResources.create src dest
                let state = 
                    ms |> List.fold (fun state m ->
                        doc |> PdfDocument.manipulateForEachPageWithFoldingStates m state
                    ) md.FlowState
                doc.Close()
                draft src dest
                { md with FlowState = state }
            )

        | Flow.Reuse r ->
            let r = r
            models |> List.collect (fun model -> r model)

        | Flow.Iac iac ->
            models |> List.iter (fun md -> iac md.Path)
            []

        | Flow.Composite fws ->
            let rec loop fws models = 
                match fws with 
                | h :: t -> 
                    let models = update h models
                    loop t models
                | [] -> models
            loop fws models

        | Flow.FromState ffs ->
            models |> List.collect (fun md ->
                let fw = ffs md.FlowState
                update fw [md]
            )

    let preflightWithModel state (fws: Flow<_> list) = 
        fws |> List.fold (fun models fw -> 
            let watch = new Stopwatch()
            watch.Start()
            let r = update fw models
            watch.Stop()
            ColoredPrintf.colorprintfn "$cyan[%A]\t$green[%A]" fw watch.Elapsed
            r
        ) [state]

    let initFlowState userState = {Tables = []; UserState = userState; LastManipulatesInfos = []}

    let preflight (src:string) (fws: Flow<_> list) = 
        preflightWithModel {Path = src; FlowState = initFlowState()} fws

    let simplePreflight (src:string) (fws: Flow<_> list) = 
        preflight src fws
        |> ignore

    let createPdf (path: string) (pageSizes: FsSize list) =
        let pdf = new PdfDocument(new PdfWriter(path))
        pageSizes |> List.iter (fun pageSize ->
            let pageSize = PageSize.ofFsSize pageSize
            pdf.AddNewPage(pageSize) |> ignore
        )
        pdf.Close()

    let batch (fs: (ManipulateArgument<_> -> FlowState<_>) list ) =
        fun (arg:ManipulateArgument<_>) ->
            fs |> List.fold (fun flowState f -> 
                let arg = { arg with FlowState = flowState }
                f arg
            ) arg.FlowState

