namespace Shrimp.Pdfargets.Hangtag.Output.VDP.Shuffle
open Shrimp.Entities.Types
open Shrimp.Pdf.Types
open Shrimp.Pdf
open Shrimp.Pdf.Reuses
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Targets
open Shrimp.Pdf.Targets.Reports
open Types
open Shrimp.Pdf.Targets.Hangtag

[<RequireQualifiedAccess>]
module Shuffle =
    open LiteDB.FSharp.Extensions

    let private updateTemplate (tarm: TargetModel) (hangtag: Hangtag) (p: Product) (state: ReportInfo<HangtagReportInfo,_>) =
        match hangtag.AutoSetHangtagTemplate.MA with 
        | MA.Auto ->
            let template = 
                let arg = state.Args
                let size = arg.DesiredSize.Value
                HangtagTemplate.ofImposingArgument size arg

            let product =
                { p with 
                    Kind =
                        ProductKind.Hangtag 
                            {hangtag with
                                HangtagTemplate = AutoSetHangtagTemplate.ofHangtagTemplate (Some template)}
                }

            let db = tarm.Database

            LiteRepository.updateItem product db |> ignore

        | MA.Manual _ -> ()
        state

    let run p hangtag tables (tarm: TargetModel) btw =
        single (fun doc (state: FlowState<ReportInfo<HangtagReportInfo,_>>) ->
            let userState = state.UserState
            ImposerSelectors.cata btw.Variable.ImposerSelectors (fun _ ->
                let (tpOrder,reportInfo) =
                    let pages = doc.Src |> PdfDocument.getAllPages
                    PreImpose.run tables hangtag userState tarm (fun plateOpenSize userState  ->
                        Benifit.gen plateOpenSize pages p tarm tables userState
                    )
                    |> Benifit.max

                tpOrder.Pages |> List.iter (fun (f,b) -> [f;b] |> List.iter (PdfPage.addToDoc(doc.Dest)))
                let reportInfo = { reportInfo with Tables = [tpOrder.TpTable] } |> updateTemplate tarm hangtag p 
                { state with UserState = reportInfo } 
            )
        )