namespace Atrous.Pdfargets.Hangtag.Output.VDP.Shuffle
open Atrous.Entities.Types
open Atrous.Pdf.Types
open Atrous.Pdf
open Atrous.Pdf.Reuses
open Atrous.Pdf.Extensions
open Atrous.Pdf.Targets
open Atrous.Pdf.Targets.Reports
open Types
open Atrous.Pdf.Targets.Hangtag

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