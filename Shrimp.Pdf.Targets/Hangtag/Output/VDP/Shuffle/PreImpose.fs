namespace Shrimp.Pdfargets.Hangtag.Output.VDP.Shuffle
open Shrimp.Entities.Types
open Shrimp.Pdf.Types
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Targets
open Shrimp.Pdf.Targets.Reports
open Types
open Shrimp.Pdf.Targets.Hangtag

[<RequireQualifiedAccess>]
module internal PreImpose =
    open LiteDB

    let private strategy plateOpenSize hspaces vspaces (tables: TypedImposerDataTable list) (size:TextRenderInfo) (state: ReportInfo<HangtagReportInfo,_>) =
        let state = Read.preImpose plateOpenSize hspaces vspaces size state
        let imposeNumber = state.ImposeMatrix ||> (*)
        let dataLength = tables |> List.sumBy (fun tb -> tb.Rows.Length)
        if dataLength > imposeNumber then 
            { state with 
                Specific = 
                    { state.Specific with
                        ImposingStrategy = FlipWay.FB }
            }
        elif dataLength <= imposeNumber / 2 then 
            let colNum,rowNum = state.ImposeMatrix
            if colNum % 2 = 0 then 
                { state with 
                    Specific = 
                        { state.Specific with
                            ImposingStrategy = FlipWay.HFlip }
                }
            else failwith "Not implemented"
        else 
            { state with 
                Specific = 
                    { state.Specific with
                        ImposingStrategy = FlipWay.FB }
            }


    let private noTemplate tables hangtag (state: ReportInfo<_,_>) (tarm: TargetModel) genBenefit =
        let bleed = Hangtag.getBleed hangtag
        let hspaces = Margin.toHSpacesFromOption bleed
        let vspaces = Margin.toVSpacesFromOption bleed
        let size = state.Size
        let plateOpenSizes = 
            tarm.GlobalConfig 
            |> GlobalConfig.getPresses 
            |> List.collect Press.getPlateSizes
            |> List.distinct

        plateOpenSizes |> List.map (fun plateOpenSize ->
            async {
                let state = strategy plateOpenSize hspaces vspaces tables size state
                let arg = state.Arg
                let isBleed = bleed.IsSome
                let arg = { arg with IsRepeated = false; Cropmark = Some Cropmark.simple; UseBleed = isBleed }
                return (genBenefit plateOpenSize {state with Arg = arg})
            }
        )
        |> Async.Parallel
        |> Async.RunSynchronously
        |> List.ofArray


    let run tables hangtag (state: ReportInfo<_,_>) (tarm: TargetModel) genBenefit =
        let noTemplate() = 
            noTemplate tables hangtag (state: ReportInfo<_,_>) (tarm: TargetModel) genBenefit
        let size = state.Size
        match hangtag.AutoSetHangtagTemplate.MA with
        | MA.Auto -> 
            noTemplate()
        | MA.Manual ht ->
            match ht with 
            | None ->
                noTemplate()
            | Some template ->
                let plateOpenSize = template |> HangtagTemplate.toPlateOpenSize
                [state |> ReportInfo.fillWithHangtagTemplate size template |> genBenefit plateOpenSize]