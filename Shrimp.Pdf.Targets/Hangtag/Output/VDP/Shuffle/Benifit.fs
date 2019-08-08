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
module Benifit =
    let gen (plateOpenSize: PlateOpenSize) pages p tarm tables (userState: ReportInfo<_,_>) =
        let imposeNum = userState.ImposeNum
        let arg = userState.Arg
        let productSpecfic = userState.Specific
        let strategy = productSpecfic.ImposingStrategy
        let lu = productSpecfic.LeastUtilization
        let initImposeNumber =
            match strategy with 
            | FlipWay.FB -> 
                imposeNum 
            | FlipWay.HFlip ->
                imposeNum / 2
            | _ -> failwith "Not implemented"

        match strategy with 
        | FlipWay.FB | FlipWay.HFlip -> 
            let typeOrders = FBTypedOrder.init tables pages
            let r =
                typeOrders |> List.map (fun typeOrder -> 
                    let rec loop imposeNum multiple previous =
                        let l = typeOrder.Pages.Length
                        let multipledNum = imposeNum * multiple
                        if multipledNum < l then
                            loop imposeNum (multiple + 1) previous
                        elif float (multipledNum - l) / float l < lu then
                            loop imposeNum (multiple + 1) previous
                        else
                            let tpOrder = 
                                typeOrder 
                                |> TypedOrder.shuffleToImposeNumber arg multipledNum
                                |> TypedOrder.sortByNumber

                            let newState = Report.run plateOpenSize p tarm tpOrder userState 
                            let currentBenifit = newState.Benifit
                            match strategy with 
                            | FlipWay.FB ->
                                match previous with 
                                | Some(previousTpOrder,preReportInfo,preMultiple) ->
                                    let preBenifit = preReportInfo.SalePrice - preReportInfo.Cost
                                    if preBenifit > currentBenifit then previousTpOrder,preReportInfo,preMultiple
                                    else loop imposeNum (multiple + 1) (Some (tpOrder,newState,multiple))
                                | None -> loop imposeNum (multiple + 1) (Some (tpOrder,newState,multiple)) 
                            | FlipWay.HFlip ->
                                tpOrder,newState,multiple
                            | _ -> failwith "Not implemented"
                                    
                    let (tpOrder,reportInfo,multiple) = loop initImposeNumber 1 None
                    tpOrder |> TypedOrder.sortById,reportInfo
                )
            r |> List.exactlyOne

        | _ -> failwith "Not implemented"

    let max (zips: ((TypedOrder<_> * ReportInfo<HangtagReportInfo,_>) list)) =
        zips 
        |> List.maxBy (fun (_,reportInfo) ->
            reportInfo.Benifit
        )