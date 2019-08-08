namespace Atrous.Pdfargets
open Atrous.Pdf.Operators


[<AutoOpen>]
module Operators =
    open Atrous.Pdf
    open Atrous.Pdf.Types
    open Atrous.Pdf.Targets.Types
    open System.IO
    open Deedle
    open System.Collections.Generic

    let run = Operators.preflight

    let runWithUserStateReturn pdf (flows: Flow<'a list> list)=
        Operators.preflightWithModel 
            {
                Path = pdf
                FlowState = initFlowState []
            }
            (flows)
        |> List.exactlyOne
        |> fun md -> md.FlowState.UserState

    let runCustom userState pdf =
        Operators.preflightWithModel (FlowModel.initCustom userState pdf) 

    let runWithMap pdf flows =
        Operators.preflightWithModel (FlowModel.initCustom Map.empty pdf) flows
        |> ignore

    let (+>+) (flows,getter,update) newFlows =
        fun reportInfo pdf ->
            let newFlowModels = 
                let subReportInfo = getter reportInfo
                runCustom subReportInfo pdf flows |> List.map (fun flowModel ->
                    FlowModel.mapUserState
                        (fun userState -> update userState reportInfo)
                        flowModel
                )
            newFlowModels |> List.collect (fun flowModel ->
                Operators.preflightWithModel flowModel newFlows
            )