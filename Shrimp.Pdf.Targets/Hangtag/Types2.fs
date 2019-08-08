namespace Atrous.Pdfargets.Hangtag

open Atrous.Entities.Types
open Atrous.Pdf.Types
open Atrous.Pdf.Colors
[<AutoOpen>]
module Type =
    open Atrous.Pdf.Targets.Reports

    [<RequireQualifiedAccess>]
    type PlateRole =
        | Front of int
        | Back of int
        | Flip 

    [<RequireQualifiedAccess>]
    module PlateRole =
        let isBack pr =
            match pr with 
            | PlateRole.Back _ -> true 
            | _ -> false

        let asBack pr =
            match pr with 
            | PlateRole.Back i -> Some i
            | _ -> None

        let isFront pr =
            match pr with 
            | PlateRole.Front _ -> true 
            | _ -> false

        let isFlip pr =
            match pr with 
            | PlateRole.Flip _ -> true 
            | _ -> false

        let isFB pr =
            match pr with 
            | PlateRole.Front _ -> true
            | PlateRole.Back  _ -> true
            | PlateRole.Flip  _ -> false

        let isCommon pr =
            match pr with 
            | PlateRole.Front i when i = 0 -> true
            | PlateRole.Back  i when i = 0 -> true
            | _ -> false


        let toStringOfRole pr =
            match pr with 
            | PlateRole.Front _ -> "正"
            | PlateRole.Back _ -> "反"
            | PlateRole.Flip _ -> "咬口左右翻"

        let private getIndex pr =
            match pr with 
            | PlateRole.Front i -> i
            | PlateRole.Back i -> i
            | _ -> 0

        let toStringOfIsCommon pr =
            if isCommon pr then "(通用)"
            else
                (getIndex pr).ToString()

        let toString pr =
            sprintf "%s %s"  (toStringOfIsCommon pr) (toStringOfRole pr)

        let paperInfo pr f =
            match pr with 
            | PlateRole.Front _ -> ""
            | PlateRole.Back _ -> f()
            | PlateRole.Flip _ -> f()


    type HangtagUnit =
        {
            PlateOpenSize: PlateOpenSize
            PlateRole: PlateRole
            Paper: Paper
        }

    [<RequireQualifiedAccess>]
    module HangtagUnit =
        open Atrous.Pdf.Targets.Reports
        open Atrous.Pdf.Utils

        let dummy =
            {
                PlateOpenSize = OpenSize.BigEight |> PlateOpenSize.OpenSize
                PlateRole = PlateRole.Flip
                Paper = Paper.``300gmsCoated``
            }

        let paperText (us: ReportUnit<HangtagUnit> list) =
            let isPositive (ru: ReportUnit<HangtagUnit>) = 
                let openSize = PlateOpenSize.asOriginOpenSize ru.Specfic.PlateOpenSize
                openSize.IsPositive
            us
            |> List.groupBy (fun ru -> ru.Specfic.Paper,isPositive ru) 
            |> List.map (fun ((p,_),rus) ->
                let s1 = Paper.toStringZH p
                let s2 = rus |> List.map (fun ru -> ru.PaperNumber) |> List.sum |> roundUp |> sprintf "%d张"
                s1 + s2
            )
            |> String.concat " "
            |> sprintf "共买%s"

    type HangtagReportUnit = ReportUnit<HangtagUnit>

    type HangtagReportInfo =
        {
            ImposingStrategy: FlipWay
            LeastUtilization: float
            DesiredColors: DesiredColor * DesiredColor option
            HangtagTemplate : HangtagTemplate option
        }

    [<RequireQualifiedAccess>]
    module HangtagReportInfo =
        let dummy = {
            ImposingStrategy = FlipWay.HFlip
            LeastUtilization = 0.18
            DesiredColors = DesiredColor.black,Some DesiredColor.black
            HangtagTemplate = None
        }

        let getPressPrice pageSize flipWay presses hangtagReportInfo =
            let fDesiredColor,bDesiredColor = hangtagReportInfo.DesiredColors
            let pressPriceOfDesiredColor = DesiredColor.pressPrice pageSize presses
            let bDesiredColor = bDesiredColor.Value
            let pressPrice = 
                match flipWay with 
                | FlipWay.FB -> 
                    pressPriceOfDesiredColor bDesiredColor |> float
                | FlipWay.HFlip ->
                    let p1 = pressPriceOfDesiredColor fDesiredColor |> float
                    let p2 = pressPriceOfDesiredColor bDesiredColor |> float
                    max p1 p2
                | _ -> failwith "Not implemented"
            pressPrice

    [<RequireQualifiedAccess>]
    module ReportUnit =
        open Atrous.Pdf.Targets.Reports
        open Atrous.Pdf.Targets.Types

        let dummyHangtag =
            FinanceUnit.createDummy HangtagUnit.dummy
        
        let fill (tarm: TargetModel) (reportInfo: ReportInfo<HangtagReportInfo,HangtagUnit>) plateOpenSize paper (reportUnit: FinanceUnit<_>) =
            let hangtagReportInfo = reportInfo.Specific
            let imposeNum = reportInfo.ImposeMatrix ||> (*)
            let flipWay = hangtagReportInfo.ImposingStrategy
            let printNum = reportUnit.PrintNum
            let originOpenSize = PlateOpenSize.asOriginOpenSize plateOpenSize
            let paperNum = printNum / float imposeNum / float (PlateOpenSize.getOpenNum plateOpenSize)
            let pageSize = PlateOpenSize.asClippedPageSize plateOpenSize
            let presses = tarm.GlobalConfig |> GlobalConfig.getPresses

            let cost = 
                let paperPrice = 
                    let unit = Paper.priceEveryPaper originOpenSize paper
                    unit * paperNum
                let pressPrice = HangtagReportInfo.getPressPrice pageSize flipWay presses hangtagReportInfo

                paperPrice + pressPrice
            let hangtagReportUnit = { HangtagUnit.dummy with PlateOpenSize = plateOpenSize;Paper = p.Paper |> Option.defaultValue dummySpecfic.Paper}
            let s = { state with Specific = hangtagReportUnit; PaperNumber = paperNum; Cost = cost }
            match flipWay with 
            | FlipWay.FB ->
                let backRoleIndex = 
                    if s.TotalIndex = 1 then 0 else s.CurrentIndex
                let s = { s with DesiredColor = bDesiredColor;Specfic = { hangtagReportUnit with PlateRole = PlateRole.Back backRoleIndex} }
                match s.CurrentIndex with 
                | 1 ->
                    let fuState =
                        let desiredColor = fDesiredColor
                        { FinanceUnit.dummyHangtag with 
                            DesiredColor = desiredColor
                            Specfic = { hangtagReportUnit with PlateRole = PlateRole.Front 0}
                            Cost = pressPriceOfDesiredColor desiredColor |> float }
                    [fuState; s]
                | _ -> 
                    [s]
            | FlipWay.HFlip ->
                let s = { s with DesiredColor = bDesiredColor + fDesiredColor;Specfic = { hangtagReportUnit with PlateRole = PlateRole.Flip} }
                [s]
            | _ -> failwith "Not implemented"

    [<RequireQualifiedAccess>]
    module internal ReportInfo =
        open iText.Kernel.Pdf.Canvas.Parser.Data
        open Atrous.Pdf.Targets.Reports
        open Atrous.Pdf.Targets
        open Atrous.Pdf.Targets.Types

        let fillWith p tpOrder state = ReportInfo.fill p tpOrder state ReportUnit.dummyHangtag

        let fillHangtagTemplate (size: TextRenderInfo) (template:HangtagTemplate) (state: ReportInfo<HangtagReportInfo,_>) =
            let newState = 
                Read.backTemplate template size state (fun arg ->
                    {arg with Cropmark = Some Cropmark.simple}
                )
            { newState with 
                Specific = 
                    { newState.Specific with 
                        HangtagTemplate = Some template
                        ImposingStrategy = FlipWay.FB } 
            }

        let fill (plateOpenSize: PlateOpenSize) (p: Product) (tarm: TargetModel) (tpOrder: TypedOrder<_>) (state: ReportInfo<HangtagReportInfo,HangtagUnit>) =
        
            let pageSize = PlateOpenSize.asClippedPageSize plateOpenSize
            let originOpenSize = PlateOpenSize.asOriginOpenSize plateOpenSize
            let presses = tarm.GlobalConfig |> GlobalConfig.getPresses
            let imposeNum = state.ImposeMatrix ||> (*)
            let hangtagReportInfo = state.Specific

            fillWith p tpOrder state (fun state ->
                let flipWay = hangtagReportInfo.ImposingStrategy
                let pressPriceOfDesiredColor = DesiredColor.pressPrice pageSize presses
                let printNum = FinanceUnit.getPrintNum state
                let paperNum = printNum / float imposeNum / float (PlateOpenSize.getOpenNum plateOpenSize)
                let cost = 
                    let paperPrice = 
                        let unit = Paper.priceEveryPaper originOpenSize p.Paper.Value
                        unit * paperNum
                    let pressPrice = HangtagReportInfo.getPressPrice pageSize flipWay presses hangtagReportInfo

                    paperPrice + pressPrice
                let hangtagReportUnit = { HangtagUnit.dummy with PlateOpenSize = plateOpenSize;Paper = p.Paper |> Option.defaultValue dummySpecfic.Paper}
                let s = { state with Specfic = hangtagReportUnit; PaperNumber = paperNum; Cost = cost }
                match flipWay with 
                | FlipWay.FB ->
                    let backRoleIndex = 
                        if s.TotalIndex = 1 then 0 else s.CurrentIndex
                    let s = { s with DesiredColor = bDesiredColor;Specfic = { hangtagReportUnit with PlateRole = PlateRole.Back backRoleIndex} }
                    match s.CurrentIndex with 
                    | 1 ->
                        let fuState =
                            let desiredColor = fDesiredColor
                            { FinanceUnit.dummyHangtag with 
                                DesiredColor = desiredColor
                                Specfic = { hangtagReportUnit with PlateRole = PlateRole.Front 0}
                                Cost = pressPriceOfDesiredColor desiredColor |> float }
                        [fuState; s]
                    | _ -> 
                        [s]
                | FlipWay.HFlip ->
                    let s = { s with DesiredColor = bDesiredColor + fDesiredColor;Specfic = { hangtagReportUnit with PlateRole = PlateRole.Flip} }
                    [s]
                | _ -> failwith "Not implemented"
            )
