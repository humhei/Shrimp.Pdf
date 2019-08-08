namespace Atrous.Pdfargets.Hangtag.Output
module internal PageSelection =
    open Atrous.Pdf.Types
    open Atrous.Pdf.Targets.Reports
    open Atrous.Pdf.Colors
    open Atrous.Pdf.Targets.Hangtag

    let notFirstPageElement =
        fun (ns: int list) -> List.skip 1 ns

    let notLastPageElement =
        fun (ns: int list) -> ns.[0..ns.Length - 2]

    let pagesElement predicate outUnit =
        fun (arg:ManipulateArgument<ReportInfo<HangtagReportInfo,HangtagUnit>>) -> 
            let state = arg.FlowState.UserState
            match state.ReportUnits |> List.tryItem (arg.PageNum - 1) with 
            | Some unit -> 
                    predicate unit
            | None -> outUnit

    let pagesElementWithRole role=
        pagesElement (fun unit -> unit.Specfic.PlateRole |> role) false

    let forPages unit outUnit =
        pagesElement unit outUnit |> forPageNum2

    let forPagesWithRole role  =
        pagesElementWithRole role |> forPageNum2

    let forBackPages = 
        forPagesWithRole PlateRole.isBack

    let forFrontPages = 
        forPagesWithRole PlateRole.isFront

    let forHFlipPage = 
        forPagesWithRole PlateRole.isFlip 

    let forContentPages = 
        forPagesWithRole (fun _ -> true)

    let forCuttingLinePages  = 
        forPages (fun _ -> false) true

    let forPagesWithColor color =
        forPages (fun unit -> color unit.DesiredColor)

    let forCMYKPages =
        forPagesWithColor DesiredColor.isCmyk

    let withPages (forPages: Manipulate<_> -> Manipulate<_>) manipulateGenerator =
        fun arg ->
            let state = arg.FlowState.UserState
            match state.ReportUnits |> List.tryItem (arg.PageNum - 1) with 
            | Some unit -> 
                let manipulate = manipulateGenerator unit
                arg |> forPages manipulate
            | None -> arg.FlowState
