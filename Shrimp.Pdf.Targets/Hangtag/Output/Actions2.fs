namespace Atrous.Pdfargets.Hangtag.Output
module internal Actions =
    open Atrous.Entities.Types
    open Atrous.Pdf.Types
    open Atrous.Pdf.Manipulates
    open Atrous.Pdf.Reuses
    open PageSelection
    open Atrous.Pdf.Targets.Actions.Manipulates
    open Atrous.Pdf.Colors
    open Atrous.Pdf
    open Atrous.Pdf.Targets
    open iText.Kernel.Colors

    let addCroppinglinePage (hangtag: Hangtag) =
        if Option.isSome hangtag.CuttingLine 
        then Flow.Composite [duplicateFirstPageToLast()]
        else Flow.Composite []


    let processColor =
        let docColor =
            fromReportUnit (fun reportUnit ->
                let desiredColor = reportUnit.DesiredColor
                match desiredColor with 
                | DesiredColor.CMYK -> 
                    addCMYKMarker
                | DesiredColor.Double (c1,c2) -> 
                    addSeperationColor [c1;c2]
                | DesiredColor.Single c ->
                    addSeperationColor [c]
            )
        [
            forContentPages docColor
        ]
        |> manipulatesStepByStep