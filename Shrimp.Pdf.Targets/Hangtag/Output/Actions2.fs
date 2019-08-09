namespace Shrimp.Pdfargets.Hangtag.Output
module internal Actions =
    open Shrimp.Entities.Types
    open Shrimp.Pdf.Types
    open Shrimp.Pdf.Manipulates
    open Shrimp.Pdf.Reuses
    open PageSelection
    open Shrimp.Pdf.Targets.Actions.Manipulates
    open Shrimp.Pdf.Colors
    open Shrimp.Pdf
    open Shrimp.Pdf.Targets
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