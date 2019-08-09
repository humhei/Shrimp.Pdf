namespace Shrimp.Pdfargets.Hangtag.Output.VDP.Shuffle
module internal Report =
    open Shrimp.Entities.Types
    open Shrimp.Pdf.Types
    open Shrimp.Pdf
    open Shrimp.Pdf.Extensions
    open Shrimp.Pdf.Colors
    open Shrimp.Pdf.Targets
    open Shrimp.Pdf.Targets.Reports
    open Types
    open Shrimp.Pdf.Targets.Hangtag

    let private dummySpecfic = HangtagUnit.dummy



