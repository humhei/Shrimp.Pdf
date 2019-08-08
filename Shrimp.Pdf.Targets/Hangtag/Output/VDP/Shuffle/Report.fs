namespace Atrous.Pdfargets.Hangtag.Output.VDP.Shuffle
module internal Report =
    open Atrous.Entities.Types
    open Atrous.Pdf.Types
    open Atrous.Pdf
    open Atrous.Pdf.Extensions
    open Atrous.Pdf.Colors
    open Atrous.Pdf.Targets
    open Atrous.Pdf.Targets.Reports
    open Types
    open Atrous.Pdf.Targets.Hangtag

    let private dummySpecfic = HangtagUnit.dummy



