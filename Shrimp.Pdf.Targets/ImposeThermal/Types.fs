namespace Shrimp.Pdfargets.ImposeThermal
module Types =

    open Shrimp.Pdf.Targets
    open iText.Kernel.Pdf
    open Shrimp.Entities.Types
    open Shrimp.Pdf
    open iText.Kernel.Font
    type ParserResult =
        {
            ThermalBackground: ThermalBackground
            SizeText: string
        }