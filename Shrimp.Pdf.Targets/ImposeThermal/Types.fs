namespace Atrous.Pdfargets.ImposeThermal
module Types =

    open Atrous.Pdf.Targets
    open iText.Kernel.Pdf
    open Atrous.Entities.Types
    open Atrous.Pdf
    open iText.Kernel.Font
    type ParserResult =
        {
            ThermalBackground: ThermalBackground
            SizeText: string
        }