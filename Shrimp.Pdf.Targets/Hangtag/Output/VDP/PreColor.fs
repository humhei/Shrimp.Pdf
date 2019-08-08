namespace Atrous.Pdfargets.Hangtag.Output.VDP
open Atrous.Entities.Types
open Atrous.Pdf
open Atrous.Pdf.Extensions
open Atrous.Pdf.Colors
open Atrous.Pdf.Targets
open Atrous.Pdf.Targets.Reports

[<RequireQualifiedAccess>]
module PreColor =
    open Atrous.Pdf.Targets.Hangtag
    open Atrous.Pdf.Types

    let run frontPath (pdfDoc: Pdf) (btw:Btw) =
        let backBleed = btw.Bleed
        Flow.Composite 
            [
                Btw.preColor()
                Read.desiredColors backBleed (fun (userState:ReportInfo<HangtagReportInfo,HangtagUnit>) colors ->
                    let frontColor = 
                        [
                            Read.desiredColors pdfDoc.Bleed (fun _ colors ->
                                colors |> Seq.exactlyOne
                            )
                        ]
                        |> runWithUserStateReturn frontPath
                        |> Colors.AI.exceptCuttingLine
                        |> Colors.AI.exceptWhite
                        |> DesiredColor.ofColors
       
                    let backColors = 
                        colors 
                        |> List.ofSeq 
                        |> List.map (Colors.Btw.exceptCuttingLine >> Colors.Btw.exceptWhite)
                    let backColor = List.concat backColors |> Colors.distinct |> List.ofSeq |> DesiredColor.ofColors

                    { userState with 
                        Specific = 
                            { userState.Specific with 
                                DesiredColors = frontColor,Some backColor } 
                    }
                )
            ]
