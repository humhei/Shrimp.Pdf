namespace Shrimp.Pdfargets.Hangtag.Output.VDP
open Shrimp.Entities.Types
open Shrimp.Pdf
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Colors
open Shrimp.Pdf.Targets
open Shrimp.Pdf.Targets.Reports

[<RequireQualifiedAccess>]
module PreColor =
    open Shrimp.Pdf.Targets.Hangtag
    open Shrimp.Pdf.Types

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
