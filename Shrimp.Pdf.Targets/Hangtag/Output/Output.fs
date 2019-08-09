
namespace Shrimp.Pdf.Targets.Hangtag.Output

open Shrimp.Entities.Types
open System.IO
open Shrimp.Pdf.Types
open Fake.IO
open Shrimp.Pdf
open iText.Kernel.Geom
open Shrimp.Pdf.Utils
open Shrimp.Pdf.Reuses
open Shrimp.Pdf.Manipulates
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Operators
open Shrimp.Pdf.Colors
open Shrimp.Pdf.Targets.Actions
open Reuses
open Shrimp.Pdf.Targets
open Shrimp.Pdf.Targets.Reports
open Types
open Actions
open Shrimp.Pdf.Targets.Hangtag.Output.VDP.Shuffle

[<RequireQualifiedAccess>]
module Output =
    open Shrimp.Pdf.Targets.Hangtag
    open Shrimp.Pdf.Targets.Hangtag.Output.VDP
    open PageSelection
    open Actions
    open Manipulates

    let run (p: Product) tarm targetName (hangtag: Hangtag) =
        let bleed = Hangtag.getBleed hangtag
        let hspaces = Margin.toHSpacesFromOption bleed
        let vspaces = Margin.toVSpacesFromOption bleed

        match hangtag.Front,hangtag.Back with 
        | DocType.AI _,Some (DocType.AI _) -> 
            failwith "Not implemented"
            let pdf,_ = convertToPdf targetName p tarm hangtag |> List.exactlyOne
            [ 
                imposeWith (fun arg ->
                    { arg with 
                        Margin = Margin.createSimple(mm 6.) 
                        HSpaces = hspaces
                        VSpaces = vspaces
                        Background = Some (Background.Size (PageSize.A3.Rotate()))
                        IsRepeated = true
                        Cropmark = Cropmark.simple |> Some
                    }
                ) 

                duplicatePages [1;2;1]
                manipulates [
                    forPageNums (fun n -> n < 3) removeCuttingLineAI
                    forPageNums (fun n -> n = 3) retatinCuttingLineAI
                    forPageNums (fun n -> n = 1) (addSeamText "正") 
                    forPageNums (fun n -> n = 2) (addSeamText "反") 
                    forPageNums (fun n -> n = 3) (addSeamText "刀版") 
                    forPageNums (fun n -> n < 3) addCMYKMarker 
                    addSeamMark
                ] 
            ]
            |> simplePreflight pdf

            TargetModel.fileInTarget p targetName ".pdf" tarm |> (fun d ->
                File.Copy(pdf,d,true)
            )

            File.delete pdf

        | DocType.AI a, None | DocType.Pdf a, None ->
            let pdf,_ = convertToPdf targetName p tarm hangtag |> List.exactlyOne
            let processCuttingLine (hangtag: Hangtag) =
                let r = 
                    if Option.isSome hangtag.CuttingLine then 
                        [
                            duplicateFirstPageToLast()
                            manipulates 
                                [
                                    forPageNumsWithTotalNumbers (fun total i -> i <> total) (removePathWithStrokeColors Colors.AI.cuttingLine)
                                    forPageNumsWithTotalNumbers (fun total i -> i = total) (retainPathWithStrokeColors Colors.AI.cuttingLine)
                                ]
                        ]
                    else 
                        [
                            manipulates 
                                [
                                    forPageNums (fun i -> true) (removePathWithStrokeColors Colors.AI.cuttingLine)
                                ]
                        ] 
                r |> Flow.Composite
            [ 
                manipulate trimToStrokeCyanOrMegenta
                imposeWith (fun arg ->
                    { arg with 
                        Margin = Margin.createSimple(mm 6.) 
                        HSpaces = hspaces
                        VSpaces = vspaces
                        Background = Some (Background.Size (OpenSize.BigEight))
                        IsRepeated = true
                        Cropmark = Cropmark.simple |> Some
                    }
                ) 

                manipulates 
                    [
                        addSeamText "单面"
                        addSeamMark
                        //processColor hangtag
                    ]
                processCuttingLine hangtag
            ]
            |> simplePreflight pdf

            TargetModel.fileInTarget p targetName ".pdf" tarm |> fun d -> File.Copy(pdf,d,true)

            File.delete pdf
        | DocType.AI pdfDoc,Some (DocType.Btw btw) | DocType.Pdf pdfDoc,Some (DocType.Btw btw)-> 
            let (pdf,_),(backPdf,tables) = convertToPdf targetName p tarm hangtag |> fun zips -> zips.[0],zips.[1]
            [
                manipulates [ bleedOutToStrokeCyanOrMagenta pdfDoc.Bleed ]
            ] |> simplePreflight pdf

            let mds = 
                [ 
                    manipulates
                        [
                            trimToStrokeBlue
                        ]
                    PreColor.run pdf pdfDoc btw
                    ImposerSelectors.cata btw.Variable.ImposerSelectors (fun _ -> 
                        let root = TargetModel.tagDir tarm 
                        assignPagesToOrders p root tables targetName
                    )
                    Read.btwSize []
                    mergeFront pdf
                    Read.extractFirstPageToBrandPage
                    Shuffle.run p hangtag tables tarm btw
                    Flow.FromState (fun state ->
                        let arg = state.UserState.Arg
                        let strategy = state.UserState.Specific.ImposingStrategy
                        imposeFB arg strategy
                    )
                    addCroppinglinePage hangtag
                    [
                        forContentPages (removePathWithStrokeColors Colors.cuttingLine)
                        forFrontPages (addSeamText "咬口正") 
                        forHFlipPage (addSeamText "咬口左右翻") 
                        withPages forBackPages (fun unit -> 
                            let text = unit.Specfic.PlateRole |> PlateRole.toString
                            addSeamText text
                        )
                        addSeamMark
                        addTextToTopRight (
                            sprintf "%s %s" todayMark tarm.CompanyName
                        )
                    ] |> Seam.cutting hangtag |> manipulatesStepByStep
                    processColor
                ]
                |> runCustom HangtagReportInfo.dummy backPdf

            let scenes =
                [
                    ReportingScene.house_partition
                    ReportingScene.house_priting
                    ReportingScene.reconciliation
                    ReportingScene.house_benefit
                ]

            mds |> List.iter (fun md -> scenes |> List.iter (ReportingScene.report md))

        | _ -> ()