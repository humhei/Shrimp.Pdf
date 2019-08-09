﻿namespace Shrimp.Pdfargets.Hangtag
module internal MakeSure =
    open Shrimp.Entities.Types
    open Shrimp.Pdf.Targets.Types
    open Shrimp.Pdf.Reuses
    open Shrimp.Pdf.Types
    open Shrimp.Pdf.Extensions
    open Shrimp.Pdf.Utils
    open Shrimp.Pdf.Operators
    open System.IO
    open Fake.IO
    open Shrimp.Pdf.Manipulates
    open Shrimp.Pdf.Targets

    let run (p: Product) tarm targetName (hangtag: Hangtag) =
        let root = TargetModel.tagDir tarm
        match hangtag.Front,hangtag.Back with 
        | DocType.AI _,Some (DocType.AI _) -> 
            let pdf,_ = convertToPdf targetName p tarm hangtag |> List.exactlyOne
            imposeWith (fun arg ->
                { arg with 
                    Margin = Margin.createSimple(mm 6.) 
                    HSpaces = [mm 3.]
                    UseBleed = true
                }
            ) 
            |> List.singleton
            |> simplePreflight pdf

            TargetModel.filesInTargetOrder p targetName ".pdf" tarm |> List.iter (fun d ->
                File.Copy(pdf,d,true)
            )

            File.delete pdf

        | DocType.AI _,Some (DocType.Btw btw) | DocType.Pdf _,Some (DocType.Btw btw) ->
            let (pdf,_),(backPdf,tables) = convertToPdf targetName p tarm hangtag |> fun zips -> zips.[0],zips.[1]
            [
                manipulates 
                    [
                        clippingToTrimBox
                        applyMargin (Margin.imposingDest)
                    ]
            ]
            |> simplePreflight pdf

            let addSizeText : Flow<ReportInfo<_,_>> =
                manipulateWithStates (fun arg -> 
                    let table = arg.ImposerTable
                    match table with 
                    | Some table ->
                        let size = 
                            let state = arg.FlowState.UserState
                            state.Size |> TextRenderInfo.getText
                        addTextToEmptySpaceOfFirstImposerRow size 0.5 table 
                    | None -> failwith "ImposerTable shouldn't be empty"
                )

            [ 
                Read.btwSize []
                Btw.makeSure p root targetName btw.Variable tables
                merge pdf
                oneColumn()
                addSizeText
            ]
            |> Shrimp.Pdf.Targets.Operators.run backPdf
            |> ignore

        | DocType.Pdf _,None ->
            let (pdf,_) = convertToPdf targetName p tarm hangtag |> List.exactlyOne
            imposeWith (fun arg ->
                { arg with 
                    Margin = Margin.createSimple(mm 6.) 
                    HSpaces = [mm 3.]
                    UseBleed = true
                }
            ) 
            |> List.singleton
            |> simplePreflight pdf

            TargetModel.filesInTargetOrder p targetName ".pdf" tarm |> List.iter (fun d ->
                File.Copy(pdf,d,true)
            )
            File.delete pdf
        | _ -> ()
