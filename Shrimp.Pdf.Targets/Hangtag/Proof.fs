namespace Atrous.Pdfargets.Hangtag
module internal Proof =
    open Atrous.Entities.Types
    open Atrous.Pdf.Targets.Types
    open Atrous.Pdf.Reuses
    open Atrous.Pdf.Types
    open Atrous.Pdf.Extensions
    open Atrous.Pdf.Utils
    open Atrous.Pdf.Operators
    open System.IO
    open Fake.IO
    open Atrous.Pdf.Manipulates
    open Atrous.Pdf.Targets.Reports
    open Atrous.Pdf.Targets
    open iText.Kernel.Pdf
    open Atrous.Pdf
    open iText.Kernel.Geom
    open iText.Kernel.Colors
    open Atrous.Pdf.Colors
    open Atrous.Pdf.Targets.Actions
    open Manipulates
    open FromState

    let run (p: Product) tarm targetName (hangtag: Hangtag) =
        let hspaces = [mm 3.]
        let vspaces = [mm 3.]
        let margin = Margin.createSimple(mm 6.) 
        match hangtag.Front,hangtag.Back with 
        | DocType.AI a1,Some (DocType.AI a2) -> 
            let pdf,_ = convertToPdf targetName p tarm hangtag |> List.exactlyOne
            [ 
                imposeWith (fun arg ->
                    { arg with 
                        Margin = margin
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

            TargetModel.filesInTargetOrder p targetName ".pdf" tarm |> List.iter (fun d ->
                File.Copy(pdf,d,true)
            )

            File.delete pdf

        | DocType.AI a, None | DocType.Pdf a, None ->
            let pdf,_ = convertToPdf targetName p tarm hangtag |> List.exactlyOne
            [ 
                manipulates 
                    [trimToStrokeCyanOrMegenta]
                imposeWith (fun arg ->
                    { arg with 
                        Margin = Margin.createSimple(mm 6.) 
                        HSpaces = [mm 3.]
                        VSpaces = [mm 3.]
                        Background = Some (Background.Size (PageSize.A3.Rotate()))
                        IsRepeated = true
                        Cropmark = Cropmark.simple |> Some
                    }
                ) 

                manipulates [
                    addSeamText "单面"
                    addCMYKMarker
                    addSeamMark
                ] 
            ]
            |> simplePreflight pdf

            TargetModel.filesInTargetOrder p targetName ".pdf" tarm |> List.iter (fun d ->
                File.Copy(pdf,d,true)
            )

            File.delete pdf

        | DocType.AI a,Some (DocType.Btw btw) -> 
            let install 
                (tables: TypedImposerDataTable list) 
                (f : FlowState<_> -> (TypedImposerDataRow * PdfPage) list -> (TypedImposerDataRow * PdfPage) list) = 
                    single <| fun doc (state: FlowState<ReportInfo<_,_>>) ->
                        let src = doc.Src
                        let dest = doc.Dest
                        let pages = PdfDocument.getAllPages src
                        let tables = 
                            [
                                {
                                    Name = tables |> List.map(fun tb -> tb.Name) |> String.concat ""
                                    Rows = tables |> List.collect (fun tb -> tb.Rows)
                                }
                            ]
                        let tables = 
                            tables |> List.map (fun tb ->
                                { tb with 
                                    Rows =
                                        let r = 
                                            List.zip tb.Rows pages 
                                            |> f state
                                        r |> List.map snd |> List.iter (fun page -> 
                                            let p = page.CopyTo(dest)
                                            dest.AddPage(p) |> ignore)
                                        r |> List.map fst
                                }
                            ) 

                        let userState = { state.UserState with Tables = tables } 
                        { state with UserState = userState }

            let (pdf,_),(backPdf,tables) = convertToPdf targetName p tarm hangtag |> fun zips -> zips.[0],zips.[1]
            let truncateToSelectors (tables: TypedImposerDataTable list) (selectors: Selector list) =
                install tables (fun model zips ->
                    let imposeNumber = model.UserState.ImposeMatrix ||> (*)
                    let unitsLength = zips.Length
                    if unitsLength > imposeNumber then 
                        let groupedUnits = 
                            zips 
                            |> List.groupBy (fun (row, page) ->
                                selectors |> List.map (fun k -> TypedImposerDataRow.getValueFromSelector k row)
                            )
                            |> List.map snd
                        List.extractEachUntil imposeNumber groupedUnits
                    else failwith "Not implemented"
                )



            [ 
                Read.btwSize 
                    [Read.preImpose (PlateOpenSize.OpenSize OpenSize.Eight) hspaces vspaces]
                truncateToSelectors tables btw.Variable.MakesureSelectors
                manipulates 
                    [
                        trimToStrokeBlue
                        replaceStrokeColorOfPath [DeviceRgb.BLUE] DeviceCmyk.CYAN
                    ]
                merge pdf
                duplicatePagesForFirstPageOfFront()
                imposeWithStateOfArgs
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
            |> Atrous.Pdf.Targets.Operators.run backPdf
            |> ignore
            | _ -> ()