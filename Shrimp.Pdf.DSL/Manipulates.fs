namespace Shrimp.Pdf
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open Shrimp.FSharp.Plus
open iText.Kernel.Pdf
open iText.Kernel.Geom
open System.IO
open Imposing

[<AutoOpen>]
module Manipulates =
    type PdfRunner with 
        static member Manipulate(pdfFile, ?backupPdfPath) = 
            fun manipulate ->
                PdfRunner.OneFileFlow(pdfFile, ?backupPdfPath = backupPdfPath) (Flow.Manipulate manipulate)
            

    type ModifyPage with
        static member AddVSpaceMiddleLines(fLine: PageNumber * RowNumber -> SpaceMiddleLine option) =
            ModifyPage.Create(
                "AddVSpaceMiddleLines",
                PageSelector.All,
                Dummy,
                (fun args renderInfos ->
                    let doc: ImposingDocument = args.UserState
                    match doc.IsDrawed with 
                    | true -> 
                        let sheet = doc.GetSheet(args.PageNum-1)
                        let sheetMargin = sheet.Margin

                        let rows = sheet.GetRows()
                        let rowNumber_row_middleLine__zippedList =
                            rows
                            |> List.indexed
                            |> List.choose(fun (i, row) ->
                                let rowNumber = RowNumber(i+1)
                                match rowNumber.Value < rows.Length with 
                                | true -> 
                                    let middleLine = fLine(PageNumber args.PageNum, rowNumber)
                                    match middleLine with 
                                    | Some middleLine -> Some (rowNumber, row, middleLine)
                                    | None -> None
                                | false -> None
                            )

                        match rowNumber_row_middleLine__zippedList with 
                        | [] -> ()
                        | _ ->
                            let pageBox = args.Page.GetActualBox()
                            let pdfCanvas = iText.Kernel.Pdf.Canvas.PdfCanvas(args.Page)
                            let vspaces = 
                                doc.ImposingArguments.Value.VSpaces
                                |> List.replicate rows.Length
                                |> List.concat

                            pdfCanvas.SaveState() |> ignore
                            rowNumber_row_middleLine__zippedList
                            |> List.iter(fun (rowNumber, row, middleLine) ->
                                let line = 
                                    let y = 
                                        let preSpaces_Total = 
                                            vspaces.[0..rowNumber.Value-2]
                                            |> List.sum

                                        let preHeights_Total =
                                            rows.[0..rowNumber.Value-2]
                                            |> List.sumBy(fun m -> m.Height)

                                        let nextSpace = vspaces.[rowNumber.Value-1]

                                        pageBox.GetTopF() - (preSpaces_Total + nextSpace / 2. + row.Height + sheetMargin.Top + preHeights_Total)

                                    let xStart = 
                                        pageBox.GetXF() 
                                        + ((1. - middleLine.EdgeLength_PercentToMargin) * sheetMargin.Left)

                                    let xEnd = 
                                        pageBox.GetRightF()
                                        - ((1. - middleLine.EdgeLength_PercentToMargin) * sheetMargin.Right)

                                    { Start = Point(xStart, y)
                                      End = Point(xEnd, y)}

                                pdfCanvas
                                |> PdfCanvas.addLine line (fun args -> middleLine.Properties)
                                |> ignore

                            )
                            pdfCanvas.RestoreState() |> ignore


                    | false -> failwithf "Cannot add hspace middle lines as imposing document was not drawed"

                    doc
                ),
                parameters = [
                    "fLine" => fLine.ToString()
                ]
            )  ||>> (List.head)

        static member AddHSpaceMiddleLines(fLine: PageNumber * ColumnNumber -> SpaceMiddleLine option) =
            ModifyPage.Create(
                "AddHSpaceMiddleLines",
                PageSelector.All,
                Dummy,
                (fun args renderInfos ->
                    let doc: ImposingDocument = args.UserState
                    match doc.IsDrawed with 
                    | true -> 
                        let sheet = doc.GetSheet(args.PageNum-1)
                        let sheetMargin = sheet.Margin

                        let cells = sheet.GetFirstRow().GetCells()

                        let colNumber_col_middleLine__zippedList =
                            cells
                            |> List.indexed
                            |> List.choose(fun (i, cell) ->
                                let colNumber = ColumnNumber(i+1)
                                match colNumber.Value < cells.Length with 
                                | true -> 
                                    let middleLine = fLine(PageNumber args.PageNum, colNumber)
                                    match middleLine with 
                                    | Some middleLine -> Some (colNumber, cell, middleLine)
                                    | None -> None
                                | false -> None
                            )

                        match colNumber_col_middleLine__zippedList with 
                        | [] -> ()
                        | _ ->
                            let pageBox = args.Page.GetActualBox()
                            let pdfCanvas = iText.Kernel.Pdf.Canvas.PdfCanvas(args.Page)
                            let hspaces = 
                                doc.ImposingArguments.Value.HSpaces
                                |> List.replicate cells.Length
                                |> List.concat

                            pdfCanvas.SaveState() |> ignore
                            colNumber_col_middleLine__zippedList
                            |> List.iter(fun (colNumber, col, middleLine) ->
                                let line = 
                                    let x = 
                                        let preSpaces_Total = 
                                            hspaces.[0..colNumber.Value-2]
                                            |> List.sum

                                        let preWidthss_Total =
                                            cells.[0..colNumber.Value-2]
                                            |> List.sumBy(fun m -> m.Size.Width)

                                        let nextSpace = hspaces.[colNumber.Value-1]

                                        pageBox.GetXF() + (preSpaces_Total + nextSpace / 2. + col.Size.Width + sheetMargin.Left + preWidthss_Total)

                                    let yStart = 
                                        pageBox.GetTopF() 
                                        - ((1. - middleLine.EdgeLength_PercentToMargin) * sheetMargin.Top)

                                    let yEnd = 
                                        pageBox.GetBottomF()
                                        + ((1. - middleLine.EdgeLength_PercentToMargin) * sheetMargin.Bottom)

                                    { Start = Point(x, yStart)
                                      End = Point(x, yEnd)}

                                pdfCanvas
                                |> PdfCanvas.addLine line (fun args -> middleLine.Properties)
                                |> ignore
                            )
                            pdfCanvas.RestoreState() |> ignore

                    | false -> failwithf "Cannot add hspace middle lines as imposing document was not drawed"

                    doc
                ),
                parameters = [
                    "fLine" => fLine.ToString()
                ]
            )  ||>> (List.head)


        static member TrimToVisible (pageSelector: PageSelector, ?margin: Margin)  =
            let margin = defaultArg margin (Margin.Create 0.)
            ModifyPage.Create(
                "trim to visible",
                pageSelector,
                PathOrText (Info.IsVisible()),
                (fun args renderInfos ->
                    let bound = 
                        renderInfos
                        |> Seq.choose (IIntegratedRenderInfo.tryGetVisibleBound BoundGettingStrokeOptions.WithStrokeWidth)
                        |> Rectangle.ofRectangles

                    args.Page.SetActualBox(bound |> Rectangle.applyMargin margin)
                    |> ignore

                ),
                parameters = [
                    "margin" => margin.ToString()
                ]
            )  ||>> ignore