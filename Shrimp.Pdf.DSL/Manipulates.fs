namespace Shrimp.Pdf
#nowarn "0104"
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open Shrimp.FSharp.Plus
open iText.Kernel.Pdf
open iText.Kernel.Geom
open System.IO


[<AutoOpen>]
module Manipulates =
    

    type PdfRunner with 
        static member Manipulate(pdfFile, ?backupPdfPath) = 
            fun manipulate ->
                PdfRunner.OneFileFlow(pdfFile, ?backupPdfPath = backupPdfPath) (Flow.Manipulate manipulate)
    
    open Imposing

    type ModifyPage with
        static member private AddVSpaceMiddleLines(fLine: PageNumber * RowNumber -> SpaceMiddleLine option) =
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

        static member private AddHSpaceMiddleLines(fLine: PageNumber * ColumnNumber -> SpaceMiddleLine option) =
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

        /// After Imposing
        static member AddSpaceMiddleLines(rowOrColumn, fLine: PageNumber * RowOrColumnNumber -> SpaceMiddleLine option) =
            match rowOrColumn with 
            | RowOrColumn.Column ->
                ModifyPage.AddHSpaceMiddleLines(
                    fun (pageNumber, columnNumber) ->
                        fLine (pageNumber, RowOrColumnNumber.ColumnNumber columnNumber)
                )

            | RowOrColumn.Row ->
                ModifyPage.AddVSpaceMiddleLines(
                    fun (pageNumber, rowNumber) ->
                        fLine (pageNumber, RowOrColumnNumber.RowNumber rowNumber)
                )


        static member ReadDataTable(pageSelector, format: DataTableParsingFormat, ?boundSelector) =
            let readBound() =
                match boundSelector with 
                | None -> Manipulate.dummy() ||>> fun _ -> []
                | Some selector ->
                    ModifyPage.Create(
                        "read dataTable bound",
                        pageSelector,
                        Selector.Path(selector),
                        fun arg infos ->
                            infos
                            |> List.ofSeq
                            |> List.map (IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth)
                            |> AtLeastOneList.Create
                            |> Rectangle.ofRectangles
                    )

            readBound()
            <+>
            ModifyPage.Create(
                "extract dataTable inside bound",
                pageSelector,
                Selector.Text(fun args -> 
                    let rect = 
                        match boundSelector with 
                        | None -> args.Page.GetActualBox()
                        | Some _ -> args.UserState.[args.PageNum-1]
                    Info.BoundIsInsideOf (AreaGettingOptions.Specfic rect) args
                ),
                fun arg infos ->
                    let textInfos =
                        infos
                        |> List.ofSeq
                        |> List.choose IIntegratedRenderInfo.asITextRenderInfo

                    let array2D = 
                        textInfos
                        |> List.chunkBySize format.ColNum

                    let __ensureNoRemainer =
                        match textInfos.Length % format.ColNum with 
                        | 0 -> ()
                        | _ -> failwithf "textInfos length %d is not exact division to format ColNum %d" textInfos.Length format.ColNum

                    array2D
            )



        static member TrimToVisible (pageSelector: PageSelector, ?margin: Margin)  =
            let margin = defaultArg margin (Margin.Create 0.)
            ModifyPage.Create(
                "trim to visible",
                pageSelector,
                PathOrText (Info.IsVisible()),
                (fun args renderInfos ->
                    let bounds = 
                        renderInfos
                        |> Seq.choose (IIntegratedRenderInfo.tryGetVisibleBound BoundGettingStrokeOptions.WithStrokeWidth)
                        |> AtLeastOneList.TryCreate

                    let bound =
                        bounds
                        |> Option.map Rectangle.ofRectangles

                    match bound with 
                    | Some bound ->
                        args.Page.SetActualBox(bound |> Rectangle.applyMargin margin)
                        |> ignore

                    | None -> 
                        failwithf "Cannot trim to visible as all infos are invisible"

                ),
                parameters = [
                    "margin" => margin.ToString()
                ]
            )  ||>> ignore


