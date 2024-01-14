namespace Shrimp.Pdf

open Shrimp.Pdf.Colors

#nowarn "0104"
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open Shrimp.FSharp.Plus
open iText.Kernel.Pdf
open iText.Kernel.Geom
open Fake.IO
open System.IO


[<AutoOpen>]
module Manipulates =
    
    //type SampledPdfFile =
    //    { PdfFile: Lazy<PdfFile>
    //      SamplePdfFile: PdfFile }
    //with 
    //    static member Create(pdfFile: PdfFile) =
    //        let samplePdfFile =
    //            let flow = 
    //                Reuses.



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
                                let y = 
                                    let preSpaces_Total = 
                                        vspaces.[0..rowNumber.Value-2]
                                        |> List.sum

                                    let preHeights_Total =
                                        rows.[0..rowNumber.Value-2]
                                        |> List.sumBy(fun m -> m.Height)

                                    let nextSpace = vspaces.[rowNumber.Value-1]

                                    pageBox.GetTopF() - (preSpaces_Total + nextSpace / 2. + row.Height + sheetMargin.Top + preHeights_Total)


                                let line = 

                                    let xStart = 
                                        pageBox.GetXF() 
                                        + ((1. - middleLine.EdgeLength_PercentToMargin) * sheetMargin.Left)

                                    let xEnd = 
                                        pageBox.GetRightF()
                                        - ((1. - middleLine.EdgeLength_PercentToMargin) * sheetMargin.Right)

                                    { Start = Point(xStart, y)
                                      End = Point(xEnd, y)}

                                let addEdgeSolidPath() =
                                    match middleLine.EdgeSolidLine with 
                                    | None -> ()
                                    | Some edgeSolidLine ->
                                        let left = 
                                            let xStart = pageBox.GetXF() + edgeSolidLine.Start_ToEdge

                                            let xEnd = xStart + edgeSolidLine.Length

                                            { Start = Point(xStart, y)
                                              End = Point(xEnd, y)}

                                        let right = 
                                            let xStart = pageBox.GetRightF() - edgeSolidLine.Start_ToEdge
                                            let xEnd = xStart - edgeSolidLine.Length
                                            { Start = Point(xStart, y)
                                              End = Point(xEnd, y)}

                                        pdfCanvas
                                        |> PdfCanvas.addLine left (fun args -> 
                                            { middleLine.Properties with DashPattern = DashPattern.Empty }
                                        )
                                        |> PdfCanvas.addLine right (fun args -> 
                                            { middleLine.Properties with DashPattern = DashPattern.Empty }
                                        )
                                        |> ignore

                                addEdgeSolidPath()

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
                                let x = 
                                    let preSpaces_Total = 
                                        hspaces.[0..colNumber.Value-2]
                                        |> List.sum

                                    let preWidthss_Total =
                                        cells.[0..colNumber.Value-2]
                                        |> List.sumBy(fun m -> m.Size.Width)

                                    let nextSpace = hspaces.[colNumber.Value-1]

                                    pageBox.GetXF() + (preSpaces_Total + nextSpace / 2. + col.Size.Width + sheetMargin.Left + preWidthss_Total)


                                let line = 

                                    let yStart = 
                                        pageBox.GetTopF() 
                                        - ((1. - middleLine.EdgeLength_PercentToMargin) * sheetMargin.Top)

                                    let yEnd = 
                                        pageBox.GetBottomF()
                                        + ((1. - middleLine.EdgeLength_PercentToMargin) * sheetMargin.Bottom)

                                    { Start = Point(x, yStart)
                                      End = Point(x, yEnd)}

                                let addEdgeSolidPath() =
                                    match middleLine.EdgeSolidLine with 
                                    | None -> ()
                                    | Some edgeSolidLine ->
                                        let top = 
                                            let yStart = pageBox.GetTopF() - edgeSolidLine.Start_ToEdge
                                            let yEnd = yStart - edgeSolidLine.Length
                                            { Start = Point(x, yStart)
                                              End = Point(x, yEnd) }

                                        let bottom = 
                                            let yStart = pageBox.GetBottomF() + edgeSolidLine.Start_ToEdge
                                            let yEnd = yStart + edgeSolidLine.Length
                                            { Start = Point(x, yStart)
                                              End = Point(x, yEnd) }

                                        pdfCanvas
                                        |> PdfCanvas.addLine top (fun args -> 
                                            { middleLine.Properties with DashPattern = DashPattern.Empty }
                                        )
                                        |> PdfCanvas.addLine bottom (fun args -> 
                                            { middleLine.Properties with DashPattern = DashPattern.Empty }
                                        )
                                        |> ignore

                                addEdgeSolidPath()

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






        static member ScaleContentsTo(pageSelector, fRect) =
            ModifyPage.Create(
                "scale Contents",
                pageSelector,
                Dummy,
                PageModifier.ScaleContentsTo(fRect),
                parameters = [
                    "fRect" => fRect.ToString()
                ]
            )  ||>> ignore


        static member ClippingContentsToPageBox(pageBoxKind: PageBoxKind, ?margin, ?pageSelector) =
            let margin = defaultArg margin (Margin.Create 0.)
            ModifyPage.Create(
                "clipping conetents to " + pageBoxKind.Text(),
                defaultArg pageSelector PageSelector.All,
                Dummy,
                PageModifier.ClippingContentsToPageBox(pageBoxKind, margin = margin),
                parameters = [
                    "margin" => margin.LoggingText
                ]
            )  ||>> ignore


        static member AddRectangleToCanvasRootArea(canvasAreaOptions, ?fArgs, ?pageSelector) =
            let fArgs = defaultArg fArgs id
            ModifyPage.Create(
                "add rectangle to canvas root area",
                defaultArg pageSelector PageSelector.All,
                Dummy,
                PageModifier.AddRectangleToCanvasRootArea(canvasAreaOptions, fArgs)
            )  ||>> ignore
            |> Manipulate.rename 
                "add rectangle to canvas root area"
                [
                    "canvasAreaOptions" => canvasAreaOptions.ToString()
                ]

        static member AddMarks(canvasAreaOptions, marks: MarkAddingElement list, ?pageSelector) =
            ModifyPage.Create(
                "add marks",
                defaultArg pageSelector PageSelector.All,
                Dummy,
                (
                    marks
                    |> List.map(fun mark -> 
                        PageModifier.AddMarks(canvasAreaOptions, marks)
                    )
                    |> PageModifier.Batch
                )

            )  ||>> ignore
            |> Manipulate.rename 
                "add marks"
                [
                    "canvasAreaOptions" => canvasAreaOptions.ToString()
                    "marks"             => marks.ToString()
                ]

        static member AddEdgeCropMarks(?canvasAreaOptions, ?cropMark, ?mapping, ?pageSelector) =
            let cropMark = defaultArg cropMark EdgeCropMark.DefaultValue
            let canvasAreaOptions = defaultArg canvasAreaOptions (AreaGettingOptions.PageBox PageBoxKind.ActualBox)
            ModifyPage.Create(
                "add Edge CropMarks",
                defaultArg pageSelector PageSelector.All,
                Dummy,
                (
                    PageModifier.AddEdgeCropMarks(canvasAreaOptions, cropMark, defaultArg mapping id)
                )

            )  ||>> ignore
            |> Manipulate.rename 
                "add edge cropMarks"
                [
                    "canvasAreaOptions" => canvasAreaOptions.ToString()
                    "cropMark"             => cropMark.ToString()
                ]
