namespace Shrimp.Pdf
open iText.Kernel.Pdf
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf.Extensions
open System.Linq

module Imposing =

    type Cropmark = 
        { Length: float
          Distance: float
          Width: float }

    [<RequireQualifiedAccess>]
    module Cropmark =
        let defaultValue = 
            { Length = mm 3.8
              Distance = mm 3.2
              Width = mm 0.2 }

    [<RequireQualifiedAccess>]
    type Sheet_PlaceTable =
        | Trim_CenterTable
        | At of Position

    [<RequireQualifiedAccess>]
    type Background =
        | PageSize of FsPageSize
        | File of string

    [<RequireQualifiedAccess>]
    module Background =
        let getPageSize (backgrounp: Background) =
            match backgrounp with 
            | Background.PageSize pageSize -> pageSize
            | _ -> failwith "Not implemented"

    type NumericFillingMode =
        { ColNums: int list 
          RowNum: int }


    type ColumnAutomaticFillingMode =
        { RowNum: int }

    type RowAutomaticFillingMode =
        { ColNums: int list }

    [<RequireQualifiedAccess>]
    type FillingMode =
        | Numeric of NumericFillingMode
        | Automatic
        | ColumnAutomatic of ColumnAutomaticFillingMode
        | RowAutomatic of RowAutomaticFillingMode


    [<RequireQualifiedAccess>]
    module FillingMode =
        let (|ColNumsSpecific|_|) = function
            | FillingMode.Numeric numericFillingMode ->
                Some numericFillingMode.ColNums
            | FillingMode.RowAutomatic rowAutomaticFillingMode -> Some rowAutomaticFillingMode.ColNums
            | _ -> None


        let (|RowNumSpecific|_|) = function
            | FillingMode.Numeric numericFillingMode ->
                Some numericFillingMode.RowNum
            | FillingMode.ColumnAutomatic columnAutomaticFillingMode -> Some columnAutomaticFillingMode.RowNum
            | _ -> None

    type _ImposingArguments =
        {
            ColNums: int list
            RowNum: int
            Cropmark: Cropmark option
            HSpaces: float list
            VSpaces: float list
            Margin: Margin
            UseBleed: bool
            Background: Background
            Sheet_PlaceTable: Sheet_PlaceTable
            XObjectRotation: Rotation
            DesiredSizeOp: FsSize option
            IsRepeated: bool
        }

    type ImposingArguments private (f: _ImposingArguments -> _ImposingArguments) =
        
        let args = 
            {
                ColNums = [0]
                RowNum = 0
                Cropmark = None
                HSpaces = [0.]
                VSpaces = [0.]
                Margin = Margin.Create(0.)
                UseBleed = false
                XObjectRotation = Rotation.None
                Background = Background.PageSize FsPageSize.A4
                Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable
                DesiredSizeOp = None
                IsRepeated = false
            } |> f
        
        let fillingMode =
            match List.distinct args.ColNums, args.RowNum with
            | [0], 0 ->
                FillingMode.Automatic

            | uniqueValues, 0 when List.exists (fun m -> m > 0) uniqueValues ->
                FillingMode.RowAutomatic { ColNums =  args.ColNums }

            | [0], rowNum when rowNum > 0 ->
                FillingMode.ColumnAutomatic { RowNum =  args.RowNum }

            | uniqueValues, rowNum when List.exists (fun m -> m > 0) uniqueValues && rowNum > 0 ->
                FillingMode.Numeric { ColNums = args.ColNums; RowNum = rowNum }

            | [], _ ->
                failwith "colNums cannot be empty"

            | _ -> failwith "Invalid token"

        member x.Value = args

        member x.FillingMode = fillingMode

        static member Create (f) = new ImposingArguments(f)

    /// coordinate origin is left top of table
    /// y: top -> bottom --------0 -> tableHeight
    type ImposingCell = 
        { Page: PdfPage
          Size: FsSize
          X: float
          Y: float
          ImposingRow: ImposingRow }

    with 
        member private x.ImposingSheet: ImposingSheet = x.ImposingRow.ImposingSheet

        member private x.ImposingDocument: ImposingDocument = x.ImposingSheet.ImposingDocument

        member private x.SplitDocument = x.ImposingDocument.SplitDocument

        member x.ImposingArguments: ImposingArguments = x.ImposingDocument.ImposingArguments
        
        member x.Cropmark = x.ImposingArguments.Value.Cropmark

        member x.UseBleed = x.ImposingArguments.Value.UseBleed

        member x.Rotation = x.ImposingArguments.Value.XObjectRotation


        static member AddToPdfCanvas (cellContentAreas_ExtendByCropmarkDistance: Rectangle list) (pdfCanvas: PdfCanvas) (cell: ImposingCell) =
            let addXObject (pdfCanvas: PdfCanvas) = 
                let xObject = cell.Page.CopyAsFormXObject(cell.SplitDocument.Writer)
                let xObjectContentBox = 
                    if cell.UseBleed
                    then 
                        match PdfFormXObject.tryGetTrimBox xObject with 
                        | Some trimBox -> trimBox
                        | None -> cell.Page.GetActualBox()
                    else cell.Page.GetActualBox()

                let scaleX = cell.Size.Width / xObjectContentBox.GetWidthF() 
                let scaleY = cell.Size.Height / xObjectContentBox.GetHeightF() 

                let affineTransfromRecord: AffineTransformRecord =
                    { ScaleX = scaleX
                      ScaleY = scaleY
                      TranslateX = cell.X - xObjectContentBox.GetXF() * scaleX
                      TranslateY = -(cell.Size.Height + cell.Y) - xObjectContentBox.GetYF() * scaleY
                      ShearX = 0. 
                      ShearY = 0. }

                pdfCanvas.AddXObject(xObject, affineTransfromRecord)

            let addCropmarks (pdfCanvas: PdfCanvas) =
                match cell.Cropmark with 
                | Some cropmark ->
                    let height = cell.Size.Height

                    let allCropmarkLines = 
                        let x = cell.X
                        let y = cell.Y
                        let distance = cropmark.Distance
                        let length = cropmark.Length
                        let width = cell.Size.Width
                        [
                            x, y-distance, x, y-distance-length                             ///left top VLine
                            x-distance, y, x-distance-length, y                             ///left top HLine
                            x-distance, y+height, x-distance-length, y+height               ///left bottom VLine
                            x, y+distance+height, x, y+distance+length+height               ///left bottom HLine
                            x+width, y-distance, x+width, y-distance-length                 ///right top VLine
                            x+distance+width, y, x+distance+length+width, y                 ///right top HLine
                            x+distance+width, y+height, x+distance+length+width, y+height   ///right bottom VLine     
                            x+width, y+distance+height, x+width, y+distance+length+height   ///right bottom HLine
                        ] |> List.map (fun (x1, y1, x2 , y2) ->
                            { Start = new Point(x1, y1); End = new Point(x2, y2) }
                        )
                    
                    let interspatialCropmarkLines = 
                        allCropmarkLines
                        |> List.filter (fun l -> List.forall (fun cropamrkRedArea -> l.IsOutsideOf(cropamrkRedArea)) cellContentAreas_ExtendByCropmarkDistance)
                    
                    (pdfCanvas, interspatialCropmarkLines)
                    ||> List.fold (fun (pdfCanvas: PdfCanvas) line ->
                        pdfCanvas
                            .MoveTo(line.Start.x, -(line.Start.y))
                            .LineTo(line.End.x, -(line.End.y))
                            .Stroke()
                    )

                | None -> pdfCanvas

            pdfCanvas
            |> addXObject
            |> addCropmarks

    and ImposingRow(sheet: ImposingSheet) = 
        let cells = ResizeArray<ImposingCell>()
        let mutable x = 0.

        member private x.ImposingArguments : ImposingArguments  = sheet.ImposingDocument.ImposingArguments

        member internal x.ImposingSheet = sheet

        member internal x.AddToCanvas cellContentAreas_ExtendByCropmarkDistance (pdfCanvas: PdfCanvas) = 
            (pdfCanvas, cells)
            ||> Seq.fold (ImposingCell.AddToPdfCanvas cellContentAreas_ExtendByCropmarkDistance)


        member internal this.Push(readerPage: PdfPage) =
            let args = this.ImposingArguments.Value
            let fillingMode = this.ImposingArguments.FillingMode

            let newCell =
                { Page = readerPage 
                  Size = match args.DesiredSizeOp with 
                          | Some size -> size 
                          | None -> FsSize.ofRectangle(readerPage.GetActualBox()) 
                  X = x
                  Y = sheet.Y
                  ImposingRow = this }

                  
            let addNewCell_UpdateState() =
                x <- x + newCell.Size.Width + args.HSpaces.[cells.Count % args.HSpaces.Length]
                cells.Add(newCell)
                true


            let willWidthExceedPageWidth = 
                let pageSize = Background.getPageSize args.Background
                newCell.X + newCell.Size.Width + args.Margin.Left + args.Margin.Right > pageSize.Width

            if willWidthExceedPageWidth then false
            else 
                match fillingMode with 
                | FillingMode.ColNumsSpecific colNums ->
                    if cells.Count = colNums.[(sheet.RowsCount - 1) % colNums.Length] then
                        false
                    else
                        addNewCell_UpdateState()

                | _ -> addNewCell_UpdateState()

        member internal this.Cells = cells

        member this.Height = 
            if cells.Count = 0 then 0.
            else
                cells
                |> Seq.map (fun cell -> cell.Size.Height)
                |> Seq.max


        member this.Width =
            match Seq.tryLast cells with 
            | Some cell -> cell.X + cell.Size.Width
            | None -> 0.



    and ImposingSheet(imposingDocument: ImposingDocument) =
        let rows = ResizeArray<ImposingRow>()

        let mutable y = 0.

        member private x.SplitDocument : SplitDocument = imposingDocument.SplitDocument

        member private x.ImposingArguments : ImposingArguments  = imposingDocument.ImposingArguments

        member internal x.ImposingDocument: ImposingDocument = imposingDocument

        member internal x.Y = y

        member internal x.Push(readerPage: PdfPage) =
            let args = x.ImposingArguments.Value
            let fillingMode = x.ImposingArguments.FillingMode

            let addNewRow_UpdateState_PushAgain() =
                match Seq.tryLast rows with 
                | None -> ()
                | Some lastRow -> 
                    y <- lastRow.Height + y + args.VSpaces.[(rows.Count - 1) % args.VSpaces.Length]

                let newRow = new ImposingRow(x)
                rows.Add(newRow)
                x.Push(readerPage)


            let willHeightExeedPageHeight =
                let pageSize = Background.getPageSize args.Background

                match Seq.tryLast rows with 
                | Some row -> row.Height + y > pageSize.Height

                | None -> false


            let willRowNumExceedSpecificRowNumResult = 
                match fillingMode with 
                | FillingMode.RowNumSpecific rowNum -> rows.Count - 1 = rowNum 
                | _ -> false


            let removeLastRow_UpdateState() =
                rows.RemoveAt(rows.Count - 1)
                |> ignore
                                
                let newLastRow = rows.[rows.Count - 1]
                y <- newLastRow.Cells.[0].Y

            if willHeightExeedPageHeight 
            then 
                let lastRow = rows.Last()
                let cells = lastRow.Cells

                if cells.Count = 1 
                then
                    cells.RemoveAt(cells.Count - 1)
                else
                    removeLastRow_UpdateState()

                false

            elif willRowNumExceedSpecificRowNumResult 
            then
                removeLastRow_UpdateState()

                false

            elif rows.Count = 0 
            then addNewRow_UpdateState_PushAgain()
            else 
                let row = rows.Last()
                if row.Push(readerPage)
                then true
                else addNewRow_UpdateState_PushAgain()




        member internal x.Draw() =
            let args = x.ImposingArguments.Value

            let newPage = x.SplitDocument.Writer.AddNewPage()
            
            let pdfCanvas = 
                PdfCanvas(newPage)

            let cellContentAreas_ExtendByCropmarkDistance = 
                match args.Cropmark with 
                | Some cropmark ->
                    let cells =
                        rows
                        |> Seq.collect (fun row -> row.Cells)
                    cells
                    |> List.ofSeq
                    |> List.map (fun cell ->
                        let rect = Rectangle.create cell.X cell.Y cell.Size.Width cell.Size.Height
                        Rectangle.applyMargin (Margin.Create (cropmark.Distance - tolerance)) rect
                    )
                | _ -> []

            (pdfCanvas, rows)
            ||> Seq.fold (fun pdfCanvas row -> 
                row.AddToCanvas cellContentAreas_ExtendByCropmarkDistance pdfCanvas
            )
            |> ignore

            let height = x.Height



            PdfPage.setPageBox (PageBoxKind.AllBox) (Rectangle.create -args.Margin.Left -(height - args.Margin.Top) x.Width height) newPage
            |> ignore


        member x.TableWidth = 
            let args = x.ImposingArguments.Value
            match args.Sheet_PlaceTable with
            | Sheet_PlaceTable.Trim_CenterTable ->
                if rows.Count = 0 then 0.
                else 
                    rows
                    |> Seq.map (fun row -> row.Width)
                    |> Seq.max

            | Sheet_PlaceTable.At _ ->
                let pageSize = Background.getPageSize args.Background
                pageSize.Width

        member x.Width = 
            let margin = x.ImposingArguments.Value.Margin
            margin.Left + margin.Right + x.TableWidth


        member x.TableHeight =
            let args = x.ImposingArguments.Value
            match args.Sheet_PlaceTable with
            | Sheet_PlaceTable.Trim_CenterTable ->
                match Seq.tryLast rows with 
                | Some row -> row.Height + x.Y
                | None -> 0.

            | Sheet_PlaceTable.At _ ->
                let pageSize = Background.getPageSize args.Background
                pageSize.Height

        member x.Height = 
            let margin = x.ImposingArguments.Value.Margin
            x.TableHeight + margin.Bottom + margin.Top

        member x.RowsCount = rows.Count 

    /// Build() -> Draw()
    and ImposingDocument (splitDocument: SplitDocument, imposingArguments: ImposingArguments) =  
        let sheets = new ResizeArray<ImposingSheet>()
            
        member x.SplitDocument: SplitDocument = splitDocument

        member x.ImposingArguments: ImposingArguments = imposingArguments

        member x.Draw() =
            if sheets.Count = 0 then failwith "cannot draw documents, sheets is empty"

            for sheet in sheets do
                sheet.Draw()

        member x.Build() =
            sheets.Clear()

            sheets.Add(new ImposingSheet(x))

            let reader = splitDocument.Reader

            for i = 1 to reader.GetNumberOfPages() do
                let readerPage = reader.GetPage(i)

                while (not (sheets.Last().Push(readerPage))) do 
                    sheets.Add(new ImposingSheet(x))



