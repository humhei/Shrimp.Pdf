
namespace Shrimp.Pdf
open iText.Kernel.Pdf
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf.Extensions
open System.Linq
open System

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
        | Size of FsSize
        | File of string

    [<RequireQualifiedAccess>]
    module Background =
        let getSize (backgrounp: Background) =
            match backgrounp with 
            | Background.Size pageSize -> pageSize
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


    type PageOrientation =
        | Landscape  = 0
        | Portrait = 1

    type DesiredPageOrientation =
        | Landscape  = 0
        | Portrait = 1
        | Automatic = 2


    type FsPageSize = private FsPageSize of size: FsSize * orientation: PageOrientation
    with 
        member x.PageOrientation = 
            let (FsPageSize (size, pageOrientation)) = x
            pageOrientation

        member x.Size = 
            let (FsPageSize (size, pageOrientation)) = x
            size

        member x.Width = x.Size.Width

        member x.Height = x.Size.Height


    [<RequireQualifiedAccess>]
    module FsPageSize =
        let create (size: FsSize) pageOrientation =
            match pageOrientation with 
            | PageOrientation.Landscape -> 
                FsPageSize (FsSize.landscape size, pageOrientation)
            | PageOrientation.Portrait ->
                FsPageSize (FsSize.portrait size, pageOrientation)
            | _ -> failwith "Invalid token"




    [<RequireQualifiedAccess>]
    module DesiredPageOrientation =

        let (|PageOrientation|_|) = function
            | DesiredPageOrientation.Landscape -> Some PageOrientation.Landscape
            | DesiredPageOrientation.Portrait -> Some PageOrientation.Portrait
            | DesiredPageOrientation.Automatic -> None
            | _ -> failwith "Invalid token"

    [<RequireQualifiedAccess>]
    type CellRotation =
        | None
        | R180WhenColNumIsEven 
        | R180WhenRowNumIsEven 

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
            DesiredPageOrientation: DesiredPageOrientation
            Sheet_PlaceTable: Sheet_PlaceTable
            DesiredSizeOp: FsSize option
            CellRotation: CellRotation
            IsRepeated: bool
        }

    with 
        static member DefaultValue =
            {
                ColNums = [0]
                RowNum = 0
                Cropmark = None
                HSpaces = [0.]
                VSpaces = [0.]
                Margin = Margin.Create(0.)
                UseBleed = false
                Background = Background.Size FsSize.A4
                Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable
                DesiredSizeOp = None
                IsRepeated = false
                CellRotation = CellRotation.None
                DesiredPageOrientation = DesiredPageOrientation.Automatic
            }

    type ImposingArguments = private ImposingArguments of fillingMode: FillingMode * value: _ImposingArguments
    with 
        member x.Value =
            let (ImposingArguments (fillingMode, value)) = x
            value

        member x.FillingMode = 
            let (ImposingArguments (fillingMode, value)) = x
            fillingMode

        static member Create(mapping: _ImposingArguments -> _ImposingArguments) = 
            let args = mapping _ImposingArguments.DefaultValue

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

            ImposingArguments(fillingMode, args)


    /// coordinate origin is left top of table
    /// y: top -> bottom --------0 -> tableHeight
    type ImposingCell = 
        { Page: PdfPage
          Size: FsSize
          X: float
          Y: float
          RowIndex: int
          ImposingRow: ImposingRow }

    with 
        member private x.ImposingSheet: ImposingSheet = x.ImposingRow.ImposingSheet

        member private x.ImposingDocument: ImposingDocument = x.ImposingSheet.ImposingDocument

        member private x.SplitDocument = x.ImposingDocument.SplitDocument


        member x.ImposingArguments: ImposingArguments = x.ImposingDocument.ImposingArguments
        
        member x.HSpaces = x.ImposingArguments.Value.HSpaces

        member x.VSpaces = x.ImposingArguments.Value.VSpaces

        member x.Cropmark = x.ImposingArguments.Value.Cropmark

        member x.UseBleed = x.ImposingArguments.Value.UseBleed

        member private cell.GetXObjectContentBox() =
            let xObjectContentBox = 
                if cell.UseBleed
                then PdfPage.getTrimBox cell.Page
                else cell.Page.GetActualBox()

            xObjectContentBox

        member private cell.GetScale() =
            let xObjectContentBox = cell.GetXObjectContentBox()
                

            let scaleX = cell.Size.Width / xObjectContentBox.GetWidthF() 
            let scaleY = cell.Size.Height / xObjectContentBox.GetHeightF() 
            scaleX, scaleY

        member private cell.GetClippedXObject() =
            let scaleX, scaleY = cell.GetScale()

            let xobject = cell.Page.CopyAsFormXObject(cell.SplitDocument.Writer)
            if cell.UseBleed
            then 
                let trimBox = PdfPage.getTrimBox cell.Page
                        
                let actualBox = cell.Page.GetActualBox()

                if Rectangle.equal trimBox actualBox then xobject
                else 
                    let (|ColumnFirst|ColumnLast|ColumnMiddle|ColumnFirstAndLast|) (cell: ImposingCell) =
                        let cells = cell.ImposingRow.Cells
                        let index = cell.ImposingRow.Cells.IndexOf(cell)
                        if cells.Count = 1 then ColumnFirstAndLast
                        else
                            match index with 
                            | 0 -> ColumnFirst
                            | index when index = cells.Count - 1 -> ColumnLast
                            | _ -> ColumnMiddle 

                    let (|RowFirst|RowLast|RowMiddle|RowFirstAndLast|) (row: ImposingRow) =
                        let rows = row.ImposingSheet.Rows
                        let index = rows.IndexOf(row)
                        if rows.Count = 1 then RowFirstAndLast
                        else
                            match  index with 
                            | 0 -> RowFirst
                            | index when index = rows.Count - 1 -> RowLast
                            | _ -> RowMiddle
                    let bbox = 
                        let x, width =

                            let index = cell.ImposingRow.Cells.IndexOf(cell)
                            match cell with 
                            | ColumnFirst ->
                                let x = actualBox.GetXF()
                                
                                let width = 
                                    let nextHSpace = cell.HSpaces.[(index) % cell.HSpaces.Length] / scaleX
                                    trimBox.GetRightF() - actualBox.GetLeftF() + nextHSpace / 2.

                                x, width

                            | ColumnFirstAndLast ->  actualBox.GetXF() ,actualBox.GetWidthF()

                            | ColumnMiddle ->
                                let preHSpace = cell.HSpaces.[(index - 1) % cell.HSpaces.Length] / scaleX
                                let nextHSpace = cell.HSpaces.[(index) % cell.HSpaces.Length] / scaleX

                                let x = trimBox.GetXF() - preHSpace / 2.
                                let width = trimBox.GetWidthF() + nextHSpace / 2. + preHSpace / 2.
                                x, width

                            | ColumnLast ->
                                let preHSpace = cell.HSpaces.[(index - 1) % cell.HSpaces.Length] / scaleX

                                let x = trimBox.GetXF() - preHSpace / 2.

                                let width = trimBox.GetWidthF() + preHSpace / 2.

                                x,width

                        let y, height = 
                            let row = cell.ImposingRow
                            let rows = row.ImposingSheet.Rows
                            let index = rows.IndexOf(row)

                            match row with 
                            | RowFirst ->
                                let nextVSpace = cell.VSpaces.[(index) % cell.VSpaces.Length] / scaleY

                                let y = trimBox.GetYF() - nextVSpace / 2.

                                let height = actualBox.GetTopF() - trimBox.GetBottomF() + nextVSpace / 2.
                                y, height

                            | RowMiddle ->
                                let preVSpace = cell.VSpaces.[(index - 1) % cell.VSpaces.Length] / scaleY

                                let nextVSpace = cell.VSpaces.[(index) % cell.VSpaces.Length] / scaleY

                                let y = trimBox.GetYF() - nextVSpace / 2.
                                let height = trimBox.GetHeightF() + nextVSpace / 2. + preVSpace / 2.
                                y, height

                            | RowFirstAndLast -> actualBox.GetYF(), actualBox.GetHeightF()
                            | RowLast -> 
                                let y = actualBox.GetYF()

                                let preVSpace = cell.VSpaces.[(index - 1) % cell.VSpaces.Length] / scaleY

                                let height = trimBox.GetTopF() - actualBox.GetBottomF() + preVSpace / 2.

                                y, height

                        Rectangle.create x y width height

                    let resetBBoxWhenTrimBoxIsNotInsideActualBox (bbox: Rectangle) =
                        let x = max (actualBox.GetXF()) (bbox.GetXF())
                        let width = min (actualBox.GetWidthF()) (bbox.GetWidthF())

                        let y = max (actualBox.GetYF()) (bbox.GetYF())
                        let height = min (actualBox.GetHeightF()) (bbox.GetHeightF())

                        Rectangle.create x y width height

                    let bbox = resetBBoxWhenTrimBoxIsNotInsideActualBox bbox

                    xobject.SetBBox(Rectangle.toPdfArray bbox) 

            else xobject

        static member AddToPdfCanvas (cellContentAreas_ExtendByCropmarkDistance: Rectangle list) (pdfCanvas: PdfCanvas) (cell: ImposingCell) =
            let addXObject (pdfCanvas: PdfCanvas) = 
                let xObject = cell.GetClippedXObject()
                let xObjectContentBox = cell.GetXObjectContentBox()
                let scaleX, scaleY = cell.GetScale()

                let affineTransform_Rotate =
                    match cell.ImposingArguments.Value.CellRotation with 
                    | CellRotation.None -> AffineTransform.GetTranslateInstance(0., 0.)
                    | CellRotation.R180WhenColNumIsEven ->
                        let index = cell.RowIndex
                        if (index + 1) % 2 = 0 
                        then AffineTransform.GetRotateInstance(- Math.PI / 180. * 180., xObjectContentBox.GetXF() + cell.Size.Width / 2., xObjectContentBox.GetYF() + cell.Size.Height / 2.  )
                        else AffineTransform.GetTranslateInstance(0., 0.)
                    | CellRotation.R180WhenRowNumIsEven ->
                        let index = cell.ImposingRow.RowIndex
                        if (index + 1) % 2 = 0 
                        then AffineTransform.GetRotateInstance(- Math.PI / 180. * 180., xObjectContentBox.GetXF() + cell.Size.Width / 2., xObjectContentBox.GetYF() + cell.Size.Height / 2.  )
                        else AffineTransform.GetTranslateInstance(0., 0.)

                let affineTransform_Scale =
                    AffineTransform.GetScaleInstance(scaleX, scaleY)


                let affineTransform_Translate =
                    AffineTransform.GetTranslateInstance(cell.X - xObjectContentBox.GetXF(), -(cell.Size.Height + cell.Y) - xObjectContentBox.GetYF())

                let affineTransform = affineTransform_Translate.Clone()
                affineTransform.Concatenate(affineTransform_Rotate)
                affineTransform.Concatenate(affineTransform_Scale)
                //affineTransform.Concatenate(affineTransform_Translate)

                pdfCanvas.AddXObject(xObject, affineTransform)

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

    and ImposingRow(sheet: ImposingSheet, rowIndex: int) = 
        let cells = ResizeArray<ImposingCell>()
        let mutable x = 0.

        member private x.ImposingArguments : ImposingArguments  = sheet.ImposingDocument.ImposingArguments

        member internal x.ImposingSheet = sheet

        member internal x.RowIndex = rowIndex

        member private x.PageSize = sheet.PageSize

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
                          | None -> 
                                if args.UseBleed then FsSize.ofRectangle(readerPage.GetTrimBox())
                                else FsSize.ofRectangle(readerPage.GetActualBox()) 
                  X = x
                  Y = sheet.Y
                  RowIndex = cells.Count
                  ImposingRow = this }

            if newCell.Size.Width > this.PageSize.Width || newCell.Size.Height > this.PageSize.Height 
            then failwithf "desired size is exceeded %A to sheet page size %A" newCell.Size this.PageSize
                  
            let addNewCell_UpdateState() =
                x <- x + newCell.Size.Width + args.HSpaces.[cells.Count % args.HSpaces.Length]
                cells.Add(newCell)
                true


            let willWidthExceedPageWidth = 
                let pageSize = this.PageSize
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

        member internal this.Cells: ResizeArray<ImposingCell> = cells

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

        member this.Y =
            match Seq.tryHead cells with 
            | Some cell -> cell.Y
            | None -> 0.

        member x.GetCells() = List.ofSeq cells


    and ImposingSheet(imposingDocument: ImposingDocument, pageOrientation: PageOrientation) =
        
        let pageSize = 
            let size = Background.getSize imposingDocument.ImposingArguments.Value.Background
            FsPageSize.create (size) pageOrientation

        let rows = ResizeArray<ImposingRow>()

        let mutable y = 0.

        member private x.SplitDocument : SplitDocument = imposingDocument.SplitDocument

        member x.ImposingArguments : ImposingArguments  = imposingDocument.ImposingArguments

        member internal x.PageSize: FsPageSize = pageSize

        member internal x.ImposingDocument: ImposingDocument = imposingDocument

        member internal x.Y = y

        member internal x.Push(readerPage: PdfPage) =

            let args = x.ImposingArguments.Value
            let fillingMode = x.ImposingArguments.FillingMode

            let addNewRow_UpdateState_PushAgain() =
                match Seq.tryLast rows with 
                | None -> ()
                | Some lastRow -> 
                    if lastRow.Cells.Count = 0 then
                        failwithf "Cannot push page to imposing sheet,please check your imposing arguments %A" x.ImposingArguments
                    y <- lastRow.Height + y + args.VSpaces.[(rows.Count - 1) % args.VSpaces.Length]

                let newRow = new ImposingRow(x, rows.Count)
                rows.Add(newRow)
                x.Push(readerPage)


            if rows.Count = 0 
            then addNewRow_UpdateState_PushAgain()
            else 
                let row = rows.Last()
                if row.Push(readerPage)
                then 
                    let removeLastRow_UpdateState() =
                        rows.RemoveAt(rows.Count - 1)
                        |> ignore
                                
                        let newLastRow = rows.[rows.Count - 1]
                        y <- newLastRow.Cells.[0].Y

                    let heightExeedPageHeight =
                        match Seq.tryLast rows with 
                        | Some row -> row.Height + y + args.Margin.Top + args.Margin.Bottom > pageSize.Height

                        | None -> false

                    let rowNumExceedSpecificRowNumResult = 
                        match fillingMode with 
                        | FillingMode.RowNumSpecific rowNum -> rows.Count > rowNum 
                        | _ -> false

                    if heightExeedPageHeight 
                    then
                        let lastRow = rows.Last()
                        let cells = lastRow.Cells

                        if cells.Count = 1 
                        then
                            cells.RemoveAt(cells.Count - 1)
                        removeLastRow_UpdateState()

                        false
                    elif rowNumExceedSpecificRowNumResult 
                    then
                        removeLastRow_UpdateState()
                        false
                    else true
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
                pageSize.Height

        member x.Height = 
            let margin = x.ImposingArguments.Value.Margin
            x.TableHeight + margin.Bottom + margin.Top

        member internal x.Rows: ResizeArray<ImposingRow> = rows
        
        member x.GetRows() = List.ofSeq rows

        member x.RowsCount = rows.Count 

        member x.GetCellsCount() = rows |> Seq.sumBy(fun row -> row.Cells.Count)

    /// Build() -> Draw()
    and ImposingDocument (splitDocument: SplitDocument, imposingArguments: ImposingArguments) =  
        let sheets = new ResizeArray<ImposingSheet>()
            
        member x.SplitDocument: SplitDocument = splitDocument

        member x.ImposingArguments: ImposingArguments = imposingArguments

        member x.Draw() =
            if sheets.Count = 0 then failwith "cannot draw documents, sheets is empty, please invoke Build() first"

            for sheet in sheets do
                sheet.Draw()

        member x.Build() =
            let args = x.ImposingArguments.Value
            sheets.Clear()

            let reader = splitDocument.Reader

            let readerPages = PdfDocument.getPages reader

            let rec produceSheet readerPages (sheet: ImposingSheet) =
                match readerPages with 
                | (readerPage : PdfPage) :: t ->

                    if sheet.Push(readerPage) 
                    then 
                        if args.IsRepeated 
                        then produceSheet readerPages sheet
                        else produceSheet t sheet
                    else 

                        match args.IsRepeated, t.Length with 
                        | true, i when i > 0 ->
                            sheet, t

                        | true, 0 -> sheet, []

                        | false, _ -> 
                            sheet, readerPages

                        | _ -> failwith "Invalid token"
                | [] -> sheet, []


            let rec produceSheets (readerPages: PdfPage list) (sheets: ImposingSheet list) =
                if readerPages.Length = 0 then sheets
                else
                    match args.DesiredPageOrientation with 
                    | DesiredPageOrientation.PageOrientation pageOrientation ->
                        let producedSheet, leftPages = 
                            produceSheet readerPages (new ImposingSheet(x, pageOrientation))
                        produceSheets leftPages (producedSheet :: sheets)

                    | DesiredPageOrientation.Automatic ->
                        let producedSheet, leftPages = 
                            let producedSheet1, leftPages1 = 
                                produceSheet readerPages (new ImposingSheet(x, PageOrientation.Landscape))

                            let producedSheet2, leftPages2 = 
                                produceSheet readerPages (new ImposingSheet(x, PageOrientation.Portrait))
                        
                            if 
                                (args.IsRepeated && producedSheet1.GetCellsCount() > producedSheet2.GetCellsCount())
                                || (not args.IsRepeated && leftPages1.Length < leftPages2.Length)
                            then producedSheet1, leftPages1
                            else producedSheet2, leftPages2

                        produceSheets leftPages (producedSheet :: sheets)

                    | _ -> failwith "Invalid token"


            let producedSheets = produceSheets readerPages []

            sheets.AddRange(List.rev producedSheets)

        member x.GetFirstCell() = sheets.[0].Rows.[0].Cells.[0]

        member x.GetFirstSheet() = sheets.[0]

        member x.GetFirstRow() = sheets.[0].Rows.[0]

        member x.GetSheets() = List.ofSeq sheets

        member x.GetSheet(index) = sheets.[index]
