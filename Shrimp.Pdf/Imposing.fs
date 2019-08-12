namespace Shrimp.Pdf
open iText.Kernel.Pdf
open System
open Fake.IO
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Geom
open iText.Kernel.Pdf.Xobject
open iText.Kernel.Pdf.Canvas
open System.IO
open iText.Layout
open System.Collections.Generic
open iText.Kernel.Font
open iText.Layout.Element
open Shrimp.Pdf.Extensions
open iText.Kernel.Pdf.Canvas.Parser.Data
open System.Collections.Concurrent
open iText.Kernel.Colors
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





    /// HFilp : Horizontal flip
    /// VFile: Vertical flip
    /// FB: Front and Backgroup
    [<RequireQualifiedAccess>]
    type FlipWay =
        | HFlip
        | VFlip
        | FB



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

            | _ -> failwith "Invalid token"

        member x.Value = args

        member x.FillingMode = fillingMode

        static member Create (f) = new ImposingArguments(f)


    type ImposingCell = 
        { Page: PdfPage
          Size: FsSize
          UseBleed: bool
          Cropmark: Cropmark option
          X: float
          Y: float
          Rotation: Rotation }


    [<RequireQualifiedAccess>]
    module ImposingCell =
        let internal addToPdfCanvas (splitDocument: SplitDocument) (pdfCanvas: PdfCanvas) (cell: ImposingCell) =
            let xObject = cell.Page.CopyAsFormXObject(splitDocument.Writer)
            let xObjectContentBox = 
                if cell.UseBleed
                then 
                    match PdfFormXObject.tryGetTrimBox xObject with 
                    | Some trimBox -> trimBox
                    | None -> cell.Page.GetActualBox()
                else cell.Page.GetActualBox()

            let affineTransfromRecord: AffineTransformRecord =
                { ScaleX = cell.Size.Width / xObjectContentBox.GetWidthF() 
                  ScaleY = cell.Size.Height / xObjectContentBox.GetHeightF() 
                  TranslateX = 0.
                  TranslateY = 0. 
                  ShearX = 0. 
                  ShearY = 0. }

            pdfCanvas.AddXObject(xObject, affineTransfromRecord)


    type ImposingRow(sheet: ImposingSheet) = 
        let cells = ResizeArray<ImposingCell>()
        let mutable x = 0.

        member private x.SplitDocument : SplitDocument = sheet.ImposingDocument.SplitDocument

        member private x.ImposingArguments : ImposingArguments  = sheet.ImposingDocument.ImposingArguments

        member internal x.AddToCanvas (pdfCanvas: PdfCanvas) = 
            (pdfCanvas, cells)
            ||> Seq.fold (ImposingCell.addToPdfCanvas x.SplitDocument)


        member internal this.Push(readerPage: PdfPage) =
            let args = this.ImposingArguments.Value
            let fillingMode = this.ImposingArguments.FillingMode

            let newCell =
                { Page = readerPage 
                  Size = match args.DesiredSizeOp with 
                          | Some size -> size 
                          | None -> FsSize.ofRectangle(readerPage.GetActualBox()) 

                  UseBleed = args.UseBleed 
                  Cropmark = args.Cropmark 
                  X = x
                  Y = sheet.Y
                  Rotation = args.XObjectRotation }

                  
            let addNewCell_UpdateState() =
                x <- x + newCell.Size.Width + args.HSpaces.[cells.Count % args.HSpaces.Length]
                cells.Add(newCell)
                true

            match fillingMode with 
            | FillingMode.Numeric numericFillingMode ->
                if cells.Count = numericFillingMode.ColNums.[sheet.RowsCount] then
                    false
                else
                    addNewCell_UpdateState()

            | FillingMode.Automatic automaticFillingMode ->
                let pageSize = Background.getPageSize automaticFillingMode.Background
                if x + newCell.Size.Width + args.Margin.Left + args.Margin.Right > pageSize.Width then 
                    false
                else 
                    addNewCell_UpdateState()


            | _ -> failwith "Not implemented"

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

        member this.GetClonedCells() =
            cells 
            |> List.ofSeq



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

            let overflowResult =
                match fillingMode with 
                | FillingMode.Numeric numericFillingMode ->
                    if rows.Count = numericFillingMode.RowNum 
                    then Result.Error ([])
                    else Result.Ok ()

                | FillingMode.Automatic automaticFillingMode ->
                    let pageSize = Background.getPageSize automaticFillingMode.Background

                    match Seq.tryLast rows with 
                    | Some row ->
                        let lastRowHeight = row.Height

                        if lastRowHeight + y > pageSize.Width 
                        then 
                            rows.Remove(row)
                            |> ignore
                            Result.Error (row.GetClonedCells())
                        else Result.Ok ()

                    | None -> Result.Ok()

                | _ -> failwith "Not implemented"

            match overflowResult with 
            | Result.Error _ -> overflowResult
            | Result.Ok () ->
                let addNewRow_UpdateState_PushAgain() =
                    match Seq.tryLast rows with 
                    | None -> ()
                    | Some lastRow -> 
                        y <- lastRow.Height + y + args.VSpaces.[rows.Count % args.VSpaces.Length]

                    let newRow = new ImposingRow(x)
                    rows.Add(newRow)
                    x.Push(readerPage)

                if rows.Count = 0 
                then addNewRow_UpdateState_PushAgain()
                else 
                    let row = rows.Last()
                    if row.Push(readerPage)
                    then Result.Ok()
                    else addNewRow_UpdateState_PushAgain()


        member internal x.Draw() =
            let newPage = x.SplitDocument.Writer.AddNewPage()
            
            let pdfCanvas = 
                PdfCanvas(newPage)

            (pdfCanvas, rows)
            ||> Seq.fold (fun pdfCanvas row -> 
                row.AddToCanvas(pdfCanvas)
            )
            |> ignore


        member x.Width =
            let isSheetTrimmed = 
                match x.ImposingArguments.Value.BackgroundOp with 
                | None -> true
                | Some backgrounp ->
                    match backgrounp.TableAlignment with 
                    | TableAlignment.Trimmed -> true
                    | _ -> false


        member x.Height =
            match Seq.tryLast rows with 
            | Some row -> row.Height 
            | None -> 0.




        member x.RowsCount = rows.Count 

        member x.Margin = x.ImposingArguments.Value.Margin

        member x.BackgroupOp = x.ImposingArguments.Value.BackgroundOp



    and ImposingDocument (splitDocument: SplitDocument, imposingArguments: ImposingArguments) =  
      // The main constructor is 'private' and so users do not see it,
      // it takes columns and calculates the maximal column length
        let sheets = new ResizeArray<ImposingSheet>()
            
        member x.SplitDocument: SplitDocument = splitDocument

        member x.ImposingArguments: ImposingArguments = imposingArguments

        member x.Draw() =
            if sheets.Count = 0 then failwith "cannot draw documents, tables is empty"

            for sheet in sheets do
                sheet.Draw()

        member x.Build() =
            sheets.Clear()

            sheets.Add(new ImposingSheet(x))

            let reader = splitDocument.Reader

            for i = 1 to reader.GetNumberOfPages() do
                let rec loop readerPages = 
                    for readerPage in readerPages do 
                        match sheets.Last().Push(readerPage) with 
                        | Result.Ok _ -> ()
                        | Result.Error cells ->
                            cells 
                            |> List.map (fun cell -> cell.Page)
                            |> loop

                let readerPage = reader.GetPage(i)
                loop [readerPage]


