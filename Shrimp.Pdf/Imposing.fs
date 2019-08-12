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
    type BackgroundKind =
        | PageSize of FsPageSize
        | File of string

    [<RequireQualifiedAccess>]
    type TableAlignment =
        | Trimmed
        | Position of Position

    [<RequireQualifiedAccess>]
    type Background =
        { TableAlignment: TableAlignment
          Kind: BackgroundKind }

    type NumericFillingMode =
        { ColNums: int list 
          RowNum: int 
          BackgroundOp: Background option }

    type AutomaticFillingMode =
        { Background: Background }

    type ColumnAutomaticFillingMode =
        { BackgroundOp: Background option
          RowNum: int }

    type RowAutomaticFillingMode =
        { BackgroundOp: Background option
          ColNums: int list }

    [<RequireQualifiedAccess>]
    type FillingMode =
        | Numeric of NumericFillingMode
        | Automatic of AutomaticFillingMode
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
            BackgroundOp: Background option
            XObjectRotation: Rotation
            DesiredSizeOp: FsSize option
            IsRepeated: bool
        }

    type ImposingArguments private (f: _ImposingArguments -> _ImposingArguments) as this =
        member val Value = 
            {
                ColNums = [0]
                RowNum = 0
                Cropmark = None
                HSpaces = [0.]
                VSpaces = [0.]
                Margin = Margin.Create(0.)
                UseBleed = false
                XObjectRotation = Rotation.None
                BackgroundOp = 
                    Some 
                        { Kind = BackgroundKind.PageSize FsPageSize.A4
                          TableAlignment = TableAlignment.Trimmed }

                DesiredSizeOp = None
                IsRepeated = false
            } |> f

        member val FillingMode = 
            let args = this.Value
            match List.distinct args.ColNums, args.RowNum, args.BackgroundOp with
            | [0], 0, Some background ->
                FillingMode.Automatic { Background = background }

            | uniqueValues, 0, backgroupOp when List.exists (fun m -> m > 0) uniqueValues ->
                FillingMode.RowAutomatic { ColNums =  args.ColNums; BackgroundOp = backgroupOp }

            | [0], rowNum, backgroupOp when rowNum > 0 ->
                FillingMode.ColumnAutomatic { RowNum =  args.RowNum; BackgroundOp = backgroupOp }

            | uniqueValues, rowNum, backgroupOp when List.exists (fun m -> m > 0) uniqueValues && rowNum > 0 ->
                FillingMode.Numeric { ColNums = args.ColNums; RowNum = rowNum; BackgroundOp = backgroupOp }

            | [0], 0, None ->
                invalidArg "args" (sprintf "Cannot convert args %A to automatic filling mode while backgroup is empty" args)

            | _ -> failwith "Invalid token"


        static member Create (f) = new ImposingArguments(f)




    [<RequireQualifiedAccess>]
    type ImposingPosition =
        | FirstANDLast = 0
        | First = 1
        | Last = 2
        | Middle = 3

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
        let internal addToPdfCanvas (splitDocument: SplitDocument) (pdfCanvas: PdfCanvas)  (cell: ImposingCell) =
            let xObject = cell.Page.CopyAsFormXObject(splitDocument.Writer)
            pdfCanvas.AddXObject(xObject, Rectangle.create cell.X cell.Y cell.Size.Width cell.Size.Height)

    type ImposingRow(table: ImposingTable) = 
        let cells = ResizeArray<ImposingCell>()

        let mutable x = 0.

        member private x.SplitDocument : SplitDocument = table.ImposingDocument.SplitDocument

        member private x.ImposingArguments : ImposingArguments  = table.ImposingDocument.ImposingArguments

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
                  Y = table.Y
                  Rotation = args.XObjectRotation }

            match fillingMode with 
            | FillingMode.Numeric numericFillingMode ->
                if cells.Count = numericFillingMode.ColNums.[table.RowsCount] then
                    false
                else
                    x <- x + newCell.Size.Width + args.HSpaces.[cells.Count % args.HSpaces.Length]
                    cells.Add(newCell)
                    true

            | _ -> failwith "Not implemented"

        member this.GetHeight() = 
            cells
            |> Seq.map (fun cell -> cell.Size.Height)
            |> Seq.max


    and ImposingTable(imposingDocument: ImposingDocument) =
        let rows = ResizeArray<ImposingRow>()

        let mutable y = 0.

        member private x.SplitDocument : SplitDocument = imposingDocument.SplitDocument

        member private x.ImposingArguments : ImposingArguments  = imposingDocument.ImposingArguments

        member internal x.ImposingDocument: ImposingDocument = imposingDocument

        member internal x.Y = y

        member internal x.Push(readerPage: PdfPage) =
            let args = x.ImposingArguments.Value
            let fillingMode = x.ImposingArguments.FillingMode

            match fillingMode with 
            | FillingMode.Numeric numericFillingMode ->

                let addNewRowAndPushAgain() =
                    match Seq.tryLast rows with 
                    | None -> ()
                    | Some lastRow -> 
                        y <- lastRow.GetHeight() + y + args.VSpaces.[rows.Count % args.VSpaces.Length]

                    let newRow = new ImposingRow(x)
                    rows.Add(newRow)
                    x.Push(readerPage)

                if rows.Count = numericFillingMode.RowNum 
                then false
                elif rows.Count = 0 
                then addNewRowAndPushAgain()
                else 
                    let row = rows.Last()
                    if row.Push(readerPage)
                    then true
                    else addNewRowAndPushAgain()
            | _ -> failwith "Not implemented"


            //rows.Add(new ImposingRow(x))

        member internal x.Draw() =
            let pdfCanvas = 
                let newPage = x.SplitDocument.Writer.AddNewPage()
                PdfCanvas(newPage)

            (pdfCanvas, rows)
            ||> Seq.fold (fun pdfCanvas row -> 
                row.AddToCanvas(pdfCanvas)
            )
            |> ignore

        member x.RowsCount = rows.Count 

        member x.Margin = x.ImposingArguments.Value.Margin

        member x.BackgroupOp = x.ImposingArguments.Value.BackgroundOp



    /// Build -> Draw -> Close
    and ImposingDocument(splitDocument: SplitDocument, imposingArguments: ImposingArguments) =
        let tables = new ResizeArray<ImposingTable>()
            
        member x.SplitDocument = splitDocument

        member x.ImposingArguments = imposingArguments

        member x.Draw() =
            if tables.Count = 0 then failwith "cannot draw documents, tables is empty"

            for table in tables do
                table.Draw()

        member x.Build() =
            tables.Clear()

            tables.Add(new ImposingTable(x))

            let reader = splitDocument.Reader

            for i = 1 to reader.GetNumberOfPages() do
                let readerPage = reader.GetPage(i)
                match tables.Last().Push(readerPage) with 
                | true -> ()
                | false ->
                    tables.Add(new ImposingTable(x))
                    match tables.Last().Push(readerPage) with 
                    | true -> ()
                    | false -> failwith "a new table cannot push readerPage"
   