namespace Shrimp.Pdf

open System.Collections.Generic

#nowarn "0104"
open Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Colors
open iText.Kernel.Font
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.Kernel.Colors
open iText.Kernel.Pdf.Canvas.Parser
open Shrimp.Pdf.DSL
open iText.Kernel.Geom
open System
open System.IO
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf
open Shrimp.FSharp.Plus
open Fake.IO
open Fake.IO.FileSystemOperators
open Shrimp.FSharp.Plus.Math
open iText.Layout



type RegularImposingSheet<'T> private (userState: 'T, imposingSheet: ImposingSheet) =
    let rows = imposingSheet.GetRows()
    let rowCount, columnCount =

        let columnCount =
            rows
            |> List.map (fun row -> row.GetCells().Length)
            |> List.distinct
            |> function
            | [ columnCount ] -> columnCount
            | columnsCount -> failwithf "Cannot create RegularImposingSheet when columnsCount are %A" columnsCount

        rows.Length, columnCount

    let cellSize =
        rows
        |> List.collect (fun row -> row.GetCells())
        |> List.map(fun m -> m.Size)
        |> List.distinct
        |> List.exactlyOne_DetailFailingText

    member x.UserState = userState

    member x.SetUserState(userState) = RegularImposingSheet(userState, imposingSheet)

    member x.RowCount = rowCount

    member x.ColumnCount = columnCount

    member x.ImposingSheet = imposingSheet

    member x.CellSize = cellSize

    member x.LandscapedSheetSize: FsSize = 
        { Width = imposingSheet.Width 
          Height = imposingSheet.Height }
        |> FsSize.landscape

    member x.RawImposingArguments = imposingSheet.ImposingArguments

    member x.ConvertedImposingArguments: _ImposingArguments =
        let rawArgs = x.RawImposingArguments
        { rawArgs.Value with 
            ColNums = [x.ColumnCount]
            RowNum = x.RowCount
            DesiredSizeOp = Some cellSize }

    member x.CellsCount = x.ColumnCount * x.RowCount

    static member Create(imposingSheet: ImposingSheet) = RegularImposingSheet<unit>((), imposingSheet)

type RegularImposingSheet = RegularImposingSheet<unit>




type PageTilingResultCount = PageTilingResultCount of int
with 
    member x.Value = 
        let (PageTilingResultCount count) = x
        count

[<AutoOpen>]
module _Reuses =

    type BackgroundOrForeground =
        | Background = 0
        | Foreground = 1


    let private emptyPdf = lazy PdfUtils.NewTempEmptyPdf()



    let private reuse name parameters f = Reuse(f = f, flowName = FlowName.Override(name, parameters))

    type PageInsertingOptions =
        | BeforePoint = 0
        | AfterPoint = 1


    type private CellIndex =
        { ColIndex: int 
          RowIndex: int 
          PageNum: int }


    type Reuses with
        static member Extract(pageSelector: PageSelector, selector, ?keepOriginPage) =    
            let keepOriginPage = defaultArg keepOriginPage false
            fun (flowModel: FlowModel<_>) (splitDocument: SplitDocument) ->
                let reader = splitDocument.Reader
                let parser = NonInitialClippingPathPdfDocumentContentParser(reader)
                let totalNumberOfPages = reader.GetNumberOfPages()
                let pageNumbers = reader.GetPageNumbers(pageSelector)

                pageNumbers
                |> List.iter(fun pageNumber ->
                    let readerPage = reader.GetPage(pageNumber)

                    let selector = 
                        let args: PageModifingArguments<_> = 
                            { UserState = flowModel.UserState
                              Page = readerPage
                              TotalNumberOfPages = totalNumberOfPages
                              PageNum = pageNumber
                            }
                        
                        Selector.toRenderInfoSelector args selector

                    let infos = 
                        let infos =
                            NonInitialClippingPathPdfDocumentContentParser.parse pageNumber selector parser

                        infos
                        |> List.ofSeq
                        |> List.map(fun info ->
                            match info with 
                            | IIntegratedRenderInfo.Path pathInfo -> 
                                pathInfo.Renewable()
                                |> RenewableInfo.Path

                            | IIntegratedRenderInfo.Text textInfo -> 
                                textInfo.Renewable()
                                |> RenewableInfo.Text
                        )

                    let writer = splitDocument.Writer
                    match keepOriginPage with 
                    | true -> 
                        let writerPage = readerPage.CopyTo(writer)
                        writer.AddPage(writerPage)
                        |> ignore

                    | false -> ()

                    let writerPage = writer.AddNewPage(PageSize(readerPage.GetActualBox()))
                    writerPage.SetPageBoxToPage(readerPage) |> ignore
                    let writerCanvas = new PdfCanvas(writerPage.GetContentStream(0), writerPage.GetResources(), writer)
                    PdfCanvas.useCanvas writerCanvas (fun writerCanvas ->
                        writerCanvas
                            .WriteLiteral("0 G\n")
                            .WriteLiteral("0 g\n")
                            |> ignore

                        for info in infos do    
                            info.CopyToDocument(writer).ApplyCtm_WriteToCanvas(writerCanvas)

                        writerCanvas
                    ) |> ignore

                )

            |> reuse 
                "ExtractorPaths"
                ["pageSelector" => pageSelector.Text
                 "selector" => selector.ToString()
                 "keepOriginPage" => keepOriginPage.ToString() ]

        static member ExtractPaths(pageSelector, pathSelector, ?keepOriginPage) =
            Reuses.Extract(pageSelector, Selector.Path pathSelector, ?keepOriginPage = keepOriginPage)

        static member Insert(insertingFile: string, ?insertingFilePageSelector: PageSelector, ?pageInsertingOptions: PageInsertingOptions, ?insertingPoint: SinglePageSelectorExpr) =
            let insertingFilePageSelector = defaultArg insertingFilePageSelector PageSelector.All
            let pageInsertingOptions = defaultArg pageInsertingOptions PageInsertingOptions.AfterPoint
            let insertingPoint = defaultArg insertingPoint (SinglePageSelectorExpr.End 1)

            fun flowModel (splitDocument: SplitDocument) ->
                if Path.GetFullPath(insertingFile) = Path.GetFullPath(splitDocument.ReaderPath) 
                then failwith "Cannot insert file to self"

                let numberOfPages = splitDocument.Reader.GetNumberOfPages()
                let insertingPoint =
                    splitDocument.Reader.GetPageNumber insertingPoint

                let tryCopyPages (reader: PdfDocument) pageFrom pageTo =
                    if pageTo >= pageFrom
                    then reader.CopyPagesTo(pageFrom, pageTo, splitDocument.Writer) |> ignore


                match pageInsertingOptions with
                | PageInsertingOptions.AfterPoint ->
                    tryCopyPages splitDocument.Reader 1 insertingPoint 
                    

                    let readerResource = new PdfDocument(new PdfReader(insertingFile))
                    let readerResourcePageNumbers = readerResource.GetPageNumbers(insertingFilePageSelector)

                    for readerResourcePageNumber in readerResourcePageNumbers do
                        let page = readerResource.GetPage(readerResourcePageNumber).CopyTo(splitDocument.Writer)
                        splitDocument.Writer.AddPage(page) |> ignore
                    readerResource.Close()


                    tryCopyPages splitDocument.Reader (insertingPoint + 1) numberOfPages

                | PageInsertingOptions.BeforePoint ->
                    tryCopyPages splitDocument.Reader 1 (insertingPoint - 1)
                    

                    let readerResource = new PdfDocument(new PdfReader(insertingFile))
                    let readerResourcePageNumbers = readerResource.GetPageNumbers(insertingFilePageSelector)

                    for readerResourcePageNumber in readerResourcePageNumbers do
                        let page = readerResource.GetPage(readerResourcePageNumber).CopyTo(splitDocument.Writer)
                        splitDocument.Writer.AddPage(page) |> ignore

                    readerResource.Close()


                    tryCopyPages splitDocument.Reader (insertingPoint) numberOfPages
                

            |> reuse 
                "Insert"
                ["insertingFile" => insertingFile.ToString()
                 "insertingFilePageSelector" => insertingFilePageSelector.ToString()
                 "pageInsertingOptions" => pageInsertingOptions.ToString()
                 "insertingPoint" => insertingPoint.ToString()]

        static member InsertEmptyPages(fEmptyPageCount, ?pageInsertingOptions: PageInsertingOptions) =
            let pageInsertingOptions = defaultArg pageInsertingOptions PageInsertingOptions.AfterPoint

            fun flowModel (splitDocument: SplitDocument) ->
                PdfDocument.getPages splitDocument.Reader
                |> List.iteri(fun i page ->
                    let pageNum = i + 1

                    let addEmptyPages() =
                        let emptyPageNumberCount = fEmptyPageCount pageNum
                        [1..emptyPageNumberCount]
                        |> List.iter(fun _ ->
                            let writerPage: PdfPage = splitDocument.Writer.AddNewPage(PageSize(page.GetActualBox()))
                            writerPage.SetPageBoxToPage(page)
                            |> ignore
                        )

                    match pageInsertingOptions with 
                    | PageInsertingOptions.BeforePoint ->
                        addEmptyPages()
                        let writerPage = page.CopyTo(splitDocument.Writer) 
                        splitDocument.Writer.AddPage(writerPage) |> ignore

                    | PageInsertingOptions.AfterPoint ->
                        let writerPage = page.CopyTo(splitDocument.Writer) 
                        splitDocument.Writer.AddPage(writerPage) |> ignore
                        addEmptyPages()
                    | _ -> ()
                )

            |> reuse 
                "InsertEmptyPages"
                ["fEmptyPageCount" => fEmptyPageCount.ToString()
                 "pageInsertingOptions" => pageInsertingOptions.ToString() ]

        /// e.g pageNumber 14 -> 4X -> 16
        ///
        /// e.g pageNumber 14 -> 6X -> 18
        static member InsertEmptyPagesToMultiple(multiple: int, ?pageInsertingOptions: PageInsertingOptions, ?insertingPoint: SinglePageSelectorExpr) =
            let pageInsertingOptions = defaultArg pageInsertingOptions PageInsertingOptions.AfterPoint
            let insertingPoint = defaultArg insertingPoint (SinglePageSelectorExpr.End 1)

            Reuse.Factory(fun flowModel splitDocument ->
                let numberOfPages = splitDocument.Reader.GetNumberOfPages()

                let preparedInsertingEmptyPagesCount = 
                    numberOfPages % multiple



                match preparedInsertingEmptyPagesCount with 
                | 0 -> Reuse.dummy() ||>> ignore
                | i -> 
                    let i = multiple - i
                    let insertingPoint = splitDocument.Reader.GetPageNumber(insertingPoint)
                    Reuses.InsertEmptyPages(
                        fEmptyPageCount = (fun pageNum ->
                            match pageNum with 
                            | EqualTo insertingPoint -> i
                            | _ -> 0
                        ),
                        pageInsertingOptions = pageInsertingOptions
                )
            )

            |> Reuse.rename
                "InsertEmptyPagesToNX"
                ["multiple" => multiple.ToString()
                 "pageInsertingOptions" => pageInsertingOptions.ToString()
                 "insertingPoint" => insertingPoint.ToString() ]

 

                


        static member Rotate (pageSelector: PageSelector, fRotation: PageNumber -> Rotation) =
            Reuse.Factory(fun flowModel splitDocument ->
                let selectedPageNums = splitDocument.Reader.GetPageNumbers(pageSelector)

                let pageNumSequence = 
                    [ 1 .. splitDocument.Reader.GetNumberOfPages() ]
                    |> List.map (fun pageNum -> 
                        if List.contains pageNum selectedPageNums
                        then (pageNum, fRotation (PageNumber pageNum))
                        else (pageNum, Rotation.None))
                    |> EmptablePageNumSequence.Create

                Reuses.SequencePages(pageNumSequence)
            )

            |> Reuse.rename
                "Rotate"
                [ "pageSelector" => pageSelector.ToString()
                  "rotation" => fRotation.ToString() ]

        static member Rotate (pageSelector: PageSelector, rotation) =
            Reuses.Rotate(pageSelector, fun _ -> rotation)
            |> Reuse.rename
                "Rotate"
                [ "pageSelector" => pageSelector.ToString()
                  "rotation" => rotation.ToString() ]

        static member Resize (pageSelector: PageSelector, pageBoxKind: PageBoxKind, fSize: PageNumber -> FsSize) =
            
            fun flowModel (splitDocument: SplitDocument) ->
                let selectedPageNumbers = splitDocument.Reader.GetPageNumbers(pageSelector) 

                PdfDocument.getPages splitDocument.Reader
                |> List.mapi (fun i page ->
                    let pageNum = i + 1
                    
                    let size = 
                        let unroundSize =  (fSize (PageNumber pageNum))
                        RoundedSize.Create unroundSize

                    if List.contains pageNum selectedPageNumbers 
                    then 
                        let actualBox = page.GetActualBox()
                        let pageBox = page.GetPageBox(pageBoxKind)
                        let affineTransform_Scale = 
                            AffineTransform.GetScaleInstance(size.Width / pageBox.GetWidthF(), size.Height / pageBox.GetHeightF())

                        let xobject = page.CopyAsFormXObject(splitDocument.Writer)

                        let affineTransform_Translate = AffineTransform.GetTranslateInstance(-actualBox.GetXF(), -actualBox.GetYF())

                        let affineTransform = affineTransform_Scale.Clone()

                        affineTransform.Concatenate(affineTransform_Translate)

                        let newPage = 
                            splitDocument.Writer.AddNewPage(
                                PageSize(
                                    Rectangle.create 
                                        0. 
                                        0. 
                                        (size.Width *  actualBox.GetWidthF() / pageBox.GetWidthF())
                                        (size.Height * actualBox.GetHeightF() / pageBox.GetHeightF()) 
                                )
                            )


                        let canvas = new PdfCanvas(newPage)
                        canvas.AddXObject(xobject, affineTransform)
                        |> ignore

                        [
                            PageBoxKind.MediaBox
                            PageBoxKind.CropBox
                            PageBoxKind.ArtBox
                            PageBoxKind.BleedBox
                            PageBoxKind.TrimBox
                        ] |> List.iter (fun pageBoxKind ->
                            let pageBox = page.GetPageBox(pageBoxKind)
                            
                            let newPageBox = 
                                affineTransform.Transform(pageBox)

                            PdfPage.setPageBox pageBoxKind newPageBox newPage
                            |> ignore
                        )



                    else 
                        let page = page.CopyTo(splitDocument.Writer)
                        splitDocument.Writer.AddPage(page) |> ignore
                    
                    size
                )
            |> reuse
                "Resize"
                ["pageSelector" => pageSelector.ToString()
                 "pageBoxKind" => pageBoxKind.ToString() 
                 "size" => fSize.ToString() ]
            
        static member Resize (pageSelector: PageSelector, pageBoxKind: PageBoxKind, size: FsSize) =
            Reuses.Resize(
                pageSelector = pageSelector,
                pageBoxKind = pageBoxKind,
                fSize = (fun _ -> size)
            )
            |> Reuse.rename
                "Resize"
                ["pageSelector" => pageSelector.ToString()
                 "pageBoxKind" => pageBoxKind.ToString() 
                 "size" => size.ToString() ]

        static member Resize (pageSelector: PageSelector, pageResizingRotatingOptions: PageResizingRotatingOptions, pageResizingScalingOptions: PageResizingScalingOptions, fSize: PageNumber -> FsSize) =
            let tryRotate() =
                match pageResizingRotatingOptions with
                | PageResizingRotatingOptions.Keep -> Reuse.dummy() ||>> ignore
                | PageResizingRotatingOptions.ColckwiseIfNeeded 
                | PageResizingRotatingOptions.CounterColckwiseIfNeeded ->
                    Reuse.Factory(fun flowModel splitDocument ->
                        let proposedRotatedPageNumbers =
                            PdfDocument.getPages splitDocument.Reader
                            |> List.indexed
                            |> List.choose (fun (i, page) ->
                                let pageNum = i + 1
                                let actualBox = page.GetActualBox()
                                match FsSize.ofRectangle actualBox, fSize (PageNumber pageNum) with
                                | FsSize.Portrait, FsSize.Portrait 
                                | FsSize.Landscape, FsSize.Landscape 
                                | FsSize.Uniform, _
                                | _, FsSize.Uniform -> None
                                | FsSize.Portrait, FsSize.Landscape
                                | FsSize.Landscape, FsSize.Portrait -> Some pageNum
                            )

                        Set.intersect
                            (Set.ofList (splitDocument.Reader.GetPageNumbers(pageSelector))) 
                            (Set.ofList proposedRotatedPageNumbers)
                        |> fun pageNumbers ->

                            if pageNumbers.IsEmpty then Reuse.dummy() ||>> ignore
                            else
                                let pageSelector = 
                                    PageSelector.Numbers (AtLeastOneSet.Create pageNumbers)

                                let rotation = 
                                    match pageResizingRotatingOptions with 
                                    | PageResizingRotatingOptions.CounterColckwiseIfNeeded -> Rotation.Counterclockwise
                                    | PageResizingRotatingOptions.ColckwiseIfNeeded -> Rotation.Clockwise
                                    | PageResizingRotatingOptions.Keep -> Rotation.None

                                Reuses.Rotate(pageSelector, rotation)
                    )

            let resize() =
                match pageResizingScalingOptions with 
                | PageResizingScalingOptions.Anamorphic -> 
                    Reuses.Resize(pageSelector, PageBoxKind.ActualBox, fSize)
                    |> Reuse.Append (fun splitDocument ->
                        PdfDocument.getPages splitDocument.Writer
                        |> List.iteri(fun i page ->
                            let actualBox = page.GetActualBox()
                            PdfPage.setPageBox PageBoxKind.AllBox actualBox
                            |> ignore
                        )
                    )

                | PageResizingScalingOptions.Uniform ->
                    Reuse(  
                        flowName =    
                            FlowName.Override(
                                "Resize",
                                [
                                    "pageSelector" => pageSelector.ToString()
                                    "size" => fSize.ToString()
                                ]
                            ),
                        f = 
                            fun flowModel splitDocument ->
                            let selectedPageNumbers = splitDocument.Reader.GetPageNumbers(pageSelector) 
                            PdfDocument.getPages splitDocument.Reader
                            |> List.mapi (fun i page ->
                                let pageNum = i + 1
                                let size = (fSize (PageNumber pageNum)) |> RoundedSize.Create
                                if List.contains pageNum selectedPageNumbers 
                                then 
                                    let actualBox = page.GetActualBox()

                                    let affineTransform_Scale = 
                                        let scaleX = size.Width / actualBox.GetWidthF()
                                        let scaleY = size.Height / actualBox.GetHeightF()
                                        AffineTransform.GetScaleInstance(min scaleX scaleY, min scaleX scaleY)

                                    let affineTransform_Translate = AffineTransform.GetTranslateInstance(-actualBox.GetXF(), -actualBox.GetYF())

                                    let affineTransform = affineTransform_Scale.Clone()

                                    affineTransform.Concatenate(affineTransform_Translate)

                                    let xobject = page.CopyAsFormXObject(splitDocument.Writer)

                                    let newPage = 
                                        splitDocument.Writer.AddNewPage(
                                            PageSize(
                                                Rectangle.create 
                                                    0. 
                                                    0. 
                                                    (size.Width)
                                                    (size.Height) 
                                            )
                                        )

                                    let affineTransform_Translate_Center =
                                        let scaledActualBox = affineTransform_Scale.Transform(actualBox)
                                        AffineTransform.GetTranslateInstance((size.Width - scaledActualBox.GetWidthF()) / 2., (size.Height - scaledActualBox.GetHeightF()) / 2.)

                                    affineTransform.PreConcatenate(affineTransform_Translate_Center)

                                    let canvas = new PdfCanvas(newPage)
                                    canvas.AddXObject(xobject, affineTransform)
                                    |> ignore

                                size
                            )
                    )

            tryRotate()
            <+>
            (resize())
           

        static member Resize (pageSelector: PageSelector, pageResizingRotatingOptions: PageResizingRotatingOptions, pageResizingScalingOptions: PageResizingScalingOptions, size: FsSize) =
            Reuses.Resize(pageSelector, pageResizingRotatingOptions, pageResizingScalingOptions, (fun _ -> size))
            |> Reuse.rename
                "Resize"
                ["pageResizingRotatingOptions" => pageResizingRotatingOptions.ToString()
                 "pageResizingScalingOptions" => pageResizingScalingOptions.ToString()
                 "pageSelector" => pageSelector.ToString()
                 "size" => size.ToString() ]


        static member Scale (pageSelector, fScaleX: PageNumber -> float, fScaleY: PageNumber -> float) =
            Reuse.Factory(fun _ splitDocument ->
                let reader = splitDocument.Reader
                
                let pageSizes = 
                    PdfDocument.getPages reader
                    |> List.map (PdfPage.getActualBox)

                let newPageSizes =
                    pageSizes
                    |> List.mapi (fun i pageSize ->
                        let pageNumber = PageNumber (i + 1)
                        let scaleX = fScaleX pageNumber
                        let scaleY = fScaleY pageNumber
                        { Width = pageSize.GetWidthF() * scaleX 
                          Height = pageSize.GetHeightF() * scaleY }
                    )
                Reuses.Resize(
                    pageSelector = pageSelector,
                    pageBoxKind = PageBoxKind.ActualBox,
                    fSize = (fun pageNum ->
                        newPageSizes.[pageNum.Value - 1]
                    )
                )
            )
            |> Reuse.rename
                "Scale"
                ["pageSelector" => pageSelector.ToString()
                 "fScaleX" => fScaleX.ToString() 
                 "fScaleY" => fScaleY.ToString() ]

                

        static member Scale(pageSelector, scaleX, scaleY) =
            Reuses.Scale(
                pageSelector = pageSelector,
                fScaleX = (fun _ -> scaleX),
                fScaleY = (fun _ -> scaleY)
            )
            |> Reuse.rename
                "Scale"
                ["pageSelector" => pageSelector.ToString()
                 "fScaleX" => scaleX.ToString() 
                 "fScaleY" => scaleY.ToString() ]


        static member Flip (pageSelector: PageSelector, fFlip: PageNumber -> Flip option) =
            Reuse.Factory(fun flowModel (splitDocument: SplitDocument) ->
                let selectedPageNums = splitDocument.Reader.GetPageNumbers(pageSelector)

                let pageNumSequence = 
                    [ 1 .. splitDocument.Reader.GetNumberOfPages() ]
                    |> List.map (fun pageNum -> 
                        if List.contains pageNum selectedPageNums
                        then 
                            match fFlip(PageNumber pageNum) with 
                            | Some flip ->
                                (pageNum, flip)
                                |> PageNumSequenceToken.PageNumWithFlip

                            | None -> PageNumSequenceToken.PageNum pageNum
                        else PageNumSequenceToken.PageNum pageNum
                    )
                    |> List.map EmptablePageNumSequenceToken.PageNumSequenceToken
                    |> EmptablePageNumSequence.Create
                Reuses.SequencePages(pageNumSequence)

            )

            |> Reuse.rename
                "Flip"
                [ "pageSelector" => pageSelector.ToString()
                  "flip" => fFlip.ToString() ]

        static member Flip (pageSelector: PageSelector, flip: Flip) =
            Reuses.Flip(pageSelector, fun _ -> Some flip)
            |> Reuse.rename
                "Flip"
                [ "pageSelector" => pageSelector.ToString()
                  "flip" => flip.ToString() ]


        static member FlipEvenPages (pageSelector: PageSelector, flip: Flip) =
            Reuses.Flip(pageSelector, fun pageNumber -> 
                match pageNumber.Value with 
                | Even -> Some flip
                | Odd -> None
            )
            |> Reuse.rename
                "FlipEvenPages"
                [ "pageSelector" => pageSelector.ToString()
                  "flip" => flip.ToString() ]

        static member DuplicatePages (pageSelector: PageSelector, copiedNumbers: CopiedNumSequence) =
            Reuse.Factory(fun flowModel splitDocument ->
                let pageNumSequence = 
                    splitDocument.Reader.GetPageNumbers(pageSelector)
                    |> List.indexed
                    |> List.collect (fun (index, pageNum) ->
                        if index < copiedNumbers.Value.Length then
                            List.replicate copiedNumbers.Value.[pageNum - 1] pageNum
                        else []
                    )
                    |> EmptablePageNumSequence.Create

                (Reuses.SequencePages pageNumSequence)
            )

            |> Reuse.rename 
                "DuplicatePages"
                ["pageSelector" => pageSelector.ToString()
                 "copiedNumbers" => copiedNumbers.ToString() ]

        static member DuplicatePages (pageSelector: PageSelector, copiedNumber: int)  =
            if copiedNumber <= 0 then failwithf "copied number %d should be bigger than 0" copiedNumber

            Reuse.Factory(
                fun flowModel (splitDocument: SplitDocument) ->
                    let pageNumSequence = 
                        splitDocument.Reader.GetPageNumbers(pageSelector)
                        |> List.map (fun _ -> copiedNumber
                        )
                        |> CopiedNumSequence.Create

                    (Reuses.DuplicatePages (pageSelector, pageNumSequence))
            )



        static member TilePages (tileTable: TileTableIndexer, ?direction: Direction) =
            let direction = defaultArg direction Direction.Horizontal
            let colNum = tileTable.ColNum
            let rowNum = tileTable.RowNum
            (Reuses.DuplicatePages (PageSelector.All, colNum * rowNum))
            <+>
            Reuse(fun _ splitDocument ->
                let reader = splitDocument.Reader
                let writer = splitDocument.Writer

                for i = 1 to reader.GetNumberOfPages() do
                    let readerPage = reader.GetPage(i)
                    let actualBox = readerPage.GetActualBox()
                    let tileIndexer = TileCellIndexer.Create((i - 1), direction)
                    let tileBox = Rectangle.getTile tileIndexer tileTable actualBox

                    let writerPage = readerPage.CopyTo(writer)
                    writer.AddPage(writerPage) |> ignore

                    PdfPage.setPageBox PageBoxKind.AllBox tileBox writerPage
                    |> ignore

                PageTilingResultCount (colNum * rowNum)
                |> List.replicate (reader.GetNumberOfPages())
                |> AtLeastOneList.Create

            ) 
            |> Reuse.rename 
                "TilePages"
                ["tileTable" => tileTable.ToString()]
            

        static member TilePages (selector: Selector<'userState>, ?sorter: SelectionSorter) =
            let sorter = defaultArg sorter (SelectionSorter.DefaultValue)

            fun (flowModel: FlowModel<'userState>) (splitDocument: SplitDocument) ->

                let reader = splitDocument.Reader
                let parser = new NonInitialClippingPathPdfDocumentContentParser(reader)
                [
                    for i = 1 to reader.GetNumberOfPages() do
                        let readerPage = reader.GetPage(i)
                        let args =
                            { Page = readerPage
                              UserState = flowModel.UserState 
                              TotalNumberOfPages = splitDocument.Reader.GetNumberOfPages() 
                              PageNum = i }
                    
                        let selector = (Selector.toRenderInfoSelector args selector)

                        let bounds = 
                            let rects = 
                                NonInitialClippingPathPdfDocumentContentParser.parse i selector parser
                                |> List.ofSeq
                                |> List.map (fun info -> 
                                    IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth info
                                )
                                |> Rectangle.removeInboxes

                            sorter.Sort (rects)


                        for bound in bounds do
                            let writer = splitDocument.Writer
                            let writerPageResource = readerPage.CopyTo(writer)
                            PdfPage.setPageBox PageBoxKind.AllBox bound writerPageResource |> ignore
                            writer.AddPage(writerPageResource)
                            |> ignore

                        yield PageTilingResultCount bounds.Length 

                ]
                |> AtLeastOneList.Create

            |> reuse 
                "TilePages"
                ["selector" => selector.ToString()]

        static member PickFromPageTilingResult(pageTilingResults: AtLeastOneList<PageTilingResultCount>, picker: PageNumSequence) =
            let pageNumSequence = 
                ([], pageTilingResults.AsList)
                ||> List.scan(fun accum m -> 
                    let maxValue = 
                        match List.tryLast accum with 
                        | Some last -> last 
                        | None -> 0

                    [(maxValue + 1) .. (m.Value + maxValue)]
                ) 
                |> List.tail
                |> List.collect (fun m ->
                    picker.Value
                    |> List.choose (fun n -> List.tryItem (n.PageNumValue - 1) m)
                )
                |> EmptablePageNumSequence.Create

            Reuses.SequencePages(pageNumSequence)


        static member ChangePageOrientation(pageSelector: PageSelector, orientation: PageOrientation) =

            Reuse.Factory(

                fun flowModel (splitDocument: SplitDocument) ->

                    let pageNumberSequence =
                        let selectedPageNumbers = splitDocument.Reader.GetPageNumbers(pageSelector)
                        PdfDocument.getPages splitDocument.Reader
                        |> List.mapi(fun i page ->
                            let pageNumber = i + 1
                            match List.contains pageNumber selectedPageNumbers with 
                            | true -> 
                                match FsSize.ofRectangle(page.GetActualBox()), orientation with 
                                | FsSize.Portrait, PageOrientation.Portrait 
                                | FsSize.Landscape, PageOrientation.Landscape
                                | FsSize.Uniform, _ -> pageNumber, Rotation.None
                                | _ -> pageNumber, Rotation.Clockwise
                            | false ->
                                pageNumber, Rotation.None
                        )
                    Reuses.SequencePages(EmptablePageNumSequence.Create pageNumberSequence)
            )
            |> Reuse.rename 
                "ChangePageOrientation"
                ["pageSelector" => pageSelector.ToString()
                 "orientation" => orientation.ToString() ]


    
        static member internal Impose_Raw (fArgs, offsetLists: option<FsPoint list list>, draw) =
            let imposingArguments = ImposingArguments.Create fArgs

            fun flowModel (splitDocument: SplitDocument) ->
                let imposingDocument = new ImposingDocument(splitDocument, imposingArguments)
                imposingDocument.Build()
                match draw with 
                | true -> 
                    match offsetLists with 
                    | Some offsetLists -> imposingDocument.Offset(offsetLists)
                    | None -> ()
                    imposingDocument.Draw()
                | false -> splitDocument.Writer.AddNewPage() |> ignore
                (imposingDocument)

            |> reuse 
                "Impose_Raw"
                ["imposingArguments" => imposingArguments.ToString()]

        
        static member ClippingContentsToPageBox(pageBoxKind: PageBoxKind, ?margin: Margin) =
            let margin = defaultArg margin Margin.Zero

            fun flowModel (splitDocument: SplitDocument) ->
                let pages = PdfDocument.getPages <| splitDocument.Reader
                let writer = splitDocument.Writer
                for page in pages do 
                    let clippingPageBox = page.GetPageBox(pageBoxKind)
                    let actualBox = page.GetActualBox()

                    let xobject = 
                        
                        Rectangle.applyMargin margin clippingPageBox 
                        |> Rectangle.toPdfArray
                        |> page.CopyAsFormXObject(writer).SetBBox

                    let newPage = 
                        splitDocument.Writer.AddNewPage(
                            PageSize(actualBox)
                        )


                    let canvas = new PdfCanvas(newPage)
                    newPage.SetPageBoxToPage(page) |> ignore
                    //let clippingBoxToActualPage =  - clippingPageBox.GetX()
                        
                    canvas.AddXObjectAbs(xobject, 0.f, 0.f)
                    |> ignore


            |> reuse 
                "ClippingContentsToPageBox"
                ["PageBoxKind" => pageBoxKind.ToString() 
                 "margin" => margin.LoggingText ]



        static member private Impose(fArgs, offsetLists) =
            let args = (ImposingArguments.Create fArgs)
            let cellRotation = args.Value.CellRotation
            match cellRotation with 
            | CellRotation.None -> Reuses.Impose_Raw(fArgs, offsetLists = offsetLists,  draw = true)
            | _ ->
                Reuse.Factory(fun flowModel doc ->
                    let doc = ImposingDocument(doc, args)
                    doc.Build()
                    let args = args.Value
                    let sequence = 
                        let baseSequence = 
                            match args.IsRepeated with 
                            | true -> 
                                doc.GetSheets()
                                |> List.mapi (fun i sheet -> 
                                    let colNum_perRow = sheet.Rows.[0].Cells.Count
                                    let rowsCount = sheet.RowsCount
                                    let sequence = List.replicate (sheet.GetCellsCount()) (i+1)
                                    sequence
                                    |> List.mapi (fun i pageNum ->
                                        let index = i
                                        { ColIndex = (index % colNum_perRow) 
                                          RowIndex = (index / colNum_perRow) 
                                          PageNum = pageNum
                                        }
                                    )
                                )
                                |> List.concat

                            | false ->

                                let sequence =
                                    let rec loop sheets accum previous_LastPageNum =
                                        match sheets with 
                                        | [] -> accum 
                                        | (sheet: ImposingSheet) :: t ->
                                            let rec loop2 rows accum previous_LastPageNum =
                                                match rows with 
                                                | [] -> accum
                                                | (row: ImposingRow) :: t ->
                                                    let cells =
                                                        row.Cells
                                                        |> Seq.mapi(fun  i cell ->
                                                            { ColIndex = cell.Index
                                                              RowIndex = cell.ImposingRow.RowIndex
                                                              PageNum = previous_LastPageNum + i + 1
                                                            }
                                                        )
                                                        |> List.ofSeq

                                                    loop2 t (accum @ cells) (previous_LastPageNum + cells.Length)

                                            let cells = loop2 (List.ofSeq sheet.Rows) [] previous_LastPageNum
                                            loop t (accum @ cells) (previous_LastPageNum + cells.Length)

                                    loop (doc.GetSheets()) [] 0

                                sequence

                        let sequence_applyCellRotation =
                            baseSequence
                            |> List.map (fun cellIndex ->
                                match cellRotation, cellIndex.ColIndex+1, cellIndex.RowIndex+1 with 
                                | CellRotation.R180WhenColNumIsEven, Even, _ -> 
                                    PageNumSequenceToken.PageNumWithRotation(cellIndex.PageNum, Rotation.R180)

                                | CellRotation.R180WhenRowNumIsEven, _, Even ->
                                    PageNumSequenceToken.PageNumWithRotation(cellIndex.PageNum, Rotation.R180)
                                    
                                | _ -> PageNumSequenceToken.PageNum cellIndex.PageNum
                            )

                        sequence_applyCellRotation
                        |> PageNumSequence.Create

                    Reuses.SequencePages sequence
                    <+>
                    Reuses.Impose_Raw
                        (
                            (fun _ -> 
                            { args with IsRepeated = false }),
                            offsetLists = offsetLists,
                            draw = true 
                        )
                    ||>> fun doc -> 
                        doc.ReSetIsRepeated(args.IsRepeated)
                        doc
                )

            |> Reuse.rename
                "Impose"
                ["imposingArguments" => args.ToString()]


        static member Impose(fArgs) =
            Reuses.Impose(fArgs, None)

        static member CreatePageTemplate() =
            fun flowModel (doc: SplitDocument) ->
                let writer = doc.Writer

                PdfDocument.getPages doc.Reader
                |> List.iteri(fun i page ->
                    let newPage = writer.AddNewPage(PageSize(page.GetActualBox()))

                    newPage.SetPageBoxToPage(page) |> ignore

                    let canvas = new Canvas(newPage, newPage.GetTrimBox())
                    canvas
                    |> Canvas.addRectangleToRootArea (fun args ->
                        { args with StrokeColor = NullablePdfCanvasColor.valueColor FsDeviceRgb.BLUE }
                    )
                    |> Canvas.addText((i+1).ToString())  (fun args ->
                        { args with 
                            CanvasFontSize = 
                                CanvasFontSize.OfRootArea 0.8
                            Position = Position.Center(0., 0.)
                        }
                    )
                    |> ignore
                )

            |> reuse "CreatePageSizeTemplate" []

        static member ImposeVertically(fArgs) =
            let args = (ImposingArguments.Create fArgs)
            match args.Value.IsRepeated with 
            | true -> Reuses.Impose(fArgs) ||>> (fun m -> m.GetRotatableImposingSheets())
            | false ->
                
                Reuses.Rotate(PageSelector.All, Rotation.Clockwise)
                <+>
                Reuse.Factory(fun flowModel doc ->
                    let fArgs rotation (args: _ImposingArguments) = 
                        (fArgs args).Rotate(rotation)
                    let args = ImposingArguments.Create (fArgs Rotation.Clockwise)
                    let imposingDoc = ImposingDocument(doc, args)
                    imposingDoc.Build()
                    let sheets = imposingDoc.GetSheets()

                    let offsetLists =
                        sheets
                        |> List.map(fun sheet ->
                            let tableWidth = sheet.TableWidth
                            sheet.GetRows()
                            |> List.map(fun row ->
                                { X = tableWidth - row.Width 
                                  Y = 0. }
                            )
                        )

                    let rec loop accum startPageNumber (sheets: ImposingSheet list) =
                        match sheets with 
                        | [] -> List.rev accum
                        | sheet :: t ->
                            let rows = sheet.GetRows()
                            let sequence = 
                                rows
                                |> List.mapi(fun i row ->
                                    let beforeRowsTotalCount = 
                                        rows.[0 .. i-1]
                                        |> List.sumBy(fun m -> m.Cells.Count)

                                    [1 .. row.Cells.Count]
                                    |> List.rev
                                    |> List.map(fun num -> num + beforeRowsTotalCount + startPageNumber)
                                )
                                |> List.concat
                                |> PageNumSequence.Create

                            loop (sequence :: accum) (sequence.Value_Al1List.Length + startPageNumber) t

                    let pageNumberSequence = 
                        loop [] 0 (sheets)
                        |> AtLeastOneList.Create
                        |> PageNumSequence.Concat

                    Reuses.SequencePages(pageNumberSequence)
                    <+> Reuses.Impose(fArgs Rotation.Clockwise, Some offsetLists)
                    <.+> Reuses.Rotate(PageSelector.All, Rotation.Counterclockwise)
                )
                ||>> (fun m -> m.GetRotatableImposingSheets().Rotate(Rotation.Counterclockwise))

            |> Reuse.rename 
                ("ImposeVertically")
                ["imposingArguments" => args.ToString()]

        static member ImposeInDirection(direction, fArgs) =
            match direction with 
            | Direction.Horizontal -> Reuses.Impose(fArgs) ||>> (fun m -> m.GetRotatableImposingSheets())
            | Direction.Vertical -> Reuses.ImposeVertically(fArgs)

        //static member Impose(fArgs) =
        //    let args = fArgs _ImposingArguments.DefaultValue
        //    let needAddDashLineIn_MiddleSpace =
        //        ( args.HSpaceExes.Value
        //          |> List.exists(fun m -> m.MiddleDashPattern.IsSome)
        //        )
        //        ||
        //        ( args.VSpaceExes.Value
        //          |> List.exists(fun m -> m.MiddleDashPattern.IsSome)
        //        )

        //    match needAddDashLineIn_MiddleSpace with 
        //    | false -> Reuses.Impose_CellRotation(fArgs)
        //    | true ->

        //        Reuses.Impose_CellRotation(fArgs)
        //        <+> (
        //            (fun (flowModel: PageModifingArguments<ImposingDocument>) (doc: SplitDocument) ->
                    
        //                let reader = doc.Reader
        //                let writer = doc.Writer

        //                let sheets = flowModel.UserState.GetSheets()

        //                PdfDocument.getPages reader 
        //                |> List.iteri(fun i page ->
        //                    let pageNum = i + 1
        //                    let page = page.CopyTo(writer)
        //                    let args: PageModifingArguments<_> =
        //                        { UserState = flowModel.UserState
        //                          Page = page
        //                          TotalNumberOfPages = reader.GetNumberOfPages()
        //                          PageNum = pageNum }

        //                    let sheet = sheets.[i]

        //                    let rowLines =
        //                        sheet.Rows
        //                        |> List.ofSeq
        //                        |> List.map(fun )


        //                    PageModifier.AddLine({})

        //                )




        //            )
        //            |> reuse
        //                "Impose"
        //                ["Args" => args.ToString()]
        //        )

        /// default useBleed: false
        static member OneColumn(?margin, ?useBleed, ?spaces) =
            Reuses.Impose(fun args -> 
                {args with 
                    Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (defaultArg margin Margin.Zero)
                    HSpaceExes = Spaces.Zero
                    VSpaceExes = defaultArg spaces Spaces.Zero
                    ColNums = [1]
                    RowNum = 0
                    IsRepeated = false
                    UseBleed = defaultArg useBleed false
                    Background = Background.Size FsSize.MAXIMUN})
            ||>> (fun m -> m.GetSheets() |> List.exactlyOne_DetailFailingText)

        /// default useBleed: false
        static member OneRow(?margin, ?useBleed, ?spaces) =
            Reuses.Impose(fun args -> 
                {args with 
                    Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (defaultArg margin Margin.Zero)
                    HSpaceExes = defaultArg spaces Spaces.Zero
                    VSpaceExes = Spaces.Zero
                    ColNums = [0]
                    RowNum = 1
                    IsRepeated = false
                    UseBleed = defaultArg useBleed false
                    Background = Background.Size FsSize.MAXIMUN})
            ||>> (fun m -> m.GetSheets() |> List.exactlyOne_DetailFailingText)

        
        /// 合并两页
        static member MergeTwoPages(useBleed: bool, ?margin: Margin, ?hspaces, ?vspaces, ?cellSize, ?direction: Direction) =
            let direction = 
                defaultArg direction Direction.Horizontal
            
            let margin = defaultArg margin Margin.Zero

            let colNum =
                match direction with 
                | Direction.Horizontal -> 2
                | Direction.Vertical -> 1

            let rowNum =
                match direction with 
                | Direction.Horizontal -> 1
                | Direction.Vertical -> 2

            let args = 
                { _ImposingArguments.DefaultValue with 
                    ColNums = [ colNum ]
                    RowNum = rowNum
                    HSpaceExes = defaultArg hspaces Spaces.Zero
                    VSpaceExes = defaultArg vspaces Spaces.Zero
                    Sheet_PlaceTable = Imposing.Sheet_PlaceTable.Trim_CenterTable margin
                    DesiredSizeOp = cellSize
                    IsRepeated = false
                    Background = Background.Size FsSize.MAXIMUN
                    UseBleed = useBleed } 

            Reuses.Impose
                (fun ops -> args )
                    
            |> Reuse.rename 
                "MergeTwoPages"
                [
                    "imposingArguments" => args.ToString()
                ]


     

        

        static member private AddBackgroundOrForeground(backgroundFile: BackgroundFile, choice: BackgroundOrForeground) =
            (fun flowModel (doc: SplitDocument) ->


                let backgroundInfos =

                    let pageBoxs = BackgroundFile.getPageBoxes backgroundFile
                    let totalPageNumber = pageBoxs.Length
                    let reader = new PdfDocument(new PdfReader(backgroundFile.ClearedPdfFile.Path))
          
                    {|  
                        Close = fun () -> reader.Close()
                        GetPageBoxAndXObject = fun (pageNumber: int) -> 
                            let index = (pageNumber-1) % totalPageNumber
                            pageBoxs.[index], (reader.GetPage(index+1).CopyAsFormXObject(doc.Writer))
                    |}

                let reader = doc.Reader
                PdfDocument.getPages reader
                |> List.mapi (fun i readerPage ->
                    let backgroundPageBox, backgroundXObject = backgroundInfos.GetPageBoxAndXObject(i+1)
                    let readerPageBox = readerPage.GetActualBox()
                    let readerXObject = readerPage.CopyAsFormXObject(doc.Writer)

                    let pageSize = 
                        readerPage.GetActualBox()
                        |> PageSize

                    let writerPage = doc.Writer.AddNewPage(pageSize)
                    writerPage.SetPageBoxToPage(readerPage) |> ignore
                    let pdfCanvas = new PdfCanvas(writerPage)
                    let bk_x = readerPageBox.GetX() - backgroundPageBox.GetX() 
                    let bk_y = readerPageBox.GetY() - backgroundPageBox.GetY() 

                    match choice with 
                    | BackgroundOrForeground.Background ->
                        pdfCanvas
                            .AddXObjectAbs(backgroundXObject, bk_x , bk_y)
                            .AddXObjectAbs(readerXObject, 0.f, 0.f)

                    | BackgroundOrForeground.Foreground -> 
                        pdfCanvas
                            .AddXObjectAbs(readerXObject, 0.f, 0.f)
                            .AddXObjectAbs(backgroundXObject, bk_x, bk_y)
                ) |> ignore

                backgroundInfos.Close()

            )
            |> reuse 
                "AddBackgroundOrForeground"
                [
                    "backgroundFile" => backgroundFile.ToString()
                    "layer" => choice.ToString()
                ]

        static member AddBackground(pageBoxKind: PageBoxKind, rectOptions: PdfCanvasAddRectangleArguments -> PdfCanvasAddRectangleArguments, ?margin) =
            (fun flowModel (doc: SplitDocument) ->
                let reader = doc.Reader
                PdfDocument.getPages reader
                |> List.mapi (fun i readerPage ->

                    let readerPageBox = 
                        match margin with 
                        | Some margin ->
                            readerPage.GetPageBox(pageBoxKind)
                            |> Rectangle.applyMargin margin
                        | None -> 
                            readerPage.GetPageBox(pageBoxKind)

                    let readerXObject = readerPage.CopyAsFormXObject(doc.Writer)

                    let pageSize = 
                        readerPage.GetActualBox()
                        |> PageSize

                    let writerPage = doc.Writer.AddNewPage(pageSize)
                    writerPage.SetPageBoxToPage(readerPage) |> ignore
                    let pdfCanvas = new PdfCanvas(writerPage)
                    PdfCanvas.useCanvas pdfCanvas (
                        PdfCanvas.addRectangle readerPageBox rectOptions
                    ) 

                    pdfCanvas
                        .AddXObjectAbs(readerXObject, 0.f, 0.f)

                ) |> ignore
            )
            |> reuse 
                "AddBackground"
                [
                    "pageBoxKind" => pageBoxKind.ToString()
                    "rectOptions" => (rectOptions PdfCanvasAddRectangleArguments.DefaultValue).ToString()
                    "margin"      => (defaultArg margin Margin.Zero).LoggingText
                ]


        static member AddBackground (backgroundFile: PdfFile) =
            Reuses.AddBackgroundOrForeground(BackgroundFile.Create backgroundFile, BackgroundOrForeground.Background)

        static member AddForeground (foregroundFile: PdfFile) =
            Reuses.AddBackgroundOrForeground(BackgroundFile.Create foregroundFile, BackgroundOrForeground.Foreground)



    type PdfRunner with 
        static member Reuse(pdfFile, ?backupPdfPath) = 
            fun reuse ->
                PdfRunner.OneFileFlow(pdfFile, ?backupPdfPath = backupPdfPath) (Flow.Reuse reuse)

        static member PreImpose_Repeated_One(baseFArgs: _ImposingArguments) =
            let preImposeFlow =
                Reuses.Impose_Raw
                    ((fun _ ->
                        let args = (baseFArgs)

                        { args with
                              IsRepeated = true }
                    ),
                    offsetLists = None,
                    draw = false )
                |> Flow.Reuse


            runWithBackup
                (Path.GetTempFileName()
                 |> Path.changeExtension ".pdf")
                emptyPdf.Value
                preImposeFlow
            |> List.exactlyOne
            |> fun flowModel -> 
                match flowModel.UserState.GetSheets() with 
                | [ sheet ] -> RegularImposingSheet<_>.Create sheet
                | sheets -> failwithf "PreImpose_Repeated_One should generate one imposing sheet, but here is %d" sheets.Length


        /// default useBleed: false
        static member OneColumn(?backupPdfPath, ?margin, ?useBleed, ?spaces) =
            fun pdfFile ->
                let reuse =
                    Reuses.OneColumn(?margin = margin, ?useBleed = useBleed, ?spaces = spaces)

                PdfRunner.Reuse(pdfFile,?backupPdfPath = backupPdfPath) reuse
    
        /// default useBleed: false
        static member OneRow(?backupPdfPath, ?margin, ?useBleed, ?spaces) =
            fun pdfFile ->
                let reuse =
                    Reuses.OneRow(?margin = margin, ?useBleed = useBleed, ?spaces = spaces)

                PdfRunner.Reuse(pdfFile,?backupPdfPath = backupPdfPath) reuse


    