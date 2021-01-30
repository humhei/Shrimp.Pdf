namespace Shrimp.Pdf

open System.Collections.Generic

#nowarn "0104"
open Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Pdf.Canvas.Parser
open Shrimp.Pdf.DSL
open iText.Kernel.Geom
open System
open System.IO
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf
open Shrimp.FSharp.Plus


// number in page sequence must be bigger than 0
[<RequireQualifiedAccess>]
type PageNumSequenceToken =
    | PageNum of int 
    | PageNumWithRotation of int * Rotation
with 
    member x.PageNumValue = 
        match x with 
        | PageNumSequenceToken.PageNum pageNum -> pageNum
        | PageNumSequenceToken.PageNumWithRotation (pageNum, _) -> pageNum

// number in page sequence must be bigger than 0
type PageNumSequence = private PageNumSequence of AtLeastOneList<PageNumSequenceToken>
with 
    member x.Value = 
        let (PageNumSequence value) = x
        value.AsList

    static member Create (sequence: int list) =
        if List.exists (fun pageNumber -> pageNumber <= 0) sequence then failwithf "number in page sequence %A must be bigger than 0" sequence
        sequence
        |> List.map PageNumSequenceToken.PageNum
        |> AtLeastOneList.Create
        |> PageNumSequence

    static member Create (sequence: (int * Rotation) list) =
        let pageNumbers = List.map fst sequence
        if List.exists (fun pageNumber -> pageNumber <= 0) pageNumbers then failwithf "number in page sequence %A must be bigger than 0" sequence
        sequence
        |> List.map PageNumSequenceToken.PageNumWithRotation
        |> AtLeastOneList.Create
        |> PageNumSequence

// number in page sequence must be bigger than 0
type CopiedNumSequence = private CopiedNumSequence of AtLeastOneList<int>
with 
    member x.Value = 
        let (CopiedNumSequence value) = x
        value.AsList

    static member Create(sequence: int list) =
        if List.exists (fun pageNumber -> pageNumber <= 0) sequence then failwithf "number in sequence %A must be bigger than 0" sequence
        sequence
        |> AtLeastOneList.Create
        |> CopiedNumSequence 



type PageResizingScalingOptions =
    | Uniform = 0
    | Anamorphic = 1

type PageResizingRotatingOptions =
    | Keep = 0
    | ColckwiseIfNeeded = 1
    | CounterColckwiseIfNeeded = 2




type PageTilingResultCount = PageTilingResultCount of int
with 
    member x.Value = 
        let (PageTilingResultCount count) = x
        count

[<AutoOpen>]
module _Reuses =

    let private reuse name paramters f = Reuse(f = f, flowName = FlowName.Override(name, paramters))

    type PageInsertingOptions =
        | BeforePoint = 0
        | AfterPoint = 1


    type Reuses =
        static member Insert(insertingFile: string, ?insertingFilePageSelector: PageSelector, ?pageInsertingOptions: PageInsertingOptions, ?insertingPoint: SinglePageSelectorExpr) =
            let insertingFilePageSelector = defaultArg insertingFilePageSelector PageSelector.All
            let pageInsertingOptions = defaultArg pageInsertingOptions PageInsertingOptions.AfterPoint
            let singlePageSelectorExpr = defaultArg insertingPoint (SinglePageSelectorExpr.End 1)

            fun flowModel (splitDocument: SplitDocument) ->
                if Path.GetFullPath(insertingFile) = Path.GetFullPath(splitDocument.ReaderPath) 
                then failwith "Cannot insert file to self"

                let numberOfPages = splitDocument.Reader.GetNumberOfPages()
                let pageNum =
                    splitDocument.Reader.GetPageNumber singlePageSelectorExpr

                let tryCopyPages (reader: PdfDocument) pageFrom pageTo =
                    if pageTo >= pageFrom
                    then reader.CopyPagesTo(pageFrom, pageTo, splitDocument.Writer) |> ignore


                match pageInsertingOptions with
                | PageInsertingOptions.AfterPoint ->
                    tryCopyPages splitDocument.Reader 1 pageNum 
                    

                    let readerResource = new PdfDocument(new PdfReader(insertingFile))
                    let readerResourcePageNumbers = readerResource.GetPageNumbers(insertingFilePageSelector)

                    for readerResourcePageNumber in readerResourcePageNumbers do
                        let page = readerResource.GetPage(readerResourcePageNumber).CopyTo(splitDocument.Writer)
                        splitDocument.Writer.AddPage(page) |> ignore
                    readerResource.Close()


                    tryCopyPages splitDocument.Reader (pageNum + 1) numberOfPages

                | PageInsertingOptions.BeforePoint ->
                    tryCopyPages splitDocument.Reader 1 (pageNum - 1)
                    

                    let readerResource = new PdfDocument(new PdfReader(insertingFile))
                    let readerResourcePageNumbers = readerResource.GetPageNumbers(insertingFilePageSelector)

                    for readerResourcePageNumber in readerResourcePageNumbers do
                        let page = readerResource.GetPage(readerResourcePageNumber).CopyTo(splitDocument.Writer)
                        splitDocument.Writer.AddPage(page) |> ignore

                    readerResource.Close()


                    tryCopyPages splitDocument.Reader (pageNum) numberOfPages
                

            |> reuse 
                "Insert"
                ["insertingFile" => insertingFile.ToString()
                 "insertingFilePageSelector" => insertingFilePageSelector.ToString()
                 "pageInsertingOptions" => pageInsertingOptions.ToString()
                 "singlePageSelectorExpr" => singlePageSelectorExpr.ToString()]


        static member Impose (fArgs) =
            let imposingArguments = ImposingArguments.Create fArgs

            fun flowModel (splitDocument: SplitDocument) ->
                let imposingDocument = new ImposingDocument(splitDocument, imposingArguments)
                imposingDocument.Build()

                imposingDocument.Draw()
                (imposingDocument)

            |> reuse 
                "Impose"
                ["imposingArguments" => imposingArguments.ToString()]

        static member MovePageBoxToOrigin(pageSelector: PageSelector) =

            fun flowModel (splitDocument: SplitDocument) ->
                let selectedPageNumbers = splitDocument.Reader.GetPageNumbers(pageSelector) 

                PdfDocument.getPages splitDocument.Reader
                |> List.iteri (fun i page ->
                    let pageNum = i + 1
                    if List.contains pageNum selectedPageNumbers 
                    then 
                        let pageBox = page.GetActualBox()
                        let width = pageBox.GetWidthF()
                        let height = pageBox.GetHeightF()

                        let xobject = page.CopyAsFormXObject(splitDocument.Writer)
                        let newPage = splitDocument.Writer.AddNewPage(PageSize(Rectangle.create 0. 0. width height))
                        let canvas = new PdfCanvas(newPage)
                        canvas.AddXObject(xobject, -pageBox.GetX(), -pageBox.GetY())
                        |> ignore
                    else 
                        let page = page.CopyTo(splitDocument.Writer)
                        splitDocument.Writer.AddPage(page) |> ignore
                )

            |> reuse
                "MovePageBoxToOrigin"
                ["pageSelector" => pageSelector.ToString()]


        /// e.g. [1; 3; 5] will pick page1, page3, page5
        static member SequencePages (pageNumSequence: PageNumSequence) =
            fun (flowModel: FlowModel<_>) (splitDocument: SplitDocument) ->
                let duplicatedReaderPages = 
                    splitDocument.Reader
                    |> PdfDocument.getPages
                    |> List.mapi (fun i page ->
                        let pageNum = i + 1
                        pageNumSequence.Value
                        |> List.filter (fun (token) -> token.PageNumValue = pageNum)
                        |> List.map (fun token -> token, page)
                    )|> List.concat

                if duplicatedReaderPages.Length = 0 
                then failwithf "Invalid sequence %A, should exists a sequence number >= 1 and <= %d" pageNumSequence (splitDocument.Reader.GetNumberOfPages())

                let pdfPageCache = new Dictionary<int, PdfPage>()
                let xObjectCache = new Dictionary<int, Xobject.PdfFormXObject>()

                for (token, page) in duplicatedReaderPages do
                    let rec loop token = 
                        match token with 
                        | PageNumSequenceToken.PageNum _ ->
                            let page = 
                                match pdfPageCache.TryGetValue token.PageNumValue with 
                                | true, page -> page
                                | false, _ ->
                                    let writerPageResource = page.CopyTo(splitDocument.Writer)
                                    pdfPageCache.Add(token.PageNumValue, writerPageResource)
                                    writerPageResource

                            splitDocument.Writer.AddPage(page) |> ignore
                
                        | PageNumSequenceToken.PageNumWithRotation (_ , rotation) ->
                            match rotation with 
                            | Rotation.None ->
                                loop (PageNumSequenceToken.PageNum token.PageNumValue)
                            | _ ->
                                let xobject =
                                    match xObjectCache.TryGetValue token.PageNumValue with 
                                    | true, xobject -> xobject
                                    | false, _ ->
                                        let xobject = page.CopyAsFormXObject(splitDocument.Writer)
                                        xObjectCache.Add(token.PageNumValue, xobject)
                                        xobject

                                let pageBox = page.GetActualBox()

                                let affineTransform =
                                    let angle = Rotation.getAngle rotation
                                    let x = pageBox.GetXF()
                                    let y = pageBox.GetYF()
                                    let affineTransfrom_Rotate = AffineTransform.GetRotateInstance(Math.PI / -180. * angle, x, y)

                                    let affineTransform_Translate = 
                                        { ScaleX = 1. 
                                          ScaleY = 1. 
                                          TranslateX = -x
                                          TranslateY = -y
                                          ShearX = 0.
                                          ShearY = 0. }
                                        |> AffineTransformRecord.toAffineTransform

                                    affineTransfrom_Rotate.PreConcatenate(affineTransform_Translate)
                                    affineTransfrom_Rotate



                                let newPage = 
                                    let newPageSize =
                                        affineTransform.Transform(pageBox)

                                    splitDocument.Writer.AddNewPage(PageSize(newPageSize))

                                let canvas = new PdfCanvas(newPage)

                                canvas.AddXObject(xobject,AffineTransformRecord.ofAffineTransform affineTransform) |> ignore

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
                    loop token

            |> reuse 
                ("SequencePages" )
                [ "pageNumSequence" => pageNumSequence.ToString() ]


        static member Rotate (pageSelector: PageSelector, rotation: Rotation) =
            Reuse.Factory(fun flowModel splitDocument ->
                let selectedPageNums = splitDocument.Reader.GetPageNumbers(pageSelector)

                let pageNumSequence = 
                    [ 1 .. splitDocument.Reader.GetNumberOfPages() ]
                    |> List.map (fun pageNum -> 
                        if List.contains pageNum selectedPageNums
                        then (pageNum, rotation)
                        else (pageNum, Rotation.None))
                    |> PageNumSequence.Create

                Reuses.SequencePages(pageNumSequence)
            )

            |> Reuse.rename
                "Rotate"
                [ "pageSelector" => pageSelector.ToString()
                  "rotation" => rotation.ToString() ]

        static member Resize (pageSelector: PageSelector, pageBoxKind: PageBoxKind, size: FsSize) =
            fun flowModel (splitDocument: SplitDocument) ->
                let selectedPageNumbers = splitDocument.Reader.GetPageNumbers(pageSelector) 

                PdfDocument.getPages splitDocument.Reader
                |> List.iteri (fun i page ->
                    let pageNum = i + 1
                    if List.contains pageNum selectedPageNumbers 
                    then 
                        let actualBox = page.GetActualBox()
                        let pageBox = page.GetPageBox(pageBoxKind)

                        let affineTransform_Scale = 
                            AffineTransform.GetScaleInstance(size.Width / pageBox.GetWidthF(), size.Height / pageBox.GetHeightF())

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
                )

            |> reuse 
                "Resize"
                ["pageSelector" => pageSelector.ToString()
                 "pageBoxKind" => pageBoxKind.ToString() 
                 "size" => size.ToString() ]

        static member Resize (pageSelector: PageSelector, pageResizingRotatingOptions: PageResizingRotatingOptions, pageResizingScalingOptions: PageResizingScalingOptions, size: FsSize) =
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
                                match FsSize.ofRectangle actualBox, size with
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

                            if pageNumbers.IsEmpty then Reuse.dummy()
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
                    Reuses.Resize(pageSelector, PageBoxKind.ActualBox, size)
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
                                "Resize,",
                                [
                                    "pageSelector" => pageSelector.ToString()
                                    "size" => size.ToString()
                                ]
                            ),
                        f = 
                            fun flowModel splitDocument ->
                            let selectedPageNumbers = splitDocument.Reader.GetPageNumbers(pageSelector) 
                            PdfDocument.getPages splitDocument.Reader
                            |> List.iteri (fun i page ->
                                let pageNum = i + 1
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
                            )
                    )

            tryRotate()
            <+>
            resize()


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
                    |> PageNumSequence.Create

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



        static member TilePages (tileTable: TileTable, ?direction: Direction) =
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
                    let tileIndexer = TileIndexer.create tileTable (direction) (i - 1)
                    let tileBox = Rectangle.getTile tileIndexer actualBox

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
                |> PageNumSequence.Create

            Reuses.SequencePages(pageNumSequence)


