namespace Shrimp.Pdf

open System.Collections.Generic

#nowarn "0104"
open Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Colors
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

type Flip =
    | HFlip = 0
    | VFlip = 1




// number in page sequence must be bigger than 0
[<RequireQualifiedAccess>]
type PageNumSequenceToken =
    | PageNum of int 
    | PageNumWithRotation of int * Rotation
    | PageNumWithFlip of int * Flip
with 
    member x.PageNumValue = 
        match x with 
        | PageNumSequenceToken.PageNum pageNum -> pageNum
        | PageNumSequenceToken.PageNumWithRotation (pageNum, _) 
        | PageNumSequenceToken.PageNumWithFlip (pageNum, _) -> pageNum

    member x.Rotation =
        match x with 
        | PageNumSequenceToken.PageNum _ -> Rotation.None
        | PageNumSequenceToken.PageNumWithRotation (_, rotation) -> rotation
        | PageNumSequenceToken.PageNumWithFlip (_, _) -> Rotation.None

    member x.MapPageNumber(mapping) =
        match x with 
        | PageNumSequenceToken.PageNum pageNum -> mapping pageNum |> PageNumSequenceToken.PageNum
        | PageNumSequenceToken.PageNumWithRotation (pageNum, rotation) -> 
            (mapping pageNum, rotation) |> PageNumSequenceToken.PageNumWithRotation
        | PageNumSequenceToken.PageNumWithFlip (pageNum, flip) -> 
            (mapping pageNum, flip) |> PageNumSequenceToken.PageNumWithFlip
            

    member x.ShuffingText =
        match x with 
        | PageNumSequenceToken.PageNum pageNum -> pageNum.ToString()
        | PageNumSequenceToken.PageNumWithRotation (pageNum, rotation) -> 
            pageNum.ToString() + 
                match rotation with 
                | Rotation.None -> ""
                | Rotation.Clockwise -> ">"
                | Rotation.Counterclockwise -> "<"
                | Rotation.R180 -> "*"

        | PageNumSequenceToken.PageNumWithFlip (pageNum, flip) -> 
            pageNum.ToString() + 
                match flip with 
                | Flip.HFlip -> "$"
                | Flip.VFlip -> "%"

exception PageNumSequenceEmptyException of string

// number in page sequence must be bigger than 0
type PageNumSequence = private PageNumSequence of AtLeastOneList<PageNumSequenceToken>
with 
    member x.Value_Al1List = 
        let (PageNumSequence value) = x
        value

    member x.Value = 
        let (PageNumSequence value) = x
        value.AsList

    member x.MapPageNumber(mapping) = 
        x.Value_Al1List
        |> AtLeastOneList.map (fun m -> m.MapPageNumber mapping)
        |> PageNumSequence

    member x.ShuffingText = 
        x.Value
        |> List.mapi (fun i m -> m.ShuffingText)
        |> String.concat " "

    static member private EnsureSequenceNotEmpty(sequence: 'a list) =
        match sequence with 
        | [] -> raise (PageNumSequenceEmptyException "PageNumberSequence is empty")
        | _ -> ()

    static member Create (sequence: int list) =
        PageNumSequence.EnsureSequenceNotEmpty sequence

        if List.exists (fun pageNumber -> pageNumber <= 0) sequence then failwithf "number in page sequence %A must be bigger than 0" sequence
        sequence    
        |> List.map (PageNumSequenceToken.PageNum)
        |> AtLeastOneList.Create
        |> PageNumSequence

    static member Create (sequence: (int * Rotation) list) =
        PageNumSequence.EnsureSequenceNotEmpty sequence
        let pageNumbers = List.map fst sequence
        if List.exists (fun pageNumber -> pageNumber <= 0) pageNumbers then failwithf "number in page sequence %A must be bigger than 0" sequence
        sequence
        |> List.map (PageNumSequenceToken.PageNumWithRotation)
        |> AtLeastOneList.Create
        |> PageNumSequence

    static member Create(tokens: PageNumSequenceToken list) =   
        PageNumSequence.EnsureSequenceNotEmpty tokens
        tokens
        |> AtLeastOneList.Create
        |> PageNumSequence

    static member Concat(sequences: PageNumSequence al1List) =
        sequences
        |> AtLeastOneList.collect(fun m -> m.Value_Al1List)
        |> PageNumSequence

[<RequireQualifiedAccess>]
type EmptablePageNumSequenceToken =
    | EmptyPage 
    | PageNumSequenceToken of PageNumSequenceToken
with 
    member x.ShuffingText =
        match x with 
        | EmptablePageNumSequenceToken.EmptyPage -> "x"
        | EmptablePageNumSequenceToken.PageNumSequenceToken m -> m.ShuffingText 

    static member Create(pageNum: int) =
        EmptablePageNumSequenceToken.PageNumSequenceToken (PageNumSequenceToken.PageNum pageNum)

    static member Create(pageNum: int, rotation) =
        EmptablePageNumSequenceToken.PageNumSequenceToken (PageNumSequenceToken.PageNumWithRotation(pageNum, rotation))



// number in page sequence must be bigger than 0
type EmptablePageNumSequence = private EmptablePageNumSequence of AtLeastOneList<EmptablePageNumSequenceToken>
with 
    member x.Value_Al1List = 
        let (EmptablePageNumSequence value) = x
        value

    member x.Value = 
        let (EmptablePageNumSequence value) = x
        value.AsList

    member x.ShuffingText = 
        x.Value
        |> List.map (fun m -> m.ShuffingText)
        |> String.concat " "

    static member Create (sequence: int list) =
        if List.exists (fun pageNumber -> pageNumber <= 0) sequence then failwithf "number in page sequence %A must be bigger than 0" sequence
        sequence
        |> List.map (PageNumSequenceToken.PageNum >> EmptablePageNumSequenceToken.PageNumSequenceToken)
        |> AtLeastOneList.Create
        |> EmptablePageNumSequence

    static member Create (sequence: (int * Rotation) list) =
        let pageNumbers = List.map fst sequence
        if List.exists (fun pageNumber -> pageNumber <= 0) pageNumbers then failwithf "number in page sequence %A must be bigger than 0" sequence
        sequence
        |> List.map (PageNumSequenceToken.PageNumWithRotation >> EmptablePageNumSequenceToken.PageNumSequenceToken)
        |> AtLeastOneList.Create
        |> EmptablePageNumSequence

    static member Create (sequence: EmptablePageNumSequenceToken list) =
        sequence
        |> AtLeastOneList.Create
        |> EmptablePageNumSequence

    static member Create (sequence: PageNumSequence) =
        sequence.Value
        |> List.map EmptablePageNumSequenceToken.PageNumSequenceToken
        |> AtLeastOneList.Create
        |> EmptablePageNumSequence

    static member Concat(sequences: EmptablePageNumSequence al1List) =
        sequences
        |> AtLeastOneList.collect(fun m -> m.Value_Al1List)
        |> EmptablePageNumSequence

// number in page sequence must be bigger than 0
type CopiedNumSequence = private CopiedNumSequence of AtLeastOneList<int>
with 
    member x.Value = 
        let (CopiedNumSequence value) = x
        value.AsList

    member x.ShuffingText =
        x.Value
        |> List.mapi (fun i copyNumber ->
            List.replicate copyNumber (i+1)
        )
        |> List.concat
        |> List.map string
        |> String.concat " "

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

    type private BackgroundOrForeground =
        | Background = 0
        | Foreground = 1

    let private emptyPdf =
        lazy
            (let path = Path.GetTempPath() </> "empty.pdf"

             if File.exists path then
                 path
             else
                 let doc = new PdfDocument(new PdfWriter(path))
                 doc.AddNewPage() |> ignore
                 doc.Close()
                 path)



    type private PageSequeningUnion =
        | EmptyPage of targetPage: PdfPage option
        | Token of PageNumSequenceToken * PdfPage

    let private reuse name paramters f = Reuse(f = f, flowName = FlowName.Override(name, paramters))

    type PageInsertingOptions =
        | BeforePoint = 0
        | AfterPoint = 1


    type private CellIndex =
        { ColIndex: int 
          RowIndex: int 
          PageNum: int }

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

        static member InsertEmptyPagesToMultiple(multiple: int, ?pageInsertingOptions: PageInsertingOptions, ?insertingPoint: SinglePageSelectorExpr, ?pageSizeSelector: SinglePageSelectorExpr) =
            let pageInsertingOptions = defaultArg pageInsertingOptions PageInsertingOptions.AfterPoint
            let singlePageSelectorExpr = defaultArg insertingPoint (SinglePageSelectorExpr.End 1)
            let pageSizeSelector = defaultArg pageSizeSelector (SinglePageSelectorExpr.End 1)

            fun flowModel (splitDocument: SplitDocument) ->
             
                let numberOfPages = splitDocument.Reader.GetNumberOfPages()
                let pageNum =
                    splitDocument.Reader.GetPageNumber singlePageSelectorExpr

                let tryCopyPages (reader: PdfDocument) pageFrom pageTo =
                    if pageTo >= pageFrom
                    then reader.CopyPagesTo(pageFrom, pageTo, splitDocument.Writer) |> ignore


                let preparedInsertingEmptyPagesCount = 
                    numberOfPages % multiple


                match preparedInsertingEmptyPagesCount with 
                | 0 -> tryCopyPages splitDocument.Reader 1 numberOfPages
                | i ->
                    let emptyPageSize_Source = 
                        let pageNum = splitDocument.Reader.GetPageNumber(pageSizeSelector)
                        splitDocument.Reader.GetPage(pageNum)

                    match pageInsertingOptions with
                    | PageInsertingOptions.AfterPoint ->
                        tryCopyPages splitDocument.Reader 1 pageNum 

                        for _ = 1 to i do
                            let writePage = splitDocument.Writer.AddNewPage()
                            writePage.SetPageBoxToPage(emptyPageSize_Source)
                            |> ignore

                        tryCopyPages splitDocument.Reader (pageNum + 1) numberOfPages

                    | PageInsertingOptions.BeforePoint ->
                        tryCopyPages splitDocument.Reader 1 (pageNum - 1)

                        for _ = 1 to i do
                            let writePage = splitDocument.Writer.AddNewPage()
                            writePage.SetPageBoxToPage(emptyPageSize_Source)
                            |> ignore

                        tryCopyPages splitDocument.Reader (pageNum) numberOfPages
                

            |> reuse 
                "InsertEmptyPagesToNX"
                ["multiple" => multiple.ToString()
                 "pageInsertingOptions" => pageInsertingOptions.ToString()
                 "singlePageSelectorExpr" => singlePageSelectorExpr.ToString() ]

 


        static member private MovePageBoxToOrigin(pageSelector: PageSelector) =

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
        static member SequencePages (pageNumSequence: EmptablePageNumSequence) =
            fun (flowModel: FlowModel<_>) (splitDocument: SplitDocument) ->
                let unionTokens = 
                    let pages =  PdfDocument.getPages splitDocument.Reader
                    pageNumSequence.Value
                    |> List.mapi (fun i token ->
                        match token with 
                        | EmptablePageNumSequenceToken.EmptyPage -> 
                            let targetPage =
                                let previous = 
                                    pageNumSequence.Value.[0..i]
                                    |> List.rev
                                    |> List.tryPick(fun m -> 
                                        match m with 
                                        | EmptablePageNumSequenceToken.PageNumSequenceToken token -> 
                                            let pageNum = token.PageNumValue
                                            Some pages.[pageNum-1]
                                        | _ -> None
                                    )

                                match previous with 
                                | Some page -> Some page
                                | None ->
                                    let after =
                                        pageNumSequence.Value.[i..]
                                        |> List.tryPick(fun m -> 
                                            match m with 
                                            | EmptablePageNumSequenceToken.PageNumSequenceToken token -> 
                                                let pageNum = token.PageNumValue
                                                Some pages.[pageNum-1]
                                            | _ -> None
                                        )
                                    match after with 
                                    | Some after -> Some after
                                    | None -> None
                                    


                            (PageSequeningUnion.EmptyPage (targetPage))
                        | EmptablePageNumSequenceToken.PageNumSequenceToken token -> 
                            (PageSequeningUnion.Token (token, pages.[token.PageNumValue-1]))
                    )

                    //|> 
                    //|> List.mapi (fun i page ->
                    //    let pageNum = i + 1
                    //    pageNumSequence.Value
                    //    |> List.choose (fun (token) ->

                    //        | EmptablePageNumSequenceToken.PageNumSequenceToken token ->

                    //    )
                    //)|> List.concat

                if unionTokens.Length = 0 
                then failwithf "Invalid sequence %A, should exists a sequence number >= 1 and <= %d" pageNumSequence (splitDocument.Reader.GetNumberOfPages())

                let pdfPageCache = new Dictionary<int, PdfPage>()
                let xObjectCache = new Dictionary<int, Xobject.PdfFormXObject>()

                for unionToken in unionTokens do
                    let rec loop (token: PageSequeningUnion) = 
                        match token with 
                        | PageSequeningUnion.EmptyPage targetPage -> 
                            match targetPage with 
                            | Some targetPage ->
                                splitDocument.Writer.AddNewPage().SetPageBoxToPage(targetPage)

                            | None -> splitDocument.Writer.AddNewPage()
                            |> ignore

                        | PageSequeningUnion.Token (token, page) ->

                            match token with 
                            | PageNumSequenceToken.PageNum _ ->
                                let page = page.CopyTo(splitDocument.Writer)
                                    //match pdfPageCache.TryGetValue token.PageNumValue with 
                                    //| true, page -> page
                                    //| false, _ ->
                                    //    let writerPageResource = 
                                    //    pdfPageCache.Add(token.PageNumValue, writerPageResource)
                                    //    writerPageResource

                                splitDocument.Writer.AddPage(page) |> ignore
                
                            | PageNumSequenceToken.PageNumWithRotation (_ , rotation) ->
                                match rotation with 
                                | Rotation.None ->
                                    PageSequeningUnion.Token((PageNumSequenceToken.PageNum token.PageNumValue), page)
                                    |> loop 

                                | _ ->
                                    let xobject =
                                        match xObjectCache.TryGetValue token.PageNumValue with 
                                        | true, xobject -> xobject
                                        | false, _ ->
                                            let xobject = page.CopyAsFormXObject(splitDocument.Writer)
                                            xObjectCache.Add(token.PageNumValue, xobject)
                                            xobject

                                    let acutalBox = page.GetActualBox()

                                    let affineTransform =
                                        let angle = Rotation.getAngle rotation
                                        let x = acutalBox.GetXF()
                                        let y = acutalBox.GetYF()
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
                                            affineTransform.Transform(acutalBox)

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

                            | PageNumSequenceToken.PageNumWithFlip (_, flip) ->
                                let xobject =
                                    match xObjectCache.TryGetValue token.PageNumValue with 
                                    | true, xobject -> xobject
                                    | false, _ ->
                                        let xobject = page.CopyAsFormXObject(splitDocument.Writer)
                                        xObjectCache.Add(token.PageNumValue, xobject)
                                        xobject

                                let actualBox = page.GetActualBox()

                                let affineTransform =
                                    let x = actualBox.GetXF()
                                    let y = actualBox.GetYF()
                                    let affineTransfrom_Rotate = 
                                        match flip with 
                                        | Flip.HFlip -> 
                                            { ScaleX = -1.0
                                              ShearX = 0.0 
                                              ShearY = 0.0 
                                              ScaleY = 1.0
                                              TranslateX = 0.0
                                              TranslateY = 0.0 }
                                            
                                        | Flip.VFlip ->
                                            { ScaleX = 1.0
                                              ShearX = 0.0 
                                              ShearY = 0.0 
                                              ScaleY = -1.0
                                              TranslateX = 0.0
                                              TranslateY = 0.0 }
                                        |> AffineTransform.ofRecord

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
                                        affineTransform.Transform(actualBox)

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


                    loop unionToken

            |> reuse 
                ("SequencePages" )
                [ "pageNumSequence" => pageNumSequence.ToString() ]

        static member SequencePages (pageNumSequence: PageNumSequence) =
            Reuses.SequencePages(EmptablePageNumSequence.Create pageNumSequence)


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
                                "Resize,",
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


    
        static member private Impose_Raw (fArgs, offsetLists: option<FsPoint list list>, draw) =
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
                        
                    canvas.AddXObject(xobject, 0.f, 0.f)
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
                    let newPage = writer.AddNewPage(PageSize(page.GetPageSize()))

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

        static member ClearDirtyInfos() =
            Reuse.Factory(fun flowModel doc ->
                let rotateFlow =
                    let pageNumberSequence = 
                        PdfDocument.getPages doc.Reader
                        |> List.mapi(fun i page ->
                            let pageNum = i + 1
                            let rotation = 
                                match page.GetFsRotation() with
                                | Rotation.None -> Rotation.None
                                | Rotation.Counterclockwise -> Rotation.Counterclockwise
                                | Rotation.R180 -> Rotation.R180
                                | Rotation.Clockwise -> Rotation.Clockwise
                            PageNumSequenceToken.PageNumWithRotation(pageNum, rotation)
                        )

                    pageNumberSequence
                    |> List.tryFind(fun m -> Rotation.notNon m.Rotation)
                    |> function
                        | Some _ ->
                            Reuses.SequencePages(PageNumSequence.Create pageNumberSequence)
                            |> Some
                        | None -> None

                match rotateFlow with 
                | Some rotateFlow ->
                    rotateFlow
                    <+>
                    Reuses.MovePageBoxToOrigin(PageSelector.All)
                | None -> 
                    Reuses.MovePageBoxToOrigin(PageSelector.All)
            )
            |> Reuse.rename
                "ClearDirtyInfos"
                []
     

        static member private AddBackgroundOrForeground(backgroundFile: BackgroundFile, choice: BackgroundOrForeground) =
            (fun flowModel (doc: SplitDocument) ->
                let backgroundInfos =
                    let pageBoxs = BackgroundFile.getPageBoxes backgroundFile
                    let totalPageNumber = pageBoxs.Length
                    let reader = new PdfDocument(new PdfReader(backgroundFile.Value.Path))
          
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

                    let readerXObject = readerPage.CopyAsFormXObject(doc.Writer)

                    let pageSize = 
                        readerPage.GetPageSize()
                        |> PageSize

                    let writerPage = doc.Writer.AddNewPage(pageSize)
                    writerPage.SetPageBoxToPage(readerPage) |> ignore
                    let pdfCanvas = new PdfCanvas(writerPage)
                    match choice with 
                    | BackgroundOrForeground.Background ->
                        pdfCanvas
                            .AddXObject(backgroundXObject, -backgroundPageBox.GetX(), -backgroundPageBox.GetY())
                            .AddXObject(readerXObject, 0.f, 0.f)

                    | BackgroundOrForeground.Foreground -> 
                        pdfCanvas
                            .AddXObject(readerXObject, 0.f, 0.f)
                            .AddXObject(backgroundXObject, -backgroundPageBox.GetX(), -backgroundPageBox.GetY())
                ) |> ignore

                backgroundInfos.Close()

            )
            |> reuse 
                "AddBackgroundOrForeground"
                [
                    "backgroundFile" => backgroundFile.ToString()
                    "layer" => choice.ToString()
                ]

        static member AddBackground(pageBoxKind: PageBoxKind, rectOptions: PdfCanvasAddRectangleArguments -> PdfCanvasAddRectangleArguments) =
            (fun flowModel (doc: SplitDocument) ->
                let reader = doc.Reader
                PdfDocument.getPages reader
                |> List.mapi (fun i readerPage ->

                    let readerPageBox = readerPage.GetPageBox(pageBoxKind)
                    let readerXObject = readerPage.CopyAsFormXObject(doc.Writer)

                    let pageSize = 
                        readerPage.GetPageSize()
                        |> PageSize

                    let writerPage = doc.Writer.AddNewPage(pageSize)
                    writerPage.SetPageBoxToPage(readerPage) |> ignore
                    let pdfCanvas = new PdfCanvas(writerPage)
                    PdfCanvas.useCanvas pdfCanvas (
                        PdfCanvas.addRectangle readerPageBox rectOptions
                    ) 

                    pdfCanvas
                        .AddXObject(readerXObject, 0.f, 0.f)

                ) |> ignore
            )
            |> reuse 
                "AddBackground"
                [
                    "pageBoxKind" => pageBoxKind.ToString()
                    "rectOptions" => (rectOptions PdfCanvasAddRectangleArguments.DefaultValue).ToString()
                ]


        static member AddBackground (backgroundFile: PdfFile) =
            Reuses.AddBackgroundOrForeground(BackgroundFile.Create backgroundFile, BackgroundOrForeground.Background)

        static member AddForeground (backgroundFile: PdfFile) =
            Reuses.AddBackgroundOrForeground(BackgroundFile.Create backgroundFile, BackgroundOrForeground.Foreground)



    type PdfRunner with 
        
        static member Reuse(pdfFile, ?backupPdfPath) = 
            fun reuse ->
                PdfRunner.OneFileFlow(pdfFile, ?backupPdfPath = backupPdfPath) (Flow.Reuse reuse)

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


    