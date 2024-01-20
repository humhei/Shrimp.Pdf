namespace Shrimp.Pdf

open System.Collections.Generic
open iText.Kernel.Pdf.Layer
open Shrimp.Pdf.Parser.Helper

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

type RotationIfNeeded =
    | Clockwise = 0
    | Counterclockwise = 1

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


type PreImposer =
    /// allowRedirectCellSize = true -> allow cellSize exceeding to page
    static member VirtualPreImpose_RepeatOne(cellSize: FsSize, imposingArguments: _ImposingArguments, allowRedirectCellSize) =
        let virtualDocument: VirtualDocument = 
            { Pages =  [
                    VirtualPdfPage.OfCellSize cellSize
                ]
            }

        let imposingArguments = 
            ImposingArguments.Create(fun _ ->
                { imposingArguments with
                    DesiredSizeOp = Some cellSize
                    IsRepeated = true }
            )

        let document = 
            ImposingDocument
                (VirtuableSplitDocument.Virtual virtualDocument, imposingArguments, allowRedirectCellSize = Some allowRedirectCellSize)
        
        document.Build()
        document.GetFirstSheet()
        |> RegularImposingSheet.Create

    /// allowRedirectCellSize = true -> allow cellSize exceeding to page
    static member VirtualPreImpose(pages: FsSize list, imposingArguments: _ImposingArguments, allowRedirectCellSize) =
        let virtualDocument: VirtualDocument = 
            { Pages =
                pages
                |> List.map(fun page ->
                    VirtualPdfPage.OfCellSize page
                )
            }

        let imposingArguments = 
            ImposingArguments.Create(fun _ -> imposingArguments)

        let document = 
            ImposingDocument
                (VirtuableSplitDocument.Virtual virtualDocument, imposingArguments, allowRedirectCellSize = Some allowRedirectCellSize)

        document.Build()
        document.GetSheets()

[<AutoOpen>]
module _Reuses =

    type BackgroundOrForeground =
        | Background = 0
        | Foreground = 1


    let private emptyPdf = lazy PdfUtils.NewTempEmptyPdf()



    let private reuse name parameters f = Reuse(f = f, flowName = FlowName.Override(name, parameters))

    
    [<RequireQualifiedAccess>]
    type PageInsertingOptionsEx =
        | BeforePointCase of SinglePageSelectorExpr
        | AfterPointCase  of SinglePageSelectorExpr
        /// e.g multple = 2, will insert pages before even pages
        | Before_InMultiple of multiple: int 
        /// e.g multple = 2, will insert pages after even pages
        | After_InMultiple of multiple: int 

    with 
        static member BeforePoint(?insertPoint) =
            PageInsertingOptionsEx.BeforePointCase(defaultArg insertPoint (SinglePageSelectorExpr.Begin 1))

        static member AfterPoint(?insertPoint) =
            PageInsertingOptionsEx.AfterPointCase(defaultArg insertPoint (SinglePageSelectorExpr.End 1))

        static member BeforeForEach(?multiple) =
            PageInsertingOptionsEx.Before_InMultiple(defaultArg multiple 1)

        static member AfterForEach(?multiple) =
            PageInsertingOptionsEx.After_InMultiple(defaultArg multiple 1)

    type PageInsertingOptions =
        | BeforePoint = 0
        | AfterPoint = 1



    type private CellIndex =
        { ColIndex: int 
          RowIndex: int 
          PageNum: int }


    type PdfDictionary with 
        member x.PutDSL(key, value) =
            x.Put(key, value)
            |> ignore
            x

    type AiLayerOptions =
        { Title: string  
          Color: FsDeviceRgb 
          Dimmed: bool 
          Editable: bool
          Preview: bool 
          Printed: bool
          Visible: bool }
    with 
        static member Create(title: string, ?color: FsDeviceRgb, ?dimmed, ?editable, ?preview, ?printed, ?visible) =
            let dimmed = defaultArg dimmed false
            let editable = defaultArg editable true
            let preview   = defaultArg preview true
            let printed   = defaultArg printed true
            let visible   = defaultArg visible true
            let color     = defaultArg color <| FsDeviceRgb.Create(10, 128, 255)

            {
                Title  = title
                Color  = color
                Dimmed = dimmed
                Editable = editable
                Preview = preview
                Printed = printed
                Visible = visible
            }


    /// Cannot get worked
    [<System.ObsoleteAttribute("Cannot get worked")>]
    type AiLayer private (pdfDocument: PdfDocument, dictionary: PdfDictionary, ops: AiLayerOptions) =
        inherit FsPdfObjectWrapper<PdfDictionary>(dictionary, true)

        member x.Title     =    ops.Title
        member x.Color     =    ops.Color
        member x.Dimmed    =    ops.Dimmed
        member x.Editable  =    ops.Editable

        member x.Preview   =    ops.Preview
        member x.Printed   =    ops.Printed
        member x.Visible   =    ops.Visible

        member x.PdfDocument = pdfDocument

        member private x.PutAll() =
            let colorValues =
                [|
                    2570
                    32896
                    65535
                |]
                |> Array.map (fun m -> PdfNumber m :> PdfObject)
                |> PdfArray

            x
                .GetPdfObject()
                .PutDSL(PdfName.Color, colorValues)
                .PutDSL(PdfName("Dimmed"), PdfBoolean x.Dimmed)
                .PutDSL(PdfName("Editable"), PdfBoolean x.Editable)
                .PutDSL(PdfName("Preview"), PdfBoolean x.Preview)
                .PutDSL(PdfName("Printed"), PdfBoolean x.Printed)
                .PutDSL(PdfName("Title"), PdfString x.Title)
                .PutDSL(PdfName("Visible"), PdfBoolean x.Visible)


        interface IPdfOCG with 
            member x.GetPdfObject() = dictionary
            member x.GetIndirectReference() = x.GetPdfObject().GetIndirectReference()

        static member Create(pdfDocument: PdfDocument, ops) =
            let aiLayer = 
                new AiLayer(
                    pdfDocument,
                    new PdfDictionary(),
                    ops)


            aiLayer.MakeIndirect(pdfDocument) |> ignore
            aiLayer.PutAll() |> ignore
            aiLayer



    [<RequireQualifiedAccess>]
    type LayerUnion =
        | Pdf of PdfLayer
        | AI of AiLayer

    let private BDC = iText.IO.Source.ByteUtils.GetIsoBytes("BDC\n")
    let private EMC = iText.IO.Source.ByteUtils.GetIsoBytes("EMC\n")

    type PdfCanvas with 
        member x.BeginLayerUnion(layer: LayerUnion) =
            match layer with 
            | LayerUnion.AI v ->
                let name = x.GetResources().AddProperties(v.GetPdfObject());
                x
                    .GetContentStream()
                    .GetOutputStream()
                    .Write(PdfName("Layer"))
                    .WriteSpace().Write(name)
                    .WriteSpace()
                    .WriteBytes(BDC)
                    .WriteNewLine
                |> ignore

                x

            | LayerUnion.Pdf v -> x.BeginLayer(v)


        member x.EndLayerUnion(layer: LayerUnion) =
            match layer with 
            | LayerUnion.AI v ->
                x.GetContentStream().GetOutputStream().WriteBytes(EMC).WriteNewLine()
                |> ignore
                x

            | _ -> x.EndLayer()


    [<RequireQualifiedAccess>]
    type LayerOptions =
        | Pdf of string
        | AI of AiLayerOptions
    with
        member x.CreateLayer(doc: PdfDocument) =    
            match x with 
            | Pdf layerName -> PdfLayer(layerName, doc) |> LayerUnion.Pdf
            | AI ops ->        AiLayer.Create(doc, ops) |> LayerUnion.AI

    [<RequireQualifiedAccess>]
    type BackgroundAddingLayerOptions =
        | Pdf of currentLayerName: string * backgroundLayerName: string
        | AI  of currentLayer: AiLayerOptions * backgroundLayer: AiLayerOptions
    with 
        static member Create(currentLayerName, backgroundLayerName) =
            BackgroundAddingLayerOptions.Pdf(currentLayerName, backgroundLayerName)

        member x.CurrentLayer =
            match x with 
            | Pdf (v, _) -> LayerOptions.Pdf v
            | AI  (v, _) -> LayerOptions.AI  v

        member x.BackgroundLayer =
            match x with 
            | Pdf (_, v) -> LayerOptions.Pdf v
            | AI  (_, v) -> LayerOptions.AI  v


            

    [<RequireQualifiedAccess>]
    type ResizeToSameSize_Target =
        | Size of FsSize
        | MinimumPageSize
        | MaximunPageSize

    [<RequireQualifiedAccess>]
    type SetBackgroundSize_PageBoxSetter =
        | SetArtBoxToContents
        | SetTrimBoxToContents
        | Non

    type Reuses with

        static member InsertEx(insertingFile: string, ?insertingFilePageSelector: PageSelector, ?pageInsertingOptions: PageInsertingOptionsEx) =
            let insertingFilePageSelector = defaultArg insertingFilePageSelector PageSelector.All
            let pageInsertingOptions = defaultArg pageInsertingOptions (PageInsertingOptionsEx.AfterPoint())

            fun flowModel (splitDocument: SplitDocument) ->
                if Path.GetFullPath(insertingFile) = Path.GetFullPath(splitDocument.ReaderPath) 
                then failwith "Cannot insert file to self"
                let readerResource = new PdfDocument(new PdfReader(insertingFile))
                let resourcePages = readerResource.GetPages(insertingFilePageSelector)
                let readerPages = splitDocument.Reader.GetPages()

                let allReaderPages =
                    match pageInsertingOptions with 
                    | PageInsertingOptionsEx.AfterPointCase (expr) ->
                        let middleLeftPageNum = 
                            splitDocument.Reader.GetPageNumbers(PageSelectorExpr.SinglePage expr)
                            |> List.exactlyOne_DetailFailingText

                        let afterPages = readerPages.[middleLeftPageNum..]
                        readerPages.[0..middleLeftPageNum-1] @ resourcePages @ afterPages


                    | PageInsertingOptionsEx.BeforePointCase (expr) ->
                        let middleLeftPageNum = 
                            splitDocument.Reader.GetPageNumbers(PageSelectorExpr.SinglePage expr)
                            |> List.exactlyOne_DetailFailingText

                        let afterPages = readerPages.[middleLeftPageNum-1..]
                        readerPages.[0 .. middleLeftPageNum-2] @ resourcePages @ afterPages

                    | PageInsertingOptionsEx.Before_InMultiple (multiple) ->
                        let pages = 
                            readerPages
                            |> List.chunkBySize multiple
                            |> List.mapi(fun i readerPages ->
                                let resourcePages_indexes =
                                    [0..readerPages.Length-1]
                                    |> List.map(fun j ->
                                        let index = i * multiple + j
                                        index % resourcePages.Length
                                    )

                                let resourcePages = 
                                    resourcePages_indexes
                                    |> List.map(fun i -> resourcePages.[i])

                                resourcePages @ readerPages
                            )
                            |> List.concat

                        pages

                    | PageInsertingOptionsEx.After_InMultiple (multiple) ->
                        let pages = 
                            readerPages
                            |> List.chunkBySize multiple
                            |> List.mapi(fun i readerPages ->
                                let resourcePages_indexes =
                                    [0..multiple-1]
                                    |> List.map(fun j ->
                                        let index = i * multiple + j
                                        index % resourcePages.Length
                                    )

                                let resourcePages = 
                                    resourcePages_indexes
                                    |> List.map(fun i -> resourcePages.[i])

                                readerPages @ resourcePages 
                            )
                            |> List.concat


                        pages



                allReaderPages
                |> List.iter(fun readerPage ->
                    let page = readerPage.CopyTo(splitDocument.Writer)
                    splitDocument.Writer.AddPage(page) |> ignore

                )
            
                readerResource.Close()
                

            |> reuse 
                "Insert"
                ["insertingFile" => insertingFile.ToString()
                 "insertingFilePageSelector" => insertingFilePageSelector.ToString()
                 "pageInsertingOptions" => pageInsertingOptions.ToString() ]

        static member Insert(insertingFile: string, ?insertingFilePageSelector: PageSelector, ?pageInsertingOptions: PageInsertingOptions, ?insertingPoint) =
            let pageInsertingOptions = defaultArg pageInsertingOptions PageInsertingOptions.AfterPoint
            let insertingPoint = defaultArg insertingPoint (SinglePageSelectorExpr.End 1)
            let pageInsertingOptionsEx =
                match pageInsertingOptions with 
                | PageInsertingOptions.AfterPoint ->
                    PageInsertingOptionsEx.AfterPointCase(insertingPoint)

                | PageInsertingOptions.BeforePoint ->
                    PageInsertingOptionsEx.BeforePointCase(insertingPoint)
                    

            Reuses.InsertEx(insertingFile, ?insertingFilePageSelector = insertingFilePageSelector, pageInsertingOptions = pageInsertingOptionsEx)


        static member InsertEmptyPages(fEmptyPageCount, ?pageInsertingOptions: PageInsertingOptions) =
            let pageInsertingOptions = defaultArg pageInsertingOptions (PageInsertingOptions.AfterPoint)

            fun flowModel (splitDocument: SplitDocument) ->
                PdfDocument.getPages splitDocument.Reader
                |> List.iteri(fun i page ->
                    let pageNum = i + 1

                    let addEmptyPages() =
                        let emptyPageNumberCount = fEmptyPageCount (PageNumber pageNum)
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
                            match pageNum.Value with 
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

        static member Resize (pageSelector: PageSelector, pageBoxKind: PageBoxKind, fSize: PdfPage -> PageNumber -> FsSize) =
            
            fun flowModel (splitDocument: SplitDocument) ->
                let selectedPageNumbers = splitDocument.Reader.GetPageNumbers(pageSelector) 

                PdfDocument.getPages splitDocument.Reader
                |> List.mapi (fun i page ->
                    let pageNum = i + 1
                    
                    let size = 
                        let unroundSize =  (fSize page (PageNumber pageNum))
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
                fSize = (fun _ _ -> size)
            )
            |> Reuse.rename
                "Resize"
                ["pageSelector" => pageSelector.ToString()
                 "pageBoxKind" => pageBoxKind.ToString() 
                 "size" => size.ToString() ]

        static member Resize (pageSelector: PageSelector, pageResizingRotatingOptions: PageResizingRotatingOptions, pageResizingScalingOptions: PageResizingScalingOptions, fSize: PdfPage -> PageNumber -> FsSize) =
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
                                match FsSize.ofRectangle actualBox, fSize page (PageNumber pageNum) with
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
                                    PageSelector.Numbers (List.ofSeq pageNumbers)

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
                                let size = (fSize page (PageNumber pageNum)) |> RoundedSize.Create
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
                                else 
                                    let newPage = page.CopyTo(splitDocument.Writer)
                                    splitDocument.Writer.AddPage(newPage)
                                    |> ignore

                                size
                            )
                    )

            tryRotate()
            <+>
            (resize())
           
        static member ResizeToSameSize (target: ResizeToSameSize_Target) =
            match target with 
            | ResizeToSameSize_Target.Size size ->
                Reuses.Resize(PageSelector.All, PageBoxKind.AllBox, size)

            | _ ->
                Reuse.Factory(fun flowModel document ->
                    let size =
                        let pageBoxes = 
                            document.Reader.GetPages()
                            |> List.map(fun m -> m.GetActualBox())
                            |> List.map(fun m -> FsSize.ofRectangle m)

                        let heights =
                            pageBoxes
                            |> List.map(fun m -> m.Height)

                        let widths =
                            pageBoxes
                            |> List.map(fun m -> m.Width)

                        match target with 
                        | ResizeToSameSize_Target.MaximunPageSize ->
                            { Width  = List.max widths 
                              Height = List.max heights }


                        | ResizeToSameSize_Target.MinimumPageSize ->
                            { Width  = List.min widths 
                              Height = List.min heights }

                        | ResizeToSameSize_Target.Size _ -> failwithf "Invalid token"

                    Reuses.Resize(PageSelector.All, PageBoxKind.AllBox, size)
                    
                )

            |> Reuse.rename 
                "ResizeToSameSize"
                [
                    "target" => target.ToString()
                ]
                        

        static member Resize (pageSelector: PageSelector, pageResizingRotatingOptions: PageResizingRotatingOptions, pageResizingScalingOptions: PageResizingScalingOptions, size: FsSize) =
            Reuses.Resize(pageSelector, pageResizingRotatingOptions, pageResizingScalingOptions, (fun _ _ -> size))
            |> Reuse.rename
                "Resize"
                ["pageResizingRotatingOptions" => pageResizingRotatingOptions.ToString()
                 "pageResizingScalingOptions" => pageResizingScalingOptions.ToString()
                 "pageSelector" => pageSelector.ToString()
                 "size" => size.ToString() ]

        /// keepDirection => bkSize.AlignDirection(pageBox)
        static member SetBackgroundSize (pageSelector: PageSelector, bkSize: FsSize, ?position: Position, ?pageBoxSetter: SetBackgroundSize_PageBoxSetter, ?keepDirection) =
            let position = defaultArg position Position.PreciseCenter
            let pageBoxSetter = defaultArg pageBoxSetter SetBackgroundSize_PageBoxSetter.SetArtBoxToContents
            let keepDirection = defaultArg keepDirection false
            fun flowModel (splitDocument: SplitDocument) ->
                let pages = splitDocument.Reader.GetPages()
                for page in pages do 
                    let pageBox = page.GetActualBox()
                    let bkSize = 
                        match keepDirection with 
                        | true -> bkSize.AlignDirection(pageBox)
                        | false -> bkSize

                    let xobject = page.CopyAsFormXObject(splitDocument.Writer)
                    let writerPage = splitDocument.Writer.AddNewPage(FsSize.toPageSize bkSize)
                    let x =
                        let baseX = 0.
                        let lengthDiff = bkSize.Width - pageBox.GetWidthF()
                        match position with 
                        | Position.Left (x, _) -> baseX 
                        | Position.XCenter (x, _) ->
                            baseX + lengthDiff / 2. 
                        | Position.Right (x, _) -> baseX + lengthDiff 
                        |> fun m -> m + position.X

                    let y = 
                        let baseY = 0.
                        let lengthDiff = bkSize.Height - pageBox.GetHeightF()
                        match position with 
                        | Position.Bottom _ -> baseY
                        | Position.YCenter _ ->
                            baseY + lengthDiff / 2.
                        | Position.Top _ -> baseY + lengthDiff
                        |> fun m -> m + position.Y

                    let canvas = PdfCanvas(writerPage)
                    canvas.AddXObjectAt(xobject, float32 x, float32 y)
                    |> ignore

                    let contectRect = Rectangle.create x y (pageBox.GetWidthF()) (pageBox.GetHeightF())
                    match pageBoxSetter with 
                    | SetBackgroundSize_PageBoxSetter.Non -> ()
                    | SetBackgroundSize_PageBoxSetter.SetArtBoxToContents ->
                        writerPage
                            .SetArtBox(contectRect)
                        |> ignore

                    | SetBackgroundSize_PageBoxSetter.SetTrimBoxToContents ->
                        writerPage
                            .SetTrimBox(contectRect)
                        |> ignore
                        
            |> reuse    
                "SetBackgroundSize"
                [
                    "PageSelector" => pageSelector.Text
                    "bkSize" => bkSize.MMText
                    "position" => position.LoggingText_MM
                    "pageBoxSetter"  => pageBoxSetter.ToString()
                    "keepDirection" => keepDirection.ToString()
                ]

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
                    fSize = (fun _ pageNum ->
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







        static member ChangePageOrientation(pageSelector: PageSelector, orientation: PageOrientation, ?fRotationIfNeeded) =
            Reuse.Factory(

                fun flowModel (splitDocument: SplitDocument) ->
                    let fRotationIfNeeded (pageNumber: PageNumber) =
                        match fRotationIfNeeded with 
                        | None -> RotationIfNeeded.Counterclockwise
                        | Some f -> f pageNumber

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
                                | _ -> 
                                    let rotation =
                                        match fRotationIfNeeded (PageNumber pageNumber) with 
                                        | RotationIfNeeded.Clockwise -> Rotation.Clockwise
                                        | RotationIfNeeded.Counterclockwise -> Rotation.Counterclockwise
                                    pageNumber, rotation
                            | false ->
                                pageNumber, Rotation.None
                        )
                    Reuses.SequencePages(EmptablePageNumSequence.Create pageNumberSequence)
            )
            |> Reuse.rename 
                "ChangePageOrientation"
                [ "pageSelector" => pageSelector.ToString()
                  "orientation" => orientation.ToString()
                  "rotationIfNeeded" => sprintf "%O" fRotationIfNeeded ]


    
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
            let setBleedDistance() =
                match args.Value.UseBleed, args.Value.BleedDistance with 
                | true, BleedDistance.SpecificValue distance -> 
                    reuse "SetBleedDistance" ["distance" => distance.ToString()] (fun flowModel document ->
                        document.Reader.GetPages()
                        |> List.iter(fun page ->
                            let newPage = page.CopyTo(document.Writer)
                            let newPage = document.Writer.AddPage(newPage)
                            let newPageBox = 
                                newPage.GetTrimBox()
                                |> Rectangle.applyMargin (Margin.Create distance)

                            newPage.SetActualBox(newPageBox)
                            |> ignore
                        )
                    )

                | _, _ ->
                   Reuse.dummy()
                   ||>> ignore

            let flow =

                match cellRotation with 
                | CellRotation.None -> 
                    setBleedDistance()
                    <+>
                    Reuses.Impose_Raw(fArgs, offsetLists = offsetLists,  draw = true)
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

                        setBleedDistance()
                        <+>
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

                
                
            flow <.+> Reuses.ClearDirtyInfos()
            |> Reuse.rename
                "Impose"
                ["imposingArguments" => args.ToString()]


        static member Impose(fArgs) =
            Reuses.Impose(fArgs, None)

        static member Impose_ForceOnePage(fArgs) =
            let args = ImposingArguments.Create fArgs

            Reuse.Factory(
                fun flowModel document ->

                    let originTotalPageNumber = document.Reader.GetNumberOfPages()

                    let checkArgsValid (args: ImposingArguments)=
                        let maxiumnCellsCount =
                            match args.FillingMode with 
                            | FillingMode.Automatic _ 
                            | FillingMode.ColumnAutomatic _ -> failwithf "Impose_ForceOnePage: unsupported fillMode %A" args.FillingMode
                            | FillingMode.RowAutomatic colNums ->
                                //v.ColNums
                                //|> List.sum
                                originTotalPageNumber

                                //failwithf "Impose_ForceOnePage: unsupported fillMode %A" args.FillingMode

                            | FillingMode.Numeric v -> 
                                v.ColNums
                                |> List.replicate v.RowNum
                                |> List.concat
                                |> List.take v.RowNum
                                |> List.sum

                        let colNumbers = 
                            match args.FillingMode with 
                            | FillingMode.Automatic _ 
                            | FillingMode.ColumnAutomatic _ -> failwithf "Impose_ForceOnePage: unsupported fillMode %A" args.FillingMode
                            | FillingMode.RowAutomatic colNums -> colNums.ColNums
                            | FillingMode.Numeric v -> v.ColNums

                        let args: _ImposingArguments = args.Value

                        let backgroundSize = 
                            match args.Background with 
                            | Background.File _ -> failwithf "Impose_ForceOnePage: unsupported background %A" args.Background
                            | Background.Size size -> size

                        let margin = 
                            match args.Sheet_PlaceTable with 
                            | Sheet_PlaceTable.At _ -> failwithf "Impose_ForceOnePage: unsupported sheet_placeTable %A" args.Sheet_PlaceTable
                            | Sheet_PlaceTable.Trim_CenterTable margin -> margin

                        maxiumnCellsCount, colNumbers, backgroundSize, margin


                    let maxiumnCellsCount, colNums, originBackgroundSize,  margin = checkArgsValid args
                    

                    let sheets = 
                        let document = ImposingDocument(document, args, allowRedirectCellSize = Some true)
                            
                        document.Build()
                        document.GetSheets()


                    let (|Spawned|NoSpawned|) (sheets: ImposingSheet list) = 

                        let isColNumberReached = 
                            let sheetColNums = sheets.Head.GetRows() |> List.map(fun m -> m.Cells.Count)
                            let sheetColNum = List.max sheetColNums

                            let colNum = List.max colNums
                            
                            sheetColNum >= colNum

                        match isColNumberReached with 
                        | true ->

                            match args.Value.IsRepeated with 
                            | true ->
                                match sheets.Head.GetCellsCount() = maxiumnCellsCount with 
                                | true -> NoSpawned ()
                                | false -> Spawned ()


                            | false ->
                                let divied = 
                                    float originTotalPageNumber / float maxiumnCellsCount
                                    |> ceil
                                    |> int

                                match sheets.Length = divied with 
                                | true -> NoSpawned ()
                                | false -> Spawned()

                        | false -> Spawned()


                    match sheets with 
                    | NoSpawned -> Reuses.Impose(fArgs)
                    | Spawned ->
                        let originBackgroundSize =
                            FsSize.rotateTo sheets.[0].PageSize.PageOrientation originBackgroundSize

                        let sheetForScale scale = 
                            let document2 =
                                let args = (ImposingArguments.Create (fun args ->  
                                    { fArgs args with 
                                        Background = 
                                            FsSize.MAXIMUN
                                            |> FsSize.mapValue(fun m -> m * 100.)
                                            |> Background.Size
                                        CellSizeScaling = scale
                                    } 
                                ))

                                ImposingDocument(document, args)

                            document2.Build()

                            document2.GetSheets()
                            |> List.exactlyOne_DetailFailingText


                        let nextScale scale =
                            let sheet = sheetForScale scale
                            let sheetSize = sheet.SheetSize

                            //let sheetSize = sheetSize.AlignDirection originBackgroundSize
                            let newScale =
                                min 
                                    (originBackgroundSize.Width  / sheetSize.Width  )
                                    (originBackgroundSize.Height / sheetSize.Height )

                            newScale * scale

                        let rec loop scale =
                            let newScale = nextScale scale
                            match scale - newScale with 
                            | SmallerThan 0.02 -> newScale - 0.02
                            | _ -> loop newScale

                        let scale = loop 1.
                            
                        Reuses.Impose(fun args ->
                            { fArgs args with 
                                CellSizeScaling = scale
                            }
                        )


            )
            |> Reuse.rename
                "Impose_ForceOnePage"
                ["imposingArguments" => args.ToString()]


        /// PageBorder + pageNumber 
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
        static member OneColumn(?margin, ?useBleed, ?spaces, ?isForce) =
            let margin = defaultArg margin Margin.Zero
            let spaces = defaultArg spaces Spaces.Zero
            let useBleed = defaultArg useBleed false
            let isForce = defaultArg isForce false
            let impose =
                match isForce with 
                | true -> Reuses.Impose_ForceOnePage
                | false -> Reuses.Impose

            impose(fun args -> 
                {args with 
                    Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (margin)
                    HSpaceExes = Spaces.Zero
                    VSpaceExes = spaces
                    ColNums = [1]
                    RowNum = 0
                    IsRepeated = false
                    UseBleed = useBleed
                    Background = Background.Size FsSize.MAXIMUN})
            ||>> (fun m -> m.GetSheets() |> List.exactlyOne_DetailFailingText)
            |> 
                Reuse.rename 
                    "OneColumn"
                    [
                        "margin" => margin.LoggingText
                        "useBleed" => useBleed.ToString()
                        "spaces" => spaces.ToString()
                        "isForce" => isForce.ToString()
                    ]
        /// default useBleed: false
        static member OneRow(?margin, ?useBleed, ?spaces, ?isForce) =
            let margin = defaultArg margin Margin.Zero
            let spaces = defaultArg spaces Spaces.Zero
            let useBleed = defaultArg useBleed false
            let isForce = defaultArg isForce false
            let impose =
                match isForce with 
                | true -> Reuses.Impose_ForceOnePage
                | false -> Reuses.Impose
            impose(fun args -> 
                {args with 
                    Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (margin)
                    HSpaceExes = spaces
                    VSpaceExes = Spaces.Zero
                    ColNums = [0]
                    RowNum = 1
                    IsRepeated = false
                    UseBleed = useBleed
                    Background = Background.Size FsSize.MAXIMUN})
            ||>> (fun m -> m.GetSheets() |> List.exactlyOne_DetailFailingText)
            |> 
                Reuse.rename 
                    "OneRow"
                    [
                        "margin" => margin.LoggingText
                        "useBleed" => useBleed.ToString()
                        "spaces" => spaces.ToString()
                        "isForce" => isForce.ToString()
                    ]
        
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


     

        



        static member private AddBackgroundOrForeground(backgroundOrForeground: BackgroundOrForeground, pageBoxKind: PageBoxKind, rectOptions: PdfCanvasAddRectangleArguments -> PdfCanvasAddRectangleArguments, ?margin, ?pageSelector) =
            let pageSelector = defaultArg pageSelector PageSelector.All
            (fun flowModel (doc: SplitDocument) ->
                let reader = doc.Reader
                PdfDocument.getPages reader
                |> List.mapi (fun i readerPage ->
                    let selectedPages = reader.GetPageNumbers(pageSelector)
                    match List.contains (i+1) selectedPages with 
                    | true ->
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
                        let pdfCanvas = 
                            match backgroundOrForeground with 
                            | BackgroundOrForeground.Background ->
                                new PdfCanvas(writerPage.NewContentStreamBefore(), writerPage.GetResources(), doc.Writer)

                            | BackgroundOrForeground.Foreground ->
                                new PdfCanvas(writerPage.NewContentStreamAfter(), writerPage.GetResources(), doc.Writer)
                            

                        PdfCanvas.useCanvas pdfCanvas (
                            PdfCanvas.addRectangle readerPageBox rectOptions
                        ) 

                        pdfCanvas
                            .AddXObjectAbs(readerXObject, 0.f, 0.f)
                        |> ignore


                    | false ->
                        let page = readerPage.CopyTo(doc.Writer)
                        doc.Writer.AddPage(page)
                        |> ignore

                ) |> ignore
            )
            |> reuse 
                "AddBackground"
                [
                    "pageSelector" => pageSelector.ToString()
                    "pageBoxKind" => pageBoxKind.ToString()
                    "rectOptions" => (rectOptions PdfCanvasAddRectangleArguments.DefaultValue).ToString()
                    "margin"      => (defaultArg margin Margin.Zero).LoggingText
                ]

        static member AddBackground (pageBoxKind: PageBoxKind, rectOptions: PdfCanvasAddRectangleArguments -> PdfCanvasAddRectangleArguments, ?margin, ?pageSelector) =
            Reuses.AddBackgroundOrForeground(BackgroundOrForeground.Background, pageBoxKind, rectOptions, ?margin = margin, ?pageSelector = pageSelector)

        static member AddForeground (pageBoxKind: PageBoxKind, rectOptions: PdfCanvasAddRectangleArguments -> PdfCanvasAddRectangleArguments, ?margin, ?xEffect, ?yEffect, ?pageSelector) =
            Reuses.AddBackgroundOrForeground(BackgroundOrForeground.Foreground, pageBoxKind, rectOptions, ?margin = margin, ?pageSelector = pageSelector)

        static member AddBackgroundOrForeground(backgroundFile: BackgroundFile, choice: BackgroundOrForeground, ?fPosition, ?pageSelector, ?layerName: BackgroundAddingLayerOptions) =
            //let xEffect = defaultArg xEffect XEffort.Middle
            //let yEffect = defaultArg yEffect YEffort.Middle

            let pageSelector = defaultArg pageSelector PageSelector.All
            (fun (flowModel:FlowModel<_>) (doc: SplitDocument) ->

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
                let selectedPages = reader.GetPageNumbers(pageSelector)
                let totalNumberOfPages = reader.GetNumberOfPages()
                PdfDocument.getPages reader
                |> List.mapi (fun i readerPage ->
                    let pageNumber = (i+1)
                    let position = 
                        match fPosition with 
                        | None -> Position.PreciseCenter
                        | Some fPosition -> 
                            let args =
                                { UserState = flowModel.UserState
                                  PageNum = pageNumber
                                  Page = readerPage
                                  TotalNumberOfPages = totalNumberOfPages }
                            fPosition args

                    match List.contains pageNumber selectedPages with 
                    | true ->
                        let backgroundPageBox, backgroundXObject = backgroundInfos.GetPageBoxAndXObject(i+1)
                        let readerPageBox = readerPage.GetActualBox()
                        let readerXObject = readerPage.CopyAsFormXObject(doc.Writer)

                        let pageSize = 
                            readerPage.GetActualBox()
                            |> PageSize

                        let writerPage = doc.Writer.AddNewPage(pageSize)
                        writerPage.SetPageBoxToPage(readerPage) |> ignore
                        let pdfCanvas = new PdfCanvas(writerPage)
                        let bk_x = 
                            let baseX =  readerPageBox.GetX() - backgroundPageBox.GetX() 
                            let lengthDiff = readerPageBox.GetWidth() - backgroundPageBox.GetWidth()
                            match position with 
                            | Position.Left (x, _) -> baseX 
                            | Position.XCenter (x, _) ->
                                baseX + lengthDiff / 2.f 
                            | Position.Right (x, _) -> baseX + lengthDiff 
                            |> fun m -> m + float32 position.X

                        let bk_y = 
                            let baseY = readerPageBox.GetY() - backgroundPageBox.GetY() 
                            let lengthDiff = readerPageBox.GetHeight() - backgroundPageBox.GetHeight()
                            match position with 
                            | Position.Bottom _ -> baseY
                            | Position.YCenter _ ->
                                baseY + lengthDiff / 2.f
                            | Position.Top _ -> baseY + lengthDiff
                            |> fun m -> m + float32 position.Y

                        match layerName with 
                        | None ->
                            match choice with 
                            | BackgroundOrForeground.Background ->
                                pdfCanvas
                                    .AddXObjectAbs(backgroundXObject, bk_x , bk_y)
                                    .AddXObjectAbs(readerXObject, 0.f, 0.f)
                                |> ignore

                            | BackgroundOrForeground.Foreground -> 
                                pdfCanvas
                                    .AddXObjectAbs(readerXObject, 0.f, 0.f)
                                    .AddXObjectAbs(backgroundXObject, bk_x, bk_y)

                                |> ignore

                        | Some layerName ->
                            let bklayer = layerName.BackgroundLayer.CreateLayer(doc.Writer)
                            let addBackground (pdfCanvas: PdfCanvas) =
                                pdfCanvas
                                    .BeginLayerUnion(bklayer)
                                    .AddXObjectAbs(backgroundXObject, bk_x , bk_y)
                                    .EndLayerUnion(bklayer)

                            let readerLayer = layerName.CurrentLayer.CreateLayer(doc.Writer)

                            let addReader (pdfCanvas: PdfCanvas) =
                                pdfCanvas
                                    .BeginLayerUnion(readerLayer)
                                    .AddXObjectAbs(readerXObject, 0.f, 0.f)
                                    .EndLayerUnion(readerLayer)


                            match choice with 
                            | BackgroundOrForeground.Background ->
                                pdfCanvas
                                |> addBackground
                                |> addReader
                                |> ignore

                            | BackgroundOrForeground.Foreground -> 
                                pdfCanvas
                                |> addReader
                                |> addBackground
                                |> ignore

                    | false ->
                        let page = readerPage.CopyTo(doc.Writer)
                        doc.Writer.AddPage(page)
                        |> ignore
                    )
                    |> ignore


                backgroundInfos.Close()

            )
            |> reuse 
                "AddBackgroundOrForeground"
                [
                    "pageSelector" => pageSelector.ToString()
                    "backgroundFile" => backgroundFile.ToString()
                    "layer" => choice.ToString()
                ]

        static member AddBackground (backgroundFile: BackgroundFile, ?position, ?pageSelector, ?layerName) =
            let createFPostion (position) =
                match position with 
                | None -> None
                | Some position -> Some (fun _ -> position)

            Reuses.AddBackgroundOrForeground(backgroundFile, BackgroundOrForeground.Background, ?fPosition = createFPostion position, ?pageSelector = pageSelector, ?layerName = layerName)


        static member AddForeground (foregroundFile: BackgroundFile, ?position, ?pageSelector, ?layerName) =
            let createFPostion (position) =
                match position with 
                | None -> None
                | Some position -> Some (fun _ -> position)

            Reuses.AddBackgroundOrForeground(foregroundFile, BackgroundOrForeground.Foreground, ?fPosition = createFPostion position, ?pageSelector = pageSelector, ?layerName = layerName)


        static member AssignToLayer (layerName) =
            (fun flowModel (doc: SplitDocument) ->
                let reader = doc.Reader
                let layer = new PdfLayer(layerName, doc.Writer)
                PdfDocument.getPages reader
                |> List.mapi (fun i readerPage ->

                    let readerXObject = readerPage.CopyAsFormXObject(doc.Writer)

                    let pageSize = 
                        readerPage.GetActualBox()
                        |> PageSize

                    let writerPage = doc.Writer.AddNewPage(pageSize)
                    writerPage.SetPageBoxToPage(readerPage) |> ignore
                    let pdfCanvas = new PdfCanvas(writerPage)

                    pdfCanvas
                        .BeginLayer(layer)
                        .AddXObjectAbs(readerXObject, 0.f, 0.f)
                        .EndLayer()

                    |> ignore



                ) |> ignore
            )
            |> reuse 
                "AssignToLayer"
                [
                    "layerName" => layerName
                ]


    type PdfRunner with 
        static member Reuse(pdfFile, ?backupPdfPath) = 
            fun reuse ->
                PdfRunner.OneFileFlow(pdfFile, ?backupPdfPath = backupPdfPath) (Flow.Reuse reuse)

        static member PreImpose_Repeated_One(baseFArgs: _ImposingArguments) =
            let args = 
                ImposingArguments.Create(fun _ ->
                    { baseFArgs with
                          IsRepeated = true }
                )

            let splitDocument = 
                let tmpPath = 
                    Path.GetTempFileNameEx()
                    |> Path.changeExtension ".pdf"
                SplitDocument.Create(emptyPdf.Value, tmpPath)

            splitDocument.Open()
            let imposingDocument = new ImposingDocument(splitDocument, args)
            imposingDocument.Build()
            splitDocument.Reader.Close()
            splitDocument.Writer.Close()

            match imposingDocument.GetSheets() with 
            | [ sheet ] -> RegularImposingSheet<_>.Create sheet
            | sheets -> failwithf "PreImpose_Repeated_One should generate one imposing sheet, but here is %d" sheets.Length


        static member SelectPages(pdfFile, pageSelector, ?backupPdfPath) =
            Reuses.SelectPages(pageSelector)
            |> PdfRunner.Reuse(pdfFile, ?backupPdfPath = backupPdfPath)


        static member SequencePages(pdfFile, pageNumSequence: PageNumSequence, ?backupPdfPath) =
            Reuses.SequencePages(pageNumSequence)
            |> PdfRunner.Reuse(pdfFile, ?backupPdfPath = backupPdfPath)


        /// default useBleed: false
        static member OneColumn(?backupPdfPath, ?margin, ?useBleed, ?spaces, ?isForce) =
            fun pdfFile ->
                let reuse =
                    Reuses.OneColumn(?margin = margin, ?useBleed = useBleed, ?spaces = spaces, ?isForce = isForce)

                PdfRunner.Reuse(pdfFile,?backupPdfPath = backupPdfPath) reuse
    
        /// default useBleed: false
        static member OneRow(?backupPdfPath, ?margin, ?useBleed, ?spaces, ?isForce) =
            fun pdfFile ->
                let reuse =
                    Reuses.OneRow(?margin = margin, ?useBleed = useBleed, ?spaces = spaces, ?isForce = isForce)

                PdfRunner.Reuse(pdfFile,?backupPdfPath = backupPdfPath) reuse


    