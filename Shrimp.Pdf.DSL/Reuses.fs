namespace Shrimp.Pdf
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

type PageNumSequence = private PageNumSequence of int list
with 
    member x.Value = 
        let (PageNumSequence value) = x
        value


[<RequireQualifiedAccess>]
module PageNumSequence =
    let create (sequence: int list) =
        if sequence.IsEmpty then failwith "page sequence cannot be empty"
        elif List.exists (fun pageNumber -> pageNumber <= 0) sequence then failwithf "number in page sequence %A must be bigger than 0" sequence
        PageNumSequence sequence

type CopiedNumSequence = private CopiedNumSequence of int list
with 
    member x.Value = 
        let (CopiedNumSequence value) = x
        value

[<RequireQualifiedAccess>]
module CopiedNumSequence =
    let create (sequence: int list) =
        if sequence.IsEmpty then failwith "sequence cannot be empty"
        elif List.exists (fun pageNumber -> pageNumber <= 0) sequence then failwithf "number in sequence %A must be bigger than 0" sequence
        CopiedNumSequence sequence

type PageResizingScaleOptions =
    | Uniform = 0
    | Anamorphic = 1

type PageResizingRotationOptions =
    | Keep = 0
    | ColckwiseIfNeeded = 1
    | CounterColckwiseIfNeeded = 2



[<AutoOpen>]
module _Reuses =
    let private reuse v = Reuse v 

    type PageInsertingOptions =
        | BeforePoint = 0
        | AfterPoint = 1


    type Reuses =
        static member Insert(insertingFile: string, ?insertingFilePageSelector: PageSelector, ?pageInsertingOptions: PageInsertingOptions, ?insertingPoint: SinglePageSelectorExpr) =
            fun flowModel (splitDocument: SplitDocument) ->
                if Path.GetFullPath(insertingFile) = Path.GetFullPath(splitDocument.ReaderName) 
                then failwith "Cannot insert file to self"

                let insertingFilePageSelector = defaultArg insertingFilePageSelector PageSelector.All
                let pageInsertingOptions = defaultArg pageInsertingOptions PageInsertingOptions.AfterPoint
                let singlePageSelectorExpr = defaultArg insertingPoint (SinglePageSelectorExpr.End 1)
                Logger.infoWithStopWatch (sprintf "Insert file %s %A to %s %s %A" insertingFile insertingFilePageSelector splitDocument.ReaderName (pageInsertingOptions.ToString()) singlePageSelectorExpr) (fun _ ->
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
                )

            |> reuse

        static member Impose (fArgs) =
            fun flowModel (splitDocument: SplitDocument) ->
                let imposingArguments = ImposingArguments.Create fArgs
                Logger.infoWithStopWatch (sprintf "IMPOSE %A" imposingArguments) (fun _ ->
                    let imposingDocument = new ImposingDocument(splitDocument, imposingArguments)
                    imposingDocument.Build()

                    imposingDocument.Draw()
                    (imposingDocument)
                )

            |> reuse

        static member MovePageBoxToOrigin(pageSelector: PageSelector) =
            fun flowModel (splitDocument: SplitDocument) ->
                let selectedPageNumbers = splitDocument.Reader.GetPageNumbers(pageSelector) 

                Logger.infoWithStopWatch (sprintf "MOVE %A pagebox to origin" selectedPageNumbers) (fun _ ->
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
                    )
                )

            |> reuse


        static member Rotate (pageSelector: PageSelector, rotation: Rotation) =
            fun (flowModel: FlowModel<_>) (splitDocument: SplitDocument) ->
                let angle = Rotation.getAngle rotation
                let selectedPageNumbers = splitDocument.Reader.GetPageNumbers(pageSelector) 
                Logger.infoWithStopWatch (sprintf "ROTATE %A by angle %g" selectedPageNumbers angle) (fun _ ->
                    PdfDocument.getPages splitDocument.Reader
                    |> List.iteri (fun i page ->
                        let pageNum = i + 1
                        if List.contains pageNum selectedPageNumbers 
                        then 
                            let pageBox = page.GetActualBox()

                            let affineTransform =
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

                            let newPageSize =
                                affineTransform.Transform(pageBox)

                            let xobject = page.CopyAsFormXObject(splitDocument.Writer)
                            let newPage = splitDocument.Writer.AddNewPage(PageSize(newPageSize))
                            let canvas = new PdfCanvas(newPage)

                            canvas.AddXObject(xobject,AffineTransformRecord.ofAffineTransform affineTransform) |> ignore

                        else 
                            let page = page.CopyTo(splitDocument.Writer)
                            splitDocument.Writer.AddPage(page) |> ignore
                    )
                )

            |> reuse

        static member Resize (pageSelector: PageSelector, pageBoxKind: PageBoxKind, size: FsSize) =
            fun flowModel (splitDocument: SplitDocument) ->
                let selectedPageNumbers = splitDocument.Reader.GetPageNumbers(pageSelector) 

                Logger.infoWithStopWatch (sprintf "Resize %A %A to %A" selectedPageNumbers pageBoxKind size) (fun _ ->
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
                    )
                )

            |> reuse

        static member Resize (pageSelector: PageSelector, pageResizingRotationOptions: PageResizingRotationOptions, pageResizingScaleOptions: PageResizingScaleOptions, size: FsSize) =
            let tryRotate() =
                match pageResizingRotationOptions with
                | PageResizingRotationOptions.Keep -> Reuse.dummy ||>> ignore
                | PageResizingRotationOptions.ColckwiseIfNeeded 
                | PageResizingRotationOptions.CounterColckwiseIfNeeded ->
                    Reuse(fun flowModel splitDocument ->
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
                            let pageSelector = 
                                PageSelector.Numbers (pageNumbers)

                            let rotation = 
                                match pageResizingRotationOptions with 
                                | PageResizingRotationOptions.CounterColckwiseIfNeeded -> Rotation.Counterclockwise
                                | PageResizingRotationOptions.ColckwiseIfNeeded -> Rotation.Clockwise
                                | PageResizingRotationOptions.Keep -> Rotation.None

                            Reuses.Rotate(pageSelector, rotation).Value flowModel splitDocument

                                
                    )

            let resize() =
                match pageResizingScaleOptions with 
                | PageResizingScaleOptions.Anamorphic -> 
                    Reuse(fun flowModel splitDocument ->
                        Reuses.Resize(pageSelector, PageBoxKind.ActualBox, size).Value flowModel splitDocument
                        PdfDocument.getPages splitDocument.Writer
                        |> List.iteri(fun i page ->
                            let actualBox = page.GetActualBox()
                            PdfPage.setPageBox PageBoxKind.AllBox actualBox
                            |> ignore
                        )
                    )
                | PageResizingScaleOptions.Uniform ->
                    Reuse(fun flowModel splitDocument ->
                        let selectedPageNumbers = splitDocument.Reader.GetPageNumbers(pageSelector) 
                        Logger.infoWithStopWatch (sprintf "Resize %A to %A" selectedPageNumbers size) (fun _ ->
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
                    )

            tryRotate()
            <+>
            resize()

        /// e.g. [1; 3; 5] will pick page1, page3, page5
        static member SequencePages (pageNumSequence: PageNumSequence) =
            fun (flowModel: FlowModel<_>) (splitDocument: SplitDocument) ->
                Logger.infoWithStopWatch (sprintf "SEQUENCEPAGES %A" pageNumSequence) (fun _ -> 
                    let duplicatedReaderPages = 
                        splitDocument.Reader
                        |> PdfDocument.getPages
                        |> List.mapi (fun i page ->
                            let pageNum = i + 1
                            pageNumSequence.Value
                            |> List.filter ((=) pageNum)
                            |> List.map (fun _ -> page)
                        )|> List.concat

                    if duplicatedReaderPages.Length = 0 
                    then failwithf "Invalid sequence %A, shoudl exists a sequence number >= 1 and <= %d" pageNumSequence (splitDocument.Reader.GetNumberOfPages())

                    for page in duplicatedReaderPages do
                        let writerPageResource = page.CopyTo(splitDocument.Writer)
                        splitDocument.Writer.AddPage(writerPageResource) |> ignore
                
                )


            |> reuse

        static member DuplicatePages (pageSelector: PageSelector, copiedNumbers: CopiedNumSequence) =

            fun flowModel (splitDocument: SplitDocument) ->
                Logger.info (sprintf "DUPLICATEPAGES %A" copiedNumbers) 

                let pageNumSequence = 
                    splitDocument.Reader.GetPageNumbers(pageSelector)
                    |> List.indexed
                    |> List.collect (fun (index, pageNum) ->
                        if index < copiedNumbers.Value.Length then
                            List.replicate copiedNumbers.Value.[pageNum - 1] pageNum
                        else []
                    )
                    |> PageNumSequence.create

                (Reuses.SequencePages pageNumSequence).Value flowModel splitDocument
            |> reuse

        static member DuplicatePages (pageSelector: PageSelector, copiedNumber: int)  =
            if copiedNumber <= 0 then failwithf "copied number %d should be bigger than 0" copiedNumber

            fun flowModel (splitDocument: SplitDocument) ->
                let pageNumSequence = 
                    splitDocument.Reader.GetPageNumbers(pageSelector)
                    |> List.map (fun _ -> copiedNumber
                    )
                    |> CopiedNumSequence.create

                (Reuses.DuplicatePages (pageSelector, pageNumSequence)).Value flowModel splitDocument
            |> reuse



        static member TilePages (tileTable: TileTable) =

            let colNum = tileTable.ColNum
            let rowNum = tileTable.RowNum

            fun flowModel (splitDocument: SplitDocument) ->
                Logger.info (sprintf "TILEPAGES %d*%d" tileTable.ColNum tileTable.RowNum) 
                let userState = (Reuses.DuplicatePages (PageSelector.All, colNum * rowNum)).Value flowModel splitDocument
                let tileTable = TileTable.create colNum rowNum
                let writer = splitDocument.Writer
                for i = 1 to writer.GetNumberOfPages() do
                    let page = writer.GetPage(i)
                    let actualBox = page.GetActualBox()
                    let tileIndexer = TileIndexer.create tileTable (i - 1)
                    let tileBox = Rectangle.getTile tileIndexer actualBox
                    PdfPage.setPageBox PageBoxKind.AllBox tileBox page
                    |> ignore
                
                userState

            |> reuse

        static member TilePages (selector: Selector<'userState>) =
            fun (flowModel: FlowModel<'userState>) (splitDocument: SplitDocument) ->
                Logger.info (sprintf "TILEPAGES by selector %A" selector)

                let reader = splitDocument.Reader
                let parser = new NonInitialClippingPathPdfDocumentContentParser(reader)
                for i = 1 to reader.GetNumberOfPages() do
                    let readerPage = reader.GetPage(i)
                    let args =
                        { Page = readerPage
                          UserState = flowModel.UserState 
                          TotalNumberOfPages = splitDocument.Reader.GetNumberOfPages() 
                          PageNum = i }
                    
                    let selector = (Selector.toRenderInfoSelector args selector)
                    let infos = NonInitialClippingPathPdfDocumentContentParser.parse i selector parser
                    for info in infos do
                        let bound = IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth info
                        let writer = splitDocument.Writer
                        let writerPageResource = readerPage.CopyTo(writer)
                        PdfPage.setPageBox PageBoxKind.AllBox bound writerPageResource |> ignore
                        writer.AddPage(writerPageResource)
                        |> ignore

            |> reuse




