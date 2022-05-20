namespace Shrimp.Pdf

open System.Collections.Generic


#nowarn "0104"
open iText.Kernel.Pdf
open Shrimp.Pdf.Colors
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf.Extensions
open System.Linq
open System
open System.Collections.Concurrent
open Shrimp.FSharp.Plus
open Shrimp.FSharp.Plus.Math
open Newtonsoft.Json
open Shrimp.Pdf.Constants

type Flip =
    | HFlip = 0
    | VFlip = 1


[<AutoOpen>]
module _PrefixReuses =
    let private reuse name parameters f = Reuse(f = f, flowName = FlowName.Override(name, parameters))



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
    
    type private PageSequeningUnion =
        | EmptyPage of targetPage: PdfPage option
        | Token of PageNumSequenceToken * PdfPage


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

    type Reuses =
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
                        canvas.AddXObjectAbs(xobject, -pageBox.GetX(), -pageBox.GetY())
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
        
                static member SelectPages (pageSelector: PageSelector) =
                    Reuse.Factory(fun flowModel doc ->
                        let pageNumberSequence = 
                            doc.Reader.GetPageNumbers(pageSelector)
                            |> PageNumSequence.Create
        
                        Reuses.SequencePages(pageNumberSequence)
                    )
                    |> Reuse.rename 
                        (sprintf "SelectPages %s" (pageSelector.Text))
                        []

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