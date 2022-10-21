﻿namespace Shrimp.Pdf

open System.Collections.Generic


#nowarn "0104"
open iText.Kernel.Pdf
open Shrimp.Pdf.Colors
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf.Extensions
open FParsec
open FParsec.CharParsers
open System.Linq
open System
open System.Collections.Concurrent
open Shrimp.FSharp.Plus
open Shrimp.FSharp.Plus.Math
open Newtonsoft.Json
open Shrimp.Pdf.Constants


module Flip =
    let ofCharOp(char: char) =
        match char with     
        | '$' -> Some Flip.HFlip
        | '%' -> Some Flip.VFlip
        | _ -> None


    let ofChar(char: char) =
        match ofCharOp char with 
        | Some flip -> flip
        | None -> failwithf "Cannot create flip from %A, avaliable chars are %A" char ["$"; "%"]


    let toChar = function
        | Flip.HFlip -> '$'
        | Flip.VFlip -> '%'

[<AutoOpen>]
module _PrefixReuses =
    let private reuse name parameters f = Reuse(f = f, flowName = FlowName.Override(name, parameters))

    type internal FlipOrRotationEnum =
        | Flip = 0
        | Rotation = 1

    // number in page sequence must be bigger than 0
    [<RequireQualifiedAccess>]
    type PageNumSequenceToken =
        | PageNum of int 
        | PageNumWithRotation of int * Rotation
        | PageNumWithFlip of int * Flip
        | PageNumWithRotationAndFlip of int * Rotation * Flip
        | PageNumWithFlipAndRotation of int * Flip * Rotation
    with 
        member x.PageNumValue = 
            match x with 
            | PageNumSequenceToken.PageNum pageNum -> pageNum
            | PageNumSequenceToken.PageNumWithRotation (pageNum, _) 
            | PageNumSequenceToken.PageNumWithFlip (pageNum, _) 
            | PageNumSequenceToken.PageNumWithRotationAndFlip (pageNum, _, _) 
            | PageNumSequenceToken.PageNumWithFlipAndRotation (pageNum, _, _) -> pageNum
    
        member x.Rotation =
            match x with 
            | PageNumSequenceToken.PageNum _ -> Rotation.None
            | PageNumSequenceToken.PageNumWithRotation (_, rotation) -> rotation
            | PageNumSequenceToken.PageNumWithFlip (_, _) -> Rotation.None
            | PageNumSequenceToken.PageNumWithRotationAndFlip (_, rotation, _) -> rotation
            | PageNumSequenceToken.PageNumWithFlipAndRotation (_, _, rotation) -> rotation
    
        member x.Flip =
            match x with 
            | PageNumSequenceToken.PageNum _
            | PageNumSequenceToken.PageNumWithRotation _ -> None
            | PageNumSequenceToken.PageNumWithFlip (_, flip) 
            | PageNumSequenceToken.PageNumWithRotationAndFlip (_, _, flip) 
            | PageNumSequenceToken.PageNumWithFlipAndRotation (_, flip, _) -> Some flip
    
        member internal x.AHeadFlipOrRotationEnum() =
            match x with 
            | PageNumSequenceToken.PageNum _
            | PageNumSequenceToken.PageNumWithRotation _ 
            | PageNumSequenceToken.PageNumWithFlip _ 
            | PageNumSequenceToken.PageNumWithRotationAndFlip _ -> FlipOrRotationEnum.Rotation
            | PageNumSequenceToken.PageNumWithFlipAndRotation _ -> FlipOrRotationEnum.Flip


        member x.MapPageNumber(mapping) =
            match x with 
            | PageNumSequenceToken.PageNum pageNum -> mapping pageNum |> PageNumSequenceToken.PageNum
            | PageNumSequenceToken.PageNumWithRotation (pageNum, rotation) -> 
                (mapping pageNum, rotation) |> PageNumSequenceToken.PageNumWithRotation
            | PageNumSequenceToken.PageNumWithFlip (pageNum, flip) -> 
                (mapping pageNum, flip) |> PageNumSequenceToken.PageNumWithFlip
               
            | PageNumSequenceToken.PageNumWithRotationAndFlip (pageNum, rotation, flip) -> 
                (mapping pageNum, rotation, flip) |> PageNumSequenceToken.PageNumWithRotationAndFlip
                
            | PageNumSequenceToken.PageNumWithFlipAndRotation (pageNum, flip, rotation) -> 
                (mapping pageNum, flip, rotation) |> PageNumSequenceToken.PageNumWithFlipAndRotation
                
    
        member x.ShuffingText =
            match x with 
            | PageNumSequenceToken.PageNum pageNum -> pageNum.ToString()
            | PageNumSequenceToken.PageNumWithRotation (pageNum, rotation) -> 
                pageNum.ToString() + (
                    (Rotation.toChar rotation)
                    |> Option.map string
                    |> Option.defaultValue ""
                )
 
    
            | PageNumSequenceToken.PageNumWithFlip (pageNum, flip) -> 
                pageNum.ToString() + 
                    (Flip.toChar flip).ToString()
    
            | PageNumSequenceToken.PageNumWithRotationAndFlip (pageNum, rotation, flip) -> 
                pageNum.ToString() + 
                    (
                        (Rotation.toChar rotation)
                        |> Option.map string
                        |> Option.defaultValue ""
                    ) +
                    (Flip.toChar flip).ToString()

            | PageNumSequenceToken.PageNumWithFlipAndRotation (pageNum, flip, rotation) -> 
                pageNum.ToString() + 
                    (Flip.toChar flip).ToString() +
                    (
                        (Rotation.toChar rotation)
                        |> Option.map string
                        |> Option.defaultValue ""
                    ) 
                    

        static member Parse(text: string) =
            let p1 = pint32 .>> eof
            let pRotation = pint32 .>>. anyOf ['>'; '<'; '*'] .>> eof
            let pFlip = pint32 .>>. anyOf ['$'; '%'] .>> eof
            let pRotationAndFlip = pint32 .>>. anyOf ['>'; '<'; '*'] .>>. anyOf ['$'; '%'] .>> eof
            let pFlipAndRotation = pint32  .>>. anyOf ['$'; '%'] .>>. anyOf ['>'; '<'; '*'] .>> eof

            match text with 
            | String.FParsec (p1) r -> PageNumSequenceToken.PageNum r
            | String.FParsec pRotationAndFlip ((pageNum, rotation), flip) ->
                let rotation = Rotation.ofChar rotation
                let flip = Flip.ofChar flip
                PageNumWithRotationAndFlip(pageNum, rotation, flip)

            | String.FParsec pFlipAndRotation ((pageNum, flip), rotation) ->
                let rotation = Rotation.ofChar rotation
                let flip = Flip.ofChar flip
                PageNumWithFlipAndRotation(pageNum, flip, rotation)

            | String.FParsec pRotation (pageNum, rotation) ->
                let rotation = Rotation.ofChar rotation
                PageNumWithRotation(pageNum, rotation)

            | String.FParsec (pFlip) (pageNum, flip: char) ->
                let flip = Flip.ofChar flip
                PageNumWithFlip(pageNum, flip)


            | _ -> failwithf "Cannot parse %A to PageNumSequenceToken" text

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
    
        static member Parse(text: string) =
            match text with 
            | "x" -> EmptablePageNumSequenceToken.EmptyPage
            | _ -> 
                PageNumSequenceToken.Parse text
                |> EmptablePageNumSequenceToken.PageNumSequenceToken

    
    
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
        static member MovePageBoxToOrigin(pageSelector: PageSelector, ?keepOriginPageBoxes: bool) =
            let keepOriginPageBoxes = defaultArg keepOriginPageBoxes false

            fun flowModel (splitDocument: SplitDocument) ->
                let selectedPageNumbers = splitDocument.Reader.GetPageNumbers(pageSelector) 

                PdfDocument.getPages splitDocument.Reader
                |> List.iteri (fun i page ->
                    let pageNum = i + 1
                    if List.contains pageNum selectedPageNumbers 
                    then 
                        let actualBox = page.GetActualBox()
                        let width = actualBox.GetWidthF()
                        let height = actualBox.GetHeightF()

                        let xobject = page.CopyAsFormXObject(splitDocument.Writer)
                        let newPage = splitDocument.Writer.AddNewPage(PageSize(Rectangle.create 0. 0. width height))
                        let canvas = new PdfCanvas(newPage)



                        let newPage =
                            match keepOriginPageBoxes with 
                            | true ->
                                let setPageBox (pageBoxKind) pdfPage =
                                    let originPageBox = page.GetPageBox(pageBoxKind)
                                    let x = originPageBox.GetXF() - actualBox.GetXF()
                                    let y = originPageBox.GetYF() - actualBox.GetYF()
                                    let rect = Rectangle.create x y (originPageBox.GetWidthF()) (originPageBox.GetHeightF())
                                    PdfPage.setPageBox pageBoxKind rect pdfPage

                                newPage
                                |> setPageBox PageBoxKind.ArtBox
                                |> setPageBox PageBoxKind.BleedBox
                                |> setPageBox PageBoxKind.TrimBox


                            | false -> newPage

                        canvas.AddXObjectAbs(xobject, -actualBox.GetX(), -actualBox.GetY())
                        |> ignore

                    else 
                        let page = page.CopyTo(splitDocument.Writer)
                        splitDocument.Writer.AddPage(page) |> ignore
                )

            |> reuse
                "MovePageBoxToOrigin"
                ["pageSelector" => pageSelector.ToString()
                 "keepOriginPageBoxes" => keepOriginPageBoxes.ToString() ]

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
                            | _ ->
                                let rotation = token.Rotation
                                let flip = token.Flip
                                    
                                match rotation, flip with 
                                | Rotation.None, None ->
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
                                        let affineTransfrom_Rotate = 
                                            AffineTransform.GetRotateInstance(Math.PI / -180. * angle, x, y)
        
                                        let affineTransfrom_Flip = 
                                            match flip with 
                                            | None -> AffineTransformRecord.DefaultValue
                                            | Some flip -> AffineTransformRecord.GetFlipInstance flip
                                
                                            |> AffineTransform.ofRecord

                                        let affineTransform_Translate = 
                                            { ScaleX = 1. 
                                              ScaleY = 1. 
                                              TranslateX = -x
                                              TranslateY = -y
                                              ShearX = 0.
                                              ShearY = 0. }
                                            |> AffineTransformRecord.toAffineTransform
                                        
                                        match token.AHeadFlipOrRotationEnum() with 
                                        | FlipOrRotationEnum.Rotation ->
                                            affineTransform_Translate.Concatenate(affineTransfrom_Flip)
                                            affineTransform_Translate.Concatenate(affineTransfrom_Rotate)

                                        | FlipOrRotationEnum.Flip ->
                                            affineTransform_Translate.Concatenate(affineTransfrom_Rotate)
                                            affineTransform_Translate.Concatenate(affineTransfrom_Flip)


                                        affineTransform_Translate
        
        
        
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

        static member ClearDirtyInfos(?keepOriginPageBoxes) =
            let keepOriginPageBoxes = defaultArg keepOriginPageBoxes false
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
                    Reuses.MovePageBoxToOrigin(PageSelector.All, keepOriginPageBoxes = keepOriginPageBoxes)
                | None -> 
                    Reuses.MovePageBoxToOrigin(PageSelector.All, keepOriginPageBoxes = keepOriginPageBoxes)
            )
            |> Reuse.rename
                "ClearDirtyInfos"
                ["keepOriginPageBoxes" => keepOriginPageBoxes.ToString() ]