﻿namespace Shrimp.Pdf
open iText.Kernel.Geom
open iText.IO.Font
open iText.IO.Font.Otf
open iText.Kernel.Font
open Akka.Configuration
open System.Reflection
open System.IO
open System
open iText.Layout
open iText.Kernel.Colors
open iText.Layout.Properties
open System.Collections.Concurrent
open iText.Kernel.Pdf


type TileTable = private TileTable of colNum: int * rowNum: int
with 
    member x.ColNum =
        let (TileTable (colNum, rowNum)) = x
        colNum

    member x.RowNum = 
        let (TileTable (colNum, rowNum)) = x
        rowNum

[<RequireQualifiedAccess>]
module TileTable = 
    let create colNum rowNum = 
        if not (colNum > 0 && rowNum > 0) then failwithf "colNum %d and rowNum %d should bigger than 0" colNum rowNum 

        TileTable(colNum, rowNum)


/// zero-based index
type TileIndexer = private TileIndexer of TileTable * index: int
with 
    member x.ColNum =
        let (TileIndexer (tileTable, index)) = x
        tileTable.ColNum

    member x.RowNum = 
        let (TileIndexer (tileTable, index)) = x
        tileTable.RowNum

    member x.Index = 
        let (TileIndexer (tileArguments, index)) = x
        index

[<RequireQualifiedAccess>]
module TileIndexer = 
    /// zero-based index
    let create (tileTable: TileTable) index =
        if index < 0 then failwithf "pieceIndex %d should >= 0" index
        TileIndexer(tileTable, index)



type RegisterableFont =
    { PdfEncodings: string
      Path: string 
      FontFamily: string }

module FontExtensions =
    let private fontRegisterCache = new ConcurrentDictionary<string, RegisterableFont>()

    type PdfFontFactory with
        static member Register(registerableFont: RegisterableFont) =
            fontRegisterCache.GetOrAdd(registerableFont.Path, fun path ->
                PdfFontFactory.Register(path)
                registerableFont
            ) |> ignore

        static member internal CreateFont(registerableFont: RegisterableFont) =
            PdfFontFactory.Register(registerableFont)

            PdfFontFactory.CreateRegisteredFont(registerableFont.FontFamily, registerableFont.PdfEncodings)


open FontExtensions

/// StandardFonts: See iText.IO.Font.Constants.StandardFonts
[<RequireQualifiedAccess>]
type PdfFontFactory =
    | Registerable of RegisterableFont
    | StandardFonts of string


[<RequireQualifiedAccess>]
type Rotation =
    | None = 0
    | Clockwise = 1
    | Counterclockwise = 2
    | R180 = 3

[<RequireQualifiedAccess>]
module Rotation =
    let getAngle = function
        | Rotation.Clockwise  -> 90.
        | Rotation.Counterclockwise -> -90.
        | Rotation.R180 -> 180.
        | Rotation.None -> 0.
        | _ -> failwith "invalid token"


type AffineTransformRecord =
    { ScaleX: float 
      ScaleY: float 
      TranslateX: float 
      TranslateY: float 
      ShearX: float 
      ShearY: float }
with
    member x.m00 = x.ScaleX

    member x.m01 = x.ShearX

    member x.m10 = x.ShearY

    member x.m11 = x.ScaleY

    member x.m02 = x.TranslateX

    member x.m12 = x.TranslateY
        



type PageBoxKind =
    | ArtBox = 0
    | BleedBox = 1
    | TrimBox = 2
    | CropBox = 3
    | MediaBox = 4
    | ActualBox = 5
    | AllBox = 6

[<RequireQualifiedAccess>]
type CanvasFontSize =
    | Numeric of size: float
    | OfRootArea of scale: float
    | OfArea of Rectangle


[<RequireQualifiedAccess>]
type RelativePosition =
    | Inbox = 0
    | CrossBox = 1
    | OutBox = 2

type StraightLine =
    { Start: Point
      End: Point }

[<RequireQualifiedAccess>]
type Position =
    | LeftBottom of float * float
    | LeftMiddle of float * float
    | LeftTop of float * float
    | TopMiddle of float * float
    | RightTop of float * float
    | RightMiddle of float * float
    | RightBottom of float * float
    | BottomMiddle of float * float
    | Center of float * float


type DashPattern =
    { DashArray: float list 
      Phase: float }

with 
    member x.DashArrayF32 =
        x.DashArray
        |> Array.ofList
        |> Array.map float32

    member x.PhaseF32 =
        float32 x.Phase

[<RequireQualifiedAccess>]
module Position =

    let (|Left|_|) = function
        | Position.LeftTop (x, y) 
        | Position.LeftBottom (x, y) 
        | Position.LeftMiddle(x, y) -> Some (x, y)
        | _ -> None

    let (|Bottom|_|) = function
        | Position.LeftBottom (x, y)
        | Position.RightBottom (x, y)
        | Position.BottomMiddle (x, y)-> Some (x, y)
        | _ -> None

    let (|Top|_|) = function
        | Position.LeftTop (x, y) 
        | Position.RightTop (x, y)
        | Position.TopMiddle(x, y) -> Some (x, y)
        | _ -> None

    let (|Right|_|) = function
        | Position.RightBottom (x, y) 
        | Position.RightTop (x, y)
        | Position.RightMiddle (x, y) -> Some (x, y)
        | _ -> None

    let (|XCenter|_|) = function
        | Position.Center (x, y) 
        | Position.BottomMiddle (x, y) 
        | Position.TopMiddle(x, y) -> Some (x, y)
        | _ -> None

    let (|YCenter|_|) = function
        | Position.Center (x, y) 
        | Position.LeftMiddle (x, y) 
        | Position.RightMiddle(x, y) -> Some (x, y)
        | _ -> None

    let (|LeftEdge|_|) = function
        | Left (0., _) -> Some ()
        | _ -> None
    
    let (|BottomEdge|_|) = function
        | Bottom (_, 0.) -> Some ()
        | _ -> None

    let (|TopEdge|_|) = function
        | Top (_, 0.) -> Some ()
        | _ -> None

    let (|RightEdge|_|) = function
        | Right (0., _)  -> Some ()
        | _ -> None

[<RequireQualifiedAccess>]
module StraightLine =
    /// http://www.navision-blog.de/blog/2008/12/02/calculate-the-intersection-of-two-lines-in-fsharp-2d/
    let intersection (line1: StraightLine) (line2: StraightLine) =
        let A,B,C,D = line1.Start,line1.End,line2.Start,line2.End
        let (Ax,Ay,Bx,By,Cx,Cy,Dx,Dy) =
            (A.x, A.y, B.x, B.y, C.x, C.y, D.x, D.y)
        let d = (Bx-Ax)*(Dy-Cy)-(By-Ay)*(Dx-Cx)  

        if  d = 0. then
        // parallel lines ==> no intersection in euclidean plane
            None
        else
            let q = (Ay-Cy)*(Dx-Cx)-(Ax-Cx)*(Dy-Cy) 
            let r = q / d
            let p = (Ay-Cy)*(Bx-Ax)-(Ax-Cx)*(By-Ay)
            let s = p / d

            if r < 0. || r > 1. || s < 0. || s > 1. then
                None // intersection is not within the line segments
            else
                Some(
                    Point(Ax+r*(Bx-Ax), Ay+r*(By-Ay))
                )  // Py



module Extensions =

    type PdfDocumentWithCachedResources =
        inherit PdfDocument
        val private fontsCache: ConcurrentDictionary<PdfFontFactory, PdfFont>
        //let fontsCache = new ConcurrentDictionary<PdfFontFactory, PdfFont>()

        member x.GetOrCreatePdfFont(fontFactory: PdfFontFactory) =
            x.fontsCache.GetOrAdd((fontFactory), fun (fontFactory) ->
                match fontFactory with 
                | PdfFontFactory.StandardFonts fontName -> PdfFontFactory.CreateFont(fontName)
                | PdfFontFactory.Registerable registerableFont ->
                    PdfFontFactory.CreateFont(registerableFont)
            )

        new (writer: string) = { inherit PdfDocument(new PdfWriter(writer)); fontsCache = new ConcurrentDictionary<_, _> () }
        new (reader: string, writer: string) =  { inherit PdfDocument(new PdfReader(reader), new PdfWriter(writer)); fontsCache = new ConcurrentDictionary<_, _> () }

    [<AutoOpen>]
    module iText = 
        open iText.Kernel.Pdf.Canvas.Parser.Data
        open iText.Kernel.Pdf
        open iText.Kernel.Pdf.Canvas
        open iText.Kernel.Pdf.Xobject

        [<RequireQualifiedAccess>]
        module GlyphLine =
            let getAllGlyphs (glyphLine: GlyphLine) =
                [ for i = 0 to glyphLine.Size() - 1 do 
                    yield glyphLine.Get(i) ]


        [<RequireQualifiedAccess>]
        module PdfFont =

            let private TYPO_ASCENDER_SCALE_COEFF = 1.2f

            let private TEXT_SPACE_COEFF = FontProgram.UNITS_NORMALIZATION |> float

            let calcAscenderAndDescender (font: PdfFont) =
                let fontMetrics = font.GetFontProgram().GetFontMetrics();
                let ascender,descender =
                    if (fontMetrics.GetWinAscender() = 0 
                        || fontMetrics.GetWinDescender() = 0 
                        || fontMetrics.GetTypoAscender() = fontMetrics.GetWinAscender() 
                        && fontMetrics.GetTypoDescender() = fontMetrics.GetWinDescender()) 
                    then
                        (fontMetrics.GetTypoAscender() |> float32) * TYPO_ASCENDER_SCALE_COEFF,
                        (fontMetrics.GetTypoDescender() |> float32) * TYPO_ASCENDER_SCALE_COEFF
                    else 
                        (fontMetrics.GetWinAscender() |> float32),
                        (fontMetrics.GetWinDescender() |> float32)
                
                ascender,descender

            /// float/pt
            let calcLineHeightUnit (font: PdfFont) =
                let (ascender, descender) = calcAscenderAndDescender font
                float (ascender - descender) / TEXT_SPACE_COEFF

            /// float/pt
            let calcLineWidthUnit (text: string) (font: PdfFont) =
                let line = font.CreateGlyphLine(text)
                let unit =
                    GlyphLine.getAllGlyphs line
                    |> List.map (fun gl -> gl.GetWidth())
                    |> List.sum
                    |> float
                unit / TEXT_SPACE_COEFF


        type TextRenderingMode = PdfCanvasConstants.TextRenderingMode

        type Rectangle with 
            member this.GetWidthF() = this.GetWidth() |> float
            member this.GetHeightF() = this.GetHeight() |> float
            member this.GetXF() = this.GetX() |> float
            member this.GetYF() = this.GetY() |> float
            member this.GetLeftF() = this.GetLeft() |> float
            member this.GetTopF() = this.GetTop() |> float
            member this.GetRightF() = this.GetRight() |> float
            member this.GetBottomF() = this.GetBottom() |> float

            member this.ToStaightLines() =
                let x = this.GetXF()
                let y = this.GetYF()
                let right = this.GetRightF()
                let top = this.GetTopF()
                [
                    {Start = Point(x,y);End = Point(x,top)}
                    {Start = Point(x,top);End = Point(right,top)}
                    {Start = Point(right,top);End = Point(right,y)}
                    {Start = Point(right,y);End = Point(x,y)}
                ]

            member this.GetEdgePoints() =
                this.ToStaightLines()
                |> List.collect(fun line -> [line.Start; line.End])
                |> List.distinct

            member this.IsOutsideOf(rect: Rectangle) =
                this.GetEdgePoints()
                |> List.forall(fun pt ->
                    pt.IsOutsideOf(rect)    
                )

            member this.IsCrossOf(rect: Rectangle) =
                this.GetEdgePoints() |> List.exists (fun pt ->
                    pt.IsInsideOf(rect)
                )

            member this.IsInsideOf(rect: Rectangle) =
                this.GetBottom() > rect.GetBottom()
                && this.GetTop() < rect.GetTop()
                && this.GetLeft() > rect.GetLeft()
                && this.GetRight() < rect.GetRight()

            member this.RelativePositionOf(rect: Rectangle) =
                if this.IsInsideOf(rect) then RelativePosition.Inbox
                elif this.IsOutsideOf(rect) then RelativePosition.OutBox
                elif this.IsCrossOf(rect) then RelativePosition.CrossBox
                else failwith "invalid token"

            member rect.GetPoint(position: Position) =
                let x = 
                    match position with 
                    | Position.XCenter (x, y) -> (rect.GetXF() + rect.GetRightF()) / 2. + x
                    | Position.Left (x, y) -> rect.GetXF() + x
                    | Position.Right (x, y) -> rect.GetRightF() + x
                    | _ -> failwith "Invalid token"


                let y = 
                    match position with 
                    | Position.YCenter (x, y) -> (rect.GetBottomF() + rect.GetTopF()) / 2. + y
                    | Position.Top (x, y) -> rect.GetTopF() + y
                    | Position.Bottom (x, y) -> rect.GetBottomF() + y
                    | _ -> failwith "Invalid token"

                new Point (x, y)


        and Point with
            member point.IsInsideOf (rect: Rectangle) =
                let x,y = point.x, point.y
                x > rect.GetXF() && x < rect.GetRightF() && y > rect.GetYF() && y < rect.GetTopF()

            member point.IsOutsideOf (rect: Rectangle) =
                let x,y = point.x, point.y
                x < rect.GetXF() || x > rect.GetRightF() || y < rect.GetYF() || y > rect.GetTopF()

        type StraightLine with 
            member line.IsOutsideOf (rect: Rectangle) =
                [line.Start; line.End] |> List.forall (fun pt ->
                    pt.IsOutsideOf rect
                )
                
            member line.IsCrossOf (rect: Rectangle) =
                let rectLines = rect.ToStaightLines()
                rectLines |> List.tryPick (fun sl ->
                    StraightLine.intersection sl line 
                )
                |> function 
                    | Some _ -> true
                    | None -> false


        [<RequireQualifiedAccess>]
        module Rectangle = 

            let inline create x y width height =
                let x = float32 x
                let y = float32 y 
                let width = float32 width
                let height = float32 height
                new Rectangle(x,y,width,height)
                
            /// <param name="points" at least two unique points></param>
            let ofPoints (points: Point seq) =
                let xs,ys = 
                    points 
                    |> Seq.map (fun p -> p.x,p.y) 
                    |> List.ofSeq
                    |> List.unzip

                let x = List.min xs
                let y = List.min ys
                let width = List.max xs - x 
                let height = List.max ys - y
                create x y width height


            /// <param name="rects" at least one rectange></param>
            let ofRectangles (rects: Rectangle seq) =
                rects |> Seq.collect (fun rect ->
                    let x = rect.GetXF()
                    let y = rect.GetYF()
                    let right = rect.GetRightF()
                    let top = rect.GetTopF()
                    [Point(x,y);Point(right,top)]
                ) |> ofPoints


            let toPdfArray (rect: Rectangle) =
                new PdfArray(rect)

            let toStraightLines (rect: Rectangle) =
                let x = rect.GetXF()
                let y = rect.GetYF()
                let right = rect.GetRightF()
                let top = rect.GetTopF()
                [
                    {Start = Point(x,y);End = Point(x,top)}
                    {Start = Point(x,top);End = Point(right,top)}
                    {Start = Point(right,top);End = Point(right,y)}
                    {Start = Point(right,y);End = Point(x,y)}
                ]


            let getXF (rect: Rectangle) =
                rect.GetX() |> float

            let getYF (rect: Rectangle) =
                rect.GetY() |> float

            let getRightF (rect: Rectangle) =
                rect.GetRight() |> float

            let getTopF (rect: Rectangle) =
                rect.GetTop() |> float

            let getWidthF (rect: Rectangle) =
                rect.GetWidth() |> float

            let rightTop (rect: Rectangle) =
                Point(rect.GetRightF(), rect.GetTopF())

            let leftBottom (rect: Rectangle) =
                Point(rect.GetXF(), rect.GetBottomF())

            let leftTop (rect: Rectangle) =
                Point(rect.GetXF(), rect.GetTopF())


            let getIntersection (rect1: Rectangle) (rect2: Rectangle) =
                rect1.GetIntersection(rect2)

            let equal (rect1: Rectangle) (rect2: Rectangle) =
                rect1.GetXF() @= rect2.GetXF()
                && rect1.GetWidthF() @= rect2.GetWidthF()
                && rect1.GetYF() @= rect2.GetYF()
                && rect1.GetHeightF() @= rect2.GetHeightF()

            /// only support Position.BottomEdge
            let increaseHeight (origin: Position) (height:float) (rect: Rectangle) = 
                match origin with 
                | Position.BottomEdge _ ->
                    let x = rect.GetXF()
                    let y = rect.GetYF()
                    let w = rect.GetWidthF()
                    let h = rect.GetHeightF() + height
                    create x y w h
                | _ -> failwith "not implemented"

            /// only support Position.BottomEdge
            let setHeight (origin: Position) (w: float) (rect: Rectangle) =
                match origin with 
                | Position.BottomEdge _ ->
                    let x = rect.GetXF()
                    let y = rect.GetYF()
                    let h = rect.GetHeightF()
                    create x y w h
                | _ -> failwith "not implemented"

            /// only support Position.LeftEdge
            let increaseWidth (origin: Position) (width:float) (rect: Rectangle) = 
                match origin with 
                | Position.LeftEdge _ ->
                    let x = rect.GetXF()
                    let y = rect.GetYF()
                    let w = rect.GetWidthF() + width
                    let h = rect.GetHeightF()
                    create x y w h

                | _ -> failwith "not implemented"

            /// only support Position.LeftEdge
            let setWidth (origin: Position) (w: float) (rect: Rectangle) =
                match origin with 
                | Position.LeftEdge _ ->
                    let x = rect.GetXF()
                    let y = rect.GetYF()
                    let h = rect.GetHeightF()
                    create x y w h
                | _ -> failwith "not implemented"

            /// only support Position.LeftBottomEdge
            let scale (origin: Position) scaleX scaleY (rect: Rectangle) =
                match origin with 
                | Position.LeftBottom (0., 0.) ->
                    let width = rect.GetWidthF() * scaleX
                    let height = rect.GetHeightF() * scaleY
                    let x = rect.GetXF()
                    let y = rect.GetYF()
                    create x y width height
                | _ -> failwith "not implemented"


            let getTile (tileIndexer: TileIndexer) (rect: Rectangle) =
                let colNum= tileIndexer.ColNum
                let rowNum = tileIndexer.RowNum
                let index = tileIndexer.Index

                let width = rect.GetWidthF() / float colNum
                let height = rect.GetHeightF() / float rowNum

                let x = 
                    let colIndex = index % colNum
                    rect.GetXF() + float colIndex * width
                
                let y =
                    let rowIndex = (index / colNum) % rowNum
                    rect.GetTopF() - height - float rowIndex * height

                create x y width height

        [<RequireQualifiedAccess>]
        module AffineTransform = 

            let create m00 m10 m01 m11 m02 m12 = 
                new AffineTransform(m00, m10, m01, m11, m02, m12)

            let toRecord (affineTransform: AffineTransform) =
                { ScaleX = affineTransform.GetScaleX() 
                  ShearX = affineTransform.GetShearX() 
                  ShearY = affineTransform.GetShearY() 
                  ScaleY = affineTransform.GetScaleY() 
                  TranslateX = affineTransform.GetTranslateX() 
                  TranslateY = affineTransform.GetTranslateY() }

            let ofRecord (record: AffineTransformRecord) =
                create 
                    record.m00
                    record.m10
                    record.m01
                    record.m11
                    record.m02
                    record.m12


            let ofMatrix (matrix: Matrix) =
                let values =
                    [| matrix.Get(Matrix.I11)
                       matrix.Get(Matrix.I12)
                       matrix.Get(Matrix.I21)
                       matrix.Get(Matrix.I22)
                       matrix.Get(Matrix.I31)
                       matrix.Get(Matrix.I32) |]

                new AffineTransform(values)

            let toMatrix (affineTransform: AffineTransform) =
                let values = Array.create 6 0.f
                affineTransform.GetMatrix(values)
                new Matrix(values.[Matrix.I11], values.[Matrix.I12], values.[Matrix.I21], values.[Matrix.I22], values.[Matrix.I31], values.[Matrix.I32])

            let transform (p0: Point) (affineTransform: AffineTransform) = 
                let p1 = new Point()
                affineTransform.Transform(p0,p1)

            let inverseTransform (p0: Point) (affineTransform: AffineTransform) = 
                let p1 = new Point()
                affineTransform.InverseTransform(p0,p1)


        type AffineTransform with 
            member this.Tranform(p: Point) =
                let p1 = new Point()
                this.Transform(p,p1)

            member this.InverseTranform(p: Point) =
                let p1 = new Point()
                this.InverseTransform(p,p1)

            member this.Transform(rect: Rectangle) = 
                let p1 = new Point (rect.GetXF(),rect.GetYF())
                let p2 = new Point (rect.GetRightF(),rect.GetTopF())
                let p3 = new Point (rect.GetXF(),rect.GetTopF())
                let p4 = new Point (rect.GetRightF(),rect.GetYF())
                [p1; p2; p3 ;p4] |> List.map (fun p -> AffineTransform.transform p this) |> Rectangle.ofPoints

            member this.InverseTransform(rect: Rectangle) = 
                let p1 = new Point (rect.GetXF(),rect.GetYF())
                let p2 = new Point (rect.GetRightF(),rect.GetTopF())
                let p3 = new Point (rect.GetXF(),rect.GetTopF())
                let p4 = new Point (rect.GetRightF(),rect.GetYF())
                [p1; p2; p3 ;p4] |> List.map (fun p -> AffineTransform.inverseTransform p this) |> Rectangle.ofPoints
    
        [<RequireQualifiedAccess>]
        module AffineTransformRecord =
            let ofAffineTransform (affineTransform: AffineTransform) =
                AffineTransform.toRecord affineTransform

            let toAffineTransform (record: AffineTransformRecord) =
                AffineTransform.ofRecord record

            let ofMatrix (matrix: Matrix) =
                AffineTransform.ofMatrix matrix
                |> ofAffineTransform

            let toMatrix (record: AffineTransformRecord) =
                AffineTransform.ofRecord record
                |> AffineTransform.toMatrix

        type AbstractRenderInfo with 
            member this.GetFillColor() =
                match this with 
                | :? PathRenderInfo as info -> info.GetFillColor()
                | :? TextRenderInfo as info -> info.GetFillColor()
                | _ -> failwith "Not implemented"

            member this.GetStrokeColor() =
                match this with 
                | :? PathRenderInfo as info -> 
                    info.GetStrokeColor()
                | :? TextRenderInfo as info -> info.GetStrokeColor()
                | _ -> failwith "Not implemented"


        [<RequireQualifiedAccess>]
        module Subpath =

            let toRawPoints (subpath: Subpath) =
                subpath.GetPiecewiseLinearApproximation()

            let toActualPoints (ctm: Matrix) subpath =
                toRawPoints subpath
                |> Seq.map (fun pt -> (AffineTransform.ofMatrix ctm).Tranform(pt))
                |> List.ofSeq

            let getActualBound ctm (subpath: Subpath) =
                toActualPoints ctm subpath
                |> Rectangle.ofPoints

            let isNotEmpty (subpath: Subpath) =
                let points = toRawPoints subpath
                points.Count > 0

        [<RequireQualifiedAccess>]
        module PathRenderInfo =

            let [<Literal>] FILLANDSTROKE = PathRenderInfo.FILL ||| PathRenderInfo.STROKE

            /// without ctm applied
            let toRawPoints (prInfo: PathRenderInfo) =
                let subpaths = prInfo.GetPath().GetSubpaths()
                subpaths |> Seq.collect Subpath.toRawPoints

            /// with ctm applied
            let toActualPoints (prInfo: PathRenderInfo) =
                let ctm = prInfo.GetCtm()
                prInfo.GetPath().GetSubpaths()
                |> List.ofSeq
                |> List.collect (Subpath.toActualPoints ctm)

            let getBound (info: PathRenderInfo) =     
                info |> toActualPoints |> Rectangle.ofPoints

            let isStrokeVisible (info: PathRenderInfo) =             
                info.GetPath().GetSubpaths().Count <> 0
                && 
                    match info.GetOperation() with 
                    | PathRenderInfo.STROKE
                    | FILLANDSTROKE -> true
                    | _ -> false

            let isFillVisible (info: PathRenderInfo) =   
                info.GetPath().GetSubpaths().Count <> 0
                &&
                    match info.GetOperation() with 
                    | PathRenderInfo.FILL 
                    | FILLANDSTROKE -> true
                    | _ -> false

            let isVisible (info: PathRenderInfo) =
                isFillVisible info || isStrokeVisible info

            let getVisibleColors (info: PathRenderInfo) =
                if isVisible info 
                then
                    let fillColor = info.GetFillColor()
                    let strokeColor = info.GetStrokeColor()
                    match info.GetOperation() with 
                    | PathRenderInfo.STROKE -> [strokeColor]
                    | FILLANDSTROKE -> [fillColor;strokeColor]
                    | PathRenderInfo.FILL -> [fillColor]
                    | others -> 
                        failwithf "unSupported path render operation %d" others
                else []

        [<RequireQualifiedAccess>]
        module TextRenderInfo =

            /// GetFontSize() * ctm.m00
            let getActualFontSize (info: TextRenderInfo) =
                let matrix = info.GetTextMatrix()
                info.GetFontSize() * matrix.Get(0) |> float

            let getHeight (info: TextRenderInfo) =
                let ascent = info.GetAscentLine()
                let descent = info.GetDescentLine()
                ascent.GetStartPoint().Get(1) - descent.GetStartPoint().Get(1)
                |> float

            let getWidth (info: TextRenderInfo) =
                let ascent = info.GetAscentLine()
                ascent.GetEndPoint().Get(0) - ascent.GetStartPoint().Get(0)
                |> float

            let getY (info: TextRenderInfo) =
                let descent = info.GetDescentLine()
                descent.GetStartPoint().Get(1)
                |> float

            let getX (info: TextRenderInfo) =
                let descent = info.GetDescentLine()
                descent.GetStartPoint().Get(0)

            let getBound (info: TextRenderInfo) =
                let width = getWidth info
                let height = getHeight info
                let x = getX info
                let y = getY info
                Rectangle.create x y width height


            let getText (info:TextRenderInfo) =
                info.GetText()

            let isFillVisible (info: TextRenderInfo) =             
                let md = info.GetTextRenderMode()
                match md with 
                | PdfCanvasConstants.TextRenderingMode.FILL 
                | PdfCanvasConstants.TextRenderingMode.FILL_STROKE -> true
                | _ -> false

            let isStrokeVisible (info: TextRenderInfo) =             
                let md = info.GetTextRenderMode()
                match md with 
                | PdfCanvasConstants.TextRenderingMode.STROKE
                | PdfCanvasConstants.TextRenderingMode.FILL_STROKE  
                | PdfCanvasConstants.TextRenderingMode.STROKE_CLIP -> true
                |  _ -> false

            
            let isVisible (info: TextRenderInfo) =
                isFillVisible info || isStrokeVisible info

            let getVisibleColors (info: TextRenderInfo) =
                let fillColor = info.GetFillColor()
                let strokeColor = info.GetStrokeColor()
                match info.GetTextRenderMode() with
                | TextRenderingMode.FILL_STROKE ->
                    [
                        fillColor
                        strokeColor
                    ]

                | TextRenderingMode.STROKE ->
                    [
                        strokeColor
                    ]

                | TextRenderingMode.FILL ->
                    [
                        fillColor
                    ]

                | others -> 
                    Logger.unSupportedTextRenderMode others
                    []


    
        [<RequireQualifiedAccess>]
        module AbstractRenderInfo = 
            let cata (fTextRenderInfo) (fPathRenderInfo) (info: AbstractRenderInfo) = 
                match info with 
                | :? TextRenderInfo as trInfo -> fTextRenderInfo trInfo
                | :? PathRenderInfo as prInfo -> fPathRenderInfo prInfo 
                | _ -> failwith "Invaid token"

            let getBound info = cata TextRenderInfo.getBound PathRenderInfo.getBound info
            
            let getVisibleColors (info: AbstractRenderInfo) = cata TextRenderInfo.getVisibleColors PathRenderInfo.getVisibleColors info

            let isTextRenderInfo (info: AbstractRenderInfo) = cata (fun _ -> true) (fun _ -> false) info

            let isPathRenderInfo (info: AbstractRenderInfo) = cata (fun _ -> false) (fun _ -> true) info

            let asTextRenderInfo (info: AbstractRenderInfo) = cata (fun trInfo -> Some trInfo) (fun _ -> None) info

            let asPathRenderInfo (info: AbstractRenderInfo) = cata (fun _ -> None) (fun prInfo -> Some prInfo) info

            let isFillVisible (info: AbstractRenderInfo) = cata TextRenderInfo.isFillVisible PathRenderInfo.isFillVisible info

            let isStrokeVisible (info: AbstractRenderInfo) = cata TextRenderInfo.isStrokeVisible PathRenderInfo.isStrokeVisible info

            let isVisible (info: AbstractRenderInfo) = cata TextRenderInfo.isVisible PathRenderInfo.isVisible info



        [<RequireQualifiedAccess>]
        module CanvasGraphicsState =
            let getDashPattern (gs: CanvasGraphicsState) =
                let dashPattern = gs.GetDashPattern()
                let values = dashPattern |> List.ofSeq
                let l = values |> List.length
                let dashArray = 
                    values.[0..l-2] 
                    |> List.collect (fun dashArray -> 
                        dashArray :?> PdfArray 
                        |> List.ofSeq 
                        |> List.map (fun dashValue -> 
                            let number = dashValue :?> PdfNumber
                            number.GetValue()
                        ))
                let phase = (values.[l-1] :?> PdfNumber).GetValue()

                { DashArray = dashArray 
                  Phase = phase }



        [<RequireQualifiedAccess>]
        module PdfFormXObject =

            let getBBox (xobject:PdfFormXObject) =
                xobject.GetBBox().ToRectangle()

            let setBBox (rect: Rectangle) (xobject:PdfFormXObject) =
                xobject.SetBBox(rect |> Rectangle.toPdfArray)


            let private tryGetPageBoxByPdfName (pdfName: PdfName) (xobject:PdfFormXObject) =
                let rect = xobject.GetPdfObject().GetAsRectangle(pdfName)

                match rect with 
                | null -> None
                | rect -> Some rect

            let tryGetTrimBox (xobject:PdfFormXObject) =
                tryGetPageBoxByPdfName PdfName.TrimBox xobject

            let tryGetCropBox (xobject:PdfFormXObject) =
                tryGetPageBoxByPdfName PdfName.CropBox xobject

            let tryGetArtBox (xobject:PdfFormXObject) =
                tryGetPageBoxByPdfName PdfName.ArtBox xobject


            let tryGetBleedBox (xobject:PdfFormXObject) =
                tryGetPageBoxByPdfName PdfName.BleedBox xobject


            let tryGetMediaBox (xobject:PdfFormXObject) =
                tryGetPageBoxByPdfName PdfName.MediaBox xobject


            let getRotateValue (xobject:PdfFormXObject) =
                match xobject.GetPdfObject().GetAsNumber(PdfName.Rotate) with 
                | null -> 0.
                | rotate -> rotate.GetValue()

            /// if target pagebox is undefined, using bbox instead
            let getPageBox (pageBoxKind: PageBoxKind) (xobject: PdfFormXObject) =
                let pdfName = 
                    match pageBoxKind with 
                    | PageBoxKind.ArtBox -> Some PdfName.ArtBox
                    | PageBoxKind.BleedBox -> Some PdfName.BleedBox
                    | PageBoxKind.TrimBox -> Some PdfName.TrimBox
                    | PageBoxKind.CropBox -> Some PdfName.CropBox
                    | PageBoxKind.MediaBox -> Some PdfName.MediaBox
                    | PageBoxKind.ActualBox -> None

                    | _ -> failwith "Invalid token"
                match pdfName with 
                | Some pdfName -> 
                    match tryGetPageBoxByPdfName pdfName xobject with 
                    | Some pageBox -> pageBox
                    | None -> getBBox xobject
                | None ->getBBox xobject


        [<RequireQualifiedAccess>]
        type PdfCanvasColor =
            | N
            | Specific of Color 


        type PdfCanvasAddRectangleArguments =
            { LineWidth: float 
              StrokeColor: PdfCanvasColor
              FillColor: PdfCanvasColor }
        with 
            static member DefaultValue =
                { LineWidth = mm 0.1 
                  StrokeColor = PdfCanvasColor.Specific DeviceGray.BLACK 
                  FillColor = PdfCanvasColor.N }

        type PdfCanvas with 
            member x.AddXObject(xObject: PdfXObject, affineTransformRecord: AffineTransformRecord) =
                x.AddXObject
                    ( xObject,
                      float32 affineTransformRecord.m00,
                      float32 affineTransformRecord.m10,
                      float32 affineTransformRecord.m01,
                      float32 affineTransformRecord.m11,
                      float32 affineTransformRecord.m02,
                      float32 affineTransformRecord.m12 )

            member x.SetStrokeColor(pdfCanvasColor: PdfCanvasColor) =
                match pdfCanvasColor with 
                | PdfCanvasColor.N -> x
                | PdfCanvasColor.Specific color -> x.SetStrokeColor(color)

            member x.SetFillColor(pdfCanvasColor: PdfCanvasColor) =
                match pdfCanvasColor with 
                | PdfCanvasColor.N -> x
                | PdfCanvasColor.Specific color -> x.SetFillColor(color)


        [<RequireQualifiedAccess>]
        module PdfCanvas = 

            /// saveState -> action -> restoreState
            let useCanvas (canvas: #PdfCanvas) action =
                canvas.SaveState() |> ignore
                
                let canvas: #PdfCanvas = action canvas

                canvas.RestoreState() |> ignore

            let setDashpattern (dashPattern: DashPattern) (canvas:PdfCanvas) =
                canvas.SetLineDash(dashPattern.DashArrayF32, dashPattern.PhaseF32)

            let setStrokeColor (color: PdfCanvasColor) (canvas:PdfCanvas) =
                canvas.SetStrokeColor(color)

            let setLineWidth (width: float) (canvas: PdfCanvas) =
                canvas.SetLineWidth(float32 width)

            let setFillColor (color: PdfCanvasColor) (canvas:PdfCanvas) =
                canvas.SetFillColor(color)

            let fill (canvas:PdfCanvas) =
                canvas.Fill()

            let fillStroke (canvas:PdfCanvas) =
                canvas.FillStroke()

            let stroke (canvas:PdfCanvas) =
                canvas.Stroke()

            let endPath (canvas: PdfCanvas) =
                canvas.EndPath()

            let showText (text: string) (canvas:PdfCanvas)=
                canvas.ShowText(text)

            let rectangle (rect: Rectangle) (canvas: PdfCanvas) =
                canvas.Rectangle(rect)

            let setTextRendingMode textRenderingMode (canvas:PdfCanvas)=
                canvas.SetTextRenderingMode(textRenderingMode)

            let addXObject (xobject: PdfFormXObject) (affineTransformRecord: AffineTransformRecord) (canvas: PdfCanvas) =
                canvas.AddXObject (xobject, affineTransformRecord)

            let beginText (canvas: PdfCanvas) =
                canvas.BeginText()

            let endText (canvas: PdfCanvas) =
                canvas.EndText()

            let addRectangle rect (mapping: PdfCanvasAddRectangleArguments -> PdfCanvasAddRectangleArguments) (canvas: PdfCanvas) =
                let args = mapping PdfCanvasAddRectangleArguments.DefaultValue
                let close =
                    match args.FillColor, args.StrokeColor with 
                    | PdfCanvasColor.N, PdfCanvasColor.N -> endPath
                    | PdfCanvasColor.Specific _, PdfCanvasColor.N -> fill
                    | PdfCanvasColor.N , PdfCanvasColor.Specific _ -> stroke
                    | PdfCanvasColor.Specific _, PdfCanvasColor.Specific _ -> fillStroke

                canvas
                |> setStrokeColor args.StrokeColor
                |> setLineWidth args.LineWidth
                |> rectangle rect
                |> close


        type CanvasAddTextArguments = 
            { PdfFontFactory: PdfFontFactory 
              CanvasFontSize: CanvasFontSize 
              FontColor: Color
              FontRotation: Rotation 
              Position: Position }

        with 
            static member DefaultValue =
                { PdfFontFactory = PdfFontFactory.StandardFonts (iText.IO.Font.Constants.StandardFonts.HELVETICA_BOLD)
                  CanvasFontSize = CanvasFontSize.Numeric 9.
                  FontColor = DeviceGray.BLACK 
                  FontRotation = Rotation.None
                  Position = Position.LeftTop (0., 0.)}


        [<RequireQualifiedAccess>]
        module Canvas =
            let addText
                text
                (mapping: CanvasAddTextArguments -> CanvasAddTextArguments)
                (canvas: Canvas) = 
                    let args = mapping CanvasAddTextArguments.DefaultValue
                    let pdfFontFactory, canvasFontSize, fontColor, fontRotation, position = 
                        args.PdfFontFactory, args.CanvasFontSize, args.FontColor, args.FontRotation, args.Position

                    let pdfFont =
                        let pdfDocument = canvas.GetPdfDocument() :?> PdfDocumentWithCachedResources
                        pdfDocument.GetOrCreatePdfFont(pdfFontFactory)

                    let fontSize =
                        let fontSizeOfArea (rect: Rectangle) = 
                            let lineHeightUnit = PdfFont.calcLineHeightUnit (pdfFont)
                            let lineWidthUnit = PdfFont.calcLineWidthUnit text pdfFont
            
                            let fontSize =
                                let horizonalMaxSize = rect.GetWidthF() / lineWidthUnit
                                let verticalMaxSize = rect.GetHeightF() / lineHeightUnit
                                (min verticalMaxSize horizonalMaxSize)

                            fontSize

                        match canvasFontSize with 
                        | CanvasFontSize.Numeric size -> size
                        | CanvasFontSize.OfRootArea (scale) ->
                            let area = canvas.GetRootArea()
                            fontSizeOfArea area * scale

                        | CanvasFontSize.OfArea (area) ->
                            fontSizeOfArea area

                    let point =
                        let rootArea = canvas.GetRootArea()
                        rootArea.GetPoint(position)

                    let horizonal = 
                        match position with 
                        | Position.XCenter (x, y) -> TextAlignment.CENTER
                        | Position.Left (x, y) -> TextAlignment.LEFT
                        | Position.Right (x, y) -> TextAlignment.RIGHT
                        | _ -> failwith "Invalid token"


                    let vertical = 
                        match position with 
                        | Position.YCenter (x, y) -> VerticalAlignment.MIDDLE
                        | Position.Top (x, y) -> VerticalAlignment.TOP
                        | Position.Bottom (x, y) -> VerticalAlignment.BOTTOM
                        | _ -> failwith "Invalid token"


                    canvas
                        .SetFont(pdfFont)
                        .SetFontColor(fontColor)
                        .SetFontSize(float32 fontSize)
                        .ShowTextAligned(text,float32 point.x,float32 point.y, Nullable(horizonal), Nullable(vertical), float32 (Rotation.getAngle fontRotation))

                
        type PdfPage with
            member this.GetActualBox() = 
                let crop = this.GetCropBox()
                let media = this.GetMediaBox()
                Rectangle.getIntersection crop media

            member this.GetActualHeight() = this.GetActualBox().GetHeight() |> float

            member this.GetActualWidth() = this.GetActualBox().GetWidth() |> float

            member page.GetPageBox(pageBoxKind) =
                match pageBoxKind with 
                | PageBoxKind.ArtBox -> page.GetArtBox()
                | PageBoxKind.BleedBox -> page.GetBleedBox()
                | PageBoxKind.TrimBox -> page.GetTrimBox()
                | PageBoxKind.CropBox -> page.GetCropBox()
                | PageBoxKind.ActualBox -> page.GetActualBox()
                | PageBoxKind.AllBox -> failwith "PageBoxKind.AllBox is settable only"
                | _ -> failwith "Invalid token"


        [<RequireQualifiedAccess>]
        module PdfPage = 

            let setMediaBox (rect: Rectangle) (page: PdfPage) =
                page.SetMediaBox rect

            let setCropBox (rect: Rectangle) (page: PdfPage) =
                page.SetCropBox rect

            let setArtBox (rect: Rectangle) (page: PdfPage) =
                page.SetArtBox rect

            let setTrimBox (rect: Rectangle) (page: PdfPage) =
                page.SetTrimBox rect

            let setBleedBox (rect: Rectangle) (page: PdfPage) =
                page.SetBleedBox rect

            let setPageBox pageBoxKind (rect: Rectangle) (page: PdfPage) =
                match pageBoxKind with 
                | PageBoxKind.TrimBox -> setTrimBox rect page
                | PageBoxKind.CropBox -> setCropBox rect page
                | PageBoxKind.ActualBox ->
                    page 
                    |> setMediaBox rect
                    |> setCropBox rect

                | PageBoxKind.AllBox ->
                    page 
                    |> setMediaBox rect
                    |> setCropBox rect
                    |> setTrimBox rect
                    |> setArtBox rect
                    |> setBleedBox rect

                | _ -> failwith "Invalid token"

            let getActualWidth (page: PdfPage) = 
                page.GetActualBox().GetWidth() |> float

            let getTrimBox (page: PdfPage) =
                page.GetTrimBox()

            let getCropBox (page: PdfPage) =
                page.GetTrimBox()


            let getMediaBox (page: PdfPage) =
                page.GetMediaBox()

            let getActualHeight (page: PdfPage) = 
                page.GetActualBox().GetHeight() |> float

            let getPageBox pageBoxKind (page: PdfPage) =
                page.GetPageBox(pageBoxKind)

            let getActualBox (page: PdfPage) =
                page.GetActualBox()

        [<RequireQualifiedAccess>]
        module PdfDocument =
            let getPages (doc: PdfDocument) =
                [
                    for i = 1 to doc.GetNumberOfPages() do
                        yield doc.GetPage(i)
                ]


