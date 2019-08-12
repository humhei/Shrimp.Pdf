namespace Shrimp.Pdf
open iText.Kernel.Geom
open iText.IO.Font
open iText.IO.Font.Otf
open iText.Kernel.Font
open Akka.Configuration
open System.Reflection
open System.IO
open System


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
    | TrimBox = 0
    | ActualBox = 1
    | CropBox = 2
    | AllBox = 3

[<RequireQualifiedAccess>]
type RelativePosition =
    | Inbox 
    | CrossBox
    | OutBox

type StraightLine =
    { Start: Point
      End: Point }

[<RequireQualifiedAccess>]
type Position =
    | LeftBottom of float * float
    | Left of float * float
    | LeftTop of float * float
    | Top of float * float
    | RightTop of float * float
    | Right of float * float
    | RightBottom of float * float
    | Bottom of float * float
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
    let (|LeftEdge|_|) = function
        | Position.LeftTop (0., _) 
        | Position.LeftBottom (0., _) 
        | Position.Left(0., _) -> Some ()
        | _ -> None
    
    let (|BottomEdge|_|) = function
        | Position.LeftBottom (_, 0.)
        | Position.RightBottom (_, 0.)
        | Position.Bottom (_, 0.)-> Some ()
        | _ -> None

    let (|TopEdge|_|) = function
        | Position.LeftTop (_, 0.) 
        | Position.RightTop (_, 0.)
        | Position.Top(_, 0.) -> Some ()
        | _ -> None

    let (|RightEdge|_|) = function
        | Position.RightBottom (0., _) 
        | Position.RightTop (0., _)
        | Position.Right (0., _) -> Some ()
        | _ -> None

[<RequireQualifiedAccess>]
module StraightLine =
    /// http://www.navision-blog.de/blog/2008/12/02/calculate-the-intersection-of-two-lines-in-fsharp-2d/
    let calcIntersection (line1: StraightLine) (line2: StraightLine) =
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
                    StraightLine.calcIntersection sl line 
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
            let createFromPoints (points: Point seq) =
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
            let createFromRectangles (rects: Rectangle seq) =
                rects |> Seq.collect (fun rect ->
                    let x = rect.GetXF()
                    let y = rect.GetYF()
                    let top = rect.GetTopF()
                    let right = rect.GetRightF()
                    [Point(x,y);Point(top,right)]
                ) |> createFromPoints


            let asPdfArray (rect: Rectangle) =
                new PdfArray(rect)

            let asStraightLines (rect: Rectangle) =
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


            let min (rect1: Rectangle) (rect2: Rectangle) =
                if rect1.IsInsideOf(rect2)
                then rect1
                else rect2

            let equal (rect1: Rectangle) (rect2: Rectangle) =
                rect1.GetXF() @= rect2.GetXF()
                && rect1.GetWidthF() @= rect2.GetWidthF()
                && rect1.GetYF() @= rect2.GetYF()
                && rect1.GetHeightF() @= rect2.GetHeightF()

            /// only support Origin.Bottom
            let increaseHeight (origin: Position) (height:float) (rect: Rectangle) = 
                match origin with 
                | Position.Bottom _ ->
                    let x = rect.GetXF()
                    let y = rect.GetYF()
                    let w = rect.GetWidthF()
                    let h = rect.GetHeightF() + height
                    create x y w h
                | _ -> failwith "not implemented"

            /// only support Origin.Bottom
            let setHeight (origin: Position) (w: float) (rect: Rectangle) =
                match origin with 
                | Position.Bottom _ ->
                    let x = rect.GetXF()
                    let y = rect.GetYF()
                    let h = rect.GetHeightF()
                    create x y w h
                | _ -> failwith "not implemented"

            /// only support Origin.Left
            let increaseWidth (origin: Position) (width:float) (rect: Rectangle) = 
                match origin with 
                | Position.LeftEdge _ ->
                    let x = rect.GetXF()
                    let y = rect.GetYF()
                    let w = rect.GetWidthF() + width
                    let h = rect.GetHeightF()
                    create x y w h

                | _ -> failwith "not implemented"

            /// only support Origin.Left
            let setWidth (origin: Position) (w: float) (rect: Rectangle) =
                match origin with 
                | Position.LeftEdge _ ->
                    let x = rect.GetXF()
                    let y = rect.GetYF()
                    let h = rect.GetHeightF()
                    create x y w h
                | _ -> failwith "not implemented"

            /// only support Origin.LeftBottom
            let scale (origin: Position) scaleX scaleY (rect: Rectangle) =
                match origin with 
                | Position.LeftBottom (0., 0.) ->
                    let width = rect.GetWidthF() * scaleX
                    let height = rect.GetHeightF() * scaleY
                    let x = rect.GetXF()
                    let y = rect.GetYF()
                    create x y width height
                | _ -> failwith "not implemented"


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
                [p1; p2; p3 ;p4] |> List.map (fun p -> AffineTransform.transform p this) |> Rectangle.createFromPoints

            member this.InverseTransform(rect: Rectangle) = 
                let p1 = new Point (rect.GetXF(),rect.GetYF())
                let p2 = new Point (rect.GetRightF(),rect.GetTopF())
                let p3 = new Point (rect.GetXF(),rect.GetTopF())
                let p4 = new Point (rect.GetRightF(),rect.GetYF())
                [p1; p2; p3 ;p4] |> List.map (fun p -> AffineTransform.inverseTransform p this) |> Rectangle.createFromPoints
    
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
                |> Rectangle.createFromPoints

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
                info |> toActualPoints |> Rectangle.createFromPoints

            let isStrokeVisible (info: PathRenderInfo) =             
                info.GetPath().GetSubpaths().Count <> 0
                && 
                    match info.GetOperation() with 
                    | PathRenderInfo.FILL
                    | FILLANDSTROKE -> true
                    | _ -> false

            let isFillVisible (info: PathRenderInfo) =   
                info.GetPath().GetSubpaths().Count <> 0
                &&
                    match info.GetOperation() with 
                    | PathRenderInfo.STROKE 
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


            let getText (rdinfo:TextRenderInfo) =
                rdinfo.GetText()

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
            let cata (fTRInfo) (fPRInfo) (info: AbstractRenderInfo) = 
                match info with 
                | :? TextRenderInfo as trInfo -> fTRInfo trInfo
                | :? PathRenderInfo as prInfo -> fPRInfo prInfo 
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
                xobject.SetBBox(rect |> Rectangle.asPdfArray)

            let tryGetTrimBox (xobject:PdfFormXObject) =
                let rect = xobject.GetPdfObject().GetAsRectangle(PdfName.TrimBox)

                match rect with 
                | null -> None
                | rect -> Some rect

            let tryGetCropBox (xobject:PdfFormXObject) =
                let rect = xobject.GetPdfObject().GetAsRectangle(PdfName.CropBox)

                match rect with 
                | null -> None
                | rect -> Some rect

            let getRotateValue (xobject:PdfFormXObject) =
                match xobject.GetPdfObject().GetAsNumber(PdfName.Rotate) with 
                | null -> 0.
                | rotate -> rotate.GetValue()

            /// if target pagebox is undefined, using bbox instead
            let getPageBox (pageBoxKind: PageBoxKind) (xobject: PdfFormXObject) =
                match pageBoxKind with 
                | PageBoxKind.TrimBox ->
                    match tryGetTrimBox xobject with 
                    | Some trimBox -> trimBox
                    | None -> getBBox xobject
                | PageBoxKind.CropBox ->
                    match tryGetCropBox xobject with 
                    | Some trimBox -> trimBox
                    | None -> getBBox xobject
                | PageBoxKind.ActualBox ->
                    getBBox xobject

                | _ -> failwith "Invalid token"




        type PdfCanvas with 
            member x.AddXObject(xObject: PdfXObject, affineTransformRecord: AffineTransformRecord) =
                x.AddXObject
                    ( xObject,float32 affineTransformRecord.m00,
                      float32 affineTransformRecord.m10,
                      float32 affineTransformRecord.m01,
                      float32 affineTransformRecord.m11,
                      float32 affineTransformRecord.m02,
                      float32 affineTransformRecord.m12 )


        [<RequireQualifiedAccess>]
        module PdfCanvas = 

            /// saveState -> f -> restoreState
            let useCanvas (canvas: #PdfCanvas) f =
                canvas.SaveState() |> ignore
                f(canvas)
                canvas.RestoreState() |> ignore

            let setDashpattern (dashPattern: DashPattern) (canvas:PdfCanvas) =
                canvas.SetLineDash(dashPattern.DashArrayF32, dashPattern.PhaseF32)

            let setStrokeColor color (canvas:PdfCanvas) =
                canvas.SetStrokeColor(color)

            let setLineWidth (width: float) (canvas: PdfCanvas) =
                canvas.SetLineWidth(float32 width)

            let setFillColor color (canvas:PdfCanvas) =
                canvas.SetFillColor(color)

            let stroke (canvas:PdfCanvas) =
                canvas.Stroke()

            let endPath (canvas: PdfCanvas) =
                canvas.EndPath()

            let showText (text: string) (canvas:PdfCanvas)=
                canvas.ShowText(text)

            let setTextRendingMode textRenderingMode (canvas:PdfCanvas)=
                canvas.SetTextRenderingMode(textRenderingMode)

            let addXObject (xobject: PdfFormXObject) (x: float) (y: float) (canvas: PdfCanvas) =
                canvas.AddXObject (xobject,float32 x,float32 y)



                
        type PdfPage with
            member this.GetActualBox() = 
                let crop = this.GetCropBox()
                let media = this.GetMediaBox()
                Rectangle.min crop media

            member this.GetActualHeight() = this.GetActualBox().GetHeight() |> float
            member this.GetActualWidth() = this.GetActualBox().GetWidth() |> float

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
                match pageBoxKind with 
                | PageBoxKind.TrimBox -> page.GetTrimBox()
                | PageBoxKind.CropBox -> page.GetCropBox()
                | PageBoxKind.ActualBox ->
                    Rectangle.min (getCropBox page) (getMediaBox page)
                | _ -> failwith "Invalid token"

            let getActualBox (page: PdfPage) =
                page.GetActualBox()

        [<RequireQualifiedAccess>]
        module PdfDocument =
            let getPages (doc: PdfDocument) =
                [
                    for i = 1 to doc.GetNumberOfPages() do
                        yield doc.GetPage(i)
                ]

        [<RequireQualifiedAccess>]
        module Canvas =
            open iText.Layout
            open iText.Kernel.Colors

            let useCanvas (page: PdfPage) (rootArea: Rectangle) f =
                let doc = page.GetDocument()
                let pdfCanvas = new PdfCanvas(page)
                PdfCanvas.useCanvas pdfCanvas (fun pdfCanvas ->
                    let canvas = 
                        new Canvas(pdfCanvas,doc,rootArea,true)
                    f canvas
                )

            let useCanvasInPageBox (page: PdfPage) (pageBoxKind: PageBoxKind) f =
                let bbox = PdfPage.getPageBox pageBoxKind page
                useCanvas page bbox f
