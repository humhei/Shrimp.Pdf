namespace Shrimp.Pdf.Extensions
open iText.Kernel.Geom
open iText.IO.Font
open iText.IO.Font.Otf
open iText.Kernel.Font
open Akka.Configuration
open System
open System.IO
open iText.Layout
open iText.Kernel.Colors
open iText.Layout.Properties
open System.Collections.Concurrent
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Xobject
open Shrimp.Pdf




[<AutoOpen>]
module iText = 

    [<RequireQualifiedAccess>]
    module GlyphLine =
        let getAllGlyphs (glyphLine: GlyphLine) =
            [ for i = 0 to glyphLine.Size() - 1 do 
                yield glyphLine.Get(i) ]


    [<RequireQualifiedAccess>]
    module PdfFont =

        let private TYPO_ASCENDER_SCALE_COEFF = 1.2f

        let private TEXT_SPACE_COEFF = FontProgram.UNITS_NORMALIZATION |> float

        let private calcAscenderAndDescender (font: PdfFont) =
            let fontMetrics = font.GetFontProgram().GetFontMetrics()
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

        /// px/pt
        let calcLineHeightUnit (font: PdfFont) =
            let (ascender, descender) = calcAscenderAndDescender font
            float (ascender - descender) / TEXT_SPACE_COEFF

        /// px/pt
        let calcLineWidthUnits (text: string) (font: PdfFont) =
            let linesOfText = text.Split([|"\r\n"; "\n"|], StringSplitOptions.None)


            linesOfText 
            |> List.ofArray
            |> List.map (fun line ->
                let line = font.CreateGlyphLine(line)
                let unit =
                    GlyphLine.getAllGlyphs line
                    |> List.map (fun gl -> gl.GetWidth())
                    |> List.sum
                    |> float
                unit / TEXT_SPACE_COEFF
            )


        let private verticalMaxSizeWhenParagraphedHeightIs height (text: string) font =
            let heightUnit = calcLineHeightUnit font
            let baseFontSize = height / heightUnit
            let linesOfText = text.Split([|"\r\n"; "\n"|], StringSplitOptions.None)

            baseFontSize / float linesOfText.Length 

        let fontSizeOfArea (rect: Rectangle) (text: string) font =
            let width = float (rect.GetWidth())
            let height = float (rect.GetHeight())

            let lineWidthUnits = calcLineWidthUnits text font
            let horizonalMaxSize = width / List.max lineWidthUnits
            let verticalMaxSize = 
                verticalMaxSizeWhenParagraphedHeightIs (height) text font
            
            min horizonalMaxSize verticalMaxSize

        /// px/pt
        let calcLineWidthWhenParagraphedHeightIs height (text: string) (font: PdfFont) =
            let fontSize = verticalMaxSizeWhenParagraphedHeightIs height text font
            let widthUnits = calcLineWidthUnits text font
            List.max widthUnits * fontSize 


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

        let isInsideOf (paramRect) (rect: Rectangle) =
            rect.IsInsideOf(paramRect)

        let isOutsideOf (paramRect) (rect: Rectangle) =
            rect.IsOutsideOf(paramRect)

        let isCrossOf (paramRect) (rect: Rectangle) =
            rect.IsCrossOf(paramRect)

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
            (create x y width height)


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


        let tryGetIntersection (rect1: Rectangle) (rect2: Rectangle) =
            match rect1.GetIntersection(rect2) with 
            | null -> None
            | rect -> Some rect

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

        let applyMargin (margin:Margin) (rect: Rectangle) =
            let left = margin.Left
            let top = margin.Top
            let right = margin.Right
            let bottom = margin.Bottom
            let x = rect.GetXF() - left
            let y = rect.GetYF() - bottom
            let width = rect.GetWidthF() + left + right 
            let height = rect.GetHeightF() + top + bottom
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
        member this.Transform(p: Point) =
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
            |> Seq.map (fun pt -> (AffineTransform.ofMatrix ctm).Transform(pt))

        let getActualBound ctm (subpath: Subpath) =
            toActualPoints ctm subpath
            |> Rectangle.ofPoints

        let isNotEmpty (subpath: Subpath) =
            let points = toRawPoints subpath
            points.Count > 0


    type BoundGettingOptions =
        | WithStrokeWidth = 0
        | WithoutStrokeWidth = 1




    [<RequireQualifiedAccess>]
    module IPathRenderInfo =

        let [<Literal>] FILLANDSTROKE = PathRenderInfo.FILL ||| PathRenderInfo.STROKE

        /// without ctm applied
        let toRawPoints (info: IPathRenderInfo) =
            let info = info.Value
            let subpaths = info.GetPath().GetSubpaths()
            subpaths |> Seq.collect Subpath.toRawPoints

        /// with ctm applied
        let toActualPoints (info: IPathRenderInfo) =
            let info = info.Value
            let ctm = info.GetCtm()
            info.GetPath().GetSubpaths()
            |> Seq.collect (Subpath.toActualPoints ctm)

        let hasStroke (info: IPathRenderInfo) =             
            let info = info.Value
            info.GetPath().GetSubpaths().Count > 0
            && 
                match info.GetOperation() with 
                | PathRenderInfo.STROKE
                | FILLANDSTROKE -> true
                | _ -> false

        let hasFill (info: IPathRenderInfo) =
            let info = info.Value
            info.GetPath().GetSubpaths().Count > 0
            &&
                match info.GetOperation() with 
                | PathRenderInfo.FILL 
                | FILLANDSTROKE -> true
                | _ -> false

        let hasFillOrStroke (info: IPathRenderInfo) =
            hasFill info || hasStroke info

        let getColors (info: IPathRenderInfo) =
            if hasFillOrStroke info 
            then
                let info = info.Value
                let fillColor = info.GetFillColor()
                let strokeColor = info.GetStrokeColor()
                match info.GetOperation() with 
                | PathRenderInfo.STROKE -> [strokeColor]
                | FILLANDSTROKE -> [fillColor;strokeColor]
                | PathRenderInfo.FILL -> [fillColor]
                | others -> 
                    failwithf "unSupported path render operation %d" others
            else []

        let getBound (boundGettingOptions: BoundGettingOptions) (info: IPathRenderInfo) = 
            let boundWithoutWidth = info |> toActualPoints |> Rectangle.ofPoints
            match boundGettingOptions with 
            | BoundGettingOptions.WithoutStrokeWidth -> boundWithoutWidth
            | BoundGettingOptions.WithStrokeWidth ->
                if hasStroke info then
                    let grahpicsState = info.Value.GetGraphicsState()
                    let widthMargin = Margin.Create(float (grahpicsState.GetLineWidth()) / 2.)
                    Rectangle.applyMargin widthMargin boundWithoutWidth
                else boundWithoutWidth

            | _ -> failwith "Invalid token"

    [<RequireQualifiedAccess>]
    module ITextRenderInfo =

        /// GetFontSize() * ctm.m00
        let getActualFontSize (info: ITextRenderInfo) =
            let info = info.Value
            let matrix = info.GetTextMatrix()
            info.GetFontSize() * matrix.Get(0) |> float

        let getHeight (info: ITextRenderInfo) =
            let info = info.Value
            let ascent = info.GetAscentLine()
            let descent = info.GetDescentLine()
            ascent.GetStartPoint().Get(1) - descent.GetStartPoint().Get(1)
            |> float

        let getWidth (info: ITextRenderInfo) =
            let info = info.Value
            let ascent = info.GetAscentLine()
            ascent.GetEndPoint().Get(0) - ascent.GetStartPoint().Get(0)
            |> float


        let getY (info: ITextRenderInfo) =
            let info = info.Value
            let descent = info.GetDescentLine()
            descent.GetStartPoint().Get(1)
            |> float

        let getX (info: ITextRenderInfo) =
            let info = info.Value
            let descent = info.GetDescentLine()
            descent.GetStartPoint().Get(0)

        let getText (info: ITextRenderInfo) =
            info.Value.GetText()

        let hasFill (info: ITextRenderInfo) =             
            let md = info.Value.GetTextRenderMode()
            match md with 
            | PdfCanvasConstants.TextRenderingMode.FILL 
            | PdfCanvasConstants.TextRenderingMode.FILL_STROKE -> true
            | _ -> false

        let hasStroke (info: ITextRenderInfo) =             
            let md = info.Value.GetTextRenderMode()
            match md with 
            | PdfCanvasConstants.TextRenderingMode.STROKE
            | PdfCanvasConstants.TextRenderingMode.FILL_STROKE  
            | PdfCanvasConstants.TextRenderingMode.STROKE_CLIP -> true
            |  _ -> false

        let getBound (boundGettingOptions: BoundGettingOptions) (info: ITextRenderInfo) =
            let width = getWidth info
            let height = getHeight info
            let x = getX info
            let y = getY info
            let boundWithoutWidth = Rectangle.create x y width height

            match boundGettingOptions with 
            | BoundGettingOptions.WithoutStrokeWidth -> boundWithoutWidth
            | BoundGettingOptions.WithStrokeWidth ->
                if hasStroke info then 
                    let grahpicsState = info.Value.GetGraphicsState()
                    let widthMargin = Margin.Create(float (grahpicsState.GetLineWidth()) / 2.)
                    Rectangle.applyMargin widthMargin boundWithoutWidth
                else boundWithoutWidth
            | _ -> failwith "Invalid token"
        

        let getColors (info: ITextRenderInfo) =
            let fillColor = info.Value.GetFillColor()
            let strokeColor = info.Value.GetStrokeColor()
            match info.Value.GetTextRenderMode() with
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
    module IAbstractRenderInfo = 
        let cata (fTextRenderInfo) (fPathRenderInfo) (info: IAbstractRenderInfo) = 
            match info with 
            | :? ITextRenderInfo as trInfo -> fTextRenderInfo trInfo
            | :? IPathRenderInfo as prInfo -> fPathRenderInfo prInfo 
            | _ -> failwith "Invaid token"

        let getBound boundGettingOptions info = cata (ITextRenderInfo.getBound boundGettingOptions) (IPathRenderInfo.getBound boundGettingOptions) info
        
        let getColors (info: IAbstractRenderInfo) = cata ITextRenderInfo.getColors IPathRenderInfo.getColors info

        let isTextRenderInfo (info: IAbstractRenderInfo) = cata (fun _ -> true) (fun _ -> false) info

        let isPathRenderInfo (info: IAbstractRenderInfo) = cata (fun _ -> false) (fun _ -> true) info

        let asTextRenderInfo (info: IAbstractRenderInfo) = cata (fun trInfo -> Some trInfo) (fun _ -> None) info

        let asPathRenderInfo (info: IAbstractRenderInfo) = cata (fun _ -> None) (fun prInfo -> Some prInfo) info

        let hasFill (info: IAbstractRenderInfo) = cata ITextRenderInfo.hasFill IPathRenderInfo.hasFill info

        let hasStroke (info: IAbstractRenderInfo) = cata ITextRenderInfo.hasStroke IPathRenderInfo.hasStroke info

        let hasStrokeOrFill (info: IAbstractRenderInfo) = hasFill info || hasStroke info

        let boundIsInsideOf  boundGettingOptions rect (info: IAbstractRenderInfo) =
            (getBound boundGettingOptions info).IsInsideOf(rect)


    [<RequireQualifiedAccess>]
    module CanvasGraphicsState =
        let getDashPattern (gs: CanvasGraphicsState) =
            let dashPattern = gs.GetDashPattern()
            let values = dashPattern |> Array.ofSeq
            let l = values.Length
            let dashArray = 
                values.[0..l-2] 
                |> Array.collect (fun dashArray -> 
                    dashArray :?> PdfArray 
                    |> Array.ofSeq 
                    |> Array.map (fun dashValue -> 
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

        member x.AddXObject(xObject: PdfXObject, affineTransform: AffineTransform) =
            x.AddXObject
                ( xObject,
                  AffineTransformRecord.ofAffineTransform affineTransform )



    [<RequireQualifiedAccess>]
    module PdfCanvas = 

        /// saveState -> action -> restoreState
        let useCanvas (canvas: #PdfCanvas) action =
            canvas.SaveState() |> ignore
            
            let canvas: #PdfCanvas = action canvas

            canvas.RestoreState() |> ignore

        let setDashpattern (dashPattern: DashPattern) (canvas:PdfCanvas) =
            canvas.SetLineDash(dashPattern.DashArrayF32, dashPattern.PhaseF32)

        let setStrokeColor (color: Color) (canvas:PdfCanvas) =
            canvas.SetStrokeColor(color)

        let setLineWidth (width: float) (canvas: PdfCanvas) =
            canvas.SetLineWidth(float32 width)

        let setFillColor (color: Color) (canvas:PdfCanvas) =
            canvas.SetFillColor(color)

        let setTextRenderingMode (textRenderingMode: int) (canvas:PdfCanvas) =
            canvas.SetTextRenderingMode(textRenderingMode)

        let fill (canvas:PdfCanvas) =
            canvas.Fill()

        let fillStroke (canvas:PdfCanvas) =
            canvas.FillStroke()

        let stroke (canvas:PdfCanvas) =
            canvas.Stroke()

        let endPath (canvas: PdfCanvas) =
            canvas.EndPath()

        let moveTo (point: Point) (canvas: PdfCanvas) =
            canvas.MoveTo(point.GetX(), point.GetY())

        let lineTo(point: Point) (canvas: PdfCanvas) =
            canvas.LineTo(point.GetX(), point.GetY())

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




            
    type PdfPage with
        member this.GetActualBox() = 
            let crop = this.GetCropBox()
            let media = this.GetMediaBox()
            match Rectangle.tryGetIntersection crop media with 
            | Some rect -> rect
            | None -> failwithf "crop box %O doesn't has Intersection to media box %O" crop media

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

        member page.SetActualBox(rect: Rectangle) =
            page
                .SetCropBox(rect)
                .SetMediaBox(rect)

        member page.SetAllBox(rect: Rectangle) =
            page
                .SetTrimBox(rect)
                .SetBleedBox(rect)
                .SetCropBox(rect)
                .SetArtBox(rect)
                .SetMediaBox(rect)

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

        let getPageEdge (innerBox: Rectangle) pageBoxKind (page: PdfPage) =
            let pageBox = page.GetPageBox(pageBoxKind)

            if innerBox.IsInsideOf(pageBox) 
            then 
                let leftBottom =
                    Rectangle.create 
                    <| pageBox.GetXF()
                    <| pageBox.GetYF()
                    <| innerBox.GetXF() - pageBox.GetXF() 
                    <| innerBox.GetYF() - pageBox.GetYF()   

                let leftMiddle =
                    Rectangle.create 
                    <| pageBox.GetXF()
                    <| leftBottom.GetTopF()
                    <| leftBottom.GetWidthF()
                    <| innerBox.GetHeightF()

                let leftTop =
                    Rectangle.create 
                    <| pageBox.GetXF()
                    <| leftMiddle.GetTopF()
                    <| leftMiddle.GetWidthF()
                    <| pageBox.GetTopF() - innerBox.GetTopF()

                let topMiddle =
                    Rectangle.create 
                    <| pageBox.GetXF() + leftTop.GetWidthF()
                    <| leftTop.GetYF()
                    <| innerBox.GetWidthF()
                    <| leftTop.GetHeightF()

                let topRight =
                    Rectangle.create 
                    <| topMiddle.GetRightF()
                    <| topMiddle.GetYF()
                    <| pageBox.GetRightF() - innerBox.GetRightF()
                    <| topMiddle.GetHeightF()

                let rightMiddle =
                    Rectangle.create 
                    <| topRight.GetXF()
                    <| leftMiddle.GetYF()
                    <| topRight.GetWidthF()
                    <| leftMiddle.GetHeightF()

                let rightBottom =
                    Rectangle.create 
                    <| rightMiddle.GetXF()
                    <| leftBottom.GetYF()
                    <| rightMiddle.GetWidthF()
                    <| leftBottom.GetHeightF()

                let bottomMiddle =
                    Rectangle.create 
                    <| topMiddle.GetXF()
                    <| leftBottom.GetYF()
                    <| topMiddle.GetWidthF()
                    <| leftBottom.GetHeightF()

                { LeftBottom = leftBottom
                  LeftMiddle = leftMiddle
                  LeftTop = leftTop
                  TopMiddle = topMiddle
                  TopRight = topRight
                  RightMiddle = rightMiddle
                  RightBottom = rightBottom
                  BottomMiddle = bottomMiddle }

            else failwithf "innerBox %O is not inside pageBox %O" innerBox pageBox

    [<RequireQualifiedAccess>]
    module PdfDocument =
        let getPages (doc: PdfDocument) =
            [
                for i = 1 to doc.GetNumberOfPages() do
                    yield doc.GetPage(i)
            ]





