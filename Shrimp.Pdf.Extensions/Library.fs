namespace Shrimp.Pdf.Extensions

open Shrimp.FSharp.Plus

#nowarn "0104"
open iText.Kernel.Geom
open iText.IO.Font
open iText.IO.Font.Otf
open iText.Kernel.Font
open System
open iText.Kernel.Colors
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


    type Rectangle with 
        member this.GetWidthF() = this.GetWidth() |> float
        member this.GetHeightF() = this.GetHeight() |> float
        member this.GetXF() = this.GetX() |> float
        member this.GetYF() = this.GetY() |> float
        member this.GetLeftF() = this.GetLeft() |> float
        member this.GetTopF() = this.GetTop() |> float
        member this.GetRightF() = this.GetRight() |> float
        member this.GetBottomF() = this.GetBottom() |> float
        member this.GetXCenter() = (this.GetLeft() + this.GetRight()) / 2.f
        member this.GetXCenterF() = this.GetXCenter() |> float
        member this.GetYCenter() = (this.GetTop() + this.GetBottom()) / 2.f
        member this.GetYCenterF() = this.GetYCenter() |> float

        member rect.applyMargin(margin :Margin) =   
            let left = margin.Left
            let top = margin.Top
            let right = margin.Right
            let bottom = margin.Bottom
            let x = rect.GetXF() - left
            let y = rect.GetYF() - bottom
            let width = rect.GetWidthF() + left + right 
            let height = rect.GetHeightF() + top + bottom
            Rectangle(float32 x, float32 y, float32 width, float32 height)

        member this.setHeight(effect: YEffect, fHeight: float -> float) =
            let height = this.GetHeightF()
            let newHeight = fHeight height

            let y =
                match effect with 
                | YEffect.Top -> this.GetTopF() - newHeight
                | YEffect.Bottom -> this.GetYF()


            Rectangle(this.GetX(), float32 y, this.GetWidth(), float32 newHeight)
                    
        member this.setWidth(effect: XEffect, fWidth: float -> float) =
            let width = this.GetWidthF()
            let newWidth = fWidth width

            let x =
                match effect with 
                | XEffect.Left -> this.GetXF()
                | XEffect.Right -> this.GetRightF() - newWidth

            Rectangle(float32 x, this.GetY(), float32 newWidth, this.GetHeight())
                    

        member this.RotateByCenter(rotation: Rotation) =
            match rotation with 
            | Rotation.R180
            | Rotation.None -> this
            | Rotation.Counterclockwise
            | Rotation.Clockwise -> 
                let xCenter = this.GetXCenter()
                let yCenter = this.GetYCenter()
                let x = xCenter - this.GetHeight() / 2.f
                let y = yCenter - this.GetWidth() / 2.f
                Rectangle(x, y, this.GetHeight(), this.GetWidth())

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
            let rect = rect.applyMargin(Margin.Create -tolerance.Value)
            this.GetBottom() > rect.GetTop() 
            || this.GetTop() < rect.GetBottom()
            || this.GetLeft() > rect.GetRight()
            || this.GetRight() < rect.GetLeft()



        member this.IsInsideOf(rect: Rectangle) =
            let rect = rect.applyMargin(Margin.Create tolerance.Value)
            this.GetBottom() > rect.GetBottom()
            && this.GetTop() < rect.GetTop()
            && this.GetLeft() > rect.GetLeft()
            && this.GetRight() < rect.GetRight()

        member this.IsCrossOf(rect: Rectangle) =
            (not <| this.IsOutsideOf(rect)) 
            &&  (not <| this.IsInsideOf(rect)) 


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


            let y = 
                match position with 
                | Position.YCenter (x, y) -> (rect.GetBottomF() + rect.GetTopF()) / 2. + y
                | Position.Top (x, y) -> rect.GetTopF() + y
                | Position.Bottom (x, y) -> rect.GetBottomF() + y

            new Point (x, y)


    and Point with
        member point.IsInsideOf (rect: Rectangle) =
            let x,y = point.x, point.y
            x > rect.GetXF() && x < rect.GetRightF() && y > rect.GetYF() && y < rect.GetTopF()

        member point.IsOutsideOf (rect: Rectangle) =
            let x,y = point.x, point.y
            x < rect.GetXF() || x > rect.GetRightF() || y < rect.GetYF() || y > rect.GetTopF()

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

        let (|Portrait|Landscape|Uniform|) (rect: Rectangle) =
            let width = rect.GetWidth()
            let height = rect.GetHeight()
            if width > height 
            then Landscape
            elif width = height then Uniform
            else Portrait

        let isInsideOf (paramRect) (rect: Rectangle) =
            rect.IsInsideOf(paramRect)

        let isOutsideOf (paramRect) (rect: Rectangle) =
            rect.IsOutsideOf(paramRect)

        let isCrossOf (paramRect) (rect: Rectangle) =
            rect.IsCrossOf(paramRect)

        let create x y width height =
            let x = float32 x
            let y = float32 y 
            let width = float32 width
            let height = float32 height
            new Rectangle(x,y,width,height)
            
        // <param name="points" at least two unique points></param>
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


        // <param name="rects" at least one rectange></param>
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
            let index = tileIndexer.Index % (colNum * rowNum)
            let direction = tileIndexer.Direction

            
            let getHSpacingAccum colIndex = 
                let baseHSpacing = tileIndexer.TileTable.HSpacing
                baseHSpacing
                |> List.replicate ((colIndex / baseHSpacing.Length) + 2)
                |> List.concat
                |> List.take (colIndex)
                |> List.sum

            let getVSpacingAccum rowIndex = 
                let baseHSpacing = tileIndexer.TileTable.VSpacing
                baseHSpacing
                |> List.replicate ((rowIndex / baseHSpacing.Length) + 2)
                |> List.concat
                |> List.take (rowIndex)
                |> List.sum

            let width = (rect.GetWidthF() - getHSpacingAccum (colNum - 1)) / float colNum

            let height = (rect.GetHeightF() - getVSpacingAccum (rowNum - 1)) / float rowNum

            let (x, y) = 


                match direction with 
                | Direction.Horizontal ->
                    let x = 
                        let colIndex = index % colNum

                        let hSpacingAccum = getHSpacingAccum colIndex

                        rect.GetXF() + float colIndex * width + hSpacingAccum
            
                    let y =
                        let rowIndex = (index / colNum) 

                        let vSpacingAccum = getVSpacingAccum rowIndex

                        rect.GetTopF() - height - float rowIndex * height - vSpacingAccum

                    (x, y)

                | Direction.Vertical ->
                    let y = 
                        let rowIndex = index % rowNum

                        let vSpacingAccum = getVSpacingAccum rowIndex

                        rect.GetTopF() - height - float rowIndex * height - vSpacingAccum
            
                    let x =
                        let colIndex = (index / rowNum) 

                        let hSpacingAccum = getHSpacingAccum colIndex

                        rect.GetXF() + float colIndex * width + hSpacingAccum

                    (x, y)


            create x y width height

        let applyMargin (margin:Margin) (rect: Rectangle) =
            rect.applyMargin(margin)

    [<RequireQualifiedAccess>]
    module AffineTransform = 

        let create m00 m10 m01 m11 m02 m12 = 
            new AffineTransform(m00, m10, m01, m11, m02, m12)

        let toRecord (affineTransform: AffineTransform) =
            AffineTransformRecord.ofAffineTransform affineTransform

        let ofRecord (record: AffineTransformRecord) =
            AffineTransformRecord.toAffineTransform record


        let ofMatrix (matrix: Matrix) =
            AffineTransformRecord.ofMatrix matrix
            |> ofRecord

        let toMatrix (affineTransform: AffineTransform) =
            toRecord affineTransform
            |> AffineTransformRecord.toMatrix

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

        let getBound (boundGettingOptions: BoundGettingStrokeOptions) (info: IPathRenderInfo) = 
            let boundWithoutWidth = info |> toActualPoints |> Rectangle.ofPoints
            match boundGettingOptions with 
            | BoundGettingStrokeOptions.WithoutStrokeWidth -> boundWithoutWidth
            | BoundGettingStrokeOptions.WithStrokeWidth ->
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
            info.Value.GetActualFontSize()

        let getHeight (info: ITextRenderInfo) =
            let info = info.Value
            let ascent = info.GetAscentLine()
            let descent = info.GetDescentLine()
            let baseHeight = 
                ascent.GetStartPoint().Get(1) - descent.GetStartPoint().Get(1)
                |> float

            let redirectedHeight = textInfoHeightRedirectPercentage.Value * baseHeight
            redirectedHeight

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
            |> float

        let getText (info: ITextRenderInfo) =
            info.Value.GetText()

        let hasFill (info: ITextRenderInfo) =             
            let textRenderMode = info.Value.GetTextRenderMode()
            match textRenderMode with 
            | TextRenderingMode.FILL 
            | TextRenderingMode.FILL_STROKE -> true
            | TextRenderingMode.STROKE -> false
            | _ -> 
                Logger.unSupportedTextRenderMode textRenderMode
                false

        let hasStroke (info: ITextRenderInfo) =             
            let textRenderMode = info.Value.GetTextRenderMode()
            match textRenderMode with 
            | TextRenderingMode.STROKE
            | TextRenderingMode.FILL_STROKE -> true  
            | TextRenderingMode.FILL -> false
            | _ -> 
                Logger.unSupportedTextRenderMode textRenderMode
                false

        let getBound (boundGettingOptions: BoundGettingStrokeOptions) (info: ITextRenderInfo) =
            let width = getWidth info
            let height = getHeight info
            let x = getX info
            let y = getY info
            let boundWithoutWidth = Rectangle.create x y width height

            match boundGettingOptions with 
            | BoundGettingStrokeOptions.WithoutStrokeWidth -> boundWithoutWidth
            | BoundGettingStrokeOptions.WithStrokeWidth ->
                if hasStroke info then 
                    let grahpicsState = info.Value.GetGraphicsState()
                    let widthMargin = Margin.Create(float (grahpicsState.GetLineWidth()) / 2.)
                    Rectangle.applyMargin widthMargin boundWithoutWidth
                else boundWithoutWidth
            | _ -> failwith "Invalid token"
        
        let getFontName (info: ITextRenderInfo) = info.Value.GetFontName()

        let fontNameIs fontName (info: ITextRenderInfo) =
            let fontName' = getFontName info

            StringIC fontName = StringIC fontName'
            || (
                    if fontName'.Contains "+"
                    then StringIC(fontName'.RightOf("+").Value) = StringIC fontName
                    else false
                )

            


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

    type IAbstractRenderInfo with 
        static member ColorIs(fillOrStrokeOptions: FillOrStrokeOptions, predicate: Color -> bool) =

            fun (info: #IAbstractRenderInfo) ->
                match fillOrStrokeOptions with 
                | FillOrStrokeOptions.FillOrStroke ->
                    IAbstractRenderInfo.hasStroke info
                    && predicate(info.Value.GetStrokeColor()) 
                    || 
                        IAbstractRenderInfo.hasFill info
                        && predicate(info.Value.GetFillColor())

                | FillOrStrokeOptions.Fill ->
                    IAbstractRenderInfo.hasFill info
                    && predicate(info.Value.GetFillColor())

                | FillOrStrokeOptions.FillAndStroke ->
                    IAbstractRenderInfo.hasStroke info
                    && predicate(info.Value.GetStrokeColor()) 
                    && 
                        IAbstractRenderInfo.hasFill info
                        && predicate (info.Value.GetFillColor())

                | FillOrStrokeOptions.Stroke ->
                    IAbstractRenderInfo.hasStroke info
                    && predicate(info.Value.GetStrokeColor())


    [<RequireQualifiedAccess>]
    module IIntegratedRenderInfo =
        [<RequireQualifiedAccess>]
        module private ClippingPathInfo =
            let tryGetActualClippingPath (info: ClippingPathInfo) = 
                match info.GetClippingPath() with 
                | null -> None
                | path -> 
                    if path.GetSubpaths() |> Seq.forall(fun subPath -> subPath.GetPiecewiseLinearApproximation().Count = 0) then None
                    else Some path
        
            let tryGetActualClippingArea (info) =
                tryGetActualClippingPath info
                |> Option.map (fun clippingPath ->
                    clippingPath.GetSubpaths()
                    |> Seq.collect(Subpath.toActualPoints (info.GetGraphicsState().GetCtm()))
                    |> Rectangle.ofPoints
                )
        
        
        [<RequireQualifiedAccess>]
        module private IAbstractRenderInfo =
            let isVisible (fillOrStrokeOptions: FillOrStrokeOptions) (clippingPathInfo: ClippingPathInfo option) (info: IAbstractRenderInfo) =
        
                match fillOrStrokeOptions with 
                | FillOrStrokeOptions.Fill -> IAbstractRenderInfo.hasFill info
                | FillOrStrokeOptions.Stroke -> IAbstractRenderInfo.hasStroke info
                | FillOrStrokeOptions.FillAndStroke -> IAbstractRenderInfo.hasFill info && IAbstractRenderInfo.hasStroke info
                | FillOrStrokeOptions.FillOrStroke -> IAbstractRenderInfo.hasFill info || IAbstractRenderInfo.hasStroke info
        
                &&
                    match info with 
                    | :? PathRenderInfo as info -> not (info.IsPathModifiesClippingPath())
                    | _ -> true
                && 
                    match clippingPathInfo with 
                    | Some clippingPathInfo ->
                        match ClippingPathInfo.tryGetActualClippingArea clippingPathInfo with 
                            | Some clippingBound ->
                                let bound = IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithStrokeWidth info
                                match Rectangle.tryGetIntersection bound clippingBound with 
                                | Some _ -> true
                                | None -> false
                            | None -> true
                    | None -> true
        
            let tryGetVisibleBound boundGettingOptions (clippingPathInfo: ClippingPathInfo option) (info: IAbstractRenderInfo) =
                if isVisible (FillOrStrokeOptions.FillOrStroke) clippingPathInfo info 
                then
                    let bound = IAbstractRenderInfo.getBound boundGettingOptions info
                    match clippingPathInfo with 
                    | None -> Some bound 
                    | Some clippingPathInfo ->
                        match ClippingPathInfo.tryGetActualClippingArea clippingPathInfo with 
                        | Some clippingBound -> Rectangle.tryGetIntersection bound clippingBound 
                        | None -> 
                            if bound.GetWidthF() @= 0. || bound.GetHeightF() @= 0. then None
                            else
                                Some bound
                else None

        let isStrokeVisible (info: IIntegratedRenderInfo) = 
            IAbstractRenderInfo.isVisible FillOrStrokeOptions.Stroke info.ClippingPathInfo info
    
        let isFillVisible (info: IIntegratedRenderInfo) = 
            IAbstractRenderInfo.isVisible FillOrStrokeOptions.Fill info.ClippingPathInfo info
    
        let tryGetVisibleBound boundGettingOptions (info: IIntegratedRenderInfo) =
            IAbstractRenderInfo.tryGetVisibleBound boundGettingOptions info.ClippingPathInfo info
    
        let isVisible (info: IIntegratedRenderInfo) =
            isFillVisible info || isStrokeVisible info

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
        member x.SetPageBoxToPage(targetPdfPage: PdfPage) =
            x
                .SetCropBox(targetPdfPage.GetCropBox())
                .SetMediaBox(targetPdfPage.GetMediaBox())
                .SetArtBox(targetPdfPage.GetArtBox())
                .SetBleedBox(targetPdfPage.GetBleedBox())
                .SetTrimBox(targetPdfPage.GetTrimBox())
            


        member x.GetPageEdge (innerBox: Rectangle, pageBoxKind) =
            let pageBox = x.GetPageBox(pageBoxKind)

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
            | PageBoxKind.MediaBox -> page.GetMediaBox()

        member page.GetFsRotation() =
            match page.GetRotation() with 
            | 0 -> Rotation.None
            | 90 -> Rotation.Clockwise
            | 180 -> Rotation.R180
            | 270 -> Rotation.Counterclockwise
            | angle -> failwithf "Cannot create rotation from angle %A" angle
            

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

        member x.GetArea(areaGettingOptions: AreaGettingOptions) =
            match areaGettingOptions with 
            | AreaGettingOptions.PageBox pageBoxKind -> x.GetPageBox(pageBoxKind)
            | AreaGettingOptions.Specfic rect -> rect
            | AreaGettingOptions.PageBoxWithOffset (pageBoxKind, margin) ->
                x.GetPageBox(pageBoxKind)
                |> Rectangle.applyMargin margin



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
            
            | PageBoxKind.ArtBox -> setArtBox rect page
            | PageBoxKind.BleedBox -> setBleedBox rect page
            | PageBoxKind.MediaBox -> setMediaBox rect page


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
        let getPages (doc: PdfDocument) = doc.GetPages()





