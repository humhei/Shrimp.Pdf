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
open Shrimp.Pdf.Constants
open iText.IO.Image



[<AutoOpen>]
module iText = 

    [<RequireQualifiedAccess>]
    type ActualLineWidth =
        | Exactly of  rawLineWidth: float * scale: float
        | Unbalance of rawLineWidth: float * scaleX: float * scaleY: float
    with 
        member x.CalculatedWidth = 
            match x with 
            | Exactly (w, s) -> w * s
            | Unbalance (w, sx, sy) -> w * (max sx sy)

            

        member x.CalculatedWidth_Horizontal = 
            match x with 
            | Exactly (w, s) -> w * s
            | Unbalance (w, sx, sy) -> w * sx


        member x.CalculatedWidth_Vertical = 
            match x with 
            | Exactly (w, s) -> w * s
            | Unbalance (w, sx, sy) -> w * sy

        member x.RawLineWidth =
            match x with 
            | Exactly (w, _) 
            | Unbalance (w, _, _) -> w

        member x.AsWidthMargin() =
            let horizontal = x.CalculatedWidth_Horizontal / 2.
            let vertical = x.CalculatedWidth_Vertical / 2.
            Margin.Create(left = horizontal, right = horizontal, top = vertical, bottom = vertical)

    type LineShapingStyle =
        { JoinStyle: int 
          CapStyle: int
          ActualLineWidth: ActualLineWidth
          DashPattern: DashPattern }
    with 
        member x.CalculatedWidth = x.ActualLineWidth.CalculatedWidth

        member x.RawLineWidth = x.ActualLineWidth.RawLineWidth


    [<RequireQualifiedAccess>]
    module PdfDictionary =
        let (|TryGet|_|) (pdfName: PdfName) (dict: PdfDictionary) =
            match dict.ContainsKey pdfName with 
            | true -> Some (dict.Get(pdfName))
            | false -> None

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
            let linesOfText = text.SplitToLines()

            linesOfText 
            |> List.map (fun line ->
                let line = font.CreateGlyphLine(line)
                let unit =
                    GlyphLine.getAllGlyphs line
                    |> List.map (fun gl -> 
                        gl.GetWidth()
                    )
                    |> List.sum
                    |> float
                unit / TEXT_SPACE_COEFF
            )


        let private verticalMaxSizeWhenParagraphedHeightIs height (text: string) font =
            let heightUnit = calcLineHeightUnit font
            let baseFontSize = height / heightUnit
            let linesOfText = text.SplitToLines()

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

        member this.MapCoordinate(f) =
            let width = this.GetWidth()
            let height = this.GetHeight()
            let newPoint: FsPoint =
                {
                    X = this.GetXF()
                    Y = this.GetYF()
                }
                |> f

            Rectangle(float32 newPoint.X, float32 newPoint.Y, width, height)

        member this.moveRightUp(right, up) =
            let width = this.GetWidth()
            let height = this.GetHeight()
            let newX = this.GetX() + right
            let newY = this.GetY() + up
            Rectangle(newX, newY, width, height)


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
        member this.GetFsCenter() = 
            { X = this.GetXCenterF() 
              Y = this.GetYCenterF() }

        member this.GetCenter() = 
            Point(this.GetXCenterF(), this.GetYCenterF())

        member x.FsRectangle() = FsRectangle.OfRectangle x
            
        member x.HorizontalAlimentOf(y: Rectangle) =
            let index = 
                [
                    abs (x.GetXCenter() - y.GetXCenter())
                    abs (x.GetLeft() - y.GetLeft())
                    abs (x.GetRight() - y.GetRight())
                ]
                |> List.indexed
                |> List.minBy snd
                |> fst

            match index with 
            | 0 -> XEffort.Middle
            | 1 -> XEffort.Left
            | 2 -> XEffort.Right
            | _ -> failwithf "Invalid token"

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

        member this.setHeight(effect: YEffort, fHeight: float -> float) =
            let height = this.GetHeightF()
            let newHeight = fHeight height

            let y =
                match effect with 
                | YEffort.Top -> this.GetTopF() - newHeight
                | YEffort.Bottom -> this.GetYF()
                | YEffort.Middle -> this.GetYCenterF() - newHeight / 2.

            Rectangle(this.GetX(), float32 y, this.GetWidth(), float32 newHeight)
                    


        member this.setWidth(effect: XEffort, fWidth: float -> float) =
            let width = this.GetWidthF()
            let newWidth = fWidth width

            let x =
                match effect with 
                | XEffort.Left -> this.GetXF()
                | XEffort.Right -> this.GetRightF() - newWidth
                | XEffort.Middle -> this.GetXCenterF() - newWidth / 2.

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

        member this.Is_InsideOrCross_Of(rect: Rectangle) =
            this.IsOutsideOf(rect)
            |> not

        member this.IsCenterPointInsideOf(paramRect: Rectangle) =
            this.GetCenter().IsInsideOf(paramRect)

        member this.IsCrossOf(rect: Rectangle) =
            (not <| this.IsOutsideOf(rect)) 
            &&  (not <| this.IsInsideOf(rect)) 


        member this.RelativePositionOf(rect: Rectangle) =
            if this.IsInsideOf(rect) then RelativePosition.Inbox
            elif this.IsOutsideOf(rect) then RelativePosition.OutBox
            elif this.IsCrossOf(rect) then RelativePosition.CrossBox
            else failwith "invalid token"

        member this.IsRelativeTo(relativePosition, rect: Rectangle) =
            match relativePosition with 
            | RelativePosition.Inbox -> this.IsInsideOf(rect)
            | RelativePosition.CrossBox -> this.IsCrossOf(rect)
            | RelativePosition.OutBox -> this.IsOutsideOf(rect)

            

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


        member rect.LeftBottom = rect.GetPoint(Position.LeftBottom(0., 0.))

        member rect.GetArea(position: Position, width, height) =
            let startPoint = rect.GetPoint(position)
            let x = 
                match position with 
                | Position.Left _ -> startPoint.GetX()
                | Position.XCenter _ -> startPoint.GetX() - width / 2.
                | Position.Right _ -> startPoint.GetX() - width

            let y = 
                match position with 
                | Position.Bottom _ -> startPoint.GetY()
                | Position.YCenter _ -> startPoint.GetY() - height / 2.
                | Position.Top _ -> startPoint.GetY() - height
            
            new Rectangle(float32 x, float32 y, float32 width, float32 height)

    and Point with
        member point.IsInsideOf (rect: Rectangle) =
            let x,y = point.x, point.y
            x > rect.GetXF() && x < rect.GetRightF() && y > rect.GetYF() && y < rect.GetTopF()

        member point.IsOutsideOf (rect: Rectangle) =
            let x,y = point.x, point.y
            x < rect.GetXF() || x > rect.GetRightF() || y < rect.GetYF() || y > rect.GetTopF()

    type PageEdge with 
        member x.Left = 
            x.LeftBottom.setHeight(YEffort.Bottom, fun m -> m + x.LeftMiddle.GetHeightF() + x.LeftTop.GetHeightF())

        member x.Right = 
            x.RightBottom.setHeight(YEffort.Bottom, fun m -> m + x.LeftMiddle.GetHeightF() + x.LeftTop.GetHeightF())

        member x.Bottom = 
            x.LeftBottom.setWidth(XEffort.Left, fun m -> m + x.BottomMiddle.GetWidthF() + x.RightBottom.GetWidthF())

        member x.Top = 
            x.LeftTop.setWidth(XEffort.Left, fun m -> m + x.BottomMiddle.GetWidthF() + x.RightBottom.GetWidthF())


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
        let toPageSize (rect: Rectangle) = PageSize(rect)

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


        // <param name="rects" at least one rectange></param>
        let ofRectangles (rects: Rectangle al1List) =
            match rects.AsList with 
            | [rect] -> rect
            | rects ->
                rects |> Seq.collect (fun rect ->
                    let x = rect.GetXF()
                    let y = rect.GetYF()
                    let right = rect.GetRightF()
                    let top = rect.GetTopF()
                    [Point(x,y);Point(right,top)]
                ) 
                |> AtLeastTwoList.Create
                |> Rectangle.ofPoints

        let equalableValue (rect: Rectangle) =
            let customEquality (x: float) =
                NearbyPX x
            (
                customEquality(rect.GetXF()),
                customEquality(rect.GetYF()),
                customEquality(rect.GetWidthF()),
                customEquality(rect.GetHeightF())
            )

        let equalTo (rect1: Rectangle) (rect2: Rectangle) =
            equalableValue rect1 = equalableValue rect2


        let distinct (rects: Rectangle al1List) =
            rects
            |> AtLeastOneList.distinctBy_explictly<_, _>(fun bound ->
                equalableValue bound
            )

        let distinctByCenter (rects: Rectangle al1List) =
            rects
            |> AtLeastOneList.distinctBy_explictly<_, _>(fun bound ->
                let customEquality (x: float) =
                    NearbyPX x

                customEquality(bound.GetXCenterF()), customEquality(bound.GetYCenterF())
            )

        let removeInboxes(rects: Rectangle list) =
            let rec loop accum (rects: Rectangle list) =
                match rects with 
                | rect :: t ->
                    let searchers = accum @ t
                    searchers
                    |> List.tryFind(fun searcher ->
                        let searcher = searcher.applyMargin(Margin.Create tolerance.Value)
                        rect.IsInsideOf searcher
                    )
                    |> function 
                        | Some _ -> loop accum t
                        | None -> loop (rect :: accum) t

                | [] -> accum

            loop [] rects
                    


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

        let increaseHeight (effect) (height:float) (rect: Rectangle) = 
            rect.setHeight(effect, fun m -> height + m)

        let setHeight (effect) (height: float) (rect: Rectangle) =
            rect.setHeight(effect, fun _ -> height)

        let increaseWidth (effect) (width:float) (rect: Rectangle) = 
            rect.setWidth(effect, fun m -> width + m)

        let setWidth (effect) (width: float) (rect: Rectangle) =
            rect.setWidth(effect, fun _ -> width)

        let scale (xEffect: XEffort) yEffect scaleX scaleY (rect: Rectangle) =
            let width = rect.GetWidthF() * scaleX
            let height = rect.GetHeightF() * scaleY

            rect
                .setWidth(xEffect, fun _ -> width)
                .setHeight(yEffect, fun _ -> height)


        let scaleX (effect) scale (rect: Rectangle) =
            let width = rect.GetWidthF() * scale
            rect.setWidth(effect, fun _ -> width)

        let scaleY (effect) scale (rect: Rectangle) =
            let height = rect.GetHeightF() * scale
            rect.setHeight(effect, fun _ -> height)


        let getTile (tileCellIndexer: TileCellIndexer) (tileTableIndexer: TileTableIndexer) (rect: Rectangle) =
            let tileIndexer = tileTableIndexer
            let colNum= tileIndexer.ColNum
            let rowNum = tileIndexer.RowNum
            let index = tileCellIndexer.Index % (colNum * rowNum)
            let direction = tileCellIndexer.Direction

            
            let getHSpacingAccum colIndex = 
                let baseHSpacing = tileIndexer.HSpacing
                baseHSpacing
                |> List.replicate ((colIndex / baseHSpacing.Length) + 2)
                |> List.concat
                |> List.take (colIndex)
                |> List.sum

            let getVSpacingAccum rowIndex = 
                let baseHSpacing = tileIndexer.VSpacing
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

        let calcMargin (insideRect: Rectangle) (rect: Rectangle) =
            { Left = insideRect.GetXF() - rect.GetXF() 
              Top  = rect.GetTopF() - insideRect.GetTopF() 
              Right = rect.GetRightF() - insideRect.GetRightF()
              Bottom = insideRect.GetBottomF() - rect.GetBottomF()  }

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

        let inverse (affineTransform: AffineTransform) =
            affineTransform.CreateInverse()

    type AffineTransform with 
        member this.Transform(p: Point) =
            let p1 = new Point()
            this.Transform(p,p1)

        member this.InverseTransform(p: Point) =
            let p1 = new Point()
            this.InverseTransform(p,p1)

        member this.Transform(rect: Rectangle) = 
            let p1 = new Point (rect.GetXF(),rect.GetYF())
            let p2 = new Point (rect.GetRightF(),rect.GetTopF())
            let p3 = new Point (rect.GetXF(),rect.GetTopF())
            let p4 = new Point (rect.GetRightF(),rect.GetYF())
            [p1; p2; p3 ;p4]
            |> AtLeastTwoList.Create
            |> AtLeastTwoList.map (fun p -> AffineTransform.transform p this) 
            |> Rectangle.ofPoints

        member this.Transform(segment: IShape) =
            let points = 
                segment.GetBasePoints()
                |> Seq.map (this.Transform)
                |> ResizeArray
            match segment with 
            | :? BezierCurve -> BezierCurve(points) :> IShape
            | _ -> Line(points.[0], points.[1]) :> IShape


        member this.Transform(subPath: Subpath) =
            let newSubPath = Subpath()

            let __addSegmentsToNewSubPath = 
                subPath.GetSegments()
                |> Seq.map(this.Transform)
                |> Seq.iter(newSubPath.AddSegment)

            newSubPath.SetClosed(subPath.IsClosed())
            newSubPath

        member this.Transform(rect: Path) = 
            let newPath = new Path()

            for subPath in rect.GetSubpaths() do
                let newSubPath = this.Transform(subPath)
                newPath.AddSubpath(newSubPath)

            newPath

        member this.Transform(values: float []) = 
            match values.Length % 2 with 
            | 0 -> 
                values
                |> Array.chunkBySize 2
                |> Array.map (fun values ->
                    this.Transform(Point(values.[0], values.[1]))
                )
                |> Array.collect(fun point ->
                    let x = point.x
                        //if this.GetScaleX() < 0.
                        //then -point.x
                        //else point.x

                    let y = point.y
                        //if this.GetScaleY() < 0.
                        //then -point.y
                        //else point.y
                        
                    [|x; y|]
                )
                //let dst: float [] = Array.zeroCreate values.Length
                //this.Transform(values, 0, dst, 0, values.Length / 2)
                //dst

            | _ -> failwithf "Cannot transform odd values %d" values.Length



        member this.InverseTransform(rect: Rectangle) = 
            let p1 = new Point (rect.GetXF(),rect.GetYF())
            let p2 = new Point (rect.GetRightF(),rect.GetTopF())
            let p3 = new Point (rect.GetXF(),rect.GetTopF())
            let p4 = new Point (rect.GetRightF(),rect.GetYF())
            [p1; p2; p3 ;p4] 
            |> AtLeastTwoList.Create
            |> AtLeastTwoList.map (fun p -> AffineTransform.inverseTransform p this) 
            |> Rectangle.ofPoints

    
    [<RequireQualifiedAccess>]
    module CanvasGraphicsState =
        let getDashPattern (gs: CanvasGraphicsState) =
            let dashPattern = gs.GetDashPattern()
            DashPattern.OfPdfArray dashPattern


        let getExtGState (gs: CanvasGraphicsState): FsExtGState =
            { OPM = gs.GetOverprintMode() |> enum
              Fill = 
                { IsOverprint = gs.GetFillOverprint() 
                  Opacity = gs.GetFillOpacity() }

              AIS = gs.GetAlphaIsShape()

              Stroke = 
                { IsOverprint = gs.GetStrokeOverprint() 
                  Opacity = gs.GetStrokeOpacity() }
              SoftMask = None
              BlendModes = 
                match gs.GetBlendMode() with 
                | :? PdfName as pdfName -> [BlendMode.ofPdfName pdfName]
                | :? PdfArray as array ->
                    [ for i in array do 
                        yield (i :?> PdfName |> BlendMode.ofPdfName)
                    ]
                | _ -> failwithf "Cannot read blend mode from %A" (gs.GetBlendMode().GetType())
             }


        let getActualLineWidth (gs: CanvasGraphicsState) =
            let lineWidth = gs.GetLineWidth() |> float
            let actualLineWidth =
                let matrix = 
                    gs.GetCtm()
                    |> AffineTransform.ofMatrix

                let scx = matrix.GetScaleX()
                let scy = matrix.GetScaleY()
                match scx @= scy with 
                | true -> ActualLineWidth.Exactly (lineWidth, (min scx scy))
                | false -> ActualLineWidth.Unbalance(lineWidth, scx, scy)

            actualLineWidth


        let getLineShapingStyle (gs: CanvasGraphicsState): LineShapingStyle =
            {
                JoinStyle    = gs.GetLineJoinStyle()
                CapStyle     = gs.GetLineCapStyle()
                DashPattern  = gs |> getDashPattern
                ActualLineWidth = getActualLineWidth gs
            }


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

        let toActualPoints (ctm: Matrix) subpath =
            Subpath.toRawPoints subpath
            |> Seq.map (fun pt -> (AffineTransform.ofMatrix ctm).Transform(pt))

        let getActualBound ctm (subpath: Subpath) =
            let points = 
                toActualPoints ctm subpath
                |> AtLeastTwoList.Create

            Rectangle.ofPoints points


        let isNotEmpty (subpath: Subpath) =
            let points = Subpath.toRawPoints subpath
            points.Count > 0





    [<RequireQualifiedAccess>]
    module IPathRenderInfo =

        let [<Literal>] FILLANDSTROKE = PathRenderInfo.FILL ||| PathRenderInfo.STROKE

        [<RequireQualifiedAccess>]
        module Operation =
            
            let (|HasFill|NoFill|)(operation: int) =
                match operation with 
                | PathRenderInfo.FILL
                | FILLANDSTROKE -> HasFill()
                | _ -> NoFill()

            let (|HasStroke|NoStroke|)(operation: int) =
                match operation with 
                | PathRenderInfo.STROKE
                | FILLANDSTROKE -> HasStroke()
                | _ -> NoStroke()


        let getLineShapingStyle (info: IPathRenderInfo): LineShapingStyle =
            info.Value.GetGraphicsState()
            |> CanvasGraphicsState.getLineShapingStyle

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
                | Operation.HasStroke -> true
                | Operation.NoStroke -> false

        let hasFill (info: IPathRenderInfo) =
            let info = info.Value
            info.GetPath().GetSubpaths().Count > 0
            &&
                match info.GetOperation() with 
                | Operation.HasFill -> true
                | Operation.NoFill -> false

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



        let getLineWidth (info: IPathRenderInfo) = 
            info.Value.GetLineWidth()

        let getActualLineWidth (info: IPathRenderInfo) = 
            CanvasGraphicsState.getActualLineWidth <| info.Value.GetGraphicsState()


        let getBound (boundGettingStrokeOptions: BoundGettingStrokeOptions) (info: IPathRenderInfo) = 
            let boundWithoutWidth = info |> toActualPoints |> AtLeastTwoList.Create |> Rectangle.ofPoints
            match boundGettingStrokeOptions with 
            | BoundGettingStrokeOptions.WithoutStrokeWidth -> boundWithoutWidth
            | BoundGettingStrokeOptions.WithStrokeWidth ->
                if hasStroke info then
                    let widthMargin = 
                        let actualWidth = getActualLineWidth info
                        actualWidth.AsWidthMargin()

                    Rectangle.applyMargin widthMargin boundWithoutWidth
                else boundWithoutWidth

            | _ -> failwith "Invalid token"

        let getExtGState (info: IPathRenderInfo) = 
            info.Value.GetGraphicsState()
            |> CanvasGraphicsState.getExtGState

    [<RequireQualifiedAccess>]
    module TextRenderInfo =
        [<RequireQualifiedAccess>]
        module TextRenderingMode =
            let (|HasFill|NoFill|) (textRenderMode: int) =
                match textRenderMode with 
                | TextRenderingMode.FILL 
                | TextRenderingMode.FILL_STROKE -> HasFill()
                | TextRenderingMode.STROKE -> NoFill()
                | _ -> 
                    PdfLogger.unSupportedTextRenderMode textRenderMode
                    NoFill()

            let (|HasStroke|NoStroke|) (textRenderMode: int) =
                match textRenderMode with 
                | TextRenderingMode.STROKE
                | TextRenderingMode.FILL_STROKE -> HasStroke()
                | TextRenderingMode.FILL -> NoStroke()
                | _ -> 
                    PdfLogger.unSupportedTextRenderMode textRenderMode
                    NoStroke()

        let getY (info: TextRenderInfo) =
            let descent = info.GetDescentLine()
            descent.GetStartPoint().Get(1)
            |> float

        let getX (info: TextRenderInfo) =
            let descent = info.GetDescentLine()
            descent.GetStartPoint().Get(0)
            |> float

        let getDocumentFontName (info: TextRenderInfo) =
            let fontNames = info.GetFont().GetFontProgram().GetFontNames()
            let fontName = 
                fontNames
                |> FsFontName.TryCreate

            match fontName with 
            | None -> DocumentFontName.Invalid
            | Some fontName -> DocumentFontName.Valid fontName

        let getAffineTransfrom(info: TextRenderInfo) =
            let matrix = 
                info.GetGraphicsState().GetCtm()
                |> AffineTransform.ofMatrix

            let textMatrix = 
                info.GetTextMatrix()
                |> AffineTransform.ofMatrix

            matrix.Concatenate textMatrix

            matrix

        let getAffineTransfromRecord(info: TextRenderInfo) =
            getAffineTransfrom info
            |> AffineTransformRecord.ofAffineTransform

        let getBoundWithoutStrokeWidth (info: TextRenderInfo) =
            let ascent = info.GetAscentLine()
            let descent = info.GetDescentLine()
            let vectorToPoint (vector: Vector) =
                Point(float <| vector.Get(0), float <| vector.Get(1))

            let rect = 
                [ 
                    ascent.GetStartPoint()      |> vectorToPoint
                    ascent.GetEndPoint()        |> vectorToPoint
                    descent.GetStartPoint()     |> vectorToPoint
                    descent.GetEndPoint()       |> vectorToPoint
                ]
                |> AtLeastTwoList.Create
                |> Rectangle.ofPoints

            rect

        let getLineShapingStyle (info: TextRenderInfo): LineShapingStyle =
            let gs = info.GetGraphicsState()
            CanvasGraphicsState.getLineShapingStyle gs

        let getWidth (info: TextRenderInfo) =
            getBoundWithoutStrokeWidth(info)
            |> fun rect -> rect.GetWidthF()

        let getHeight (info: TextRenderInfo) =
            getBoundWithoutStrokeWidth(info)
            |> fun rect -> rect.GetHeightF()


        let hasStroke (info: TextRenderInfo) =             
            let textRenderMode = info.GetTextRenderMode()
            match textRenderMode with 
            | TextRenderingMode.HasStroke -> true  
            | TextRenderingMode.NoStroke -> false

        let getBound (boundGettingStrokeOptions: BoundGettingStrokeOptions) (info: TextRenderInfo) =
            
            let boundWithoutWidth = getBoundWithoutStrokeWidth info

            match boundGettingStrokeOptions with 
            | BoundGettingStrokeOptions.WithoutStrokeWidth -> boundWithoutWidth
            | BoundGettingStrokeOptions.WithStrokeWidth -> 
                boundWithoutWidth
                //if hasStroke info then 
                //    let grahpicsState = info.GetGraphicsState()
                //    let widthMargin = Margin.Create(float (grahpicsState.GetLineWidth()) / 2.)
                //    Rectangle.applyMargin widthMargin boundWithoutWidth
                //else boundWithoutWidth
            | _ -> failwith "Invalid token"



    [<RequireQualifiedAccess>]
    module ITextRenderInfo =
        type private TextRenderInfo with 
            member info.ScaleY() =      
                let matrix = 
                    info.GetGraphicsState().GetCtm()
                    |> AffineTransformRecord.ofMatrix

                let textMatrix = 
                    info.GetTextMatrix()
                    |> AffineTransformRecord.ofMatrix

                matrix.ScaleY * textMatrix.ScaleY

            member private info.GetAffineTransfrom() =
                let matrix = 
                    info.GetGraphicsState().GetCtm()
                    |> AffineTransform.ofMatrix

                let textMatrix = 
                    info.GetTextMatrix()
                    |> AffineTransform.ofMatrix

                matrix.Concatenate textMatrix

                matrix

            member private info.GetAffineTransfromRecord() =
                info.GetAffineTransfrom()
                |> AffineTransformRecord.ofAffineTransform

            member info.GetTextRotation() =
                let affimeRecord = info.GetAffineTransfromRecord() 
                let rotation = 
                    let tolerance = 0.00001
                    let ng_tolerance = -tolerance

                    let (|ApproximateZero|_|) (v: float) =
                        if v > ng_tolerance && v < tolerance
                        then Some ()
                        else None

                    match affimeRecord.ShearX, affimeRecord.ShearY with 
                    | SmallerThan ng_tolerance, BiggerThan tolerance -> Rotation.Counterclockwise
                    | BiggerThan tolerance, SmallerThan ng_tolerance -> Rotation.Clockwise
                    | ApproximateZero, ApproximateZero -> 
                        match affimeRecord.ScaleX, affimeRecord.ScaleY with 
                        | BiggerThan tolerance, BiggerThan tolerance -> Rotation.None
                        | SmallerThan ng_tolerance, SmallerThan ng_tolerance -> Rotation.R180
                        | _ -> failwithf "Cannot get text rotation from %A" affimeRecord
                    | _ -> failwithf "Cannot get text rotation from %A" affimeRecord

                rotation

            member info.GetActualFontSize() =
                let affime = info.GetAffineTransfrom() 
                //let m = AffineTransform.GetRotateInstance(90.) |> AffineTransformRecord.ofAffineTransform
                //let affimeRecord = info.GetAffineTransfromRecord()

                let baseFontSize = info.GetFontSize()
                
                let textRotation = info.GetTextRotation()
                let direction = textRotation |> Rotation.toDirection

                let rect = new Rectangle(1.f, baseFontSize)
                let newRect = affime.Transform(rect)

                match direction with 
                | Direction.Vertical -> newRect.GetWidthF()
                | Direction.Horizontal -> newRect.GetHeightF()


                //let fontSize = info.GetFontSize()
                //let scaleY = info.ScaleY()
                //float (fontSize) * scaleY

            member info.ToTransformedFontSize(actualFontSize: float) =
                actualFontSize / (info.ScaleY()) 

            member info.GetFontName() =
                TextRenderInfo.getDocumentFontName info
          

        /// GetFontSize() * ctm.m00
        let getTextRotation (info: ITextRenderInfo) =
            info.Value.GetTextRotation()

        /// GetFontSize() * ctm.m00
        let getActualFontSize (info: ITextRenderInfo) =
            info.Value.GetActualFontSize()

        /// actualFontSize / ctm.m00
        let toTransformedFontSize actualFontSize (info: ITextRenderInfo) =
            info.Value.ToTransformedFontSize(actualFontSize)

        let getHeight (info: ITextRenderInfo) =
            info.Value
            |> TextRenderInfo.getHeight

        let getLineShapingStyle (info: ITextRenderInfo): LineShapingStyle =
            TextRenderInfo.getLineShapingStyle info.Value


        let getY (info: ITextRenderInfo) =
            info.Value
            |> TextRenderInfo.getY

        let getX (info: ITextRenderInfo) =
            info.Value
            |> TextRenderInfo.getX



        let hasFill (info: ITextRenderInfo) =             
            let textRenderMode = info.Value.GetTextRenderMode()
            match textRenderMode with 
            | TextRenderInfo.TextRenderingMode.HasFill -> true
            | TextRenderInfo.TextRenderingMode.NoFill -> false

        let hasStroke (info: ITextRenderInfo) =             
            info.Value
            |> TextRenderInfo.hasStroke

        let getBounds (boundGettingStrokeOptions: BoundGettingStrokeOptions) (info: ITextRenderInfo) = 
            match info.EndTextState with 
            | EndTextState.No 
            | EndTextState.Undified -> 
                
                {| ConcatedBounds = [] 
                   Bound =
                    TextRenderInfo.getBound boundGettingStrokeOptions info.Value |}
                
            | EndTextState.Yes ->
                let bounds = 
                    info.ConcatedTextInfo.AsList
                    |> List.map(TextRenderInfo.getBound boundGettingStrokeOptions)

                {| ConcatedBounds = bounds
                   Bound =
                    bounds
                    |> AtLeastOneList.Create
                    |> Rectangle.ofRectangles |}

        
        let getBound (boundGettingStrokeOptions: BoundGettingStrokeOptions) (info: ITextRenderInfo) = 
            match info.EndTextState with 
            | EndTextState.No 
            | EndTextState.Undified -> TextRenderInfo.getBound boundGettingStrokeOptions info.Value
            | EndTextState.Yes ->
                info.ConcatedTextInfo.AsList
                |> List.map(TextRenderInfo.getBound boundGettingStrokeOptions)
                |> AtLeastOneList.Create
                |> Rectangle.ofRectangles

        

        let getWidth (info: ITextRenderInfo) =
            match info.EndTextState with 
            | EndTextState.No 
            | EndTextState.Undified -> TextRenderInfo.getWidth info.Value
            | EndTextState.Yes -> (getBound BoundGettingStrokeOptions.WithoutStrokeWidth info).GetWidthF()


        let getDenseBound (boundGettingStrokeOptions: BoundGettingStrokeOptions) (info: ITextRenderInfo) =
            let bound = (getBound boundGettingStrokeOptions info)
            bound.setHeight(YEffort.Middle, fun m -> m * 0.42)


        let getFontName (info: ITextRenderInfo) = info.Value.GetFontName()


        let fontNameIs (fontName: string) (info: ITextRenderInfo) =
            let fontName2 = getFontName info
            fontName2.SameFontNameTo(fontName)


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
                PdfLogger.unSupportedTextRenderMode others
                []

        let getExtGState (info: ITextRenderInfo) : FsExtGState =    
            let gs: CanvasGraphicsState = info.Value.GetGraphicsState()
            CanvasGraphicsState.getExtGState gs

    //[<RequireQualifiedAccess>]
    //module ImageData = 
    //    let colorSpace (imageData: ImageData)=
    //        match imageData.GetColorEncodingComponentsNumber() with 
    //        | 1 -> ColorSpace.Gray
    //        | 3 -> ColorSpace.Rgb
    //        | 4 -> ColorSpace.Cmyk
    //        | colorSpaceNumber -> failwithf "Cannot get colorSpace from %d" colorSpaceNumber


    [<RequireQualifiedAccess>]
    module IImageRenderInfo =   
        let getUnclippedBound (info: IImageRenderInfo) =
            let image = info.Value

            let imageCtm = 
                image.GetImageCtm()
                |> AffineTransform.ofMatrix

            imageCtm.Transform(Rectangle.create 0 0 1 1)
            //let imageCtm = ctm.Concatenate imageCtm

            //let x, width =
            //    match imageCtm.ScaleX with 
            //    | BiggerOrEqual 0 -> imageCtm.TranslateX, imageCtm.ScaleX
            //    | SmallerThan 0 -> 
            //        imageCtm.TranslateX + imageCtm.ScaleX, abs imageCtm.ScaleX
            //    | _ -> failwith "Invalid token"

            //let y, height =
            //    match imageCtm.ScaleY with 
            //    | BiggerOrEqual 0 -> imageCtm.TranslateY, imageCtm.ScaleY
            //    | SmallerThan 0 -> 
            //        imageCtm.TranslateY + imageCtm.ScaleY, abs imageCtm.ScaleY
            //    | _ -> failwith "Invalid token"

            //let rect = Rectangle.create x y width height
            //let imageCtm = 
            //    imageCtm
            //    |> AffineTransformRecord.toAffineTransform

            //let rect2 = FsRectangle.OfRectangle rect
            //rect 



    [<RequireQualifiedAccess>]
    module IAbstractRenderInfo = 
        let cata (fTextRenderInfo) (fPathRenderInfo) (info: IAbstractRenderInfo) = 
            match info with 
            | :? ITextRenderInfo as trInfo -> fTextRenderInfo trInfo
            | :? IPathRenderInfo as prInfo -> fPathRenderInfo prInfo 
            | :? IImageRenderInfo -> failwith "Invaid token"
            | _ -> failwith "Invaid token"

        let getExtGState info = cata (ITextRenderInfo.getExtGState) (IPathRenderInfo.getExtGState) info

        let getBound boundGettingStrokeOptions info = cata (ITextRenderInfo.getBound boundGettingStrokeOptions) (IPathRenderInfo.getBound boundGettingStrokeOptions) info
        
        let getStrokeColor (info: IAbstractRenderInfo) = info.Value.GetStrokeColor() 

        let getFillColor (info: IAbstractRenderInfo) = info.Value.GetFillColor()

        let getDenseBound boundGettingStrokeOptions info = cata (ITextRenderInfo.getDenseBound boundGettingStrokeOptions) (IPathRenderInfo.getBound boundGettingStrokeOptions) info
        
        let getColors (info: IAbstractRenderInfo) = cata ITextRenderInfo.getColors IPathRenderInfo.getColors info

        let isTextRenderInfo (info: IAbstractRenderInfo) = cata (fun _ -> true) (fun _ -> false) info

        let isPathRenderInfo (info: IAbstractRenderInfo) = cata (fun _ -> false) (fun _ -> true) info

        let asTextRenderInfo (info: IAbstractRenderInfo) = cata (fun trInfo -> Some trInfo) (fun _ -> None) info

        let asPathRenderInfo (info: IAbstractRenderInfo) = cata (fun _ -> None) (fun prInfo -> Some prInfo) info

        let hasFill (info: IAbstractRenderInfo) = cata ITextRenderInfo.hasFill IPathRenderInfo.hasFill info

        let hasStroke (info: IAbstractRenderInfo) = cata ITextRenderInfo.hasStroke IPathRenderInfo.hasStroke info

        let hasStrokeOrFill (info: IAbstractRenderInfo) = hasFill info || hasStroke info

        let boundIsInsideOf  boundGettingStrokeOptions rect (info: IAbstractRenderInfo) =
            (getBound boundGettingStrokeOptions info).IsInsideOf(rect)

        let getDashPattern (info: IAbstractRenderInfo) = 
            let info = info.Value
            CanvasGraphicsState.getDashPattern <| info.GetGraphicsState()

        let isDashLine (info: IAbstractRenderInfo) =
            let dashPattern = getDashPattern info
            not dashPattern.IsEmpty

        let (|DashLine|EntityLine|) (info: IAbstractRenderInfo) =
            let dashPattern = getDashPattern info
            match dashPattern.IsEmpty with 
            | true -> EntityLine
            | false -> DashLine dashPattern


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
    module internal ClippingPathInfos =
        let (|NonExists|IntersectedNone|IntersectedSome|) (infos: ClippingPathInfos) =
            match infos.ClippingPathInfoState, infos.XObjectClippingBoxState with 
            | ClippingPathInfoState.Init, XObjectClippingBoxState.Init -> NonExists ()
            | ClippingPathInfoState.Intersected (clippingPathInfo), XObjectClippingBoxState.Init ->
                match (ClippingPathInfo.getActualClippingArea clippingPathInfo.ClippingPathInfo) with 
                | ClippingPathInfoResult.IntersectedSome clippingArea -> IntersectedSome clippingArea
                | ClippingPathInfoResult.IntersectedNone -> IntersectedNone

            | ClippingPathInfoState.Init, XObjectClippingBoxState.IntersectedSome rect ->
                IntersectedSome rect

            | ClippingPathInfoState.Intersected (clippingPathInfo), XObjectClippingBoxState.IntersectedSome rect ->
                let clippingArea = ClippingPathInfo.getActualClippingArea clippingPathInfo.ClippingPathInfo
                match clippingArea with 
                | ClippingPathInfoResult.IntersectedNone -> IntersectedNone
                | ClippingPathInfoResult.IntersectedSome clippingArea ->
                    match Rectangle.tryGetIntersection clippingArea rect with 
                    | Some rect -> IntersectedSome rect
                    | None -> IntersectedNone

            | _, XObjectClippingBoxState.IntersectedNone -> IntersectedNone

    [<RequireQualifiedAccess>]
    module IIntegratedRenderInfo =
        //[<Struct>]
        //type private ClippingPathInfos =
        //    { ClippingPathInfo: ClippingPathInfo option 
        //      XObjectClippingBox: Rectangle option }
        //with 
        //    member x.TryGetAcutalClippingArea() =  
        //        let clippingArea1 =
        //            match x.ClippingPathInfo with 
        //            | Some clippingPathInfo -> Some (ClippingPathInfo.getActualClippingArea clippingPathInfo)
        //            | None -> None
                
        //        match clippingArea1, x.XObjectClippingBox with 
        //        | Some box1, Some box2 -> Rectangle.tryGetIntersection box1 box2
        //        | Some box1, None -> Some box1
        //        | None, Some box2 -> Some box2
        //        | None, None -> None

        //    member x.IsEmpty =
        //        match x.ClippingPathInfo, x.XObjectClippingBox with 
        //        | None, None -> true
        //        | _ -> false




        [<RequireQualifiedAccess>]
        module private IAbstractRenderInfo =
            [<RequireQualifiedAccess; Struct>]
            type private VisibleBoundResult =
                | None
                | Some of Rectangle
                | SameToInfoBound

            let private getVisibleBound boundGettingStrokeOptions (fillOrStrokeOptions: FillOrStrokeOptions) (clippingPathInfos: ClippingPathInfos) (info: IAbstractRenderInfo) =
                let b = 
                    match fillOrStrokeOptions with 
                    | FillOrStrokeOptions.Fill -> IAbstractRenderInfo.hasFill info
                    | FillOrStrokeOptions.Stroke -> IAbstractRenderInfo.hasStroke info
                    | FillOrStrokeOptions.FillAndStroke -> IAbstractRenderInfo.hasFill info && IAbstractRenderInfo.hasStroke info
                    | FillOrStrokeOptions.FillOrStroke -> IAbstractRenderInfo.hasFill info || IAbstractRenderInfo.hasStroke info
                    &&
                        match info with 
                        | :? PathRenderInfo as info -> not (info.IsPathModifiesClippingPath())
                        | _ -> true

                match b with 
                | true ->
                    match clippingPathInfos with 
                    | ClippingPathInfos.IntersectedNone -> VisibleBoundResult.None
                    | ClippingPathInfos.IntersectedSome clippingBound -> 
                        let bound = IAbstractRenderInfo.getBound boundGettingStrokeOptions info
                        match Rectangle.tryGetIntersection bound clippingBound with 
                        | Some rect -> VisibleBoundResult.Some rect
                        | None -> VisibleBoundResult.None
                    | ClippingPathInfos.NonExists -> VisibleBoundResult.SameToInfoBound 
                | false -> VisibleBoundResult.None
        
            let isVisible (fillOrStrokeOptions: FillOrStrokeOptions) (clippingPathInfos: ClippingPathInfos) (info: IAbstractRenderInfo) =
                match getVisibleBound BoundGettingStrokeOptions.WithStrokeWidth fillOrStrokeOptions clippingPathInfos info with 
                | VisibleBoundResult.None -> false
                | VisibleBoundResult.Some _ -> true
                | VisibleBoundResult.SameToInfoBound -> true

            let tryGetVisibleBound boundGettingStrokeOptions (clippingPathInfos: ClippingPathInfos) (info: IAbstractRenderInfo) =
                match getVisibleBound boundGettingStrokeOptions FillOrStrokeOptions.FillOrStroke clippingPathInfos info with 
                | VisibleBoundResult.None -> None
                | VisibleBoundResult.Some rect -> Some rect
                | VisibleBoundResult.SameToInfoBound -> None

        let isStrokeVisible (info: IIntegratedRenderInfo) = 
            IAbstractRenderInfo.isVisible FillOrStrokeOptions.Stroke info.ClippingPathInfos info
    
        let isFillVisible (info: IIntegratedRenderInfo) = 
            IAbstractRenderInfo.isVisible FillOrStrokeOptions.Fill info.ClippingPathInfos info
    
        let tryGetVisibleBound boundGettingStrokeOptions (info: IIntegratedRenderInfo) =
            IAbstractRenderInfo.tryGetVisibleBound boundGettingStrokeOptions info.ClippingPathInfos info
    
        let isVisible (info: IIntegratedRenderInfo) =
            IAbstractRenderInfo.isVisible FillOrStrokeOptions.FillOrStroke info.ClippingPathInfos info


    [<RequireQualifiedAccess>]
    module PdfFormXObject =

        let getBBox (xobject:PdfFormXObject) =
            xobject.GetBBox().ToRectangle()

        let tryGetMatrix (xobject:PdfFormXObject) = 
            let ctm = xobject.GetPdfObject().Get(PdfName.Matrix)
            match ctm with 
            | null -> None
            | ctm ->
                let ctm = (ctm :?> PdfArray).ToDoubleArray()
                AffineTransform(ctm)
                |> Some

        let getActualBox (xobject:PdfFormXObject) =
            let bbox = xobject.GetBBox().ToRectangle()
            let ctm = xobject.GetPdfObject().Get(PdfName.Matrix)
            match ctm with 
            | null -> bbox
            | ctm ->
                let ctm = (ctm :?> PdfArray).ToDoubleArray()
                let ctm = AffineTransform(ctm)
                ctm.Transform(bbox)

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
        member this.AddXObjectAbs(xobject: PdfXObject, x, y) =
            this.AddXObjectWithTransformationMatrix(
                xobject,
                1.f,
                0.f,
                0.f,
                1.f,
                x,
                y
            )

        member x.AddXObject(xObject: PdfXObject, affineTransformRecord: AffineTransformRecord) =
            x.AddXObjectWithTransformationMatrix
                ( xObject,
                  float32 affineTransformRecord.m00,
                  float32 affineTransformRecord.m10,
                  float32 affineTransformRecord.m01,
                  float32 affineTransformRecord.m11,
                  float32 affineTransformRecord.m02,
                  float32 affineTransformRecord.m12 )

        member x.AddImage(image: ImageData, affineTransformRecord: AffineTransformRecord) =
            x.AddImageWithTransformationMatrix
                ( image,
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

        member canvas.SetActualLineWidth (ctm, width: ActualLineWidth) =
            match width with 
            | ActualLineWidth.Exactly _ -> canvas.SetLineWidth(float32 width.CalculatedWidth)
            | ActualLineWidth.Unbalance _ ->
                let matrix = AffineTransform.ofMatrix ctm
                canvas
                    .SaveState()
                    .ConcatMatrix(matrix)
                    .SetLineWidth(float32 width.RawLineWidth)
                    .ConcatMatrix(matrix.Clone().CreateInverse())
                    .RestoreState()

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

        let setLineCapStyle (style) (canvas: PdfCanvas) =
            canvas.SetLineCapStyle(style)

        let setLineJoinStyle (style) (canvas: PdfCanvas) =
            canvas.SetLineJoinStyle(style)

        let setLineWidth (width: float) (canvas: PdfCanvas) =
            canvas.SetLineWidth(float32 width)

        let setActualLineWidth ctm (width: ActualLineWidth) (canvas: PdfCanvas) =
            canvas.SetActualLineWidth(ctm, width)

        let setLineShapingStyle (lineStyle: LineShapingStyle) (canvas: PdfCanvas) =
            canvas
                .SetLineCapStyle(lineStyle.CapStyle)
                .SetLineJoinStyle(lineStyle.JoinStyle)
                .SetLineWidth(float32 lineStyle.RawLineWidth)
            |> setDashpattern lineStyle.DashPattern

        let setAppliedLineShapingStyle (ctm: Matrix) (lineStyle: LineShapingStyle) (canvas: PdfCanvas) =
            canvas
                .SetLineCapStyle(lineStyle.CapStyle)
                .SetLineJoinStyle(lineStyle.JoinStyle)
                .SetActualLineWidth(ctm, lineStyle.ActualLineWidth)
            |> setDashpattern lineStyle.DashPattern

        let setFillColor (color: Color) (canvas:PdfCanvas) =
            canvas.SetFillColor(color)

        let setPathRenderColorByOperation (operation: int) (fillColor: Color) (strokeColor) (canvas:PdfCanvas) =
            match operation with 
            | PathRenderInfo.FILL -> setFillColor fillColor canvas
            | PathRenderInfo.STROKE -> setStrokeColor strokeColor canvas
            | IPathRenderInfo.FILLANDSTROKE -> 
                canvas
                |> setFillColor fillColor
                |> setStrokeColor strokeColor

            | _ -> canvas

        let closePathByOperation (operation: int) (fillingRule: int) (canvas:PdfCanvas) =
            match operation with 
            | PathRenderInfo.FILL -> 
                match fillingRule with 
                | FillingRule.EVEN_ODD -> canvas.EoFill()
                | FillingRule.NONZERO_WINDING -> canvas.Fill()
                | _ -> failwithf "Invalid token"

            | PathRenderInfo.STROKE -> canvas.Stroke()
            | IPathRenderInfo.FILLANDSTROKE -> 
                match fillingRule with 
                | FillingRule.EVEN_ODD -> canvas.EoFillStroke()
                | FillingRule.NONZERO_WINDING -> canvas.FillStroke()
                | _ -> failwithf "Invalid token"
            | _ -> canvas.EndPath()

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

        let ellipse (rect: Rectangle) (canvas: PdfCanvas) =
            canvas.Ellipse(rect.GetXF(), rect.GetYF(), rect.GetRightF(), rect.GetTopF())

        let addXObject (xobject: PdfXObject) (affineTransformRecord: AffineTransformRecord) (canvas: PdfCanvas) =
            canvas.AddXObject (xobject, affineTransformRecord)

        let beginText (canvas: PdfCanvas) =
            canvas.BeginText()

        let setFontAndSize (font, size) (canvas: PdfCanvas) =   
            canvas.SetFontAndSize(font, size)

        let endText (canvas: PdfCanvas) =
            canvas.EndText()



    type Rectangle with 
        member x.GetPageEdge (innerBox: Rectangle) =
            let pageBox = x

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
            
    type PdfPage with
        member x.SetPageBoxToPage(targetPdfPage: PdfPage) =
            x
                .SetCropBox(targetPdfPage.GetCropBox())
                .SetMediaBox(targetPdfPage.GetMediaBox())
                .SetArtBox(targetPdfPage.GetArtBox())
                .SetBleedBox(targetPdfPage.GetBleedBox())
                .SetTrimBox(targetPdfPage.GetTrimBox())
            


        member x.GetPageEdge (innerBox: Rectangle, pageBoxKind) =
            let pageBox: Rectangle = x.GetPageBox(pageBoxKind)
            pageBox.GetPageEdge(innerBox)
        


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
            | PageBoxKind.AllBox -> 
                page.GetActualBox()
                //failwith "PageBoxKind.AllBox is settable only"
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
            | AreaGettingOptions.FsSpecfic rect -> rect.AsRectangle
            | AreaGettingOptions.PageBoxWithOffset (pageBoxKind, margin) ->
                x.GetPageBox(pageBoxKind)
                |> Rectangle.applyMargin margin


        member page.ClippingContentsToPageBox(pageBoxKind: PageBoxKind, ?margin: Margin) =
            let pageBox = page.GetPageBox(pageBoxKind)
            let contentBox = 
                pageBox |> Rectangle.applyMargin (defaultArg margin Margin.Zero)
            let writerCanvas = 
                new PdfCanvas(page.NewContentStreamBefore(), page.GetResources(), page.GetDocument())

            do 
                let stream = writerCanvas.GetContentStream().GetOutputStream()
                stream.SetLocalHighPrecision(true)

            writerCanvas
                .SaveState()
                .Rectangle(contentBox)
                .EoClip()
                .EndPath()
                |> ignore

            writerCanvas.AttachContentStream(page.NewContentStreamAfter())
                    
            writerCanvas.RestoreState() |> ignore


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
                |> setArtBox rect
                |> setBleedBox rect
                |> setTrimBox rect


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

    type PdfPage with
        member x.SetPageBox(pageBoxKind, rect) =
            PdfPage.setPageBox pageBoxKind rect x


    [<RequireQualifiedAccess>]
    module PdfDocument =
        let getPages (doc: PdfDocument) = doc.GetPages()




    type OffsetablePdfCanvas(contentStream: PdfStream, resources, pdfDocument, ?xOffset, ?yOffset) =
        inherit PdfCanvas(contentStream, resources, pdfDocument)
        let xOffset = defaultArg xOffset 0.f
        let yOffset = defaultArg yOffset 0.f
        do 
            let stream = contentStream.GetOutputStream()
            stream.SetLocalHighPrecision(true)

        member x.XOffset = xOffset

        member x.YOffset = yOffset

        new (xobject: PdfFormXObject, pdfDocument, ?xOffset, ?yOffset) =
            new OffsetablePdfCanvas(xobject.GetPdfObject(), xobject.GetResources(), pdfDocument, ?xOffset = xOffset, ?yOffset = yOffset)

        new (pdfPage: PdfPage, ?xOffset, ?yOffset) =
            let canvas = new PdfCanvas(pdfPage)
            new OffsetablePdfCanvas(canvas.GetContentStream(), pdfPage.GetResources(), pdfPage.GetDocument(), ?xOffset = xOffset, ?yOffset = yOffset)

        member x.ApplyOffsetToCtm(ctm: Matrix) =
            let ctm = AffineTransform.ofMatrix ctm
            //ctm.Translate(float xOffset, float yOffset)
            ctm