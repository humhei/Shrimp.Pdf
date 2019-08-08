namespace Atrous.Pdf
open iText.Kernel.Geom
open Atrous
module Extensions =
    open System.Collections.Generic

    let tolerance = 0.1

    let (@=) (f1: float) (f2: float) = 
        abs (f1 - f2) < tolerance
    
    module Types =
        [<RequireQualifiedAccess>]
        type Relative =
            | Inbox 
            | CrossBox
            | OutBox

    [<AutoOpen>]
    module iText = 
        open iText.Kernel.Pdf.Canvas.Parser.Data
        open iText.Kernel.Pdf
        open iText.Kernel.Pdf.Canvas
        open iText.Kernel.Pdf.Xobject
        open Types

        [<RequireQualifiedAccess>]
        module GlyphLine =
            open iText.IO.Font.Otf

            let getAllGlyphs (gl: GlyphLine) =
                [
                    for i = 0 to gl.Size() - 1 do 
                        yield gl.Get(i)
                ]

        [<RequireQualifiedAccess>]
        module PdfFont =
            open iText.IO.Font
            open iText.Kernel.Font
            open iText.IO.Font.Otf
            open System

            let TYPO_ASCENDER_SCALE_COEFF = 1.2f
            let TEXT_SPACE_COEFF = FontProgram.UNITS_NORMALIZATION |> float
            let calculateAscenderDescender (font: PdfFont) =
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

            let calculateLineWidthUnit (text: string) (font: PdfFont) =
                let line = font.CreateGlyphLine(text)
                let unit =
                    GlyphLine.getAllGlyphs line
                    |> List.map (fun gl -> gl.GetWidth())
                    |> List.sum
                    |> float
                unit / TEXT_SPACE_COEFF

            let [<Literal>] heightLerance = 1.2


        type TextRenderingMode = PdfCanvasConstants.TextRenderingMode

        type StraightLine =
            {
                Start: Point
                End: Point
            }

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
                            (Ax+r*(Bx-Ax)),  // Px
                            (Ay+r*(By-Ay)))  // Py

        type Rectangle with 
            member this.GetWidthF() = this.GetWidth() |> float
            member this.GetHeightF() = this.GetHeight() |> float
            member this.GetXF() = this.GetX() |> float
            member this.GetYF() = this.GetY() |> float
            member this.GetLeftF() = this.GetLeft() |> float
            member this.GetTopF() = this.GetTop() |> float
            member this.GetRightF() = this.GetRight() |> float
            member this.GetBottomF() = this.GetBottom() |> float

        [<RequireQualifiedAccess>]
        module Rectangle = 
            open iText.Kernel.Pdf

            let inline create x y width height =
                let x = float32 x
                let y = float32 y 
                let width = float32 width
                let height = float32 height
                new Rectangle(x,y,width,height)

            let private createFromTuplePoints (ps: seq<float * float>) =
                let xs,ys = ps |> List.ofSeq |> List.unzip
                let x = List.min xs
                let y = List.min ys
                let width = List.max xs - x 
                let height = List.max ys - y
                create x y width height
                
            let createFromPoints (ps: Point seq) =
                ps |> Seq.map (fun p -> p.x,p.y) |> createFromTuplePoints

            let createFromRectangles (rects: Rectangle seq) =
                rects |> Seq.collect (fun rect ->
                    let x = rect.GetXF()
                    let y = rect.GetYF()
                    let top = rect.GetTopF()
                    let right = rect.GetRightF()
                    [(x,y);(top,right)]
                ) |> createFromTuplePoints


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


            let getX (rect: Rectangle) =
                rect.GetX() |> float

            let getY (rect: Rectangle) =
                rect.GetY() |> float

            let getRight (rect: Rectangle) =
                rect.GetRight() |> float

            let getTop (rect: Rectangle) =
                rect.GetTop() |> float

            let getWidth (rect: Rectangle) =
                rect.GetWidth() |> float

            let pointInsideBox (p: float * float) (rect: Rectangle) =
                let x,y = p
                x > getX rect && x < getRight rect && y > getY rect && y < getTop rect

            let pointOutsideBox (p: float * float) (rect: Rectangle) =
                let x,y = p
                x < getX rect || x > getRight rect || y < getY rect || y > getTop rect




            let rightTop (rect: Rectangle) =
                rect.GetRightF(),rect.GetTopF()

            let leftBottom (rect: Rectangle) =
                rect.GetXF(),rect.GetYF()
            
            let boundPoints (rect: Rectangle) =
                [leftBottom rect;rightTop rect]
            
            let straightLineoutsideBox (x1,y1,x2,y2) (rect2: Rectangle) = 

                [(x1,y1);(x2,y2)] |> List.forall (fun pt ->
                    pointOutsideBox pt rect2
                )

            let straightLinecrosssideBox (line: StraightLine) (rect: Rectangle) = 
                let rectLines = rect |> asStraightLines
                rectLines |> List.tryPick (fun sl ->
                    StraightLine.calcIntersection sl line 
                )
                |> function 
                    | Some _ -> true
                    | None -> false

            let outsideBox (rect1: Rectangle) (rect2: Rectangle) = 
                boundPoints rect1 |> List.forall (fun pt ->
                    pointOutsideBox pt rect2
                )

            let crossBox (rect1: Rectangle) (rect2: Rectangle) =
                boundPoints rect1 |> List.exists (fun pt ->
                    pointInsideBox pt rect2
                )

            let inbox (rect1: Rectangle) (rect2: Rectangle) =
                rect1.GetBottom() > rect2.GetBottom()
                && rect1.GetTop() < rect2.GetTop()
                && rect1.GetLeft() > rect2.GetLeft()
                && rect1.GetRight() < rect2.GetRight()

            let relativePostion (rect1: Rectangle) (rect2: Rectangle) =
                if inbox rect1 rect2 then Relative.Inbox
                elif outsideBox rect1 rect2 then Relative.OutBox
                elif crossBox rect1 rect2 then Relative.CrossBox
                else Logger.invalidToken()

            let min (rect1: Rectangle) (rect2: Rectangle) =
                if inbox rect1 rect2 
                then rect1
                else rect2


            let equal (rect1: Rectangle) (rect2: Rectangle) =
                rect1.GetXF() @= rect2.GetXF()
                && rect1.GetWidthF() @= rect2.GetWidthF()
                && rect1.GetYF() @= rect2.GetYF()
                && rect1.GetHeightF() @= rect2.GetHeightF()

            let increaseHeight (l:float) (rect: Rectangle) = 
                let x = rect.GetXF()
                let y = rect.GetYF()
                let w = rect.GetWidthF()
                let h = rect.GetHeightF() + l
                create x y w h

            let scale scaleX scaleY (rect: Rectangle) =
                let width = rect.GetWidthF() * scaleX
                let height = rect.GetHeightF() * scaleY
                let x = rect.GetXF()
                let y = rect.GetYF()
                create x y width height

            let setWidth (w: float) (rect: Rectangle) =
                let x = rect.GetXF()
                let y = rect.GetYF()
                let h = rect.GetHeightF()
                create x y w h
        [<RequireQualifiedAccess>]
        module Matrix = 
            let toListOfSix (matrix: Matrix) = 
                [
                    matrix.Get(0)
                    matrix.Get(1)
                    matrix.Get(3)
                    matrix.Get(4)
                    matrix.Get(6)
                    matrix.Get(7)
                ]

        [<RequireQualifiedAccess>]
        module AffineTransform = 
            let create m00 m10 m01 m11 m02 m12 = 
                new AffineTransform(m00,m10,m01,m11,m02,m12)

            let createFromMatrix (matrix: Matrix) =
                [|
                    matrix.Get(0)
                    matrix.Get(1)
                    matrix.Get(3)
                    matrix.Get(4)
                    matrix.Get(6)
                    matrix.Get(7)
                |]
                |> AffineTransform

            let transform (p0: Point) (atf:AffineTransform) = 
                let p1 = new Point()
                atf.Transform(p0,p1)

            let inverseTransform (p0: Point) (atf:AffineTransform) = 
                let p1 = new Point()
                atf.InverseTransform(p0,p1)


        type AffineTransform with 
            member this.TransformRectangle(rect: Rectangle) = 
                let p1 = new Point (rect.GetXF(),rect.GetYF())
                let p2 = new Point (rect.GetRightF(),rect.GetTopF())
                let p3 = new Point (rect.GetXF(),rect.GetTopF())
                let p4 = new Point (rect.GetRightF(),rect.GetYF())
                [p1; p2; p3 ;p4] |> List.map (fun p -> AffineTransform.transform p this) |> Rectangle.createFromPoints

            member this.InverseTransformRectangle(rect: Rectangle) = 
                let p1 = new Point (rect.GetXF(),rect.GetYF())
                let p2 = new Point (rect.GetRightF(),rect.GetTopF())
                let p3 = new Point (rect.GetXF(),rect.GetTopF())
                let p4 = new Point (rect.GetRightF(),rect.GetYF())
                [p1; p2; p3 ;p4] |> List.map (fun p -> AffineTransform.inverseTransform p this) |> Rectangle.createFromPoints
    
        module Point =
            let transform affine point =
                AffineTransform.transform point affine
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
        open iText.Kernel.Pdf.Canvas.Parser.Data

        [<RequireQualifiedAccess>]
        module Subpath =



            let toLiteralPoints (subpath: Subpath) =
                subpath.GetPiecewiseLinearApproximation()


            let toPoints (ctm:Matrix) subpath =
                toLiteralPoints subpath
                |> Seq.map (Point.transform (AffineTransform.createFromMatrix ctm))
                |> List.ofSeq

            let getBound ctm (subpath: Subpath) =
                toPoints ctm subpath
                |> Rectangle.createFromPoints

            let hasPoints (subpath: Subpath) =
                toLiteralPoints subpath
                |> Seq.length
                |> function
                    | 0 -> false
                    | _ -> true

        [<RequireQualifiedAccess>]
        module PathRenderInfo =

            let [<Literal>] FILLANDSTROKE = PathRenderInfo.FILL ||| PathRenderInfo.STROKE

            let toLiteralPoints (prInfo: PathRenderInfo) =
                let subpaths = prInfo.GetPath().GetSubpaths()
                subpaths |> Seq.collect Subpath.toLiteralPoints

            let toPoints (prInfo: PathRenderInfo) =
                let ctm = prInfo.GetCtm()
                prInfo.GetPath().GetSubpaths()
                |> List.ofSeq
                |> List.collect (Subpath.toPoints ctm)

            let getBound (info: PathRenderInfo) =     
                info |> toPoints |> Rectangle.createFromPoints

            let hasSolidFill (info: PathRenderInfo) =             
                match info.GetOperation() with 
                | PathRenderInfo.FILL
                | FILLANDSTROKE -> true
                | _ -> false

            let hasSolidStroke (info: PathRenderInfo) =             
                match info.GetOperation() with 
                | PathRenderInfo.STROKE 
                | FILLANDSTROKE -> true
                | _ -> false

            let getColors (info: PathRenderInfo) =
                let fillColor = info.GetFillColor()
                let strokeColor = info.GetStrokeColor()
                match info.GetOperation() with 
                | PathRenderInfo.STROKE -> [strokeColor]
                | FILLANDSTROKE -> [fillColor;strokeColor]
                | PathRenderInfo.FILL -> [fillColor]
                | others -> 
                    Logger.notSupportedPathRendingMode others
                    []

            let isVisible (info: PathRenderInfo) =
                info.GetPath().GetSubpaths().Count <> 0
                && (hasSolidStroke info || hasSolidFill info)

        [<RequireQualifiedAccess>]
        module TextRenderInfo =
            open iText.Kernel.Pdf.Canvas
            open iText.Kernel.Pdf.Canvas.Parser.Data
            let getFontSize (info: TextRenderInfo) =
                let matrix = info.GetTextMatrix()
                info.GetFontSize() * matrix.Get(0) |> float

            let getHeight (info: TextRenderInfo) =
                let ascent = info.GetAscentLine()
                let descent = info.GetDescentLine()
                ascent.GetStartPoint().Get(1) - descent.GetStartPoint().Get(1)
            
            let getWidth (info: TextRenderInfo) =
                let ascent = info.GetAscentLine()
                ascent.GetEndPoint().Get(0) - ascent.GetStartPoint().Get(0)
            
            let getY (info: TextRenderInfo) =
                let descent = info.GetDescentLine()
                descent.GetStartPoint().Get(1)

            let getX (info: TextRenderInfo) =
                let descent = info.GetDescentLine()
                descent.GetStartPoint().Get(0)

            let getBound (info: TextRenderInfo) =
                let width = getWidth info
                let height = getHeight info
                let x = getX info
                let y = getY info
                Rectangle.create x y width height

            let getColors (info: TextRenderInfo) =
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
                    Logger.notSupportedTextRendingMode others
                    []

            let getText (rdinfo:TextRenderInfo) =
                rdinfo.GetText()

            let hasSolidFill (info: TextRenderInfo) =             
                let md = info.GetTextRenderMode()
                match md with 
                | PdfCanvasConstants.TextRenderingMode.FILL 
                | PdfCanvasConstants.TextRenderingMode.FILL_STROKE -> true
                | _ -> false

            let hasSolidStroke (info: TextRenderInfo) =             
                let md = info.GetTextRenderMode()
                match md with 
                | PdfCanvasConstants.TextRenderingMode.STROKE 
                | PdfCanvasConstants.TextRenderingMode.FILL_STROKE  
                | PdfCanvasConstants.TextRenderingMode.STROKE_CLIP -> true
                |  _ -> false


            
            let isVisible (info: TextRenderInfo) =
                hasSolidStroke info || hasSolidFill info

    
        [<RequireQualifiedAccess>]
        module AbstractRenderInfo = 
            open iText.Kernel.Pdf.Canvas.Parser.Data
            let cata (fTRInfo) (fPRInfo) (info: AbstractRenderInfo) = 
                match info with 
                | :? TextRenderInfo as trInfo -> fTRInfo trInfo
                | :? PathRenderInfo as prInfo -> fPRInfo prInfo 
                | _ -> Logger.invalidToken()

            let getBound info = cata TextRenderInfo.getBound PathRenderInfo.getBound info
            
            let getColors (info: AbstractRenderInfo) = cata TextRenderInfo.getColors PathRenderInfo.getColors info

            let isTextRenderInfo (info: AbstractRenderInfo) = cata (fun _ -> true) (fun _ -> false) info

            let isPathRenderInfo (info: AbstractRenderInfo) = cata (fun _ -> false) (fun _ -> true) info

            let asTextRenderInfo (info: AbstractRenderInfo) = cata (fun trInfo -> Some trInfo) (fun _ -> None) info

            let asPathRenderInfo (info: AbstractRenderInfo) = cata (fun _ -> None) (fun prInfo -> Some prInfo) info

            let hasSolidFill (info: AbstractRenderInfo) = cata TextRenderInfo.hasSolidFill PathRenderInfo.hasSolidFill info

            let hasSolidStroke (info: AbstractRenderInfo) = cata TextRenderInfo.hasSolidStroke PathRenderInfo.hasSolidStroke info

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
                dashArray,phase

        [<RequireQualifiedAccess>]
        module PdfFormXObject =
            open iText.Kernel.Pdf.Xobject
            open iText.Kernel.Pdf

            let getBBox (xobject:PdfFormXObject) =
                xobject.GetBBox().ToRectangle()

            let setBBox (rect: Rectangle) (xobject:PdfFormXObject) =
                xobject.SetBBox(rect |> Rectangle.asPdfArray)

            let tryGetTrimBox (xobject:PdfFormXObject) =
                let rect = xobject.GetPdfObject().GetAsRectangle(PdfName.TrimBox)

                match rect with 
                | null -> None
                | rect -> Some rect

            let getRotate (xobject:PdfFormXObject) =
                match xobject.GetPdfObject().GetAsNumber(PdfName.Rotate) with 
                | null -> 0.
                | rotate -> rotate.GetValue()

        [<RequireQualifiedAccess>]
        module PdfCanvas = 

            let useCanvas (canvas:PdfCanvas) f =
                canvas.SaveState() |> ignore
                f()
                canvas.RestoreState() |> ignore

            let setDashpattern (patternArray: float32 []) (phase: float32) (canvas:PdfCanvas) =
                canvas.SetLineDash(patternArray,phase)

            let setStrokeColor color (canvas:PdfCanvas) =
                canvas.SetStrokeColor(color)

            let setFillColor color (canvas:PdfCanvas) =
                canvas.SetFillColor(color)

            let stroke (canvas:PdfCanvas) =
                canvas.Stroke()

            let newPath (canvas: PdfCanvas) =
                canvas.NewPath()

            let showText (text: string) (canvas:PdfCanvas)=
                canvas.ShowText(text)

            let setTextRendingMode i (canvas:PdfCanvas)=
                canvas.SetTextRenderingMode(i)

            let addXObject (xobject: PdfFormXObject) (x: float) (y: float) (canvas: PdfCanvas) =
                canvas.AddXObject (xobject,float32 x,float32 y)

            let addXObjectOfTrimBox (xobject: PdfFormXObject) (x: float) (y: float) width height (canvas: PdfCanvas) =
                let bbox = 
                    xobject 
                    |> PdfFormXObject.tryGetTrimBox
                    |> function
                        | Some rect -> rect
                        | None -> xobject |> PdfFormXObject.getBBox

                let tf = new AffineTransform()


                let rect = 
                    let rect = tf.TransformRectangle(bbox)
                    let scaleX,scaleY =
                        width / rect.GetWidthF(),
                        height / rect.GetHeightF()
                    tf.Scale(scaleX,scaleY)
                    tf.TransformRectangle(bbox)
                
                let matrix = Array.create 6 0.f
                tf.GetMatrix(matrix)

                let setClippingBox xobject =
                    xobject |> PdfFormXObject.setBBox rect

                canvas.AddXObject
                    (setClippingBox xobject,matrix.[0],
                        matrix.[1],matrix.[2],matrix.[3],
                        matrix.[4] - rect.GetX() + float32 x, 
                        matrix.[5] - rect.GetY() + float32 y) 
                    |> ignore

            let addXObjectWithMatrix (xobject: PdfFormXObject) (x: float) (y: float) (scaleX: float) (scaleY: float) (canvas: PdfCanvas) =
                let rect = new Rectangle(float32 x,float32 y,float32 scaleX,float32 scaleY)
                canvas.AddXObject (xobject,rect)

            //let addText (position: Position) (text: string) font fontSize (page: PdfPage) (canvas: PdfCanvas) = 
            //    match position.PageBoxzzz with 
            //    | PageBoxKind.CropBox ->
            //        let bbox = page |> PdfPage.getCropBox
            //        addTextToRect position.Orientation text font fontSize bbox canvas 

            //    | _ -> failwith "Not implemented"

        type PdfCanvas with

            member this.AddInDirectXObject (f: PdfDocument -> PdfFormXObject,x: float,y: float) =
                let doc = this.GetDocument()
                let xobject = f doc
                this.AddXObject(xobject,float32 x,float32 y)

            member inline this.SetLineWidth (x: float) =
                let x = float32 x
                this.SetLineWidth(x)

            //let addXObjectWithMatrixAndRotation (xobject: PdfFormXObject) (x: float) (y: float) (scaleX: float) (scaleY: float) (rotation: Rotation) (canvas: PdfCanvas) =
            //    let affine = new AffineTransform(scaleX,0.,0.,scaleY,x,y)
            //    affine.Rotate(radians -rotation.Degree,rotation.BBox.GetXF(),rotation.BBox.GetYF())
            //    let matrix = Array.create 6 0.f
            //    affine.GetMatrix(matrix)
            //    if rotation.Degree = 90. then
            //        canvas.AddXObject (xobject,matrix.[0],matrix.[1],matrix.[2],matrix.[3],matrix.[4],matrix.[5] + rotation.BBox.GetHeight())
            //    else failwith "Not implemented"



        [<RequireQualifiedAccess>]
        module Text =
            open iText.Layout.Element

            let toColoredText (text: string) color =
                let t = new Text(text)
                t.SetFontColor(color)

                
        type PdfPage with
            member this.GetBBox() = 
                let crop = this.GetCropBox()
                let media = this.GetMediaBox()
                Rectangle.min crop media

            member this.GetHeight() = this.GetBBox().GetHeight() |> float
            member this.GetWidth() = this.GetBBox().GetWidth() |> float

        [<RequireQualifiedAccess>]
        module PdfPage = 
            open iText.Kernel.Pdf.Canvas

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

            let getWidth (page: PdfPage) = 
                page.GetBBox().GetWidth() |> float

            let getTrimBox (page: PdfPage) =
                page.GetTrimBox()

            let getCropBox (page: PdfPage) =
                page.GetTrimBox()

            let getHeight (page: PdfPage) = 
                page.GetBBox().GetHeight() |> float

            let getCanvas (page: PdfPage) =
                new PdfCanvas(page)

            let addToDoc (doc: PdfDocument) (page: PdfPage) =
                let page = page.CopyTo(doc)
                doc.AddPage(page) |> ignore

        [<RequireQualifiedAccess>]
        module Canvas =
            open iText.Layout
            open iText.Kernel.Colors

            let setIndirectFontColor (canvas: Canvas) (f: PdfDocument -> Color) =
                let color = f (canvas.GetPdfDocument())
                canvas.SetFontColor(color)

            let useCanvas (page: PdfPage) (bbox: Rectangle) f =
                let doc = page.GetDocument()
                let pdfCanvas = new PdfCanvas(page)
                PdfCanvas.useCanvas pdfCanvas (fun _ ->
                    let canvas = 
                        new Canvas(pdfCanvas,doc,bbox,true)
                    f canvas
                )

            let useCanvasWithPageBox (page: PdfPage) f =
                let bbox = page.GetBBox()
                useCanvas page bbox f


