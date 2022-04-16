namespace Shrimp.Pdf

open Newtonsoft.Json
open iText.IO.Font

#nowarn "0104"
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas.Parser.Data
open FParsec
open iText.Kernel.Pdf
open Shrimp.FSharp.Plus


[<AutoOpen>]
module ExtensionTypes =

    type PdfLiteral with 
        member x.Text() = x.ToString()

    let private shortFontName (fontName: string) =
        if fontName.Contains "+"
        then fontName.RightOf("+").Value
        else fontName

    type FsFontName [<JsonConstructor>] (fontName: string) =
        inherit POCOBase<StringIC>(fontName |> shortFontName |> StringIC)
    
        let shortFontName = shortFontName fontName
            
        [<JsonProperty>]
        member private x.FontName = fontName
    
        member x.ShortFontName = shortFontName
    
        member x.LoggingText = shortFontName
    
        static member Create (fontName: FontNames) =
            let fontName = fontName.GetFontName()
            FsFontName(fontName)
    
    

    [<StructuredFormatDisplay("{LoggingText}")>]
    type FsRectangle =
        { X: float 
          Y: float 
          Width: float 
          Height: float }
    with 
        member x.AsRectangle =
            Rectangle
                ( float32 x.X, 
                  float32 x.Y,
                  float32 x.Width,
                  float32 x.Height )

        member x.Left = x.X

        member x.Bottom = x.Y

        member x.Right = x.Left + x.Width

        member x.Top = x.Bottom + x.Height



        static member OfRectangle(rect: Rectangle) =
            { X      = rect.GetX()       |> float
              Y      = rect.GetY()       |> float
              Width  = rect.GetWidth()   |> float
              Height = rect.GetHeight()  |> float
            }

        member private x.MMValue =
            { X = userUnitToMM x.X
              Y = userUnitToMM x.Y 
              Width = userUnitToMM x.Width
              Height = userUnitToMM x.Height }

        member x.LoggingText = 
            let x = x.MMValue
            sprintf "RectMM %.1f %.1f %.1f %.1f" x.X x.Y x.Width x.Height

        static member create x y width height =
            { X = x 
              Y = y 
              Width = width
              Height = height }

        override x.ToString() = x.LoggingText

    [<RequireQualifiedAccess>]
    module Subpath =

        let toRawPoints (subpath: Subpath) =
            subpath.GetPiecewiseLinearApproximation()

    [<RequireQualifiedAccess>]
    module Rectangle =
        // <param name="points" at least two unique points></param>
        let ofPoints (points: Point al2List) =
            let xs,ys = 
                points.AsList 
                |> List.ofSeq
                |> List.map (fun p -> p.x,p.y) 
                |> List.unzip

            let x = List.min xs
            let y = List.min ys
            let width = List.max xs - x 
            let height = List.max ys - y
            Rectangle(float32 x, float32 y, float32 width, float32 height)
    
    type FsOPM =
        | Standard = 0
        | Illustractor = 1

    type FsExtGState =
        { 
            OPM: FsOPM
            IsStrokeOverprint: bool
            IsFillOverprint: bool
        }
    with 
        static member DefaultValue =
            { OPM = FsOPM.Illustractor 
              IsStrokeOverprint = false
              IsFillOverprint = false }

        static member StrokeOverprint =
            { FsExtGState.DefaultValue with 
                IsStrokeOverprint = true 
            }

        static member FillOverprint =
            { FsExtGState.DefaultValue with 
                IsFillOverprint = true 
            }

        static member FillStrokeOverprint =
            { FsExtGState.DefaultValue with 
                IsFillOverprint = true 
                IsStrokeOverprint = true
            }

    type FsPoint =
        { X: float 
          Y: float }
    with 
        member private x.MMValue =
            { X = userUnitToMM x.X
              Y = userUnitToMM x.Y }

        static member Zero =
            { X = 0. 
              Y = 0. }

        static member OfPoint(point: Point) =
            { X = point.x 
              Y = point.y }

        member x.AsPoint = Point(x.X, x.Y)

    type Direction =
        | Vertical = 0
        | Horizontal = 1

    type BoundGettingStrokeOptions =
        | WithStrokeWidth = 0
        | WithoutStrokeWidth = 1


    type FillOrStrokeOptions =
        | Stroke = 0
        | Fill = 1
        | FillOrStroke = 2
        | FillAndStroke = 3

    type PageNumber(v) =
        inherit POCOBaseV<int>(v)

        let __checkPageNumberValid =
            match v > 0 with 
            | true -> ()
            | false -> failwithf "Cannot create pageNumber by %d" v

        member x.Value = v



    type IAbstractRenderInfo =
        abstract member Value: AbstractRenderInfo

    type IAbstractRenderInfoIM =
        inherit IAbstractRenderInfo
        

    type IPathRenderInfo =
        inherit IAbstractRenderInfo
        abstract member Value: PathRenderInfo
            
    type ITextRenderInfo =
        inherit IAbstractRenderInfo
        abstract member Value: TextRenderInfo

    type IImageRenderInfo =
        inherit IAbstractRenderInfoIM
        abstract member Value: ImageRenderInfo

    type IntegratedRenderInfoTag =
        | Path = 0
        | Text = 1

    type IntegratedRenderInfoTagIM =
        | Path = 0
        | Text = 1
        | Image = 2

    [<RequireQualifiedAccess>]
    module IntegratedRenderInfoTagIM =
        let asIntegratedRenderInfoTag = function
            | IntegratedRenderInfoTagIM.Path -> IntegratedRenderInfoTag.Path |> Some
            | IntegratedRenderInfoTagIM.Text -> IntegratedRenderInfoTag.Text |> Some
            | IntegratedRenderInfoTagIM.Image -> None

    [<RequireQualifiedAccess; Struct>]
    type XObjectClippingBoxState =
        | Init 
        | IntersectedSome of Rectangle
        | IntersectedNone 

    
    [<Struct; RequireQualifiedAccess>]
    type ClippingPathInfoResult =
        | IntersectedSome of Rectangle
        | IntersectedNone 

    [<RequireQualifiedAccess>]
    module ClippingPathInfo =
        let private getActualClippingPath (info: ClippingPathInfo) = 
            match info.GetClippingPath() with 
            | null -> failwith "Not implemented"
            | path -> path
    
        let getActualClippingArea (info) =
            let clippingPath = getActualClippingPath info
            let points = 
                clippingPath.GetSubpaths()
                |> Seq.collect(Subpath.toRawPoints)
                |> List.ofSeq

            match points with 
            | [] -> ClippingPathInfoResult.IntersectedNone
            | _ -> Rectangle.ofPoints (AtLeastTwoList.Create points) |> ClippingPathInfoResult.IntersectedSome



    
    [<RequireQualifiedAccess; Struct>]
    type ClippingPathInfoState =
        | Init 
        | Intersected of ClippingPathInfo
    with 
        member x.ActualClippingPathArea =
            match x with 
            | ClippingPathInfoState.Init _ -> None
            | ClippingPathInfoState.Intersected v -> Some (ClippingPathInfo.getActualClippingArea v)






    [<Struct>]
    type ClippingPathInfos =
        { XObjectClippingBoxState: XObjectClippingBoxState 
          ClippingPathInfoState: ClippingPathInfoState }


    type IIntegratedRenderInfoIM =
        inherit IAbstractRenderInfoIM
        abstract member TagIM: IntegratedRenderInfoTagIM
        abstract member ClippingPathInfos: ClippingPathInfos

    type IIntegratedRenderInfo =
        inherit IAbstractRenderInfo
        abstract member Tag: IntegratedRenderInfoTag
        abstract member ClippingPathInfos: ClippingPathInfos





    type PageEdge =
        { LeftMiddle: Rectangle 
          LeftBottom: Rectangle 
          LeftTop: Rectangle 
          TopMiddle: Rectangle 
          TopRight: Rectangle 
          RightMiddle: Rectangle 
          RightBottom: Rectangle 
          BottomMiddle: Rectangle }


    [<RequireQualifiedAccess>]
    module PageEdge =
        let getAreas pageEdge =
            [ pageEdge.LeftMiddle
              pageEdge.LeftTop
              pageEdge.LeftBottom
              pageEdge.TopMiddle
              pageEdge.TopRight
              pageEdge.RightMiddle
              pageEdge.RightBottom
              pageEdge.BottomMiddle ]

    type Margin = 
        { Left: float
          Top: float
          Right: float
          Bottom: float }
    with   
        static member Create(value) =
            { Left = value 
              Top = value 
              Right = value 
              Bottom = value }

        static member Create(left, top, right, bottom) =
            { Left = left 
              Top = top 
              Right = right 
              Bottom = bottom }

        static member Create(values: float list) =
            match values with 
            | [ left; top; right; bottom ] ->
                { Left = left
                  Top = top
                  Right = right
                  Bottom = bottom }

            | _ -> failwithf "values' length %d is not equal to 4" values.Length

        static member Zero = Margin.Create(0.)

        static member MM6 = Margin.Create(mm 6.)

        member x.LoggingText = 
            let distincted =
                [x.Left; x.Top; x.Right; x.Bottom]
                |> List.distinct

            match distincted with 
            | [one] -> sprintf "Margin %.1f" one
            | _ -> sprintf "Margin %.1f %.1f %.1f %.1f" x.Left x.Top x.Right x.Bottom


    [<RequireQualifiedAccess>]
    module Margin =
        let getValues (margin: Margin) =
            [ margin.Left; margin.Top; margin.Right; margin.Bottom]

        let mapValues f (margin: Margin) =
            { Left = f margin.Left
              Top = f margin.Top
              Bottom = f margin.Bottom
              Right = f margin.Right
            }

        let mapValuesByDirection direction f (margin: Margin) =
            match direction with 
            | Direction.Horizontal ->
                { margin with 
                    Left = f margin.Left
                    Right = f margin.Right
                }

            | Direction.Vertical ->
                { margin with 
                    Top =    f margin.Top
                    Bottom = f margin.Bottom
                }

      


    type TileTable = 
        private TileTable of colNum: int * rowNum: int * hSpacing: float list * vSpacing: float list
    with 
        member x.ColNum =
            let (TileTable (colNum, rowNum, hSpacing, vSpacing)) = x
            colNum

        member x.RowNum = 
            let (TileTable (colNum, rowNum, hSpacing, vSpacing)) = x
            rowNum

        member x.HSpacing = 
            let (TileTable (colNum, rowNum, hSpacing, vSpacing)) = x
            hSpacing

        member x.VSpacing = 
            let (TileTable (colNum, rowNum, hSpacing, vSpacing)) = x
            vSpacing

        static member Create(colNum, rowNum, ?HSpacing, ?VSpacing) =
            if not (colNum > 0 && rowNum > 0) then failwithf "colNum %d and rowNum %d should bigger than 0" colNum rowNum 

            TileTable(colNum, rowNum, defaultArg HSpacing [0.], defaultArg VSpacing [0.])



    /// zero-based index
    type TileIndexer = private TileIndexer of TileTable * Direction * index: int
    with 
        member x.ColNum =
            let (TileIndexer (tileTable, direction, index)) = x
            tileTable.ColNum

        member x.RowNum = 
            let (TileIndexer (tileTable, direction, index)) = x
            tileTable.RowNum

        member x.Index = 
            let (TileIndexer (tileTable, direction, index)) = x
            index

        member x.Direction = 
            let (TileIndexer (tileTable, direction, index)) = x
            direction

        member x.TileTable =
            let (TileIndexer (tileTable, direction, index)) = x
            tileTable


    [<RequireQualifiedAccess>]
    module TileIndexer = 
        /// zero-based index
        let create (tileTable: TileTable) direction index =
            if index < 0 then failwithf "pieceIndex %d should >= 0" index
            TileIndexer(tileTable, direction, index)




    [<RequireQualifiedAccess>]
    type Rotation =
        | None = 0
        | Clockwise = 1
        | Counterclockwise = 2
        | R180 = 3

    type Margin with 
        member x.Rotate(rotation: Rotation) =
            match rotation with
            | Rotation.Clockwise ->
                Margin.Create(x.Bottom, x.Left, x.Top, x.Right)
            | Rotation.Counterclockwise ->
                Margin.Create(x.Top, x.Right, x.Bottom, x.Left)

            | Rotation.None -> x
            | Rotation.R180 -> Margin.Create(x.Right, x.Bottom, x.Left, x.Top) 


    [<RequireQualifiedAccess>]
    module Rotation =
        let getAngle = function
            | Rotation.Clockwise  -> 90.
            | Rotation.Counterclockwise -> -90.
            | Rotation.R180 -> 180.
            | Rotation.None -> 0.

        let ofAngle angle = 
            let angle = 
                match angle with 
                | SmallerThan 0. -> (abs angle) + 180.
                | _ -> angle

            match (angle) % 360. with
            | 90. -> Rotation.Clockwise
            | 180. -> Rotation.R180
            | 0. -> Rotation.None
            | 270. -> Rotation.Counterclockwise
            | v -> failwithf "Cannot create rotation by angle %A" angle

        let concatenate (rotation1) (rotation2) =
            getAngle rotation1 + getAngle rotation2
            |> ofAngle


        let getRadians rotation = 
            let angle = getAngle rotation
            (System.Math.PI / 180.) * angle
            


        let isNon = function
            | Rotation.None  -> true
            | _ -> false

        let notNon = function
            | Rotation.None  -> false
            | _ -> true


    type AffineTransformRecord =
        { ScaleX: float 
          ShearX: float 
          ShearY: float
          ScaleY: float 
          TranslateX: float 
          TranslateY: float }
    with
        member x.m00 = x.ScaleX

        member x.m01 = x.ShearX

        member x.m10 = x.ShearY

        member x.m11 = x.ScaleY

        member x.m02 = x.TranslateX

        member x.m12 = x.TranslateY
        
    module AffineTransformRecord =
        let ofAffineTransform (affineTransform: AffineTransform) =
            { ScaleX = affineTransform.GetScaleX() 
              ShearX = affineTransform.GetShearX() 
              ShearY = affineTransform.GetShearY() 
              ScaleY = affineTransform.GetScaleY() 
              TranslateX = affineTransform.GetTranslateX() 
              TranslateY = affineTransform.GetTranslateY() }

        let toAffineTransform (record: AffineTransformRecord) =
            new AffineTransform(
                record.m00,
                record.m10,
                record.m01,
                record.m11,
                record.m02,
                record.m12
            )


        let ofMatrix (matrix: Matrix) =
            let values =
                [| matrix.Get(Matrix.I11)
                   matrix.Get(Matrix.I12)
                   matrix.Get(Matrix.I21)
                   matrix.Get(Matrix.I22)
                   matrix.Get(Matrix.I31)
                   matrix.Get(Matrix.I32) |]
            
            values
            |> AffineTransform
            |> ofAffineTransform
     

        let toMatrix (record: AffineTransformRecord) =
            let values = Array.create 6 0.f
            (toAffineTransform record).GetMatrix(values)
            new Matrix(values.[Matrix.I11], values.[Matrix.I12], values.[Matrix.I21], values.[Matrix.I22], values.[Matrix.I31], values.[Matrix.I32])


    type DashPattern =
        { DashArray: float []
          Phase: float }

    with 
        member x.DashArrayF32 =
            x.DashArray
            |> Array.map float32

        member x.PhaseF32 =
            float32 x.Phase

        static member Create(value) =
            { DashArray = [|value|]
              Phase = value }

        static member Empty = {DashArray = [||]; Phase = 0.}

    type PageBoxKind =
        | ArtBox = 0
        | BleedBox = 1
        | TrimBox = 2
        | CropBox = 3
        | MediaBox = 4
        | ActualBox = 5
        | AllBox = 6


    [<RequireQualifiedAccess>]
    type AreaGettingOptions =
        | PageBox of PageBoxKind
        | PageBoxWithOffset of PageBoxKind * Margin
        | FsSpecfic of FsRectangle
    with    
        static member Specfic(rect) = 
            AreaGettingOptions.FsSpecfic(FsRectangle.OfRectangle rect)



    [<RequireQualifiedAccess>]
    type CanvasFontSize =
        | Numeric of size: float
        | OfRootArea of scale: float
        | OfFsArea of FsRectangle
    with    
        static member OfArea(rect) =
            CanvasFontSize.OfFsArea(FsRectangle.OfRectangle rect)

    [<RequireQualifiedAccess>]
    type RelativePosition =
        | Inbox = 0
        | CrossBox = 1
        | OutBox = 2



    type StraightLine =
        { Start: Point
          End: Point }


    type YEffect =
        | Top = 0
        | Bottom  = 1
        | Middle  = 2

    type XEffect =
        | Left = 0
        | Right = 1
        | Middle  = 2

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




    [<RequireQualifiedAccess>]
    module Position =

        let (|Left|XCenter|Right|) = function
            | Position.LeftTop (x, y) 
            | Position.LeftBottom (x, y) 
            | Position.LeftMiddle(x, y) -> Left (x, y)
            | Position.Center (x, y) 
            | Position.BottomMiddle (x, y) 
            | Position.TopMiddle(x, y) -> XCenter (x, y)
            | Position.RightBottom (x, y) 
            | Position.RightTop (x, y)
            | Position.RightMiddle (x, y) -> Right (x, y)

        let (|Bottom|Top|YCenter|) = function
            | Position.LeftBottom (x, y)
            | Position.RightBottom (x, y)
            | Position.BottomMiddle (x, y)-> Bottom (x, y)
            | Position.LeftTop (x, y) 
            | Position.RightTop (x, y)
            | Position.TopMiddle(x, y) -> Top (x, y)
            | Position.Center (x, y) 
            | Position.LeftMiddle (x, y) 
            | Position.RightMiddle(x, y) -> YCenter (x, y)

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

        let getValue position = 
            match position with
            | Position.LeftBottom (x, y)
            | Position.LeftMiddle (x, y)
            | Position.LeftTop (x, y) 
            | Position.TopMiddle (x, y) 
            | Position.RightTop (x, y)
            | Position.RightMiddle (x, y)
            | Position.RightBottom (x, y)
            | Position.BottomMiddle (x, y) 
            | Position.Center (x, y) -> x, y

        let mapValue (mapping) position = 
            match position with
            | Position.LeftBottom (x, y)-> Position.LeftBottom (mapping (x, y))
            | Position.LeftMiddle (x, y) -> Position.LeftMiddle (mapping (x, y))
            | Position.LeftTop (x, y) -> Position.LeftTop (mapping (x, y))
            | Position.TopMiddle (x, y) -> Position.TopMiddle (mapping (x, y))
            | Position.RightTop (x, y) -> Position.RightTop (mapping (x, y))
            | Position.RightMiddle (x, y) -> Position.RightMiddle (mapping (x, y))
            | Position.RightBottom (x, y) -> Position.RightBottom (mapping (x, y))
            | Position.BottomMiddle (x, y) -> Position.BottomMiddle (mapping (x, y))
            | Position.Center (x, y) -> Position.Center (mapping (x, y))

    type TextRenderingMode = iText.Kernel.Pdf.Canvas.PdfCanvasConstants.TextRenderingMode
    type LineJoinStyle = iText.Kernel.Pdf.Canvas.PdfCanvasConstants.LineJoinStyle
    type LineCapStyle = iText.Kernel.Pdf.Canvas.PdfCanvasConstants.LineCapStyle
    type FillingRule = iText.Kernel.Pdf.Canvas.PdfCanvasConstants.FillingRule



    [<RequireQualifiedAccess>]
    type SinglePageSelectorExpr =
        | Begin of int
        | End of int

    with 
        member x.AsSingleIndexExpr =
            match x with 
            | SinglePageSelectorExpr.Begin v -> SingleIndexExpr.Begin v
            | SinglePageSelectorExpr.End v -> SingleIndexExpr.End v

        static member OfSingleIndexExpr(expr) =
            match expr with 
            | SingleIndexExpr.Begin v -> SinglePageSelectorExpr.Begin v
            | SingleIndexExpr.End v   -> SinglePageSelectorExpr.End v  

        override x.ToString() =
            match x with 
            | SinglePageSelectorExpr.Begin i -> i.ToString()
            | SinglePageSelectorExpr.End i -> "R" + i.ToString()

    [<RequireQualifiedAccess>]
    type PageSelectorExpr = 
        | SinglePage of SinglePageSelectorExpr
        | Between of SinglePageSelectorExpr * SinglePageSelectorExpr
        | Compose of PageSelectorExpr list

    with
        member x.AsIndexExpr =
            match x with 
            | PageSelectorExpr.SinglePage  v       ->  IndexExpr.Single   v.AsSingleIndexExpr       
            | PageSelectorExpr.Between    (v1, v2) ->  IndexExpr.Between     (v1.AsSingleIndexExpr, v2.AsSingleIndexExpr) 
            | PageSelectorExpr.Compose     v       ->  
                v       
                |> List.map(fun m -> m.AsIndexExpr)
                |> IndexExpr.Compose

        static member OfIndexExpr(expr) =
            match expr with 
            | IndexExpr.Single  v       ->  
                PageSelectorExpr.SinglePage  (SinglePageSelectorExpr.OfSingleIndexExpr v)
            | IndexExpr.Between    (v1, v2) ->  
                let v1 =  SinglePageSelectorExpr.OfSingleIndexExpr v1
                let v2 =  SinglePageSelectorExpr.OfSingleIndexExpr v2
                PageSelectorExpr.Between (v1, v2)
            | IndexExpr.Compose     v       ->  
                v       
                |> List.map(PageSelectorExpr.OfIndexExpr)
                |> PageSelectorExpr.Compose

        override x.ToString() =
            match x with 
            | PageSelectorExpr.SinglePage expr -> expr.ToString()
            | PageSelectorExpr.Between (expr1, expr2) -> expr1.ToString() + "-" + expr2.ToString()
            | Compose exprs ->
                exprs
                |> List.map (fun m -> m.ToString())
                |> String.concat ", "

    [<RequireQualifiedAccess>]
    module PageSelectorExpr = 
        let create (exprText: string) =
            IndexExpr.create exprText
            |> PageSelectorExpr.OfIndexExpr



    [<RequireQualifiedAccess>]
    type PageSelector =
        | Last
        | First
        | All
        | Expr of PageSelectorExpr
        | Numbers of AtLeastOneSet<int>
    with 
        override x.ToString() =
            match x with 
            | PageSelector.Last -> "1R"
            | PageSelector.First -> "1"
            | PageSelector.All -> "ALL"
            | PageSelector.Expr expr -> expr.ToString()
            | PageSelector.Numbers numbers -> 
                let numbers = 
                    numbers.Value
                    |> Set.toList
                    |> List.map string
                    |> String.concat ", "

                let numbers = "[" + numbers + "]"

                if numbers.Length > 80
                then 
                    let chars = 
                        numbers.ToCharArray()
                        |> Array.take 80

                    let v = new System.String(chars)

                    v + ".... "

                else 
                    numbers

        static member Number(pageNumber: int) =
            AtLeastOneSet.Create [pageNumber]
            |> PageSelector.Numbers

    type PdfDocument with

        member pdfDocument.GetPageNumber(pageSelectorExpr: SinglePageSelectorExpr) =
            let totalPageNum = pdfDocument.GetNumberOfPages()
            
            match pageSelectorExpr with
            | SinglePageSelectorExpr.Begin (i) -> i
            | SinglePageSelectorExpr.End (i) -> totalPageNum - i + 1


        member pdfDocument.GetPageNumbers(pageSelectorExpr: PageSelectorExpr) =

            match pageSelectorExpr with 
            | PageSelectorExpr.SinglePage singlePage -> [pdfDocument.GetPageNumber singlePage]

            | PageSelectorExpr.Between (beginExpr, endExpr) ->
                [pdfDocument.GetPageNumber beginExpr .. pdfDocument.GetPageNumber endExpr]

            | PageSelectorExpr.Compose compose ->
                compose
                |> List.collect (pdfDocument.GetPageNumbers)
                |> List.distinct

        member pdfDocument.GetPages() =
            [
                for i = 1 to pdfDocument.GetNumberOfPages() do
                    pdfDocument.GetPage(i)
            ]

        member pdfDocument.GetPages(pageSelectorExpr: PageSelectorExpr) =
            let pages = pdfDocument.GetPages()
            let numbers = pdfDocument.GetPageNumbers(pageSelectorExpr)
            pages 
            |> List.indexed
            |> List.filter(fun (i, page) ->
                List.contains (i+1) (numbers)
            )
            |> List.map snd

        member pdfDocument.GetPageNumbers(pageSelector) =
            let numberOfPages = pdfDocument.GetNumberOfPages()
            match pageSelector with 
            | PageSelector.First -> [1]
            | PageSelector.Last -> [numberOfPages]
            | PageSelector.Expr expr -> 
                pdfDocument.GetPageNumbers(expr)
            | PageSelector.All -> [1..numberOfPages]
            | PageSelector.Numbers numbers -> 
                let intersectedNumbers =
                    Set.intersect
                        numbers.Value
                        (Set.ofList [1..numberOfPages])
                    |> Set.toList

                intersectedNumbers