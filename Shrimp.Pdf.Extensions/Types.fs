namespace Shrimp.Pdf
#nowarn "0104"
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas.Parser.Data
open FParsec
open iText.Kernel.Pdf
open Shrimp.FSharp.Plus


[<AutoOpen>]
module ExtensionTypes =

    

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

    type IPathRenderInfo =
        inherit IAbstractRenderInfo
        abstract member Value: PathRenderInfo
            
    type ITextRenderInfo =
        inherit IAbstractRenderInfo
        abstract member Value: TextRenderInfo

    type IntegratedRenderInfoTag =
        | Path = 0
        | Text = 1

    type IIntegratedRenderInfo =
        inherit IAbstractRenderInfo
        abstract member Tag: IntegratedRenderInfoTag
        abstract member ClippingPathInfo: ClippingPathInfo option

    [<Struct>]
    type IntegratedPathRenderInfo =
        { PathRenderInfo: PathRenderInfo 
          ClippingPathInfo: ClippingPathInfo option }
    with 
        interface IPathRenderInfo with 
            member x.Value = x.PathRenderInfo

        interface IAbstractRenderInfo with 
            member x.Value = x.PathRenderInfo :> AbstractRenderInfo

        interface IIntegratedRenderInfo with 
            member x.Tag = IntegratedRenderInfoTag.Path
            member x.ClippingPathInfo = x.ClippingPathInfo

    [<Struct>]
    type IntegratedTextRenderInfo =
        { TextRenderInfo: TextRenderInfo 
          ClippingPathInfo: ClippingPathInfo option }

    with 
        interface ITextRenderInfo with 
            member x.Value = x.TextRenderInfo

        interface IAbstractRenderInfo with 
            member x.Value = x.TextRenderInfo :> AbstractRenderInfo

        interface IIntegratedRenderInfo with 
            member x.Tag = IntegratedRenderInfoTag.Text

            member x.ClippingPathInfo = x.ClippingPathInfo

    [<RequireQualifiedAccess>]
    type IntegratedRenderInfo =
        | Text of IntegratedTextRenderInfo
        | Path of IntegratedPathRenderInfo

    with 
        member x.ClippingPathInfo =
            match x with 
            | IntegratedRenderInfo.Text info -> info.ClippingPathInfo
            | IntegratedRenderInfo.Path info -> info.ClippingPathInfo

        member x.RenderInfo : AbstractRenderInfo =
            match x with 
            | IntegratedRenderInfo.Text info -> info.TextRenderInfo :> AbstractRenderInfo
            | IntegratedRenderInfo.Path info -> info.PathRenderInfo :> AbstractRenderInfo


    [<RequireQualifiedAccess>]
    module IIntegratedRenderInfo =

        let (|Text|Path|) (info: IIntegratedRenderInfo) = 
            match info.Tag with 
            | IntegratedRenderInfoTag.Path -> Path (info :?> IntegratedPathRenderInfo)
            | IntegratedRenderInfoTag.Text -> Text (info :?> IntegratedTextRenderInfo)
            | _ -> failwith "Invalid token"

        let asIPathRenderInfo (info: IIntegratedRenderInfo) = 
            match info with
            | Path info -> Some (info)
            | _ -> None 

        let asITextRenderInfo (info: IIntegratedRenderInfo) =
            match info with
            | Text info -> Some (info)
            | _ -> None 





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
            sprintf "Margin %.1f %.1f %.1f %.1f" x.Left x.Top x.Right x.Bottom


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
            | _ -> failwith "invalid token"

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
        | Specfic of Rectangle


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



    [<RequireQualifiedAccess>]
    type SinglePageSelectorExpr =
        | Begin of int
        | End of int

    with 
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
        let private parser() = 

            let pSinglePage = 
                let pBegin = 
                    pint32 |>> (fun i -> 
                        if i > 0 then  SinglePageSelectorExpr.Begin i
                        else failwithf "page num %d should be bigger than 0" i
                    )

                let pEnd = 
                    (pstringCI "R") >>. pint32 |>> (fun i ->
                        if i > 0 then  SinglePageSelectorExpr.End i
                        else failwithf "page num %d should be bigger than 0" i
                    )

                (pEnd)
                <|> (pBegin)

            let pBetween = 
                (pSinglePage .>>? pchar '-' .>>. pSinglePage )
                |>> PageSelectorExpr.Between

            sepBy1 ((pBetween <|> (pSinglePage |>> PageSelectorExpr.SinglePage)) .>> spaces) (pchar ',')

        let create (exprText: string) =
            match run (parser()) exprText with 
            | Success (result, _, _) -> 
                match result with 
                | [expr] -> expr
                | _ -> PageSelectorExpr.Compose result

            | Failure (errorMsg, _, _) -> failwithf "%s" errorMsg



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