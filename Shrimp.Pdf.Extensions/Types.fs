﻿namespace Shrimp.Pdf
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas.Parser.Data
open FParsec
open iText.Kernel.Pdf
open Shrimp.FSharp.Plus


[<AutoOpen>]
module ExtensionTypes =

    type BoundGettingStrokeOptions =
        | WithStrokeWidth = 0
        | WithoutStrokeWidth = 1


    type FillOrStrokeOptions =
        | Stroke = 0
        | Fill = 1
        | FillOrStroke = 2
        | FillAndStroke = 3

    type PageNumber = PageNumber of int
    with 
        member x.Value =
            let (PageNumber value) = x
            value 

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

    [<RequireQualifiedAccess>]
    module Margin =
        let getValues (margin: Margin) =
            [ margin.Left; margin.Top; margin.Right; margin.Bottom]

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
            | SinglePageSelectorExpr.End i -> i.ToString() + "R"

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
            | PageSelector.Numbers numbers -> numbers.ToString()



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