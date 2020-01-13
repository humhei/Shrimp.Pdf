namespace Shrimp.Pdf
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas.Parser.Data


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
    { DashArray: float []
      Phase: float }

with 
    member x.DashArrayF32 =
        x.DashArray
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
module Rectangle =
    let (|Portrait|Landscape|Uniform|) (rect: Rectangle) =
        let width = rect.GetWidth()
        let height = rect.GetHeight()
        if width > height 
        then Portrait
        elif width = height then Uniform
        else Landscape