namespace Shrimp.Pdf
open iText.Kernel.Pdf
open iText.Kernel.Geom
open Shrimp.Pdf.Extensions



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
           

[<RequireQualifiedAccess>]
module Rectangle = 
    let applyMargin (margin:Margin) (rect: Rectangle) =
        let left = margin.Left
        let top = margin.Top
        let right = margin.Right
        let bottom = margin.Bottom
        let x = rect.GetXF() - left
        let y = rect.GetYF() - bottom
        let width = rect.GetWidthF() + left + right 
        let height = rect.GetHeightF() + top + bottom
        Rectangle.create x y width height

[<RequireQualifiedAccess>]
type Rotation =
    | None = 0
    | Clockwise = 1
    | Counterclockwise = 2
    | R180 = 3

[<RequireQualifiedAccess>]
module Rotation =
    let getDegree = function
        | Rotation.Clockwise  -> 90.
        | Rotation.Counterclockwise -> -90.
        | Rotation.R180 -> 180.
        | Rotation.None -> 0.
        | _ -> failwith "invalid token"


type FsSize =
    { Width: float 
      Height: float }

[<RequireQualifiedAccess>]
module FsSize =
    let create width height =
        { Width = width
          Height = height }


    let ofRectangle (rect: Rectangle) =
        { Width = rect.GetWidthF() 
          Height = rect.GetHeightF() }

type FsPageSize = FsPageSize of FsSize
with 
    member x.Size = 
        let (FsPageSize size) = x
        size

    member x.Width = 
        let (FsPageSize size) = x
        size.Width

    member x.Height = x.Size.Height

[<RequireQualifiedAccess>]
module FsPageSize =
    let create width height = FsPageSize (FsSize.create width height)


    let landscape (size: FsPageSize) =
        { Width = max size.Width size.Height
          Height = min size.Width size.Height }

    let portrait (size: FsPageSize) =
        { Width = min size.Width size.Height
          Height = max size.Width size.Height }

    let rotate (rotation: Rotation) size = 
        match rotation with 
        | Rotation.None
        | Rotation.R180 -> size
        | Rotation.Clockwise 
        | Rotation.Counterclockwise ->
            { Height = size.Width
              Width = size.Height }
        | _ -> failwith "Invalid token"
        
    let clockwise size = 
        rotate Rotation.Clockwise size

    let ofPageSize (pageSize: PageSize) =
        let width = pageSize.GetWidthF()
        let height = pageSize.GetHeightF()
        create width height

    let toPageSize (fsPageSize: FsPageSize) =
        new PageSize(float32 fsPageSize.Width,float32 fsPageSize.Height)



    let A0 = ofPageSize PageSize.A0

    let A1 = ofPageSize PageSize.A1

    let A2 = ofPageSize PageSize.A2

    let A3 = ofPageSize PageSize.A3

    let A4 = ofPageSize PageSize.A4

[<RequireQualifiedAccess>]
module PageSize =
    let ofFsPageSize (fsPageSize: FsPageSize) =
        FsPageSize.toPageSize fsPageSize


type ContentResizeMode =
    | CropOrAdd = 0
    | Anamorphic = 1
    | Uniform = 2

[<RequireQualifiedAccess>]
module PdfPage =
    let increaseHeight 
        (origin: Position)
        (pageboxKind: PageBoxKind)
        (contentResizeMode: ContentResizeMode)
        (length: float)
        (page: PdfPage) =

        let newRect = 
            page 
            |> PdfPage.getPageBox pageboxKind
            |> Rectangle.increaseHeight origin length
            
        match contentResizeMode with 
        | ContentResizeMode.CropOrAdd ->
            PdfPage.setPageBox PageBoxKind.AllBox newRect page 
        | _ -> failwith "not implemented"



type SplitDocument private (reader: string, writer: string) =
    let reader = new PdfDocument(new PdfReader(reader))

    let writer = new PdfDocument(new PdfWriter(writer))

    member x.Reader = reader

    member x.Writer = writer
        
    static member Create(reader, writer) = new SplitDocument(reader, writer)


             

