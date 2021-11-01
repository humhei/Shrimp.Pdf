namespace Shrimp.Pdf

open Newtonsoft.Json

#nowarn "0104"
open iText.Kernel.Pdf
open iText.Kernel.Geom
open Shrimp.Pdf.Extensions
open System.IO
open Shrimp.FSharp.Plus

type PageOrientation =
    | Landscape  = 0
    | Portrait = 1

type FsSize =
    { Width: float 
      Height: float }
with 
    member internal x.MMValues =
        { Width = userUnitToMM x.Width 
          Height = userUnitToMM x.Height}

    member internal x.Round() =
        { Width = x.Width
          Height = x.Height }

type RoundedSize = private RoundedSize of FsSize
with 
    member x.Value =
        let (RoundedSize v) = x
        v

    member x.Width = x.Value.Width

    member x.Height = x.Value.Height

    member internal x.MMValues = x.Value.MMValues

    static member Create(size: FsSize) =
        size.Round()
        |> RoundedSize


[<RequireQualifiedAccess>]
module FsSize =
    let (|Portrait|Landscape|Uniform|) (fsSize: FsSize) =
        let width = fsSize.Width
        let height = fsSize.Height
        if width > height 
        then Landscape
        elif width = height then Uniform
        else Portrait


    let create width height =
        { Width = width
          Height = height }


    let ofRectangle (rect: Rectangle) =
        { Width = rect.GetWidthF() 
          Height = rect.GetHeightF() }

    let landscape (size: FsSize) =
        if size.Width >= size.Height then size
        else
            { Width = max size.Width size.Height
              Height = min size.Width size.Height }

    let portrait (size: FsSize) =
        if size.Height >= size.Width then size
        else
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

    let rotateTo (pageOrientation: PageOrientation) size =
        match pageOrientation with
        | PageOrientation.Landscape -> landscape size
        | PageOrientation.Portrait -> portrait size

    let clockwise size = 
        rotate Rotation.Clockwise size

    let ofPageSize (pageSize: PageSize) =
        let width = pageSize.GetWidthF()
        let height = pageSize.GetHeightF()
        create width height

    let toPageSize (size: FsSize) =
        new PageSize(float32 size.Width,float32 size.Height)

    let applyMargin margin (size: FsSize) =
        toPageSize size
        |> Rectangle.applyMargin margin
        |> ofRectangle


    let A0 = ofPageSize PageSize.A0

    let A1 = ofPageSize PageSize.A1

    let A2 = ofPageSize PageSize.A2

    let A3 = ofPageSize PageSize.A3

    let A4 = ofPageSize PageSize.A4

    let MAXIMUN = { Width = mm 5080.; Height = mm 5080. }

type FsSize with    
    member private x.OppositeDirection(targetSize: FsSize) =
        match targetSize with 
        | FsSize.Portrait _ -> FsSize.portrait x
        | FsSize.Landscape _ -> FsSize.landscape x
        | FsSize.Uniform _ -> x

    member private x.OppositeDirection(targetSize: iText.Kernel.Geom.Rectangle) =
        match FsSize.ofRectangle targetSize with 
        | FsSize.Portrait _ -> FsSize.portrait x
        | FsSize.Landscape _ -> FsSize.landscape x
        | FsSize.Uniform _ -> x

    member x.AlignDirection(targetSize: FsSize) = x.OppositeDirection(targetSize)

    member x.AlignDirection(targetSize: iText.Kernel.Geom.Rectangle) = x.OppositeDirection(targetSize)

type FsPageSize [<JsonConstructor>] (size: FsSize, pageOrientation) =
    inherit POCOBase<FsSize * PageOrientation>(size, pageOrientation)
    let size = FsSize.rotateTo pageOrientation size
    
    [<JsonProperty>]
    member x.PageOrientation = pageOrientation

    [<JsonProperty>]
    member x.Size = size

    member x.Width = size.Width

    member x.Height = size.Height

    override x.Equals(y) =
        match y with 
        | :? FsPageSize as pageSize -> pageSize.Size = x.Size
        | _ -> failwithf "Cannot compare different types"

    override x.GetHashCode() =
        x.Size.GetHashCode()

    interface System.IComparable with 
        member x.CompareTo(y) =
            match y with 
            | :? FsPageSize as pageSize -> (x.Size :> System.IComparable).CompareTo(pageSize.Size)
            | _ -> failwithf "Cannot compare different types"


[<RequireQualifiedAccess>]
module FsPageSize =
    let create (size: FsSize) pageOrientation =
        match pageOrientation with 
        | PageOrientation.Landscape -> 
            FsPageSize (FsSize.landscape size, pageOrientation)
        | PageOrientation.Portrait ->
            FsPageSize (FsSize.portrait size, pageOrientation)
        | _ -> failwith "Invalid token"

type ReaderDocument (reader: string) =
    let reader = new PdfDocument(new PdfReader(reader))

    member x.Reader = reader


type SplitDocument internal (reader: string, writer: string) =
    let mutable readerDocument: PdfDocument option = None

    let mutable writerDocument: PdfDocumentWithCachedResources option = None

    let mutable isOpend = false

    member x.ReaderPath = reader

    member x.WriterPath = writer

    member x.Reader = 
        match readerDocument with 
        | Some reader -> reader
        | None -> failwith "document is not open yet please option it first"

    member x.Writer = 
        match writerDocument with 
        | Some writer -> writer
        | None -> failwith "document is not open yet please option it first"

    member internal x.Open() =
        if not isOpend 
        then
            match readerDocument with 
            | Some readerDocument1 ->
                if readerDocument1.IsClosed() 
                then
                    readerDocument <- Some (new PdfDocument(new PdfReader(reader)))
                else 
                    failwith "Old document is not closed yet"

            | None -> readerDocument <- Some (new PdfDocument(new PdfReader(reader)))

            match writerDocument with 
            | Some writerDocument1 ->
                if writerDocument1.IsClosed() 
                then
                    writerDocument <- Some (new PdfDocumentWithCachedResources(writer))
                else 
                    failwith "Old document is not closed yet"
            | None ->
                writerDocument <- Some(new PdfDocumentWithCachedResources(writer))

        isOpend <- true

    member internal x.CloseAndDraft() =
        x.Reader.Close()
        x.Writer.Close()

        File.Delete(reader)
        File.Move(writer, reader)
        isOpend <- false

    static member Create(reader, writer) = new SplitDocument(reader, writer)


             
[<AutoOpen>]
module _Types_Ex =
    type PdfPage with 
        member x.GetPageEdge(innerBox: FsSize, pageBoxKind: PageBoxKind) =
            let pageBox = x.GetPageBox(pageBoxKind)

            let margin = 
                let width = pageBox.GetWidthF() - innerBox.Width
                let height = pageBox.GetHeightF() - innerBox.Height

                let width_div_2 = -width / 2.
                let height_div_2 = -height / 2.

                Margin.Create(width_div_2, height_div_2, width_div_2, height_div_2)

            x.GetPageEdge(Rectangle.applyMargin margin pageBox, pageBoxKind)
