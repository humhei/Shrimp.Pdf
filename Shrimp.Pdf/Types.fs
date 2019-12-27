namespace Shrimp.Pdf
open iText.Kernel.Pdf
open iText.Kernel.Geom
open Shrimp.Pdf.Extensions
open System.IO


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



    let clockwise size = 
        rotate Rotation.Clockwise size

    let ofPageSize (pageSize: PageSize) =
        let width = pageSize.GetWidthF()
        let height = pageSize.GetHeightF()
        create width height

    let toPageSize (size: FsSize) =
        new PageSize(float32 size.Width,float32 size.Height)


    let A0 = ofPageSize PageSize.A0

    let A1 = ofPageSize PageSize.A1

    let A2 = ofPageSize PageSize.A2

    let A3 = ofPageSize PageSize.A3

    let A4 = ofPageSize PageSize.A4

    let MAXIMUN = { Width = mm 5080; Height = mm 5080 }

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
            PdfPage.setPageBox pageboxKind newRect page 
        | _ -> failwith "not implemented"


type ReaderDocument (reader: string) =
    let reader = new PdfDocument(new PdfReader(reader))

    member x.Reader = reader


type SplitDocument (reader: string, writer: string, pdfDocumentCache: PdfDocumentCache) =
    let mutable readerDocument = new PdfDocument(new PdfReader(reader))

    let mutable writerDocument = new PdfDocumentWithCachedResources(writer, pdfDocumentCache)

    member x.ReaderName = reader

    member x.Reader = readerDocument

    member x.Writer = writerDocument

    member x.ReOpen() =
        readerDocument.Close()
        writerDocument.Close()

        File.Delete(reader)
        File.Copy(writer, reader, true)

        readerDocument <- new PdfDocument(new PdfReader(reader))
        writerDocument <- new PdfDocumentWithCachedResources(writer, writerDocument)

    static member Create(reader, writer, pdfDocumentCache) = new SplitDocument(reader, writer, pdfDocumentCache)


             

