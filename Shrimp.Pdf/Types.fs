namespace Shrimp.Pdf

open Newtonsoft.Json
#nowarn "0104"
open iText.Kernel.Pdf
open iText.Kernel.Geom
open Shrimp.Pdf.Extensions
open System.Collections.Generic
open System.IO
open Shrimp.FSharp.Plus
open Shrimp.FSharp.Plus.Text
open Shrimp.Pdf.Colors

type PdfModifyOptions = Shrimp.Pdf.Parser.PdfModifyOptions

[<AutoOpen>]
module _IAbstractRenderInfoExtensions =
    type IAbstractRenderInfo with 
        static member FsColorIs(fillOrStrokeOptions, fsColor: FsColor) =
            fun (info: #IAbstractRenderInfo) ->
                IAbstractRenderInfo.ColorIs(fillOrStrokeOptions, fun color ->
                    FsColor.OfItextColor color
                    |> FsColor.equal fsColor
                ) info

    type PathInfoRecord =
        { FillColor: FsColor
          StrokeColor: FsColor
          Bound: FsRectangle }
    
    type TextInfoRecord =
        { PdfConcatedWord: PdfConcatedWord
          FontSize: float 
          TextRotation: Rotation
          FillColor: FsColor
          StrokeColor: FsColor
          FontName: DocumentFontName
          Bound: FsRectangle
          DenseBound: FsRectangle
          EndTextState: EndTextState
          TextRenderingMode: FsTextRenderMode
          IsShading: bool
        }
    with 
        member x.Text = x.PdfConcatedWord.ConcatedText()
    
    type ImageRenderInfoRecord =
        { UnclippedBound: FsRectangle
          ImageColorSpaceData: ImageColorSpaceData
          VisibleBound: FsRectangle option }


    type IntegratedPathRenderInfo with 
        member integratedInfo.RecordValue =
            let renderInfo = integratedInfo.PathRenderInfo
            { FillColor = renderInfo.GetFillColor()     |> FsColor.OfItextColor
              StrokeColor = renderInfo.GetStrokeColor() |> FsColor.OfItextColor
              Bound = 
                let bound = (IPathRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth integratedInfo)
                bound.FsRectangle()

              }

    type IntegratedTextRenderInfo with 
        member integratedInfo.RecordValue =
            let renderInfo = integratedInfo.TextRenderInfo
            { PdfConcatedWord = integratedInfo.PdfConcatedWord()
              FontSize = ITextRenderInfo.getActualFontSize integratedInfo
              FontName = ITextRenderInfo.getFontName integratedInfo
              TextRotation = ITextRenderInfo.getTextRotation integratedInfo
              FillColor = renderInfo.GetFillColor()      |> FsColor.OfItextColor
              StrokeColor = renderInfo.GetStrokeColor()  |> FsColor.OfItextColor
              Bound = 
                let bound = ITextRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth integratedInfo
                bound.FsRectangle()
              IsShading = integratedInfo.IsShading
              DenseBound =
                let bound = ITextRenderInfo.getDenseBound BoundGettingStrokeOptions.WithoutStrokeWidth integratedInfo
                bound.FsRectangle()
              EndTextState = integratedInfo.EndTextState
              TextRenderingMode = integratedInfo.TextRenderMode
            }


    type IntegratedImageRenderInfo with 
        member x.RecordValue =
            { UnclippedBound = IImageRenderInfo.getUnclippedBound x |> FsRectangle.OfRectangle
              ImageColorSpaceData = x.ImageColorSpaceData
              VisibleBound = 
                x.VisibleBound() 
                |> Option.map FsRectangle.OfRectangle 
            }

type PageOrientation =
    | Landscape  = 0
    | Portrait = 1





type FsSize =
    { Width: float 
      Height: float }
with 
    member internal x.MMValues =
        {| Width = userUnitToMM x.Width 
           Height = userUnitToMM x.Height |}

    member internal x.Round() =
        { Width = x.Width
          Height = x.Height }

    member x.MapValue mapping =
        { Width = mapping x.Width
          Height = mapping x.Height }

    member x.ScaleInto(targetSize: FsSize) =
        let scaleX = targetSize.Width / x.Width
        let scaleY = targetSize.Height / x.Height
        let scale = min scaleX scaleY
        x.MapValue(fun m -> m * scale)

    member x.MMText =
        let x = x.MMValues
        sprintf "%.1fx%.1f" x.Width x.Height

    member x.MultipleValue = x.Width * x.Height

        

[<AutoOpen>]
module _FsSizeExtensions =
    type Rectangle with 
        member x.GetArea(position, size) =
            x.GetArea(position, size.Width, size.Height)



//[<CustomEquality; NoComparison>]
//type RoundedSize = private RoundedSize of FsSize
//with 
//    member x.Value =
//        let (RoundedSize v) = x
//        v

//    member x.Width = x.Value.Width

//    member x.Height = x.Value.Height

//    member internal x.MMValues = x.Value.MMValues

//    static member Create(size: FsSize) =
//        size.Round()
//        |> RoundedSize


//    override x.GetHashCode() = x.Value.GetHashCode()

//    override x.Equals(y) =
//        match y with 
//        | :? RoundedSize as y -> x.Width @= y.Width && x.Height @= y.Height
//        | _ -> false


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

    let ofFsRectangle (rect: FsRectangle) =
        { Width = rect.Width
          Height = rect.Height }

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

    let mapValue (fValue) (size: FsSize) =
        { Width = fValue size.Width
          Height = fValue size.Height }

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
        ofRectangle pageSize

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

    let A4 = { Width = mm 210.; Height = mm 297. }

    /// { Width = mm 148; Height = mm 210. }
    let A5_INT = { Width = mm 148; Height = mm 210. }


    let MAXIMUN = 
        { Width = mm Shrimp.Pdf.Constants.MAXIMUM_MM_WIDTH
          Height = mm Shrimp.Pdf.Constants.MAXIMUM_MM_WIDTH }

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

    member x.Orientation =
        match x with 
        | FsSize.Landscape -> PageOrientation.Landscape
        | FsSize.Uniform -> PageOrientation.Landscape
        | FsSize.Portrait -> PageOrientation.Portrait

    /// default alignDirection is true
    member x.IsInsideOf(targetSize: FsSize, ?alignDirection) =
        let x = 
            match defaultArg alignDirection true with 
            | true -> x.AlignDirection targetSize
            | false -> x
        x.Width <= targetSize.Width
        && x.Height <= targetSize.Height


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

type PageOrientationChecker =
    { PageOrientation: PageOrientation
      CellSizes: FsSize list
      Margin: Margin
      BackgroundSize: FsSize }

[<RequireQualifiedAccess>]
module PageOrientationChecker =
    let (|ValidOrientation|InvalidOrientation|) (checker: PageOrientationChecker) =
        let orientation = checker.PageOrientation
        let backgroundSize = checker.BackgroundSize
        let cellSizes = checker.CellSizes
        let margin = checker.Margin

        let backgroundSize = 
            match orientation with 
            | PageOrientation.Landscape -> FsSize.landscape backgroundSize
            | PageOrientation.Portrait -> FsSize.portrait backgroundSize

    
        let exceedHorizontal =
            cellSizes 
            |> List.tryFind(fun cellSize ->
                cellSize.Width + margin.Left + margin.Right >= backgroundSize.Width + Constants.tolerance
            )

        let exceedVertical =
            cellSizes 
            |> List.tryFind(fun cellSize ->
                cellSize.Height + margin.Top + margin.Bottom >= backgroundSize.Height + Constants.tolerance
            )

        match exceedHorizontal, exceedVertical with 
        | None _, None _ -> ValidOrientation
        | Some v, _ 
        | _, Some v -> InvalidOrientation v



type SplitDocument internal (reader: string, writer: string) =
    let mutable readerDocument: Lazy<ReaderDocument> option = None

    let mutable writerDocument: Lazy<PdfDocumentWithCachedResources> option = None

    let mutable isOpened = false

    member x.ReaderPath = reader

    member x.WriterPath = writer

    member private x.LazyReader = 
        match readerDocument with 
        | Some reader -> reader
        | None -> failwith "document is not open yet please open it first"

    member x.Reader = x.LazyReader.Value.Reader
        
    member private x.LazyWriter = 
        match writerDocument with 
        | Some writer -> writer
        | None -> failwith "document is not open yet please open it first"

    member x.Writer = x.LazyWriter.Value 
     
    member internal splitDocument.CopyOCGS() =
        let __copyOCG = 
            let fromOcProps = 
                splitDocument.Reader.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.OCProperties)

            match fromOcProps with 
            | null -> ()
            | fromOcProps ->
                match fromOcProps.GetAsDictionary(PdfName.D) with 
                | null -> ()
                | from_d ->

                    let toVisitedObjects = HashSet()

                    let toDocLayers = HashSet()

                    for page in splitDocument.Writer.GetPages() do
                        let rec loop (resources: PdfDictionary) =
                            match toVisitedObjects.Contains resources with 
                            | false ->
                                let props = resources.GetAsDictionary(PdfName.Properties)
                                match props with 
                                | null -> ()
                                | props ->
                                    for prop in props.EntrySet() do
                                        match prop.Value with 
                                        | :? PdfDictionary as prop ->
                                            match toDocLayers.Contains(prop) with 
                                            | true -> ()
                                            | false ->
                                                match prop.Get(PdfName.Type) with 
                                                | :? PdfName as name -> 
                                                    match name.GetValue() with 
                                                    | "OCG" ->
                                                        toDocLayers.Add(prop)
                                                        |> ignore
                                                    | _ -> ()

                                                | _ -> ()
                                        | _ -> ()

                                let xobjects = resources.GetAsDictionary(PdfName.XObject)
                                match xobjects with 
                                | null -> ()
                                | _ ->
                                    for name in xobjects.KeySet() do 
                                        let xobject = xobjects.GetAsStream(name)
                                        match toVisitedObjects.Contains xobject with 
                                        | true -> ()
                                        | false -> 
                                            toVisitedObjects.Add(xobject) |> ignore
                                            let resources = xobject.GetAsDictionary(PdfName.Resources)
                                            match resources with 
                                            | null -> ()
                                            | resources -> loop resources


                                toVisitedObjects.Add(resources) |> ignore

                            | true -> ()


                        loop (page.GetResources().GetPdfObject())


                    let toOCProps  =
                        let dict = 
                            splitDocument.Writer.GetCatalog().GetPdfObject()

                        let ocProps = new PdfDictionary()
                        dict.Put(PdfName.OCProperties, ocProps) |> ignore
                        ocProps

                    let __fillToOCProps = 
                    
                        toOCProps.Put(PdfName.D, new PdfDictionary()) |> ignore
                        toOCProps.Put(PdfName.OCGs, new PdfArray()) |> ignore

                    let toD = toOCProps.GetAsDictionary(PdfName.D)
                    let toOCGS = toOCProps.GetAsArray(PdfName.OCGs)

                    let toDocLayers =
                        toDocLayers
                        |> List.ofSeq
                        |> List.map(fun docLayer ->
                            let layerName = docLayer.GetAsString(PdfName.Name).GetValue()
                            layerName, docLayer
                        )

                    let __fillD = 

                        for from_dProp in from_d.EntrySet() do    
                            match from_dProp.Value with 
                            | :? PdfArray as array ->
                                let rec loopToDocs (array: PdfArray) =
                                    
                                    let toDocs = new PdfArray()
                                    for from_layer in array do 
                                        match from_layer with 
                                        | :? PdfDictionary as from_layer ->
                                            let from_layer = from_layer :?> PdfDictionary
                                            let from_LayerName = from_layer.GetAsString(PdfName.Name).GetValue()
                                            let toDocLayers =
                                                toDocLayers
                                                |> List.filter(fun (toName, _) ->
                                                    toName = from_LayerName
                                                )
                                                |> List.map snd
                                                |> List.map(fun m -> m :> PdfObject)
                                                |> List.toArray


                                            let array = PdfArray(toDocLayers)
                                            toDocs.AddAll(array)

                                        | :? PdfArray as from_array ->
                                            let to_array = loopToDocs from_array
                                            toDocs.Add(to_array)

                                        | :? PdfString -> toDocs.Add(from_layer)

                                        | _ -> failwithf "Not implemented from_layer is %A here" (from_layer.GetType())

                                    toDocs

                                let toDocs = loopToDocs array

                                toD.Put(from_dProp.Key, toDocs)
                                |> ignore

                            | _ -> ()


                    let __fillOCGS =
                        let toDocLayers = 
                            toDocLayers
                            |> List.map snd
                            |> List.map(fun m -> m :> PdfObject)
                            |> List.toArray

                        toOCGS.AddAll(toDocLayers)

                    ()
                
        ()

    member internal x.Open() =
        if not isOpened 
        then
            match readerDocument with 
            | Some readerDocument1 ->
                match readerDocument1 with 
                | Lazy.ValueCreated readerDocument1 ->
                    if readerDocument1.Reader.IsClosed() 
                    then
                        readerDocument <- Some (lazy new ReaderDocument(reader))
                    else 
                        failwith "Old document is not closed yet"

                | Lazy.NotCreated -> readerDocument <- Some (lazy new ReaderDocument(reader))

            | None -> readerDocument <- Some (lazy new ReaderDocument(reader))

            match writerDocument with 
            | Some writerDocument1 ->
                match writerDocument1 with 
                | Lazy.ValueCreated writerDocument1 ->
                    if writerDocument1.IsClosed() 
                    then
                        writerDocument <- Some (lazy new PdfDocumentWithCachedResources(writer))
                    else 
                        failwith "Old document is not closed yet"
                | _ -> writerDocument <- Some (lazy new PdfDocumentWithCachedResources(writer))

            | None ->
                writerDocument <- Some(lazy new PdfDocumentWithCachedResources(writer))

        isOpened <- true

    member internal x.CloseAndDraft() =
        match x.LazyReader, x.LazyWriter with 
        | Lazy.ValueCreated readerDocument, Lazy.ValueCreated writerDocument ->
            readerDocument.Reader.Close()
            writerDocument.CloseAndClearCache()
            File.Delete(reader)
            File.Move(writer, reader)

        | Lazy.ValueCreated readerDocument, Lazy.NotCreated ->
            readerDocument.Reader.Close()

        | Lazy.NotCreated, Lazy.ValueCreated writerDocument ->
            writerDocument.CloseAndClearCache()
            File.Delete(reader)
            File.Move(writer, reader)

        | Lazy.NotCreated, Lazy.NotCreated -> ()

        isOpened <- false

    member internal x.TryCloseAndDisposeWriter_IfOpened() =
        match isOpened with 
        | true ->   
            
            match x.LazyReader with 
            | Lazy.ValueCreated readerDocument -> readerDocument.Reader.Close()
            | Lazy.NotCreated _ -> ()

            match x.LazyWriter with 
            | Lazy.ValueCreated writerDocument ->
                
                match writerDocument.GetNumberOfPages() with 
                | 0 -> writerDocument.AddNewPage() |> ignore
                | _ -> ()

                writerDocument.CloseAndClearCache()
                File.Delete(writer)

            | Lazy.NotCreated _ -> ()

            isOpened <- false
        | false -> ()


    static member Create(reader, writer) = new SplitDocument(reader, writer)


             
[<AutoOpen>]
module _Types_Ex =
    type TextInfoRecord with
        member x.Is(fontName: string, fontSize: float, ?fillColor: FsColor) =
            
            let ifFontNameTheSame = x.FontName.SameFontNameTo(fontName)
    
            ifFontNameTheSame
            && fontSize @= x.FontSize
            &&
                match fillColor with 
                | Some fillColor -> FsColor.equal fillColor (x.FillColor)
                | None -> true
    
        member x.Pick(fontName, fontSize, picker, ?fillColor) =
            match x.Is(fontName, fontSize, ?fillColor = fillColor) with 
            | true -> picker x.Text
            | false -> None

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
