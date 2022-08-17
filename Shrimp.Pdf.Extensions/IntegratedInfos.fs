namespace Shrimp.Pdf

open Shrimp.Pdf.Extensions
open System.Collections.Generic

#nowarn "0104"
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas.Parser.Data
open FParsec
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Font
open Shrimp.FSharp.Plus
open iText.IO.Image
open Shrimp.Pdf.Constants.Operators



[<AutoOpen>]
module _OperatorRangeExtensions =
    
    [<RequireQualifiedAccess>]
    module PdfCanvas =
        let writeOperatorRange (operatorRange: OperatorRange) (pdfCanvas: PdfCanvas) =
            let outputStream = pdfCanvas.GetContentStream().GetOutputStream()
            let operands = operatorRange.Operands
            for i = 0 to operands.Count - 1 do
                let operand = operands.[i]
                if i = operands.Count - 1 then 
                    outputStream.Write(operand).WriteNewLine()
                    |> ignore
                else 
                    outputStream.Write(operand).WriteSpace()
                    |> ignore
    
            pdfCanvas

[<AutoOpen>]
module IntegratedInfos =

    type PathInfoRecord =
        { FillColor: iText.Kernel.Colors.Color 
          StrokeColor: iText.Kernel.Colors.Color 
          Bound: FsRectangle }



    [<Struct>]
    type IntegratedPathRenderInfo =
        { PathRenderInfo: PathRenderInfo 
          ClippingPathInfos: ClippingPathInfos
          AccumulatedPathOperatorRanges: seq<OperatorRange> }
    with 
        member x.IsShading = 
            match x.PathRenderInfo with 
            | :? PdfShadingPathRenderInfo -> true
            | _ -> false

        member x.IsClippingPath = x.PathRenderInfo.IsPathModifiesClippingPath()

        member integratedInfo.RecordValue =
            let renderInfo = integratedInfo.PathRenderInfo
            { FillColor = renderInfo.GetFillColor()
              StrokeColor = renderInfo.GetStrokeColor()
              Bound = 
                let bound = (IPathRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth integratedInfo)
                bound.FsRectangle()
              }


        interface IPathRenderInfo with 
            member x.Value = x.PathRenderInfo

        interface IAbstractRenderInfo with 
            member x.Value = x.PathRenderInfo :> AbstractRenderInfo

        interface IAbstractRenderInfoIM with 
            member x.Value = x.PathRenderInfo :> AbstractRenderInfo

        interface IIntegratedRenderInfoIM with 
            member x.TagIM = IntegratedRenderInfoTagIM.Path
            member x.ClippingPathInfos = x.ClippingPathInfos

        interface IIntegratedRenderInfo with 
            member x.Tag = IntegratedRenderInfoTag.Path
            member x.ClippingPathInfos = x.ClippingPathInfos

 

    type TextInfoRecord =
        { Text: string 
          FontSize: float 
          FillColor: iText.Kernel.Colors.Color 
          StrokeColor: iText.Kernel.Colors.Color 
          FontName: DocumentFontName
          Bound: FsRectangle
          DenseBound: FsRectangle
          EndTextState: EndTextState }
            

    [<Struct; System.Diagnostics.DebuggerDisplay("IntegratedTextRenderInfo: {RecordValue}")>]
    type IntegratedTextRenderInfo =
        { TextRenderInfo: TextRenderInfo 
          ClippingPathInfos: ClippingPathInfos
          EndTextState: EndTextState
          ConcatedTextInfos: IntegratedTextRenderInfo seq
          OperatorRange: OperatorRange option }

    with 
        member integratedInfo.Text() =
            let renderInfo = integratedInfo.TextRenderInfo
            match integratedInfo.EndTextState with 
            | EndTextState.Yes -> 
                integratedInfo.ConcatedTextInfos
                |> Seq.map(fun m -> m.TextRenderInfo.GetText())
                |> String.concat ""
            | EndTextState.No
            | EndTextState.Undified -> renderInfo.GetText()


        
        member x.IsShow_EndTextState = 
            match x.EndTextState with 
            | EndTextState.Undified 
            | EndTextState.Yes -> true
            | EndTextState.No -> false

        member integratedInfo.RecordValue =
            let renderInfo = integratedInfo.TextRenderInfo
            { Text = integratedInfo.Text()
              FontSize = ITextRenderInfo.getActualFontSize integratedInfo
              FontName = ITextRenderInfo.getFontName integratedInfo
              FillColor = renderInfo.GetFillColor()
              StrokeColor = renderInfo.GetStrokeColor()
              Bound = 
                let bound = ITextRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth integratedInfo
                bound.FsRectangle()
              DenseBound =
                let bound = ITextRenderInfo.getDenseBound BoundGettingStrokeOptions.WithoutStrokeWidth integratedInfo
                bound.FsRectangle()
              EndTextState = integratedInfo.EndTextState

            }

        interface IAbstractRenderInfoIM with 
            member x.Value = x.TextRenderInfo :> AbstractRenderInfo

        interface ITextRenderInfo with 
            member x.Value = x.TextRenderInfo

            member x.EndTextState = x.EndTextState

            member x.ConcatedTextInfos =
                x.ConcatedTextInfos
                |> Seq.map(fun m -> m.TextRenderInfo)


        interface IAbstractRenderInfo with 
            member x.Value = x.TextRenderInfo :> AbstractRenderInfo

        interface IIntegratedRenderInfoIM with 
            member x.TagIM = IntegratedRenderInfoTagIM.Text
            member x.ClippingPathInfos = x.ClippingPathInfos
            
        interface IIntegratedRenderInfo with 
            member x.Tag = IntegratedRenderInfoTag.Text
            member x.ClippingPathInfos = x.ClippingPathInfos




    type IndexableColorSpace =
        { ColorSpace: ColorSpace 
          IndexTable: option<byte []>
          Decode: PdfArray option }

    [<RequireQualifiedAccess>]
    type ImageColorSpaceData =
        | ImageMask 
        | Indexable of IndexableColorSpace

    type ImageRenderInfoRecord =
        { UnclippedBound: FsRectangle
          ImageColorSpaceData: ImageColorSpaceData
          VisibleBound: FsRectangle option }

    []
    type FsImageData =
        | ImageData of ImageData
        | IndexedRgb


    [<Struct>]
    type IntegratedImageRenderInfo =
        { ImageRenderInfo: ImageRenderInfo 
          ClippingPathInfos: ClippingPathInfos
          LazyImageData: Lazy<FsImageData>
          LazyColorSpace: Lazy<ImageColorSpaceData option> 
        }

    with 
        member x.ImageData = x.LazyImageData.Value

        member x.ImageColorSpaceData = 
            match x.LazyColorSpace.Value with 
            | None -> 
                let colorSpace = 
                    match x.ImageData.GetColorEncodingComponentsNumber() with 
                    | 1 -> ColorSpace.Gray
                    | 4 -> ColorSpace.Cmyk 
                    | 3 -> ColorSpace.Rgb 
                    | number -> failwithf "Cannot determain color space from ComponentsNumber %d" number
                
                { ColorSpace = colorSpace 
                  IndexTable = None
                  Decode = None }
                |> ImageColorSpaceData.Indexable

            | Some imageColorSpaceData -> imageColorSpaceData

        member x.VisibleBound() = 
            let unclippedBound = IImageRenderInfo.getUnclippedBound x
            match x.ClippingPathInfos with 
            | ClippingPathInfos.IntersectedNone -> None
            | ClippingPathInfos.IntersectedSome rect ->
                Rectangle.tryGetIntersection unclippedBound rect
            | ClippingPathInfos.NonExists ->
                unclippedBound
                |> Some


        member x.UnclippedBound() =
            IImageRenderInfo.getUnclippedBound x

        member x.Dpi =
            let bound =  IImageRenderInfo.getUnclippedBound x
            let width = x.ImageData.GetWidth()
            let height = x.ImageData.GetHeight()
            let dpi_x = float width / userUnitToMM (bound.GetWidthF())     |> inchToMM |> round |> int
            let dpi_y = float height / userUnitToMM (bound.GetHeightF())  |> inchToMM |> round |> int
            {| X = dpi_x
               Y = dpi_y |}

        member x.RecordValue =
            { UnclippedBound = IImageRenderInfo.getUnclippedBound x |> FsRectangle.OfRectangle
              ImageColorSpaceData = x.ImageColorSpaceData
              VisibleBound = 
                x.VisibleBound() 
                |> Option.map FsRectangle.OfRectangle 
            }

        interface IAbstractRenderInfoIM with 
            member x.Value = x.ImageRenderInfo :> AbstractRenderInfo

        interface IImageRenderInfo with 
            member x.Value = x.ImageRenderInfo

        interface IIntegratedRenderInfoIM with 
            member x.TagIM = IntegratedRenderInfoTagIM.Image
            member x.ClippingPathInfos = x.ClippingPathInfos
            




    [<RequireQualifiedAccess>]
    type IntegratedRenderInfo =
        | Text of IntegratedTextRenderInfo
        | Path of IntegratedPathRenderInfo

    with 
        member x.ClippingPathInfos =
            match x with 
            | IntegratedRenderInfo.Text info -> info.ClippingPathInfos
            | IntegratedRenderInfo.Path info -> info.ClippingPathInfos

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

    [<RequireQualifiedAccess>]
    module IIntegratedRenderInfoIM =

        let (|Text|Path|Image|) (info: IIntegratedRenderInfoIM) = 
            match info.TagIM with 
            | IntegratedRenderInfoTagIM.Path -> Path (info :?> IntegratedPathRenderInfo)
            | IntegratedRenderInfoTagIM.Text -> Text (info :?> IntegratedTextRenderInfo)
            | IntegratedRenderInfoTagIM.Image -> Image (info :?> IntegratedImageRenderInfo)
            | _ -> failwith "Invalid token"

        let (|Vector|Pixel|) (info: IIntegratedRenderInfoIM) = 
            match info.TagIM with 
            | IntegratedRenderInfoTagIM.Path -> Vector ((info :?> IntegratedPathRenderInfo) :> IIntegratedRenderInfo)
            | IntegratedRenderInfoTagIM.Text -> Vector ((info :?> IntegratedTextRenderInfo) :> IIntegratedRenderInfo)
            | IntegratedRenderInfoTagIM.Image -> Pixel (info :?> IntegratedImageRenderInfo)
            | _ -> failwith "Invalid token"

        let asIPathRenderInfo (info: IIntegratedRenderInfoIM) = 
            match info with
            | Path info -> Some (info)
            | _ -> None 

        let asITextRenderInfo (info: IIntegratedRenderInfoIM) =
            match info with
            | Text info -> Some (info)
            | _ -> None 

        let asVector (info: IIntegratedRenderInfoIM) =
            match info with
            | Vector info -> Some (info)
            | Pixel _ -> None 

        let asIImageRenderInfo (info: IIntegratedRenderInfoIM) = 
            match info with
            | Image info -> Some (info)
            | _ -> None 

        let getDenseBound boundGettingStrokeOptions info = 
            match info with 
            | Vector vector -> IAbstractRenderInfo.getDenseBound boundGettingStrokeOptions vector  |> Some
            | Pixel info -> info.VisibleBound()

        let getBound boundGettingStrokeOptions info = 
            match info with 
            | Vector vector -> IAbstractRenderInfo.getBound boundGettingStrokeOptions vector |> Some
            | Pixel info -> info.VisibleBound()


        let isVisible (info: IIntegratedRenderInfoIM) =
            match info with 
            | Vector vector -> IIntegratedRenderInfo.isVisible vector
            | Pixel image -> 
                match image.VisibleBound() with 
                | Some bound -> true
                | None -> false


namespace Shrimp.Pdf.Extensions

[<AutoOpen>]
module _IntegratedInfosExtensions =
    open Shrimp.Pdf


    [<RequireQualifiedAccess>]
    module ITextRenderInfo =
        let getText (info: ITextRenderInfo) =
            (info :?> IntegratedTextRenderInfo).Text()