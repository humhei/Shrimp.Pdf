namespace Shrimp.Pdf

open Shrimp.Pdf.Extensions
open System.Collections.Generic
open iText.IO.Source
open iText.Kernel.Pdf.Canvas.Parser

#nowarn "0104"
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas.Parser.Data
open FParsec
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Xobject
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Font
open Shrimp.FSharp.Plus
open Shrimp.FSharp.Plus.Operators
open iText.IO.Image
open Shrimp.Pdf.Constants.Operators

[<Struct>]
type InfoContainerID =
    | Page 
    | XObject of int * int

[<AutoOpen>]
module _FsParserGraphicsStateValueUtils =
    
    [<RequireQualifiedAccess>]
    module FsExtGState = 
        let defaultCustomHashCode =
            let defaultValue1 = FsExtGState.DefaultValue
            let defalutValue2 = { defaultValue1 with BlendModes = [BlendMode.Normal] }
            [
                defaultValue1.GetCustomHashCode()
                defalutValue2.GetCustomHashCode()
            ]

type FsParserGraphicsStateValue(containerID: InfoContainerID list, gs: ParserGraphicsState, invokeByGS: bool) =
    let fsExtState =
        let softMask =
            match invokeByGS with 
            | true ->
                match gs.GetSoftMask() with 
                | :? PdfName as pdfName ->
                    match pdfName with 
                    | EqualTo PdfName.None -> None
                    | _ -> failwithf "Cannot parse %A to SoftMask" pdfName

                | null -> None
                | mask ->
                    let hash = hashNumberOfPdfIndirectReference <| mask.GetIndirectReference()
                    let ref = 
                        (mask :?> PdfDictionary)
                            .Get(PdfName "G")
                            .GetIndirectReference()
                            .GetRefersTo()
                            :?> PdfDictionary
                    let rawBBox = 
                        let floatArray = 
                            ref
                                    .GetAsArray(PdfName.BBox)
                                    .ToDoubleArray()
                        FsRectangle.create floatArray[0] floatArray[1] floatArray[2] floatArray[3]

                    let ctm1 = AffineTransformRecord.ofMatrix (gs.GetCtm())
                    let ctm2 = 
                        ref.GetAsArray(PdfName.Matrix)
                        |> AffineTransformRecord.ofPdfArray

                    let ctm = ctm1.Concatenate(ctm2)

                    let actualBox = 
                        let ctm = AffineTransformRecord.toAffineTransform ctm
                        ctm.Transform(rawBBox.AsRectangle)
                        |> FsRectangle.OfRectangle

                    {
                        SoftMask = 
                            {
                                ID = hash
                                PdfObject_SkipComparation = SkipComparation mask
                            }

                        Ctm = (ctm)
                        RawBBox = rawBBox
                        ActualBBox = actualBox
                    }

                    |> Some

            | false -> None

        let extState = CanvasGraphicsState.getExtGState gs
        { extState with SoftMask = softMask }

    let customHashCode = fsExtState.GetCustomHashCode()

    let invokeByGS = 
        match invokeByGS with 
        | false -> false
        | true -> 
            List.contains customHashCode FsExtGState.defaultCustomHashCode
            |> not

    member x.CustomHashCode = customHashCode

    member x.InvokeByGS = invokeByGS

    member x.FsExtState = fsExtState

    member x.ContainerID = containerID

type FsParserGraphicsState private (gs: FsParserGraphicsStateValue) =

    let innerStack = 
        let stack = Stack()
        stack.Push(gs)
        stack

    member x.InnerStack = innerStack

    member x.ProcessGraphicsStateResource(containerID, gs: ParserGraphicsState) =
        let gs = FsParserGraphicsStateValue(containerID, gs, invokeByGS = true)
        innerStack.Push(gs)
        
    new (containerID, gs: ParserGraphicsState, invokeByGS) =
        let value = FsParserGraphicsStateValue(containerID, gs, invokeByGS = invokeByGS)
        new FsParserGraphicsState(value)


[<AutoOpen>]
module _OperatorRangeExtensions =
    
    [<RequireQualifiedAccess>]
    module PdfCanvas =
        let writeOperatorRange (operatorRange: OperatorRange) (pdfCanvas: PdfCanvas) =
            let outputStream = pdfCanvas.GetContentStream().GetOutputStream()
            let operands = operatorRange.Operands
            match operatorRange.Operator.Text() with 
            | EI -> 
                let stream = operands.[0] :?> PdfStream
                let __BI =
                    outputStream.WriteString(BI).WriteNewLine() |> ignore
                    let dict = stream :> PdfDictionary
                    for pair in dict.EntrySet() do  
                        outputStream.Write(pair.Key)
                            .WriteSpace()
                            .Write(pair.Value)
                            .WriteNewLine()
                        |> ignore

                let __ID = 
                    outputStream.WriteString(ID).WriteNewLine() |> ignore

                    use memoryStream = new System.IO.MemoryStream()

                    let bytes = 
                        //outputStream.GetOutputStream().Flush();
                        let bytes = (stream.GetOutputStream().GetOutputStream() :?> ByteArrayOutputStream).ToArray()
                        bytes

                    outputStream.WriteBytes(bytes).WriteNewLine()


                outputStream.WriteString(EI).WriteNewLine()
                |> ignore

                pdfCanvas

            | _ ->
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
          AccumulatedPathOperatorRanges: seq<OperatorRange>
          GsStates: list<FsParserGraphicsStateValue>
          ContainerID: InfoContainerID list
        }
    with 
        member x.IsShading = 
            match x.PathRenderInfo with 
            | :? PdfShadingPathRenderInfo -> true
            | _ -> false

        member x.IsClippingPath = x.PathRenderInfo.IsPathModifiesClippingPath()

        /// Revealed ExtGState
        member x.GetAppliedExtGState() =
            let exState = CanvasGraphicsState.getExtGState (x.PathRenderInfo.GetGraphicsState())

            //let exState =
            //    exState
            //        .SetFillOpacity(defaultArg x.FillOpacity 1.0f)
            //        .SetStrokeOpacity(defaultArg x.FillOpacity 1.0f)
            
            exState

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
        { PdfConcatedWord: PdfConcatedWord
          FontSize: float 
          TextRotation: Rotation
          FillColor: iText.Kernel.Colors.Color 
          StrokeColor: iText.Kernel.Colors.Color 
          FontName: DocumentFontName
          Bound: FsRectangle
          DenseBound: FsRectangle
          EndTextState: EndTextState
          
          }
    with 
        member x.Text = x.PdfConcatedWord.ConcatedText()

    [<Struct; System.Diagnostics.DebuggerDisplay("IntegratedTextRenderInfo: {RecordValue}")>]
    type IntegratedTextRenderInfo =
        { TextRenderInfo: TextRenderInfo 
          ClippingPathInfos: ClippingPathInfos
          EndTextState: EndTextState
          ConcatedTextInfo: ConcatedTextInfo
          OperatorRange: OperatorRange option
          GsStates: list<FsParserGraphicsStateValue>
          ContainerID: InfoContainerID list
          }

    with 
        

        member x.SplitToWords() =
            let gsStates = x.GsStates
            let containderID = x.ContainerID
            match x.EndTextState with 
            | EndTextState.No
            | EndTextState.Undified -> [x]
            | EndTextState.Yes ->
                let clippingPathInfos = x.ClippingPathInfos
                x.ConcatedTextInfo.AsList
                |> List.map(fun textInfo ->
                    {   
                        TextRenderInfo = textInfo
                        EndTextState = EndTextState.No
                        OperatorRange = None
                        ConcatedTextInfo = {HeadWordInfo = textInfo; FollowedWordInfos = []}
                        ClippingPathInfos = clippingPathInfos
                        GsStates = gsStates
                        ContainerID = containderID
                    }
                )

        /// Revealed ExtGState
        member x.GetAppliedExtGState() =
            let exState = CanvasGraphicsState.getExtGState (x.TextRenderInfo.GetGraphicsState())

            //let exState =
            //    exState
            //        .SetFillOpacity(defaultArg x.FillOpacity 1.0f)
            //        .SetStrokeOpacity(defaultArg x.FillOpacity 1.0f)
        
            exState

        member integratedInfo.ConcatedText(?wordSep) =
            let renderInfo = integratedInfo.TextRenderInfo
            match integratedInfo.EndTextState with 
            | EndTextState.Yes -> 
                integratedInfo.ConcatedTextInfo.ConcatedText(?wordSep = wordSep)
            | EndTextState.No
            | EndTextState.Undified -> renderInfo.GetText()

        member integratedInfo.PdfConcatedWord() =
            let renderInfo = integratedInfo.TextRenderInfo
            match integratedInfo.EndTextState with 
            | EndTextState.Yes -> 
                integratedInfo.ConcatedTextInfo.PdfConcatedWord()
            | EndTextState.No
            | EndTextState.Undified -> 
                { HeadWord = renderInfo.GetText() 
                  FollowedWords = [] }

    
        member x.IsShow_EndTextState = 
            match x.EndTextState with 
            | EndTextState.Undified 
            | EndTextState.Yes -> true
            | EndTextState.No -> false

        member integratedInfo.RecordValue =
            let renderInfo = integratedInfo.TextRenderInfo
            { PdfConcatedWord = integratedInfo.PdfConcatedWord()
              FontSize = ITextRenderInfo.getActualFontSize integratedInfo
              FontName = ITextRenderInfo.getFontName integratedInfo
              TextRotation = ITextRenderInfo.getTextRotation integratedInfo
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

            member x.ConcatedTextInfo = x.ConcatedTextInfo


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

    type IndexedRGBImageData =
        { ImageXObject: PdfImageXObject
          ImageType:    ImageType }
    with 
        member x.GetWidth() = x.ImageXObject.GetWidth()

        member x.GetHeight() = x.ImageXObject.GetHeight()

        member x.Size() = System.Drawing.Size(x.GetWidth() |> int, x.GetHeight() |> int)

    [<RequireQualifiedAccess>]
    type FsImageData =
        | ImageData of ImageData
        | IndexedRgb of IndexedRGBImageData
    with 

        member x.GetColorEncodingComponentsNumber() =
            match x with 
            | ImageData v -> v.GetColorEncodingComponentsNumber()
            | IndexedRgb _ -> 3

        member x.GetBpc() =
            match x with 
            | ImageData v -> v.GetBpc()
            | IndexedRgb _ -> 2

        member x.GetOriginalType() =
            match x with 
            | ImageData v -> v.GetOriginalType()
            | IndexedRgb indexedRGB -> indexedRGB.ImageType

        member x.GetWidth() =
            match x with 
            | ImageData v -> v.GetWidth()
            | IndexedRgb xobject -> xobject.GetWidth() 


        member x.GetHeight() =
            match x with 
            | ImageData v -> v.GetHeight()
            | IndexedRgb xobject -> xobject.GetHeight() 

        member x.GetData() =
            match x with 
            | ImageData v -> v.GetData()
            | IndexedRgb xobject -> failwithf "Cannot get bytes for indexedRGB image data"

    [<Struct>]
    type IntegratedImageRenderInfo =
        { ImageRenderInfo: ImageRenderInfo 
          ClippingPathInfos: ClippingPathInfos
          LazyImageData: Lazy<FsImageData>
          LazyColorSpace: Lazy<ImageColorSpaceData option> 
          GsStates: list<FsParserGraphicsStateValue>
          ContainerID: InfoContainerID list
        }

    with 
        member x.GetAppliedExtGState() =
            let exState = CanvasGraphicsState.getExtGState (x.ImageRenderInfo.GetGraphicsState())

            //let exState =
            //    exState
            //        .SetFillOpacity(defaultArg x.FillOpacity 1.0f)
            //        .SetStrokeOpacity(defaultArg x.FillOpacity 1.0f)
            
            exState

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

        let tryGetVisibleBound boundGettingStrokeOptions (info) =
            match info with 
            | Vector vector -> 
                IIntegratedRenderInfo.tryGetVisibleBound boundGettingStrokeOptions vector
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
        let getConcatedText wordSep (info: ITextRenderInfo) =
            (info :?> IntegratedTextRenderInfo).ConcatedText(?wordSep = wordSep)

        let getPdfConcatedText (info: ITextRenderInfo) =
            (info :?> IntegratedTextRenderInfo).PdfConcatedWord()