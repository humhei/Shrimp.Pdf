
namespace Shrimp.Pdf.DSL
#nowarn "0104"
open Shrimp.Pdf.Extensions
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open System.Runtime.CompilerServices
open System.IO
open Shrimp.FSharp.Plus
open iText.Kernel.Geom
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open Fake.IO
open Fake.IO.FileSystemOperators

type ISABRenderInfo = 
    abstract member GetFillColor: unit -> FsColor option 
    abstract member GetStrokeColor: unit -> FsColor option 
    abstract member GetBound: BoundGettingStrokeOptions -> Rectangle
    abstract member GetDenseBound: BoundGettingStrokeOptions -> Rectangle
    abstract member Tag: IntegratedRenderInfoTag
    

type ISABPathRenderInfo =
    inherit ISABRenderInfo
    

type ISABTextRenderInfo =
    inherit ISABRenderInfo
    abstract member ConcatedText: ?wordSep: string -> string
    abstract member PdfConcatedWord: unit -> PdfConcatedWord

    abstract member GetRawFontSize: unit -> float32 
    abstract member GetActualFontSize: unit -> float
    abstract member GetFontName: unit -> DocumentFontName 

    abstract member GetWidth: unit -> float
    abstract member GetTextMatrix: unit -> Matrix
    abstract member GetMatrix: unit -> Matrix
    
[<AutoOpen>]
module _ISABTextRenderInfoExtensions =
    type ISABTextRenderInfo with 
        member x.FontNameIs(fontName: string) =
            let fontName2 = x.GetFontName()
            fontName2.SameFontNameTo(fontName)

        member x.FontSizeIs(fontSize) =
            let fontSize2 = x.GetActualFontSize()
            fontSize @= fontSize2

        member x.FontNameAndSizeIs(fontName, fontSize) =
            x.FontSizeIs(fontSize)
            && x.FontNameIs(fontName)

        member x.CtmAndTextCtm() =
            { Matrix = x.GetMatrix()
              TextMatrix = x.GetTextMatrix() }

        member info.TransformedTextMatrixAndRawSize(text: PdfConcatedWord, font, actualSize: float, ?alignment: XEffort) =
            let alignment = defaultArg alignment XEffort.Left
            let transformedFontSize = 
                let affime = info.CtmAndTextCtm()
                affime.ToTransformedSize(actualSize)

            let difference() =
                let originWidth = info.GetWidth()
                let newWidth =
                    text.SplitByLine()
                    |> List.map(fun text ->
                        let widthUnits = 
                            let text = text.ConcatedText()
                            PdfFont.calcLineWidthUnits text font
                        List.max widthUnits * actualSize
                    )
                    |> List.max

                newWidth - originWidth

            let originTransform() = 
                info.GetTextMatrix()
                |> AffineTransformRecord.ofMatrix

            let textTransform =
                match alignment with 
                | XEffort.Left -> None
                | XEffort.Middle ->
                    let difference = difference()
                    let offset = -(difference / 2.) 
                    let transform = originTransform()
                    transform.Translate(offset, 0.)
                    |> Some

                | XEffort.Right ->
                    let offset = -difference()
                    let transform = originTransform()
                    transform.Translate(offset, 0.)
                    |> Some

            {|
                TransformedFontSize = transformedFontSize
                TextTransform = textTransform
            |}

type IntegratedPathRenderInfo2(pathRenderInfo: IntegratedPathRenderInfo) =
    let innerInfo = pathRenderInfo.PathRenderInfo

    member x.Value = pathRenderInfo

    interface ISABPathRenderInfo with 
        member x.Tag = IntegratedRenderInfoTag.Path
        member x.GetDenseBound(boundStokeOptions) = IPathRenderInfo.getBound boundStokeOptions pathRenderInfo
        member x.GetBound(boundStokeOptions) = IPathRenderInfo.getBound boundStokeOptions pathRenderInfo
        member x.GetFillColor() = 
            match innerInfo.GetOperation() with 
            | IPathRenderInfo.Operation.HasFill -> 
                innerInfo.GetFillColor()
                |> FsColor.OfItextColor
                |> Some

            | IPathRenderInfo.Operation.NoFill -> None

        member x.GetStrokeColor() = 
            match innerInfo.GetOperation() with 
            | IPathRenderInfo.Operation.HasStroke -> 
                innerInfo.GetStrokeColor()
                |> FsColor.OfItextColor
                |> Some

            | IPathRenderInfo.Operation.NoStroke -> None


type IntegratedTextRenderInfo2(textRenderInfo: IntegratedTextRenderInfo) =
    let innerInfo = textRenderInfo.TextRenderInfo

    member x.Value = textRenderInfo

    interface ISABTextRenderInfo with
        member x.GetDenseBound(boundStokeOptions) = ITextRenderInfo.getDenseBound boundStokeOptions textRenderInfo
        member x.GetBound(boundStokeOptions) = ITextRenderInfo.getBound boundStokeOptions textRenderInfo
        member x.GetFillColor() = 
            match innerInfo.GetTextRenderMode() with 
            | TextRenderInfo.TextRenderingMode.HasFill -> 
                innerInfo.GetFillColor()
                |> FsColor.OfItextColor
                |> Some

            | TextRenderInfo.TextRenderingMode.NoFill -> None

        member x.GetStrokeColor() = 
            match innerInfo.GetTextRenderMode() with 
            | TextRenderInfo.TextRenderingMode.HasStroke -> 
                innerInfo.GetStrokeColor()
                |> FsColor.OfItextColor
                |> Some

            | TextRenderInfo.TextRenderingMode.NoStroke -> None

        member x.GetMatrix() = innerInfo.GetGraphicsState().GetCtm() 
        member x.GetTextMatrix() = innerInfo.GetTextMatrix()
        member x.ConcatedText(?wordSep) = ITextRenderInfo.getConcatedText wordSep textRenderInfo
        member x.GetActualFontSize() = ITextRenderInfo.getActualFontSize textRenderInfo

        member x.GetFontName() = ITextRenderInfo.getFontName textRenderInfo
        member x.GetRawFontSize() = innerInfo.GetFontSize()
        member x.PdfConcatedWord() = textRenderInfo.PdfConcatedWord()
        member x.GetWidth() = ITextRenderInfo.getWidth textRenderInfo

        member x.Tag = IntegratedRenderInfoTag.Text



[<RequireQualifiedAccess>]
module ISABRenderInfo =
    let asText (info: ISABRenderInfo) =
        match info.Tag with 
        | IntegratedRenderInfoTag.Path -> None
        | IntegratedRenderInfoTag.Text -> Some (info :?> ISABTextRenderInfo)

    let asPath (info: ISABRenderInfo) =
        match info.Tag with 
        | IntegratedRenderInfoTag.Text -> None
        | IntegratedRenderInfoTag.Path -> Some (info :?> ISABPathRenderInfo)

    let (|Path|Text|) (info: ISABRenderInfo) =
        match info.Tag with 
        | IntegratedRenderInfoTag.Text -> Text(info :?> ISABTextRenderInfo)
        | IntegratedRenderInfoTag.Path -> Path(info :?> ISABPathRenderInfo)

    let ofIIntegratedRenderInfo(info: IIntegratedRenderInfo) =
        match info with 
        | IIntegratedRenderInfo.Path info -> IntegratedPathRenderInfo2(info) :> ISABRenderInfo
        | IIntegratedRenderInfo.Text info -> IntegratedTextRenderInfo2(info) :> ISABRenderInfo

type SABPageModifier<'userState, 'newUserState> = PageModifingArguments<'userState> -> seq<ISABRenderInfo> -> 'newUserState

