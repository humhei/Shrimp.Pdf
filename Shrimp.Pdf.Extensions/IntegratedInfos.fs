namespace Shrimp.Pdf

open Shrimp.Pdf.Extensions

#nowarn "0104"
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas.Parser.Data
open FParsec
open iText.Kernel.Pdf
open Shrimp.FSharp.Plus
open iText.IO.Image



[<AutoOpen>]
module IntegratedInfos =


    type PathInfoRecord =
        { FillColor: iText.Kernel.Colors.Color 
          StrokeColor: iText.Kernel.Colors.Color 
          Bound: FsRectangle }

    [<Struct>]
    type IntegratedPathRenderInfo =
        { PathRenderInfo: PathRenderInfo 
          ClippingPathInfos: ClippingPathInfos }
    with 
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
          FontName: FsFontName
          Bound: FsRectangle
          DenseBound: FsRectangle }
            
            

    [<Struct>]
    type IntegratedTextRenderInfo =
        { TextRenderInfo: TextRenderInfo 
          ClippingPathInfos: ClippingPathInfos }

    with 
        member integratedInfo.RecordValue =
            let renderInfo = integratedInfo.TextRenderInfo
            { Text = renderInfo.GetText()
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
            }

        interface IAbstractRenderInfoIM with 
            member x.Value = x.TextRenderInfo :> AbstractRenderInfo

        interface ITextRenderInfo with 
            member x.Value = x.TextRenderInfo

        interface IAbstractRenderInfo with 
            member x.Value = x.TextRenderInfo :> AbstractRenderInfo

        interface IIntegratedRenderInfoIM with 
            member x.TagIM = IntegratedRenderInfoTagIM.Text
            member x.ClippingPathInfos = x.ClippingPathInfos
            
        interface IIntegratedRenderInfo with 
            member x.Tag = IntegratedRenderInfoTag.Text
            member x.ClippingPathInfos = x.ClippingPathInfos



    type ImageRenderInfoRecord =
        { Bound: FsRectangle
          ColorSpace: ColorSpace }

    [<Struct>]
    type IntegratedImageRenderInfo =
        { ImageRenderInfo: ImageRenderInfo 
          ClippingPathInfos: ClippingPathInfos
          LazyImageData: Lazy<ImageData> }

    with 
        member x.ImageData = x.LazyImageData.Value

        member x.ColorSpace = ImageData.colorSpace x.ImageData

        member x.Dpi =
            let bound =  IImageRenderInfo.getBound x
            let width = x.ImageData.GetWidth()
            let height = x.ImageData.GetHeight()
            let dpi_x = float width / userUnitToMM (bound.GetWidthF())     |> inchToMM |> round |> int
            let dpi_y = float height / userUnitToMM (bound.GetHeightF())  |> inchToMM |> round |> int
            {| X = dpi_x
               Y = dpi_y |}

        member x.RecordValue =
            { Bound = IImageRenderInfo.getBound x |> FsRectangle.OfRectangle
              ColorSpace = x.ColorSpace }

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

        let asIImageRenderInfo (info: IIntegratedRenderInfoIM) = 
            match info with
            | Image info -> Some (info)
            | _ -> None 
