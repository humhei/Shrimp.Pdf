namespace Shrimp.Pdf

open Shrimp.Pdf.Extensions

#nowarn "0104"
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas.Parser.Data
open FParsec
open iText.Kernel.Pdf
open Shrimp.FSharp.Plus


[<AutoOpen>]
module IntegratedInfos =
    
    type PathInfoRecord =
        { FillColor: iText.Kernel.Colors.Color 
          StrokeColor: iText.Kernel.Colors.Color 
          Bound: Rectangle }

    [<Struct>]
    type IntegratedPathRenderInfo =
        { PathRenderInfo: PathRenderInfo 
          ClippingPathInfos: ClippingPathInfos }
    with 
        member integratedInfo.RecordValue =
            let renderInfo = integratedInfo.PathRenderInfo
            { FillColor = renderInfo.GetFillColor()
              StrokeColor = renderInfo.GetStrokeColor()
              Bound = IPathRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth integratedInfo}


        interface IPathRenderInfo with 
            member x.Value = x.PathRenderInfo

        interface IAbstractRenderInfo with 
            member x.Value = x.PathRenderInfo :> AbstractRenderInfo

        interface IIntegratedRenderInfo with 
            member x.Tag = IntegratedRenderInfoTag.Path
            member x.ClippingPathInfos = x.ClippingPathInfos

    type TextInfoRecord =
        { Text: string 
          FontSize: float 
          FillColor: iText.Kernel.Colors.Color 
          StrokeColor: iText.Kernel.Colors.Color 
          FontName: string
          Bound: Rectangle }


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
              Bound = ITextRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth integratedInfo}

        interface ITextRenderInfo with 
            member x.Value = x.TextRenderInfo

        interface IAbstractRenderInfo with 
            member x.Value = x.TextRenderInfo :> AbstractRenderInfo

        interface IIntegratedRenderInfo with 
            member x.Tag = IntegratedRenderInfoTag.Text

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


