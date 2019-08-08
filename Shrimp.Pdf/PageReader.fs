namespace Atrous.PdfageReader

open Atrous.Pdf.Types
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas.Parser
open Atrous.Pdf.Parser
open Atrous.Pdf.Filters
open Atrous.Pdf.Extensions

type PageReaderArgument =
    {
        Page: PdfPage
        Parser: PdfDocumentContentParser
        PageNum: int
    }

[<RequireQualifiedAccess>]
module PageReader =
    open Atrous.Pdf.Select
    open Atrous.Pdf.Colors
    //let private pathsAll filters (arg: PageReaderArgument) =
    //    Extract.paths arg.PageNum (all filters) arg.Parser |> List.ofSeq

    let paths filters (arg: PageReaderArgument) = 
        Extract.paths arg.PageNum (path filters) arg.Parser |> List.ofSeq

    let rects filters (arg: PageReaderArgument) =
        let paths = paths filters arg
        paths 
        |> List.map PathRenderInfo.getBound

    let analyticSubpaths filters (arg: PageReaderArgument) =
        let paths = paths [PathRenderInfo.isVisible] arg
        let filter = all (AnalyticSubpath.hasPoints :: filters) 
        paths 
        |> List.collect (fun path ->
            path.GetPath().GetSubpaths()
            |> List.ofSeq
            |> List.filter (fun subpath -> 
                filter {PathRenderInfo = path;Subpath = subpath}
            )
            |> List.map (fun subpath -> {PathRenderInfo = path;Subpath = subpath})
        )

    let texts filters (arg: PageReaderArgument) = 
        Extract.texts arg.PageNum (text filters) arg.Parser |> List.ofSeq

    let infosCrossOrInBox getBox margin (arg: PageReaderArgument) = 
        let pageBox = arg.Page |> getBox |> Rectangle.applyMargin margin
        Extract.all arg.PageNum (visible [Select.crossOrInRegion pageBox]) arg.Parser
    
    let colorsCrossOrInBox getBox margin (arg: PageReaderArgument) = 
        let infos = infosCrossOrInBox getBox margin arg
        infos |> Seq.collect AbstractRenderInfo.getColors |> Colors.distinct |> List.ofSeq

    let infosCrossOrInBBox margin (arg: PageReaderArgument) = 
        infosCrossOrInBox PdfPage.getBBox margin arg

    let colorsCrossOrInInBBox margin (arg: PageReaderArgument) = 
        colorsCrossOrInBox PdfPage.getBBox margin arg

[<RequireQualifiedAccess>]
module ManipulateArgument =
    let toPageReaderArgument (arg: ManipulateArgument<_>) =
        {
            Page = arg.Page
            Parser = arg.Parser
            PageNum = arg.PageNum
        }

    let usePageReader pageReader (arg: ManipulateArgument<_>) =
        toPageReaderArgument arg
        |> pageReader

    let readPaths filters (arg: ManipulateArgument<_>) =
        usePageReader (PageReader.paths filters) arg

    let readTexts filters (arg: ManipulateArgument<_>) =
        usePageReader (PageReader.texts filters) arg


