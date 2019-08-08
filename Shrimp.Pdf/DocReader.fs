namespace Atrous.Pdf

open Atrous.Pdf.Types
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas.Parser
open Atrous.Pdf.Parser
open Atrous.Pdf.Filters
open Atrous.Pdf.Extensions
open Atrous.Pdf.PageReader

module DocReader =
    [<RequireQualifiedAccess>]
    module PdfDocument =
        let internal usePageReader pageReader (doc: PdfDocument) =
            let parser = new PdfDocumentContentParser(doc)
            PdfDocument.getAllPages doc |> List.mapi (fun i page ->
                {
                    Page = page
                    Parser = parser
                    PageNum = i + 1
                } |> pageReader
            )

        let readTexts filters (doc: PdfDocument) =
            usePageReader (PageReader.texts filters) doc

        let readRects filters (doc: PdfDocument) =
            usePageReader (PageReader.rects filters) doc

        let readAnalyticSubpaths filters (doc: PdfDocument) =
            usePageReader (PageReader.analyticSubpaths filters) doc

        let colorsCrossOrInBBox margin (doc: PdfDocument) =
            usePageReader (PageReader.colorsCrossOrInInBBox margin) doc

        let colorsCrossOrInBox getBox margin (doc: PdfDocument) =
            usePageReader (PageReader.colorsCrossOrInBox getBox margin) doc

    [<RequireQualifiedAccess>]
    module ColorCard =
        open Atrous.Pdf.Colors

        let toColoredText colorCard = 
            let colorNameToText colorName =
                fun (doc: PdfDocument) ->
                    let color = 
                        PdfDocument.usePageReader (PageReader.colorsCrossOrInInBBox Margin.empty) doc
                        |> List.concat 
                        |> List.choose Color.asSeparation 
                        |> List.find (fun separation ->
                            Separation.colorName separation = colorName
                        )

                    Text.toColoredText colorName color

            match colorCard with 
            | ColorCard.Enum enum -> ColorMap.toColoredText enum |> ColoredText.Common
            | ColorCard.Pantone pantone -> 
                pantone.ToString()
                |> colorNameToText
                |> ColoredText.FromDoc
            | ColorCard.TPX tpx -> 
                tpx.ToString()
                |> colorNameToText
                |> ColoredText.FromDoc
