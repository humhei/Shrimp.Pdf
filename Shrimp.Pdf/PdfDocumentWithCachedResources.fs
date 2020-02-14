namespace Shrimp.Pdf
open iText.Kernel.Font
open System.IO
open iText.Kernel.Colors
open System.Collections.Concurrent
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Colors

module private FontExtensions =
    let private fontRegisterCache = new ConcurrentDictionary<string, RegisterableFont>()

    type PdfFontFactory with
        static member Register(registerableFont: RegisterableFont) =
            fontRegisterCache.GetOrAdd(registerableFont.Path, fun path ->
                PdfFontFactory.Register(path)
                registerableFont
            ) |> ignore

        static member CreateFont(registerableFont: RegisterableFont) =
            PdfFontFactory.Register(registerableFont)
            PdfFontFactory.CreateRegisteredFont(registerableFont.FontFamily, registerableFont.PdfEncodings)


open FontExtensions
open iText.Kernel.Pdf.Colorspace


[<RequireQualifiedAccess>]
type FsPdfFontFactory =
    | Registerable of RegisterableFont
    /// StandardFonts: See iText.IO.Font.Constants.StandardFonts
    | StandardFonts of string



[<RequireQualifiedAccess>]
type ResourceColor =
    | Pantone of PantoneColorEnum
    | Tpx of TPXColorEnum
    | Registration 
    | CustomSeparation of FsSeparation
    | Lab of FsLab

type private PdfDocumentCache (pdfDocument: unit -> PdfDocument) =
    let fontsCache = new ConcurrentDictionary<FsPdfFontFactory, PdfFont>()
    let colorsCache = new ConcurrentDictionary<ResourceColor, Color>()
    let mutable labColorSpace = None

    member internal x.GetOrCreateColor(resourceColor: ResourceColor) =
        colorsCache.GetOrAdd((resourceColor), fun (color) ->
            let labToItextColor (labColor: FsLab) =
                match labColorSpace with 
                | None -> 
                    let color = labColor.ToItextColor()
                    labColorSpace <- Some (color.GetColorSpace() :?> PdfCieBasedCs.Lab)
                    color
                | Some colorSpace ->
                    labColor.ToItextColor(colorSpace)

            match color with 
            | ResourceColor.Pantone pantoneColorEnum -> Color.pantoneSolidCoated pantoneColorEnum  (pdfDocument())
            | ResourceColor.Tpx tpxColorEnum -> Color.pantoneTPX tpxColorEnum  (pdfDocument())
            | ResourceColor.Registration -> Color.registion (pdfDocument())
            | ResourceColor.CustomSeparation separation ->
                let valueColor = 
                    match separation.Color with 
                    | FsValueColor.Lab labColor -> labToItextColor labColor
                    | color -> FsValueColor.ToItextColor color

                Separation.Create(separation.Name, valueColor, separation.Transparency) :> Color


            | ResourceColor.Lab labColor -> labToItextColor labColor
        )

    member internal x.GetOrCreateFont(fontFactory: FsPdfFontFactory) =
        fontsCache.GetOrAdd((fontFactory), fun (fontFactory) ->

            let fontFamily, fontPdfEncodings = 
                match fontFactory with 
                | FsPdfFontFactory.Registerable fontFactory ->
                    fontFactory.FontFamily, fontFactory.PdfEncodings
                | FsPdfFontFactory.StandardFonts fontFamily ->
                    fontFamily,""

            match (pdfDocument()).FindFont(fontFamily, fontPdfEncodings) with
            | null -> 
                match fontFactory with 
                | FsPdfFontFactory.StandardFonts fontName -> PdfFontFactory.CreateFont(fontName)
                | FsPdfFontFactory.Registerable registerableFont ->
                    if File.Exists(registerableFont.Path)
                    then PdfFontFactory.CreateFont(registerableFont)
                    else failwithf "Cannot find font %s" registerableFont.Path
            | pdfFont -> pdfFont
        )


type PdfDocumentWithCachedResources =
    inherit PdfDocument
    val private cache: PdfDocumentCache
    //let fontsCache = new ConcurrentDictionary<PdfFontFactory, PdfFont>()

    member x.GetOrCreatePdfFont(fontFactory: FsPdfFontFactory) =
        x.cache.GetOrCreateFont(fontFactory)

    member x.GetOrCreateColor(resourceColor: ResourceColor) =
        x.cache.GetOrCreateColor(resourceColor)


    new (writer: string) as this = 
        { inherit PdfDocument(new PdfWriter(writer)); cache = new PdfDocumentCache((fun _ -> this :> PdfDocument)) }

    new (reader: string, writer: string) as this =  
        { inherit PdfDocument(new PdfReader(reader), new PdfWriter(writer)); cache = new PdfDocumentCache(fun _ -> this :> PdfDocument) }

    new (reader: string, writer: string, oldDocument: PdfDocumentWithCachedResources) =  
        { inherit PdfDocument(new PdfReader(reader), new PdfWriter(writer)); cache = oldDocument.cache }



type IntegratedDocument private (reader: string, writer: string) =
    let mutable pdfDocument = new PdfDocumentWithCachedResources(reader, writer)

    member x.ReaderPath = reader

    member x.WriterPath = writer

    member x.Value = pdfDocument

    member x.ReOpen() =
        pdfDocument.Close()

        File.Delete(reader)
        File.Move(writer, reader)

        pdfDocument <- new PdfDocumentWithCachedResources(reader, writer, pdfDocument)

    static member Create(reader, writer) = new IntegratedDocument(reader, writer)



