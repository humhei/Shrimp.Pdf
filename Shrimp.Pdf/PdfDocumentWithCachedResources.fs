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
        static member private Register(registerableFont: RegisterableFont) =
            fontRegisterCache.GetOrAdd(registerableFont.Path, fun path ->
                PdfFontFactory.Register(path)
                registerableFont
            ) |> ignore

        /// NoNeed to invoke PdfFontFactory.Register first
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

type private PdfDocumentCache private (pdfDocument: unit -> PdfDocument, fontsCache: ConcurrentDictionary<FsPdfFontFactory, PdfFont>, colorsCache: ConcurrentDictionary<ResourceColor, Color>) =
    let mutable labColorSpace = None

    member internal x.Spawn(pdfDocument: unit -> PdfDocument) =
        PdfDocumentCache(pdfDocument, new ConcurrentDictionary<_, _>(), colorsCache)

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

    new (pdfDocument: unit -> PdfDocument) =
        PdfDocumentCache(pdfDocument, new ConcurrentDictionary<FsPdfFontFactory, PdfFont>(),  new ConcurrentDictionary<ResourceColor, Color>())

type PdfDocumentWithCachedResources =
    inherit PdfDocument
    val private cache: PdfDocumentCache

    member x.GetOrCreatePdfFont(fontFactory: FsPdfFontFactory) =
        x.cache.GetOrCreateFont(fontFactory)

    member x.GetOrCreateColor(resourceColor: ResourceColor) =
        x.cache.GetOrCreateColor(resourceColor)


    new (writer: string) as this = 
        { inherit PdfDocument(new PdfWriter(writer)); cache = new PdfDocumentCache((fun _ -> this :> PdfDocument)) }

    new (reader: string, writer: string) as this =  
        { inherit PdfDocument(new PdfReader(reader), new PdfWriter(writer)); cache = new PdfDocumentCache(fun _ -> this :> PdfDocument) }

    new (reader: string, writer: string, oldDocument: PdfDocumentWithCachedResources) as this =  
        { inherit PdfDocument(new PdfReader(reader), new PdfWriter(writer)); cache = oldDocument.cache.Spawn(fun _ -> this :> PdfDocument) }



type IntegratedDocument internal (reader: string, writer: string) =
    let mutable pdfDocument = None
    let mutable isOpened = false

    member x.ReaderPath = reader

    member x.WriterPath = writer

    member x.Value = 
        match pdfDocument with 
        | Some document -> document
        | None -> failwith "document is not open yet please option it first"

    member internal x.Open() =
        if not isOpened
        then
            match pdfDocument with 
            | Some (oldPdfDocument: PdfDocumentWithCachedResources) ->
                if oldPdfDocument.IsClosed()
                then 
                    pdfDocument <- Some (new PdfDocumentWithCachedResources(reader, writer, oldPdfDocument))
                else failwith "Old document is not closed yet"
            | None ->
                pdfDocument <- Some (new PdfDocumentWithCachedResources(reader, writer))

        isOpened <- true

    member internal x.CloseAndDraft() =
        x.Value.Close()
        File.Delete(reader)
        File.Move(writer, reader)

        isOpened <- false

    static member Create(reader, writer) = new IntegratedDocument(reader, writer)



