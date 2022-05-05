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
open iText.IO.Font.Constants
open Shrimp.FSharp.Plus
open iText.IO.Font
open iText.IO.Font.Otf


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
open Newtonsoft.Json





open Shrimp.Pdf.FontNames.Query

[<RequireQualifiedAccess>]
type FsPdfFontFactory =
    | Registerable of RegisterableFont
    /// StandardFonts: See iText.IO.Font.Constants.StandardFonts
    | StandardFonts of string
    | DocumentFont of FsFontName
with 
    static member CreateDocumentFont(fontName: FontNames) =
        fontName.FsFontName
        |> FsPdfFontFactory.DocumentFont

    member x.LoggingText =
        match x with 
        | FsPdfFontFactory.Registerable v -> v.LoggingText
        | FsPdfFontFactory.StandardFonts v -> v
        | FsPdfFontFactory.DocumentFont (documentFont) -> documentFont.LoggingText



[<RequireQualifiedAccess>]
type ResourceColor =
    | Pantone of PantoneColorEnum
    | Tpx of TPXColorEnum
    | Registration 
    | CustomSeparation of FsSeparation
    | Lab of FsLab


type private PdfDocumentCache private 
    (pdfDocument: unit -> PdfDocumentWithCachedResources,
     fontsCache: ConcurrentDictionary<FsPdfFontFactory, PdfFont>,
     colorsCache: ConcurrentDictionary<ResourceColor, Color>,
     extGStateCache: ConcurrentDictionary<FsExtGState, Extgstate.PdfExtGState>) =
    let mutable labColorSpace = None

    member internal x.Clear() = 
        fontsCache.Clear()
        colorsCache.Clear()
        extGStateCache.Clear()

    member internal x.Spawn(pdfDocument: unit -> PdfDocumentWithCachedResources) =
        PdfDocumentCache(pdfDocument, new ConcurrentDictionary<_, _>(), new ConcurrentDictionary<_, _>() (*colorsCache*),extGStateCache)

    member internal x.CacheDocumentFonts(fonts) =
        for (font: PdfFont) in fonts do
            let fontNames = font.GetFontProgram().GetFontNames()
            fontsCache.GetOrAdd(FsPdfFontFactory.DocumentFont(FsFontName.Create(fontNames)), fun _ ->
                font
            ) |> ignore

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
            
            let font = 
                match fontFactory with 
                | FsPdfFontFactory.Registerable fontFactory ->
                    (pdfDocument()).FindFont(fontFactory.FontFamily, fontFactory.PdfEncodings)
                | FsPdfFontFactory.StandardFonts fontFamily ->
                    (pdfDocument()).FindFont(fontFamily,"")
                | FsPdfFontFactory.DocumentFont font -> 
                    fontsCache.GetOrAdd(fontFactory, fun _ ->
                        failwithf "Cannot create document font %s, please cache it before using it with PdfDocumentWithCachedResources.CacheDocumentFonts" font.LoggingText
                    )

            match font with
            | null -> 
                match fontFactory with 
                | FsPdfFontFactory.StandardFonts fontName -> PdfFontFactory.CreateFont(fontName)
                | FsPdfFontFactory.Registerable registerableFont ->
                    if File.Exists(registerableFont.Path)
                    then 
                        match PdfFontFactory.CreateFont(registerableFont) with 
                        | null -> failwithf "Cannot create font %s by %A" registerableFont.Path registerableFont
                        | font -> font
                    else failwithf "Cannot find registerable font %s" registerableFont.Path
                | FsPdfFontFactory.DocumentFont documentFont ->
                    failwithf "Cannot find document font %s" documentFont.LoggingText

            | pdfFont -> pdfFont
        )

    member internal x.GetOrCreateExtGState(extGState: FsExtGState) = 
        extGStateCache.GetOrAdd(extGState, fun (extGState) ->
            let pdfExtGState = new Extgstate.PdfExtGState()
            pdfExtGState
                .SetOverprintMode(int extGState.OPM)
                .SetFillOverPrintFlag(extGState.IsFillOverprint)
                .SetStrokeOverPrintFlag(extGState.IsStrokeOverprint)
        )

    new (pdfDocument: unit -> PdfDocumentWithCachedResources) =
        PdfDocumentCache
            (pdfDocument,
             new ConcurrentDictionary<FsPdfFontFactory, PdfFont>(),
             new ConcurrentDictionary<ResourceColor, Color>(),
             new ConcurrentDictionary<FsExtGState, Extgstate.PdfExtGState>())

and PdfDocumentWithCachedResources =
    inherit PdfDocument
    val private cache: PdfDocumentCache



    member x.GetOrCreatePdfFont(fontFactory: FsPdfFontFactory) =
        x.cache.GetOrCreateFont(fontFactory)

    //member private x.ClearCache() = x.cache.Clear()

    /// defaultPageSelector is First
    member x.CacheDocumentFonts(?pageSelector: PageSelector) =
        let pageSelector = defaultArg pageSelector PageSelector.First
        let pageNumbers = 
            x.GetPageNumbers(pageSelector) 
            |> List.distinct

        let parser = new NonInitialClippingPathPdfDocumentContentParser(x)

        let infos = 
            pageNumbers
            |> List.collect(fun pageNumber ->
                NonInitialClippingPathPdfDocumentContentParser.parse
                    pageNumber
                    (RenderInfoSelector.Text(fun _ -> true))
                    parser
                |> List.ofSeq
                |> List.choose IIntegratedRenderInfo.asITextRenderInfo
            )

        let fonts = 
            infos
            |> List.ofSeq
            |> List.choose IIntegratedRenderInfo.asITextRenderInfo
            |> List.map(fun info -> info.TextRenderInfo.GetFont())

        x.cache.CacheDocumentFonts(fonts)

    member x.GetOrCreateColor(resourceColor: ResourceColor) =
        x.cache.GetOrCreateColor(resourceColor)

    member internal x.GetDocumentFontsInternal() = base.GetDocumentFonts()

    member pdfDocument.GetOrCreateColor(pdfCanvasColor: PdfCanvasColor) =
        match pdfCanvasColor with 
        | PdfCanvasColor.Value color -> 
            match color with 
            | FsValueColor.Lab lab ->
                let resourceColor = ResourceColor.Lab lab
                pdfDocument.GetOrCreateColor(resourceColor) 
            | _ -> color |> FsValueColor.ToItextColor

        | PdfCanvasColor.Separation separation ->
            let resourceColor = ResourceColor.CustomSeparation separation
            pdfDocument.GetOrCreateColor(resourceColor) 

        | PdfCanvasColor.ColorCard colorCard ->
            match colorCard with 
            | ColorCard.KnownColor knownColor ->
                DeviceRgb.fromKnownColor knownColor
                :> Color
            | ColorCard.Pantone pantoneColor ->
                let resourceColor = ResourceColor.Pantone pantoneColor
                pdfDocument.GetOrCreateColor(resourceColor) 

            | ColorCard.TPX tpxColor ->
                let resourceColor = ResourceColor.Tpx tpxColor
                pdfDocument.GetOrCreateColor(resourceColor) 

        | PdfCanvasColor.Registration ->
                let resourceColor = ResourceColor.Registration
                pdfDocument.GetOrCreateColor(resourceColor) 

    member x.GetOrCreateExtGState(extGState: FsExtGState) = 
        x.cache.GetOrCreateExtGState(extGState)
        
    //member x.CloseAndClearCache() = 
    //    x.ClearCache()
    //    x.Close()

    new (writer: string) as this = 
        { inherit PdfDocument(new PdfWriter(writer)); cache = new PdfDocumentCache((fun _ -> this)) }

    new (reader: string, writer: string) as this =  
        { inherit PdfDocument(new PdfReader(reader), new PdfWriter(writer)); cache = new PdfDocumentCache((fun _ -> this)) }

    new (reader: string, writer: string, oldDocument: PdfDocumentWithCachedResources) as this =  
        { inherit PdfDocument(new PdfReader(reader), new PdfWriter(writer)); cache = oldDocument.cache.Spawn(fun _ -> this) }


type IntegratedDocument internal (reader: string, writer: string) =
    let mutable pdfDocument: Lazy<PdfDocumentWithCachedResources> option = None
    let mutable isOpened = false

    member x.ReaderPath = reader

    member x.WriterPath = writer

    member x.LazyValue =
        match pdfDocument with 
        | Some document -> document
        | None -> failwith "document is not open yet please open it first"


    member x.Value = x.LazyValue.Value

    member internal x.Open() =
        if not isOpened
        then
            match pdfDocument with 
            | Some (oldPdfDocument) ->
                match oldPdfDocument with 
                | Lazy.ValueCreated oldPdfDocument ->
                    if oldPdfDocument.IsClosed()
                    then 
                        pdfDocument <- Some (lazy new PdfDocumentWithCachedResources(reader, writer, oldPdfDocument))
                    else failwith "Old document is not closed yet"

                | Lazy.NotCreated ->
                    pdfDocument <- Some (lazy new PdfDocumentWithCachedResources(reader, writer))
                    

            | None ->
                pdfDocument <- Some (lazy new PdfDocumentWithCachedResources(reader, writer))

        isOpened <- true

    member internal x.CloseAndDraft() =
        match x.LazyValue with 
        | Lazy.ValueCreated value ->
            value.Close()
            File.Delete(reader)
            File.Move(writer, reader)

        | Lazy.NotCreated _ -> ()

        isOpened <- false

    member internal x.TryCloseAndDisposeWriter_IfOpened() =
        match isOpened with 
        | true ->
            match x.LazyValue with 
            | Lazy.ValueCreated value -> value.Close()
            | Lazy.NotCreated _ -> ()

            isOpened <- false
        | false -> ()

    static member Create(reader, writer) = new IntegratedDocument(reader, writer)



