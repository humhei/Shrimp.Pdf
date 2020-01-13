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


[<RequireQualifiedAccess>]
type PdfCanvasColor = 
    | N
    | ITextColor of Color
    | Separation of FsSeparation
    | ColorCard of ColorCard
    | Registration
    | Lab of FsLab
with 
    member pdfCanvasColor.IsEqualTo(color: Color) =
        match pdfCanvasColor with 
        | PdfCanvasColor.N -> false
        | PdfCanvasColor.ITextColor color1 -> Color.equal color color1
        | PdfCanvasColor.Separation separation1 ->
            match color with 
            | :? Separation as separation ->
                let colorSpace = separation.GetColorSpace() :?> PdfSpecialCs.Separation

                let colorSpacePdfArray = 
                    colorSpace.GetPdfObject() :?> PdfArray

                let colorName = 
                    let uri = 
                        (colorSpacePdfArray.Get(1)
                         |> string).TrimStart('/')
                    uri.Replace("#20", " ")

                if colorName = separation1.Name && (separation.GetColorValue().[0] = float32 separation1.Transparency)
                then 
                    
                    let color = 
                        colorSpace.GetAlterateColor()


                    let color1 = separation1.Color

                    color.EqualWhenColorValueRounded color1

                else false
            | _ -> false
        | PdfCanvasColor.ColorCard colorCard1 ->

            match colorCard1 with 
            | ColorCard.Pantone _ 
            | ColorCard.TPX _ ->
                let fsValueColor1 =
                    match colorCard1 with 
                    | ColorCard.Pantone pantoneColor1 ->
                        FsLab.OfPantone pantoneColor1
                        |> FsValueColor.Lab

                    | ColorCard.TPX tpxColor1 ->
                        FsLab.OfTpx tpxColor1
                        |> FsValueColor.Lab

                    | ColorCard.KnownColor knownColor -> 
                        failwith "Invalid token"

                let separationName1 = 
                    match colorCard1 with 
                    | ColorCard.Pantone pantoneColor1 ->
                        pantoneColor1.ToString()

                    | ColorCard.TPX tpxColor1 ->
                        tpxColor1.ToString()
                    | ColorCard.KnownColor _ -> failwith "Invalid token"


                let separation1 = 
                    { Color = fsValueColor1
                      Name = separationName1
                      Transparency = 1.
                    }
                    |> PdfCanvasColor.Separation
                
                separation1.IsEqualTo(color)
            | ColorCard.KnownColor knownColor1 ->
                let itextColor1 = 
                    (DeviceRgb.fromKnownColor knownColor1 :> Color)
                    |> PdfCanvasColor.ITextColor

                itextColor1.IsEqualTo(color)

        | PdfCanvasColor.Lab labColor1 -> 
            match color with 
            | :? Lab as labColor ->
                let colorValue1 = (FsValueColor.Lab labColor1).GetColorValue()
                let colorValue = labColor.GetColorValue() |> List.ofArray
                colorValue = colorValue1
            | _ -> false

        | PdfCanvasColor.Registration ->
            let registration1 = 
                { Name = "All";
                  Color = 
                    FsValueColor.Cmyk 
                        { C = 1.f 
                          M = 1.f 
                          Y = 1.f 
                          K = 1.f  }
                  Transparency = 1.
                }
                |> PdfCanvasColor.Separation
            registration1.IsEqualTo(color)


type IntegratedDocument private (reader: string, writer: string) =
    let mutable pdfDocument = new PdfDocumentWithCachedResources(reader, writer)

    member x.ReaderName = reader

    member x.WriterName = writer

    member x.Value = pdfDocument

    member x.ReOpen() =
        pdfDocument.Close()

        File.Delete(reader)
        File.Move(writer, reader)

        pdfDocument <- new PdfDocumentWithCachedResources(reader, writer, pdfDocument)

    static member Create(reader, writer) = new IntegratedDocument(reader, writer)


[<RequireQualifiedAccess>]
module IntegratedDocument =
    type private Modifier = _SelectionModifierFixmentArguments -> list<PdfCanvas -> PdfCanvas>
    
    let modify (name) (pageSelector: PageSelector) (selectorModifierMappingFactory: (PageNumber * PdfPage) -> Map<SelectorModiferToken, RenderInfoSelector * Modifier>) (document: IntegratedDocument) =
        let selectedPageNumbers = document.Value.GetPageNumbers(pageSelector) 
        let totalNumberOfPages = document.Value.GetNumberOfPages()
        Logger.infoWithStopWatch (sprintf "MODIFY: \n%s" name) (fun _ ->
            for i = 1 to totalNumberOfPages do
                if List.contains i selectedPageNumbers then
                    let page = document.Value.GetPage(i)
                    let selectorModifierMapping = selectorModifierMappingFactory (PageNumber i, page)
                    for pair in selectorModifierMapping do
                        PdfPage.modify (Map.ofList[pair.Key, pair.Value]) page
                
        )

