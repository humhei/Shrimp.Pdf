namespace Atrous.Pdf.Colors
open Atrous.Pdf
open iText.Kernel.Pdf
open Atrous.Pdf.ColorBook
open Atrous

module ColorConverter = 
    open Colourful.Conversion
    open iText.Kernel.Colors
    open Colourful

    [<RequireQualifiedAccess>]
    module LabColor =

        let getL (c: LabColor) =
            c.L

        let encodeToString (values: int list) =
            match values with 
            | [l;a;b] -> 
                sprintf "0x%02x%02x%02x" l (a + 128) (b + 128)
            | _ -> invalidArg "value" (values.ToString())
        

        let fromHex (hex: int) =
            let l = hex >>> 16 |> byte |> int
            let a = hex >>> 8 |> byte |> int
            let b = hex >>> 0 |> byte |> int
            let a2 = a - 128
            let b2 = b - 128
            [l;a2;b2] |> List.map float |> LabColor

    let private converter = new ColourfulConverter(TargetRGBWorkingSpace = RGBWorkingSpaces.sRGB)
    converter.WhitePoint <- Illuminants.D65
    let toLight (c: Color) = 
        match c with
        | :? DeviceRgb as rgb -> 
            let r,g,b = 
                let cs = rgb.GetColorValue()
                cs |> Seq.map float |> List.ofSeq |> fun cs -> cs.[0],cs.[1],cs.[2]
            let input = new RGBColor(r,g,b)
            converter.ToLab(input).L
        | :? DeviceGray as gray ->
            gray.GetColorValue().[0] |> float |> fun n -> n * 100.
        | _ -> failwith "Not implemented"

    [<RequireQualifiedAccess>]
    module DeviceRgb =
        open Colourful

        let fromLab (lab: LabColor) =
            let rgb = converter.ToRGB(lab)
            let r = float32 rgb.R
            let g = float32 rgb.G
            let b = float32 rgb.B
            DeviceRgb(r,g,b)



[<RequireQualifiedAccess>]
module DeviceRgb =
    open iText.Kernel.Colors
    let magenta = DeviceRgb(255,0,255)
    
[<RequireQualifiedAccess>]
module DeviceGray =
    open iText.Kernel.Colors
    let pantoneSignature = DeviceGray(0.754f)

[<RequireQualifiedAccess>]
module DeviceCmyk =
    open iText.Kernel.Colors
    let white = DeviceCmyk(0,0,0,0)

[<RequireQualifiedAccess>]
module Separation =
    open iText.Kernel.Colors
    open iText.Kernel.Pdf.Colorspace
    let equal (c1: Separation) (c2: Separation) =
        let c1ColorSpace = c1.GetColorSpace().GetPdfObject() :?> PdfArray |> Seq.map string |> String.concat "-"
        let c2ColorSpace = c2.GetColorSpace().GetPdfObject() :?> PdfArray |> Seq.map string |> String.concat "-"
        let b = c1ColorSpace = c2ColorSpace && c1.GetColorValue() = c2.GetColorValue()
        b

    let colorName (color: Separation) =
        match color.GetColorSpace() with 
        | :? PdfSpecialCs.Separation as colorSpace ->
            let array = colorSpace.GetPdfObject() :?> PdfArray
            array.GetAsName(1) |> ColorBook.normalizePdfName
        | _ -> Logger.notImplemented()


[<RequireQualifiedAccess>]
module Color =
    open Fake.IO
    open System
    open Atrous.Pdf.Parser
    open iText.Kernel.Pdf.Colorspace
    open iText.Kernel
    open iText.Kernel.Pdf.Canvas.Parser
    open iText.Kernel.Colors
    open Extensions
    open Fake.IO.Globbing.Operators
    open System.IO
    open iText.Layout.Element
    open Atrous.Utils

    let isCmyk (color: Color) =
        match color with 
        | :? DeviceCmyk -> true
        | _ -> false

    let toColoredText (text: string) color =
        let t = new Text(text)
        t.SetFontColor(color)

    let _registion =
        let file = Path.getFullName "Resources/Colors/registion.pdf"
        let doc = new PdfDocument(new PdfReader(file))
        let parser = new PdfDocumentContentParser(doc)
        let infos = Extract.paths 1 (fun _ -> true) parser
        let info = infos |> Seq.exactlyOne
        info.GetStrokeColor() :?> Separation

    let injectSeperation (writer: PdfDocument) (color: Separation) =
        let colorSpace = 
            let spcs = 
                lock writer (fun _ ->
                    color.GetColorSpace().GetPdfObject().CopyTo(writer)
                )
            PdfColorSpace.MakeColorSpace(spcs)
        Color.MakeColor(colorSpace,_registion.GetColorValue())

    let registion (writer:PdfDocument) =
        injectSeperation writer _registion
        
    let isSeparation (c: Color) =
        match c with 
        | :? Colors.Separation -> true 
        | _ -> false

    let asSeparation (c: Color) =
        match c with 
        | :? Colors.Separation as sepa -> Some sepa
        | _ -> None

    let equal (c1: Color) (c2: Color) =
        let isSeparationEqual = 
            match c1 with 
            | :? Colors.Separation as c1 ->
                match c2 with 
                | :? Colors.Separation as c2 -> Separation.equal c1 c2
                | _ -> false
            | _ -> false
        isSeparationEqual || c1 = c2
        
    let comparer =
        CustomEqualityComparer<Color>(equal)

    let _colorBook text =
        let file = !!("Resources/Colors/*/*.pdf") |> Seq.find (fun path ->
            let fileName = Path.GetFileNameWithoutExtension (path)
            fileName = text
        )
        let doc = new PdfDocument(new PdfReader(file))
        let parser = new PdfDocumentContentParser(doc)
        let infos = Extract.paths 2 (fun _ -> true) parser
        let info = infos |> Seq.exactlyOne
        info.GetFillColor() :?> Separation

    let colorBook text writer =
        injectSeperation writer (_colorBook text)

    let _pantone (pantoneEnum: PantoneColorEnum) =
        let text = pantoneEnum.ToString()
        _colorBook text

    let pantone (pantoneEnum: PantoneColorEnum) writer =
        injectSeperation writer (_pantone pantoneEnum)

[<RequireQualifiedAccess>]
module Colors =
    open iText.Kernel.Colors
    open Atrous.Pdf.Extensions
    open Atrous.Extensions
    let distinct (colors: Color seq) =
        colors |> Seq.disctintByComparer Color.comparer
        
    let contain c cs =
        cs |> List.exists (Color.equal c)

    let except cs1 cs2 =
        cs2 |> List.filter (fun c -> 
            contain c cs1 
            |> not
        )

type ColorMap =
    | Red = 0
    | Black = 1
    | Magenta = 2


[<RequireQualifiedAccess>]
module ColorMap =
    open iText.Kernel.Colors
    open System.Collections.Generic

    let colorMaps : IDictionary<ColorMap,Color list> =
        [
            ColorMap.Red,[DeviceCmyk(0,100,100,0) :> Color]
            ColorMap.Black,[DeviceCmyk.BLACK;DeviceGray.BLACK]
            ColorMap.Magenta,[DeviceCmyk.MAGENTA]
        ] |> dict

    let findCmykColor s =
        colorMaps.[s] |> Seq.find Color.isCmyk

    let ofColor (color: Color) =
        let pair = 
            colorMaps |> Seq.find (fun (KeyValue(k,v)) ->
                v |> List.exists (Color.equal color)
            )
        pair.Key

    let toZH (c: ColorMap) =
        match c with 
        | ColorMap.Black -> "黑色"
        | ColorMap.Red -> "红色"
        | ColorMap.Magenta -> "玫红色"
        | _ -> Logger.invalidToken()

    let toColoredText s =
        let color = findCmykColor s
        let s = toZH s
        Color.toColoredText s color

[<RequireQualifiedAccess>]
type ColoredText =
    | Common of iText.Layout.Element.Text
    | FromDoc of (PdfDocument -> iText.Layout.Element.Text)

[<RequireQualifiedAccess>]
module ColoredText =
    let toTextElement doc coloredText =
        match coloredText with 
        | ColoredText.Common text -> text
        | ColoredText.FromDoc textGenerator -> textGenerator doc

[<RequireQualifiedAccess>]
type ColorCard =
    | Enum of ColorMap
    | Pantone of PantoneColorEnum
    | TPX of TPXColorEnum
    
[<RequireQualifiedAccess>]
module ColorCard =
    open iText.Kernel.Colors
    open System

    let parse (colorName: string) =
        match Enum.TryParse(colorName) with
        | true,color -> ColorCard.Pantone color 
        | false,_ -> 
            match Enum.TryParse(colorName) with 
            | true,color -> ColorCard.TPX color
            | false,_ -> failwithf "Cannot find colorCard by %s" colorName

    let ofColor (color: Color) =
        match color with 
        | :? Separation as separation ->
            let name = Separation.colorName separation
            parse name
        | :? DeviceCmyk | :? DeviceRgb | :? DeviceGray ->
            ColorMap.ofColor color |> ColorCard.Enum
        | _ -> Logger.notImplemented()


[<RequireQualifiedAccess>]
type DesiredColor =
    | CMYK
    | Single of ColorCard
    | Double of ColorCard * ColorCard

[<RequireQualifiedAccess>]
module DesiredColor =
    let black =
        ColorMap.Black 
        |> ColorCard.Enum
        |> DesiredColor.Single 

    let isBlack = function 
        | DesiredColor.Single colorCard ->
            match colorCard with 
            | ColorCard.Enum enum when enum = ColorMap.Black -> true
            | _ -> false
        | _ -> false

    let (|Black|_|) dc =
        if isBlack dc then Some dc else None

    let isCmyk dc =
        match dc with 
        | DesiredColor.CMYK -> true
        | _ -> false
