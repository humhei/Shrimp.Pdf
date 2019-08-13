namespace Shrimp.Pdf.Colors
open iText.Kernel.Pdf
open Shrimp.Pdf
open Colourful.Conversion
open iText.Kernel.Colors
open Colourful
open System
open iText.Kernel
open Extensions
open System.Linq
open System.Collections.Generic


[<RequireQualifiedAccess>]
module Colourful =

    let private avaliableRGBWorkingSpacesTexts =
        [
            "SRGB"           
            "PROPHOTORGB"    
            "PALSECAMRGB"    
            "NTSCRGB"        
            "EKTASPACEPS5"   
            "DONRGB4"        
            "COLORMATCHRGB"  
            "CIERGB"         
            "SMPTECRGB"      
            "BRUCERGB"       
            "BESTRGB"        
            "APPLESRGB"      
            "ADOBERGB1998"   
            "ECIRGBV2"       
            "REC2020"        
            "REC709"         
            "SRGBSIMPLIFIED" 
            "BETARGB"        
            "WIDEGAMUTRGB"   
        ]

    let targetRGBWoringSpace =
        let text = config.GetString("shrimp.pdf.colourful.targetRGBWorkingSpace")
        match text.ToUpper() with 
        | "SRGB"            -> RGBWorkingSpaces.sRGB
        | "PROPHOTORGB"     -> RGBWorkingSpaces.ProPhotoRGB
        | "PALSECAMRGB"     -> RGBWorkingSpaces.PALSECAMRGB
        | "NTSCRGB"         -> RGBWorkingSpaces.NTSCRGB
        | "EKTASPACEPS5"    -> RGBWorkingSpaces.EktaSpacePS5
        | "DONRGB4"         -> RGBWorkingSpaces.DonRGB4
        | "COLORMATCHRGB"   -> RGBWorkingSpaces.ColorMatchRGB
        | "CIERGB"          -> RGBWorkingSpaces.CIERGB
        | "SMPTECRGB"       -> RGBWorkingSpaces.SMPTECRGB
        | "BRUCERGB"        -> RGBWorkingSpaces.BruceRGB
        | "BESTRGB"         -> RGBWorkingSpaces.BestRGB
        | "APPLESRGB"       -> RGBWorkingSpaces.ApplesRGB
        | "ADOBERGB1998"    -> RGBWorkingSpaces.AdobeRGB1998
        | "ECIRGBV2"        -> RGBWorkingSpaces.ECIRGBv2
        | "REC2020"         -> RGBWorkingSpaces.Rec2020
        | "REC709"          -> RGBWorkingSpaces.Rec709
        | "SRGBSIMPLIFIED"  -> RGBWorkingSpaces.sRGBSimplified
        | "BETARGB"         -> RGBWorkingSpaces.BetaRGB
        | "WIDEGAMUTRGB"    -> RGBWorkingSpaces.WideGamutRGB
        | _ -> failwithf "targetRGBWoringSpace: expect one of %A" avaliableRGBWorkingSpacesTexts

    let converter = 
        ColourfulConverter(TargetRGBWorkingSpace = targetRGBWoringSpace)

    let private avaliableWhitePoints =
        [
            "A"
            "B"
            "C"
            "D50"
            "D55"
            "D65"
            "D75"
            "E"
            "F2"
            "F7"
            "F11"
        ]


    converter.WhitePoint <- 
        match config.GetString("").ToUpper() with
        |"A"    -> Illuminants.A
        |"B"    -> Illuminants.B
        |"C"    -> Illuminants.C
        |"D50"  -> Illuminants.D50
        |"D55"  -> Illuminants.D55
        |"D65"  -> Illuminants.D65
        |"D75"  -> Illuminants.D75
        |"E"    -> Illuminants.E
        |"F2"   -> Illuminants.F2
        |"F7"   -> Illuminants.F7
        |"F11"  -> Illuminants.F11
        | _ -> failwithf "whitePoint: expect one of %A" avaliableWhitePoints


[<RequireQualifiedAccess>]
module LabColor =

    /// encode to colorbook enum value
    let encode (labColor: LabColor) =
        sprintf "0x%02x%02x%02x" (int labColor.L) (int labColor.a + 128) (int labColor.b + 128)

    /// decode from colorbook enum value
    let decode (hex: int) =
        let l = hex >>> 16 |> byte |> int
        let a = hex >>> 8 |> byte |> int
        let b = hex >>> 0 |> byte |> int
        let a2 = a - 128
        let b2 = b - 128
        [l; a2; b2] |> List.map float |> LabColor



[<RequireQualifiedAccess>]
module DeviceRgb =
    let ofLab (lab: LabColor) =
        let rgb = Colourful.converter.ToRGB(lab)
        let r = float32 rgb.R
        let g = float32 rgb.G
        let b = float32 rgb.B
        DeviceRgb(r,g,b)


[<RequireQualifiedAccess>]
module DeviceCmyk =
    let WHITE = DeviceCmyk(0,0,0,0)

[<RequireQualifiedAccess>]
module Separation =
    let equal (c1: Separation) (c2: Separation) =
        let c1ColorSpace = c1.GetColorSpace().GetPdfObject() :?> PdfArray |> Seq.map string |> String.concat "-"
        let c2ColorSpace = c2.GetColorSpace().GetPdfObject() :?> PdfArray |> Seq.map string |> String.concat "-"
        let b = c1ColorSpace = c2ColorSpace && c1.GetColorValue() = c2.GetColorValue()
        b

[<RequireQualifiedAccess>]
module Color =

    let isCmyk (color: Color) =
        match color with 
        | :? DeviceCmyk -> true
        | _ -> false


    let registion (writer:PdfDocument) =
        PdfDocument.obtainSperationColorFromResources "registration" writer
        
    let isSeparation (c: Color) =
        match c with 
        | :? Colors.Separation -> true 
        | _ -> false

    let asSeparation (c: Color) =
        match c with 
        | :? Colors.Separation as sepa -> Some sepa
        | _ -> None

    let isValueEqual (c1: Color) (c2: Color) =
        let isSeparationEqual = 
            match c1 with 
            | :? Colors.Separation as c1 ->
                match c2 with 
                | :? Colors.Separation as c2 -> Separation.equal c1 c2
                | _ -> false
            | _ -> false
        isSeparationEqual || c1 = c2

    let pantoneSolidCoated (pantoneEnum: PantoneColorEnum) writer =
        PdfDocument.obtainSperationColorFromResources (@"Pantone+ Solid Coated/" + pantoneEnum.ToString()) writer

    let pantoneTPX (tpxColorEnum: TPXColorEnum) writer =
        PdfDocument.obtainSperationColorFromResources (@"TPX/" + tpxColorEnum.ToString()) writer


[<RequireQualifiedAccess>]
module Colors =
    let distinct (colors: Color seq) =
        let comparer =
            { new IEqualityComparer<Color> with 
                member __.Equals(x,y) = 
                    Color.isValueEqual x y

                member __.GetHashCode(_) = 0
            }

        colors.Distinct(comparer)
        
    let contain color colors =
        colors |> List.exists (Color.isValueEqual color)

    let except colors1 colors2 =
        colors2 |> List.filter (fun c -> 
            contain c colors1 
            |> not
        )

[<RequireQualifiedAccess>]
type PrintingColor =
    | CMYK
    | Single of ColorCard
    | Double of ColorCard * ColorCard

[<RequireQualifiedAccess>]
module PrintingColor =
    let BLACK =
        KnownColor.Black
        |> ColorCard.KnownColor
        |> PrintingColor.Single 

    let isBlack = function 
        | PrintingColor.Single colorCard ->
            match colorCard with 
            | ColorCard.KnownColor enum when enum = KnownColor.Black -> true
            | _ -> false
        | _ -> false

    let isCmyk printingColor =
        match printingColor with 
        | PrintingColor.CMYK -> true
        | _ -> false

