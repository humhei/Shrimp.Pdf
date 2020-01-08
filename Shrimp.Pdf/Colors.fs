namespace Shrimp.Pdf.Colors
open System.Drawing
open iText.Kernel.Pdf
open Shrimp.Pdf
open iText.Kernel.Colors
open System
open iText.Kernel
open Shrimp.Pdf.Extensions
open System.Linq
open System.Collections.Generic
open iText.Kernel.Colors


type LabColor =
    { L: float 
      a: float
      b: float }


[<RequireQualifiedAccess>]
module DeviceRgb =

    let fromKnownColor(knownColor: KnownColor) =
        let color = Color.FromKnownColor(knownColor)
        new DeviceRgb(int color.R, int color.G, int color.B)

    let MAGENTA = new DeviceRgb(1.f, 0.f, 1.f) :> Color

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

