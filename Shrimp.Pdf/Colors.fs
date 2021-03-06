﻿namespace Shrimp.Pdf.Colors
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
open iText.Kernel.Pdf.Function
open iText.Kernel.Pdf.Colorspace
open Resources

[<AutoOpen>]
module _Colors =

    [<RequireQualifiedAccess>]
    type ValueEqualOptions =
        | Exactly
        | RoundedValue of digits: int

    with 
        static member DefaultRoundedValue = ValueEqualOptions.RoundedValue 3

    type ColorSpace =
        | Gray = 0
        | Rgb = 1
        | Cmyk = 2
        | Lab = 3

    let private whitePoint = 
        lazy 
            config.Value.GetFloatList("shrimp.pdf.colors.labWhitePoint")
            |> Array.ofSeq

    type FsDeviceRgb =
        { R: float32 
          G: float32 
          B: float32 }

    type FsLab =
        { L: float32 
          a: float32
          b: float32 }
    with 

        member x.ToItextColor(?colorSpace: PdfCieBasedCs.Lab) =
            match colorSpace with 
            | Some colorSpace ->
                new Lab(
                    colorSpace,
                    [| x.L; x.a; x.b |]) :> Color

            | None -> 
                new Lab(
                    whitePoint.Value,
                    null,
                    [| -128.f; 127.f; -128.f; 127.f |], 
                    [| x.L; x.a; x.b |]) :> Color

        static member private OfHex(hex: int) =
            let l = hex >>> 16 |> byte |> float32
            let a = hex >>> 8 |> byte |> float32
            let b = hex >>> 0 |> byte |> float32
            let a2 = a - 128.f
            let b2 = b - 128.f
            { L = l 
              a = a2 
              b = b2 }

        static member OfPantone(pantoneColor: PantoneColorEnum) =
            FsLab.OfHex (int pantoneColor)

        static member OfTpx(pantoneColor: TPXColorEnum) =
            FsLab.OfHex (int pantoneColor)


    type FsDeviceCmyk =
        { C: float32 
          M: float32
          Y: float32 
          K: float32 }

    type FsGray = FsGray of float32

    [<RequireQualifiedAccess>]
    type FsValueColor =
        | Rgb of FsDeviceRgb
        | Cmyk of FsDeviceCmyk
        | Lab of FsLab
        | Gray of FsGray
    with 

        member x.MapColorValue(mapping) =
            match x with
            | FsValueColor.Rgb rgbColor ->
                { R = mapping rgbColor.R
                  G = mapping rgbColor.G
                  B = mapping rgbColor.B }
                |> FsValueColor.Rgb

            | FsValueColor.Cmyk cmykColor ->
                { C = mapping cmykColor.C 
                  M = mapping cmykColor.M 
                  Y = mapping cmykColor.Y
                  K = mapping cmykColor.K }
                |> FsValueColor.Cmyk

            | FsValueColor.Gray (FsGray grayColor) ->
                FsValueColor.Gray (FsGray (mapping grayColor))

            | FsValueColor.Lab (labColor) ->
                { L = mapping labColor.L
                  a = mapping labColor.a
                  b = mapping labColor.b }
                |> FsValueColor.Lab

        member x.GetColorValue() =
            match x with 
            | FsValueColor.Cmyk cmyk ->
                [ cmyk.C; cmyk.M; cmyk.Y; cmyk.K ]

            | FsValueColor.Gray (FsGray v) ->
                [ v ]

            | FsValueColor.Rgb rgb ->
                [ rgb.R; rgb.G; rgb.B ]

            | FsValueColor.Lab lab -> [ lab.L; lab.a; lab.b ]

        member x.IsEqualTo(y: FsValueColor, valueEqualOptions: ValueEqualOptions) =
            FsValueColor.IsEqual (x, y, valueEqualOptions)

        static member IsEqual (color1: FsValueColor, color2: FsValueColor, valueEqualOptions: ValueEqualOptions) =
            let isColorSpaceEqual =
                match color1, color2 with
                | FsValueColor.Rgb _, FsValueColor.Rgb _
                | FsValueColor.Cmyk _, FsValueColor.Cmyk _
                | FsValueColor.Lab _, FsValueColor.Lab _
                | FsValueColor.Gray _, FsValueColor.Gray _ -> true
                | _ -> false

            isColorSpaceEqual &&
                match valueEqualOptions with 
                | ValueEqualOptions.Exactly -> 
                    let colorValue1 =
                        color1.GetColorValue()

                    let colorValue2 =
                        color2.GetColorValue()

                    colorValue1 = colorValue2

                | ValueEqualOptions.RoundedValue digits ->
                    let colorValue1 =
                        color1.GetColorValue()
                        |> List.map (fun v -> System.Math.Round(float v, digits) )

                    let colorValue2 =
                        color2.GetColorValue()
                        |> List.map (fun v -> System.Math.Round(float v, digits) )

                    colorValue1 = colorValue2

        static member Invert = function
            | FsValueColor.Rgb rgbColor ->
                { R = 1.f - rgbColor.R
                  G = 1.f - rgbColor.G
                  B = 1.f - rgbColor.B }
                |> FsValueColor.Rgb

            | FsValueColor.Cmyk cmykColor ->
                { C = 1.f - cmykColor.C 
                  M = 1.f - cmykColor.M 
                  Y = 1.f - cmykColor.Y
                  K = 1.f - cmykColor.K }
                |> FsValueColor.Cmyk

            | FsValueColor.Gray (FsGray grayColor) ->
                FsValueColor.Gray (FsGray (1.f - grayColor))

            | FsValueColor.Lab (labColor) ->
                { L = 100.f - labColor.L
                  a = -labColor.a
                  b = -labColor.b }
                |> FsValueColor.Lab

        static member ToItextColor fsValueColor =
            match fsValueColor with
            | FsValueColor.Cmyk cmyk ->
                new DeviceCmyk(cmyk.C, cmyk.M, cmyk.Y, cmyk.K) :> Color

            | FsValueColor.Gray (FsGray v) ->
                let grayColor = new DeviceGray()
                grayColor.SetColorValue([|v|]) 
                grayColor :> Color

            | FsValueColor.Rgb rgb ->
                new DeviceRgb(rgb.R, rgb.G, rgb.B) :> Color

            | FsValueColor.Lab lab -> lab.ToItextColor()

        static member OfItextColor (color: Color) = 
            match color with 
            | :? DeviceCmyk as color -> 
                let colorValues = color.GetColorValue()
                { C = colorValues.[0] 
                  M = colorValues.[1] 
                  Y = colorValues.[2]
                  K = colorValues.[3] }
                |> FsValueColor.Cmyk

            | :? DeviceRgb as color ->
                let colorValues = color.GetColorValue()
                { R = colorValues.[0] 
                  G = colorValues.[1] 
                  B = colorValues.[2] }
                |> FsValueColor.Rgb

            | :? DeviceGray as color ->
                let colorValues = color.GetColorValue()
                FsGray colorValues.[0]
                |> FsValueColor.Gray

            | :? Lab as color ->
                let colorValues = color.GetColorValue()
                { 
                    L = colorValues.[0]
                    a = colorValues.[1]
                    b = colorValues.[2]
                }
                |> FsValueColor.Lab

            | _ -> failwithf "Cannot convert %s to fsValueColor" (color.GetType().FullName)

    type FsSeparation =
        { Name: string 
          Color: FsValueColor
          Transparency: float }
    with 
        static member Create(name: string, color: Color, ?transparency) =
            { Name = name 
              Color = FsValueColor.OfItextColor color
              Transparency = defaultArg transparency 1. }

        static member Create(name: string, color: FsLab, ?transparency) =
            { Name = name 
              Color = FsValueColor.Lab color
              Transparency = defaultArg transparency 1. }


        static member Create(name: string, color: FsValueColor, ?transparency) =
            { Name = name 
              Color = color
              Transparency = defaultArg transparency 1. }

        static member Registration =
            { Name = "All";
              Color = 
                FsValueColor.Cmyk 
                    { C = 1.f 
                      M = 1.f 
                      Y = 1.f 
                      K = 1.f  }
              Transparency = 1.
            }

        static member IsEqual(color1: FsSeparation, color2: FsSeparation, valueEqualOptions) =
            color1.Name = color2.Name
            && color1.Transparency = color2.Transparency
            && FsValueColor.IsEqual (color1.Color, color2.Color, valueEqualOptions)


        static member OfPantone(color: PantoneColorEnum) =
            let fsValueColor = FsLab.OfPantone color
    
            let separationName1 = color.ToString()
            
            { Color = FsValueColor.Lab fsValueColor
              Name = separationName1
              Transparency = 1. }

        static member OfTpx(color: TPXColorEnum) =
            let fsValueColor = FsLab.OfTpx color
    
            let separationName1 = color.ToString()
            
            { Color = FsValueColor.Lab fsValueColor
              Name = separationName1
              Transparency = 1. }

    [<RequireQualifiedAccess>]
    module DeviceRgb =
        let fromKnownColor(knownColor: KnownColor) =
            let color = Color.FromName(knownColor.ToString())
            new DeviceRgb(int color.R, int color.G, int color.B)
    
    
    type DeviceRgb with 
        static member MAGENTA = new DeviceRgb(1.f, 0.f, 1.f) :> Color

    type DeviceCmyk with
        static member WHITE = DeviceCmyk(0,0,0,0)
    
    let private (|PdfName|_|) (pdfName0: PdfName) (pdfName1: PdfName) =
        if pdfName0 = pdfName1 
        then Some ()
        else None

    type PdfSpecialCs.Separation with 
        member x.GetAlternateSpace() =
            let colorSpacePdfArray = x.GetPdfObject() :?> PdfArray
            match colorSpacePdfArray.Get(2) with
            | :? PdfArray as pdfArray -> pdfArray.GetAsName(0)
            | :? PdfName as pdfName -> pdfName 
            | _ -> failwith "Invalid token "

        member x.GetAlternateColorValue() =
            let colorSpacePdfArray = x.GetPdfObject() :?> PdfArray
            let colorSpacePdfFunction = 
                match colorSpacePdfArray.GetAsDictionary(3) with
                | null ->
                    colorSpacePdfArray.GetAsStream(3) :> PdfDictionary
                | v -> v
            let pdfFunctionType = colorSpacePdfFunction.GetAsInt(PdfName.FunctionType)

            match pdfFunctionType.Value with
            | 0 ->
                let alterColorSpace = x.GetAlternateSpace()
                let range = 
                    colorSpacePdfFunction.GetAsArray(PdfName.Range)
                    |> List.ofSeq
                    |> List.map (fun m -> (m :?> PdfNumber).FloatValue())

                match alterColorSpace with 
                | PdfName PdfName.Lab ->

                    let getColorValue valueGroup = 
                        match valueGroup with 
                        | [0.f; v] -> v
                        | [v; 0.f] -> v
                        | _ -> failwith "Invalid token"

                    [ range.[0]; getColorValue range.[2..3]; getColorValue range.[4..5] ]

                | PdfName PdfName.DeviceCMYK ->
                    [ range.[1]; range.[3]; range.[5]; range.[7] ]

                | _ -> failwith "Not implemnted"
            | 2 ->
                colorSpacePdfFunction.GetAsArray(PdfName.C1)
                |> List.ofSeq
                |> List.map (fun m -> (m :?> PdfNumber).FloatValue())

            | _ -> failwith "Not implemnted"

        member private x.GetAlterateColor(colorValue: float32 list) =
            match x.GetAlternateSpace() with 
            | PdfName PdfName.Lab -> 
                { L = colorValue.[0]
                  a = colorValue.[1]
                  b = colorValue.[2] }
                |> FsValueColor.Lab

            | PdfName PdfName.DeviceRGB ->
                { R = colorValue.[0] 
                  G = colorValue.[1]
                  B = colorValue.[2] }
                |> FsValueColor.Rgb

            | PdfName PdfName.DeviceGray ->
                FsValueColor.Gray(FsGray colorValue.[0])

            | PdfName PdfName.DeviceCMYK ->
                { C = colorValue.[0] 
                  M = colorValue.[1]
                  Y = colorValue.[2] 
                  K = colorValue.[3] }
                |> FsValueColor.Cmyk

            | _-> failwith "Not implemented"

        member x.GetAlterateColor() =
            x.GetAlterateColor(x.GetAlternateColorValue())

    type Separation with 


        member separation.GetAlterateColor() =
            let colorSpace = separation.GetColorSpace() :?> PdfSpecialCs.Separation
            let color = 
                colorSpace.GetAlterateColor()

            color.MapColorValue(fun color -> color * (separation.GetColorValue().[0]))

        static member private Range(color: Color) =
            let colorValues = color.GetColorValue()
            match color with 
            | :? Lab ->
                [| 0.f; 100.f; -128.f; 127.f; -128.f; 127.f |]

            | :? DeviceRgb ->
                [| colorValues.[0]; 1.f; colorValues.[1]; 1.f; colorValues.[2]; 1.f |]

            | :? DeviceGray ->
                [| colorValues.[0]; 1.f |]

            | :? DeviceCmyk ->
                [| 0.f; colorValues.[0];  0.f; colorValues.[1]; 0.f; colorValues.[2]; 0.f; colorValues.[3]; |]

            | _ -> failwithf "Invalid token"


        static member Create(name, color: DeviceCmyk, ?transparency: float) =
            let transparency = defaultArg transparency 1.
            let range = Separation.Range color

            let separationPdfFunction =
                new PdfFunction.Type2(
                    PdfArray([| 0; 1 |]), 
                    PdfArray(range),
                    PdfArray([|0; 0; 0; 0|]),
                    PdfArray(color.GetColorValue()),
                    PdfNumber(1)
                )
            let colorSpace = new PdfSpecialCs.Separation(name, new PdfDeviceCs.Cmyk(), separationPdfFunction)
            new Separation(colorSpace, float32 transparency)

        static member Create(name, color: DeviceRgb, ?transparency: float) =
            let transparency = defaultArg transparency 1.
            let separationPdfFunction =
                let range = Separation.Range color
                new PdfFunction.Type2(
                    PdfArray([| 0; 1 |]), 
                    PdfArray(range),
                    PdfArray([|1; 1; 1|]),
                    PdfArray(color.GetColorValue()),
                    PdfNumber(1)
                )

            let colorSpace = new PdfSpecialCs.Separation(name, PdfDeviceCs.Rgb(), separationPdfFunction)
            new Separation(colorSpace, float32 transparency)

        static member Create(name, color: DeviceGray, ?transparency: float) =
            let transparency = defaultArg transparency 1.
            let range = Separation.Range color

            let separationPdfFunction =
                new PdfFunction.Type2(
                    PdfArray([| 0; 1 |]), 
                    PdfArray(range),
                    PdfArray([|1|]),
                    PdfArray(color.GetColorValue()),
                    PdfNumber(1)
                )
            let colorSpace = new PdfSpecialCs.Separation(name, new PdfDeviceCs.Gray(), separationPdfFunction)
            new Separation(colorSpace, float32 transparency)

        static member Create(name, color: Lab, ?transparency: float) =
            let transparency = defaultArg transparency 1.
            let separationPdfFunction =
                let range = Separation.Range color
                new PdfFunction.Type2(
                    PdfArray([| 0; 1 |]), 
                    PdfArray(range),
                    PdfArray([|100; 0; 0|]),
                    PdfArray(color.GetColorValue()),
                    PdfNumber(1)
                )

            let colorSpace = new PdfSpecialCs.Separation(name, color.GetColorSpace(), separationPdfFunction)
            new Separation(colorSpace, float32 transparency)


        static member Create(name, color: Color, ?transparency: float) =
            match color with 
            | :? DeviceRgb as color -> Separation.Create(name, color, ?transparency = transparency)
            | :? DeviceCmyk as color ->  Separation.Create(name, color, ?transparency = transparency)
            | :? DeviceGray as color -> Separation.Create(name, color, ?transparency = transparency)
            | :? Lab as color -> Separation.Create(name, color, ?transparency = transparency)
            | _ -> failwithf "cannot create separation from color %s" (color.GetType().FullName)


    type FsSeparation with 

        static member OfSeparation(separation: Separation) =
            let colorSpace = separation.GetColorSpace() :?> PdfSpecialCs.Separation
    
            let colorSpacePdfArray = 
                colorSpace.GetPdfObject() :?> PdfArray
    
            let colorName = 
                let uri = 
                    (colorSpacePdfArray.Get(1)
                     |> string).TrimStart('/')
                uri.Replace("#20", " ")
    
            FsSeparation.Create(colorName, separation.GetAlterateColor())
       

        member separation1.IsEqualTo(color: Color, valueEqualOptions) =
            match color with 
            | :? Separation as separation ->
                let separation0 = FsSeparation.OfSeparation separation
                FsSeparation.IsEqual(separation0, separation1, valueEqualOptions)

            | _ -> false

        static member Contains(valueEqualOptions) =
            fun (color: Color) (fsSeparations: FsSeparation list) ->
                fsSeparations
                |> List.exists (fun m -> m.IsEqualTo(color, valueEqualOptions))

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
    
        let isGray (color: Color) =
            match color with 
            | :? DeviceGray -> true
            | _ -> false

        let isRgb (color: Color) =
            match color with 
            | :? DeviceRgb -> true
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
    
        let equal (c1: Color) (c2: Color) =
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
    
        let fromKnownColor (knownColor: KnownColor) =
            match knownColor with 
            | KnownColor.Black -> DeviceGray.BLACK :> Color
            | KnownColor.White -> DeviceGray.WHITE :> Color
            | _ -> DeviceRgb.fromKnownColor knownColor :> Color

        let (|EqualTo|_|) color1 color2 =
            if equal color1 color2 then Some ()
            else None

    [<RequireQualifiedAccess>]
    module Colors =
        let distinct (colors: Color seq) =
            let comparer =
                { new IEqualityComparer<Color> with 
                    member __.Equals(x,y) = 
                        Color.equal x y
    
                    member __.GetHashCode(_) = 0
                }
    
            colors.Distinct(comparer)
            
        let contains color colors =
            colors |> List.exists (Color.equal color)
    
        let tryFindIndex color colors =
            colors |> List.tryFindIndex (Color.equal color)

        let except colors1 colors2 =
            colors2 |> List.filter (fun c -> 
                contains c colors1 
                |> not
            )


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
            | PdfCanvasColor.Separation separation1 -> separation1.IsEqualTo(color, ValueEqualOptions.DefaultRoundedValue)
            | PdfCanvasColor.ColorCard colorCard1 ->
    
                match colorCard1 with 
                | ColorCard.Pantone _ 
                | ColorCard.TPX _ ->
                    let separation1 =
                        match colorCard1 with 
                        | ColorCard.Pantone pantoneColor1 -> FsSeparation.OfPantone pantoneColor1
                        | ColorCard.TPX tpxColor1 -> FsSeparation.OfTpx tpxColor1
                        | ColorCard.KnownColor knownColor -> failwith "Invalid token"
                    
                    separation1.IsEqualTo(color, ValueEqualOptions.RoundedValue 0)
                | ColorCard.KnownColor knownColor1 ->
                    let itextColor1 = 
                        (Color.fromKnownColor knownColor1)
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
                 FsSeparation.Registration.IsEqualTo(color, ValueEqualOptions.RoundedValue 0)

        static member Contains(color: Color) (pdfCanvasColor: PdfCanvasColor list) =
            pdfCanvasColor
            |> List.exists(fun pdfCanvasColor -> pdfCanvasColor.IsEqualTo(color))
  



    type Color with 
        member x.IsEqualTo(fsSeparation: FsSeparation, valueEqualOptions) =
            fsSeparation.IsEqualTo(x, valueEqualOptions)

        member x.GetFsColorSpace() =
            match x with 
            | :? DeviceCmyk -> ColorSpace.Cmyk
            | :? DeviceGray -> ColorSpace.Gray
            | :? DeviceRgb -> ColorSpace.Rgb
            | :? Lab -> ColorSpace.Lab 
            | :? Separation as separation -> 
                let alterateColor = 
                    separation.GetAlterateColor()
                    |> FsValueColor.ToItextColor
                alterateColor.GetFsColorSpace()
            | _ -> failwithf "Cannot get colorspace from %A" x


        member x.IsInColorSpace(colorSpace: ColorSpace) =
            x.GetFsColorSpace() = colorSpace
         

    [<RequireQualifiedAccess>]
    type FsItextPersistableColor =
        | Value of Color
        | Separation of FsSeparation

    with 
        static member OfItextColor(color: Color) =
            match color with 
            | :? Separation as separationColor ->
                FsSeparation.OfSeparation separationColor
                |> FsItextPersistableColor.Separation

            | _ -> FsItextPersistableColor.Value (color)

        
        static member IsEqual (color1: FsItextPersistableColor, color2: FsItextPersistableColor, valueEqualOptions) =
            match color1, color2 with 
            | FsItextPersistableColor.Value color1, FsItextPersistableColor.Value color2 ->
                FsValueColor.IsEqual(FsValueColor.OfItextColor color1, FsValueColor.OfItextColor color2, valueEqualOptions)
         
            | FsItextPersistableColor.Separation color1, FsItextPersistableColor.Separation color2 ->
                FsSeparation.IsEqual (color1, color2, valueEqualOptions)

            | _ -> false

        static member Contains(valueEqualOptions) =
            fun (color: FsItextPersistableColor) (colors: FsItextPersistableColor list) ->
                colors 
                |> List.exists(fun color1 ->
                    FsItextPersistableColor.IsEqual(color, color1, valueEqualOptions)
                )

        static member Except valueEqualOptions =
            fun (excepts: FsItextPersistableColor list) (colors: FsItextPersistableColor list) ->
                colors 
                |> List.filter(fun color ->
                    not (FsItextPersistableColor.Contains valueEqualOptions color excepts)
                )