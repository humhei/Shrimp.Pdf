namespace Shrimp.Pdf.Colors

open iText.Kernel.Exceptions

#nowarn "0104"
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
open FParsec
open FParsec.CharParsers

open System.Collections.Concurrent
open Shrimp.FSharp.Plus
open Shrimp.FSharp.Plus.Text
open Shrimp.Pdf.icms2
open Shrimp.FSharp.Plus

[<AutoOpen>]
module _Colors =


    type DeviceRgb with 
        static member OfHex(hex: int) =
            let color = System.Drawing.Color.FromArgb(hex)
            DeviceRgb(int color.R, int color.G, int color.B)


    type Icc with 
        member x.ColorSpace = 
            match x with 
            | Icc.Rgb _-> ColorSpace.Rgb
            | Icc.Cmyk _-> ColorSpace.Cmyk
            | Icc.Lab _-> ColorSpace.Lab
            | Icc.Gray _-> ColorSpace.Gray

    type ToleranceColorValue = ToleranceColorValue of float
    with    
        member x.Value =
            let (ToleranceColorValue v) = x
            v

    /// Unserializable
    type NearbyColorValue(v: float, tolerance: ToleranceColorValue) =
        inherit CustomEquatable<float>(v, fun a b ->
            (abs (a-b)) <= tolerance.Value
        )

        member x.Value = v


    type ValueEqualOptionsTolerance = 
        { Rgb: ToleranceColorValue 
          Cmyk: ToleranceColorValue
          Lab: ToleranceColorValue
          Gray: ToleranceColorValue}
    with    

        

        member x.Item(colorSpace: ColorSpace) =
            match colorSpace with 
            | ColorSpace.Gray -> x.Gray
            | ColorSpace.Lab -> x.Lab
            | ColorSpace.Cmyk -> x.Cmyk
            | ColorSpace.Rgb -> x.Rgb


        static member DefaultValue =
            { Rgb  =  ToleranceColorValue 0.0025 
              Cmyk = ToleranceColorValue 0.01
              Lab  = ToleranceColorValue 0.01
              Gray = ToleranceColorValue 0.0025 }

        static member Rough =
            { Rgb  =  ToleranceColorValue 0.01
              Cmyk = ToleranceColorValue  1.0
              Lab  = ToleranceColorValue  1.0
              Gray = ToleranceColorValue  0.01 }
            

    [<RequireQualifiedAccess>]
    type ValueEqualOptions =
        | Exactly
        | RoundedValue of ValueEqualOptionsTolerance

    with 
        static member DefaultRoundedValue = 
            ValueEqualOptions.RoundedValue(ValueEqualOptionsTolerance.DefaultValue)

        static member DefaultRough =
            ValueEqualOptions.RoundedValue(ValueEqualOptionsTolerance.Rough)
            





    let private whitePoint = 
        lazy 
            config.Value.GetFloatList("shrimp.pdf.colors.labWhitePoint")
            |> Array.ofSeq

    /// valueRange: Black 0 -> White 1
    type FsGray = FsGray of float32
    with 
        member x.Value =
            let (FsGray v) = x
            v

        static member BLACK = FsGray 0.0f
        static member WHITE = FsGray 1.0f
        static member GRAY = FsGray 0.5f
        
        member x.ToItextColor() =
            DeviceGray(x.Value)

        member x.LoggingText_Raw = 
            let (FsGray v) = x
            sprintf "K %.2f" v

        member x.LoggingText =
            match x with 
            | EqualTo FsGray.BLACK -> "K Black"
            | EqualTo FsGray.WHITE -> "K White"
            | _ -> x.LoggingText_Raw

        static member OfLoggingText_Raw(text) = 
            let parser = 
                pstringCI "K " >>. pfloat .>> eof

            match run parser text with 
            | Success (r, _, _) -> FsGray (float32 r)
            | Failure (error, _, _) -> failwith error

    /// valueRange: 0 -> 1
    type FsDeviceRgb =
        { R: float32 
          G: float32 
          B: float32 }
    with 
        member x.Range255 =
            {  R = x.R * 255.f 
               G = x.G * 255.f
               B = x.B * 255.f }

        member x.Values =
            [ x.R; x.G; x.B ]

        member x.Gray_Math =
            x.R * 0.2126f + x.G * 0.7152f + x.B * 0.0722f
            |> FsGray

        static member RED = { R = 1.0f; G = 0.0f; B = 0.0f }
        static member GREEN = { R = 0.0f; G = 1.0f; B = 0.0f }
        static member BLUE = { R = 0.0f; G = 0.0f; B = 1.0f }
        static member MAGENTA = { R = 1.0f; G = 0.0f; B = 1.0f }
        static member YELLOW = { R = 1.0f; G = 1.0f; B = 0.0f }
        static member BLACK = { R = 0.0f; G = 0.0f; B = 0.0f }
        static member WHITE = { R = 1.0f; G = 1.0f; B = 1.0f }
        static member GRAY = { R = 0.5f; G = 0.5f; B = 0.5f }

        member x.MapValue(mapping) =
            { R = mapping x.R
              G = mapping x.G 
              B = mapping x.B }




        static member Create(r, g, b) =
            let ensureValueValid v =
                match v with 
                | SmallerOrEqual 1.0f & BiggerOrEqual 0.0f -> ()
                | _ -> failwithf "Cannot create RGB from %A" (r, g, b)

            ensureValueValid r
            ensureValueValid g
            ensureValueValid b

            { R = r
              G = g
              B = b }


        static member Create(r, g, b) =
            FsDeviceRgb.Create(float32 r / 255.f, float32 g / 255.f, float32 b / 255.f)
        
        static member private RegularColorMapping =
            [ FsDeviceRgb.RED => "RED"           
              FsDeviceRgb.GREEN => "GREEN"        
              FsDeviceRgb.BLUE => "BLUE"         
              FsDeviceRgb.MAGENTA => "MAGENTA"   
              FsDeviceRgb.YELLOW => "YELLOW"     
              FsDeviceRgb.BLACK => "BLACK"       
              FsDeviceRgb.WHITE => "WHITE"       
              FsDeviceRgb.GRAY => "GRAY" ]

        member x.LoggingText_Raw = 
            let range255 = x.Range255
            sprintf "RGB %.0f %.0f %.0f" (range255.R) range255.G range255.B

        member x.LoggingText = 
            let colorName = 
                FsDeviceRgb.RegularColorMapping
                |> List.tryPick(fun (color, name) ->
                    match color = x with 
                    | true -> Some name
                    | false -> None
                )

            match colorName with 
            | Some colorName -> "RGB " + colorName
            | None -> x.LoggingText_Raw
                

        static member OfLoggingText_Raw(text: string) = 
            let parser = 
                pstringCI "RGB " >>. (sepBy1 pfloat spaces) .>> eof

            match run parser text with 
            | Success (r, _, _) -> FsDeviceRgb.Create(int r.[0], int r.[1], int r.[2])
            | Failure (error, _, _) -> failwith error

        static member FromKnownColor(knownColor: KnownColor) =
            let color = Color.FromName(knownColor.ToString())
            { R = float color.R / 255. |> float32 
              G = float color.G / 255. |> float32 
              B = float color.B / 255. |> float32 }

    [<RequireQualifiedAccess>]
    module FsDeviceRgb = 
        let (|Gray|Rgb|) (rgb: FsDeviceRgb) =
            match List.distinct (rgb.MapValue(fun m -> Math.Round(float m, 2) |> float32).Values) with 
            | [v] -> Gray 
            | _ -> Rgb

    type FsLab =
        { 
          /// 0 -> 100.
          L: float32 
          /// -128 -> 127
          a: float32
          /// -128 -> 127
          b: float32 }
    with 

        static member WHITE = { L = 100.f; a = 0.f; b = 0.f }

        static member BLACK = { L = 0.f; a = 0.f; b = 0.f }

        member x.Values =
            [ x.L; x.a; x.b ]

        member x.LoggingText_Raw = 
            sprintf "LAB %.1f %.1f %.1f" (x.L) x.a x.b

        member x.LoggingText = 
            match x with 
            | EqualTo FsLab.WHITE -> "LAB White"
            | EqualTo FsLab.BLACK -> "LAB Black"
            | _ -> x.LoggingText_Raw

        static member OfLoggingText_Raw(text) = 
            let parser = 
                pstringCI "LAB " >>. (sepBy1 pfloat spaces) .>> eof

            match run parser text with 
            | Success (r, _, _) -> 
                { L = r.[0] |> float32 
                  a = r.[1] |> float32 
                  b = r.[2] |> float32 }
            | Failure (error, _, _) -> failwith error

        static member Create(l, a, b) =
            { L = l 
              a = a
              b = b }

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


    /// valueRange: 0 -> 1
    type FsDeviceCmyk =
        { C: float32 
          M: float32
          Y: float32 
          K: float32 }
    with 
        member x.Range100 =
            { C = x.C * 100.f; M = x.M * 100.f; Y = x.Y * 100.f; K = x.K *100.f}

        member x.Values =
            [ x.C; x.M; x.Y; x.K ]

        static member Create(c, m, y, k) =
            { C = c; M = m; Y = y; K = k}
            

        static member CYAN = { C = 1.0f; M = 0.0f; Y = 0.0f; K = 0.0f }
        static member MAGENTA = { C = 0.0f; M = 1.0f; Y = 0.0f; K = 0.0f }
        static member YELLOW = { C = 0.0f; M = 0.0f; Y = 1.0f; K = 0.0f }
        static member BLACK = { C = 0.0f; M = 0.0f; Y = 0.0f; K = 1.0f }
        static member WHITE = { C = 0.0f; M = 0.0f; Y = 0.0f; K = 0.0f }
        static member GRAY = { C = 0.0f; M = 0.0f; Y = 0.0f; K = 0.5f }
        static member RED =  { C = 0.0f; M = 1.0f; Y = 1.0f; K = 0.0f }
        static member GREEN =  { C = 1.0f; M = 0.0f; Y = 1.0f; K = 0.0f }

        member x.LoggingText_Raw =
            let colorName = sprintf "%.2f %.2f %.2f %.2f" (x.C) x.M x.Y x.K
            "CMYK " + colorName
        
        /// Color => Literal NAME
        ///
        /// e.g: FsDeviceCmyk.CYAN => CYAN
        static member RegularColorMapping =
            [
                FsDeviceCmyk.CYAN => "CYAN"
                FsDeviceCmyk.RED => "RED"
                FsDeviceCmyk.GREEN => "GREEN"
                FsDeviceCmyk.MAGENTA => "MAGENTA"
                FsDeviceCmyk.YELLOW => "YELLOW"
                FsDeviceCmyk.BLACK => "BLACK"
                FsDeviceCmyk.WHITE => "WHITE"
                FsDeviceCmyk.GRAY   => "GRAY"
            ]

        member x.LoggingText = 
            let colorName = 
                FsDeviceCmyk.RegularColorMapping
                |> List.tryPick(fun (color, name) ->
                    match color = x with 
                    | true -> Some name
                    | false -> None
                )

            match colorName with 
            | Some colorName -> "CMYK " + colorName
            | None -> x.LoggingText_Raw

            

        static member OfLoggingText_Raw(text) = 
            let parser = 
                pstringCI "CMYK " >>. (sepBy1 pfloat spaces) .>> eof

            match run parser text with 
            | Success (r, _, _) ->
                let r = List.map float32 r
                FsDeviceCmyk.Create(r.[0], r.[1], r.[2], r.[3])
            
            | Failure (error, _, _) -> failwith error




    [<RequireQualifiedAccess>]
    type FsValueColor =
        | Rgb of FsDeviceRgb
        | Cmyk of FsDeviceCmyk
        | Lab of FsLab
        | Gray of FsGray
    with 

        member x.ColorSpace =
            match x with
            | FsValueColor.Rgb  _ -> ColorSpace.Rgb

            | FsValueColor.Cmyk _ -> ColorSpace.Cmyk

            | FsValueColor.Gray _ -> ColorSpace.Gray

            | FsValueColor.Lab _ -> ColorSpace.Lab

        member x.ToItextColor() = 
            FsValueColor.ToItextColor x

        member x.IsInColorSpace(colorSpace: ColorSpace) =
            x.ColorSpace = colorSpace

        static member CreateRGB(r, g, b: int) =
            FsDeviceRgb.Create(r, g, b)
            |> FsValueColor.Rgb

        static member CreateRGB(r, g, b: float32) =
            FsDeviceRgb.Create(r, g, b)
            |> FsValueColor.Rgb

        member x.LoggingText_Raw = 
            match x with
            | FsValueColor.Rgb rgbColor -> rgbColor.LoggingText_Raw

            | FsValueColor.Cmyk cmykColor -> cmykColor.LoggingText_Raw

            | FsValueColor.Gray grayColor -> grayColor.LoggingText_Raw

            | FsValueColor.Lab (labColor) -> labColor.LoggingText_Raw

        member x.LoggingText = 
            match x with
            | FsValueColor.Rgb rgbColor -> rgbColor.LoggingText

            | FsValueColor.Cmyk cmykColor -> cmykColor.LoggingText

            | FsValueColor.Gray grayColor -> grayColor.LoggingText

            | FsValueColor.Lab (labColor) -> labColor.LoggingText



        static member OfLoggingText_Raw(text: string) =
            match text with 
            | String.StartsWithIC "CMYK " -> 
                FsDeviceCmyk.OfLoggingText_Raw text
                |> FsValueColor.Cmyk

            | String.StartsWithIC "RGB" ->
                FsDeviceRgb.OfLoggingText_Raw text
                |> FsValueColor.Rgb

            | String.StartsWithIC "K " ->
                FsGray.OfLoggingText_Raw text
                |> FsValueColor.Gray    

            | String.StartsWithIC "LAB " ->
                FsLab.OfLoggingText_Raw text
                |> FsValueColor.Lab

            | _ -> failwithf "Cannot parsing %s to FsValueColor" text

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

        member x.GetColorArrayValues() =
            match x with 
            | FsValueColor.Cmyk cmyk ->
                [| cmyk.C; cmyk.M; cmyk.Y; cmyk.K |]

            | FsValueColor.Gray (FsGray v) ->
                [| v |]

            | FsValueColor.Rgb rgb ->
                [| rgb.R; rgb.G; rgb.B |]

            | FsValueColor.Lab lab -> [| lab.L; lab.a; lab.b |]


        member x.IsEqualTo(y: FsValueColor, ?valueEqualOptions: ValueEqualOptions) =
            FsValueColor.IsEqual (x, y, defaultArg valueEqualOptions ValueEqualOptions.DefaultRoundedValue) 

        static member BLACK = FsGray.BLACK |> FsValueColor.Gray
        static member WHITE = FsGray.WHITE |> FsValueColor.Gray
        static member GRAY = FsGray.GRAY |> FsValueColor.Gray

 
        static member RGB_BLACK = FsDeviceRgb.BLACK |> FsValueColor.Rgb
        static member RGB_WHITE = FsDeviceRgb.WHITE |> FsValueColor.Rgb
        static member RGB_RED = FsDeviceRgb.RED |> FsValueColor.Rgb
        static member RGB_BLUE = FsDeviceRgb.BLUE |> FsValueColor.Rgb
        static member RGB_MAGENTA = FsDeviceRgb.MAGENTA |> FsValueColor.Rgb


        static member CMYK_WHITE = FsDeviceCmyk.WHITE |>     FsValueColor.Cmyk
        static member CMYK_CYAN = FsDeviceCmyk.CYAN |>     FsValueColor.Cmyk
        static member CMYK_BLACK = FsDeviceCmyk.BLACK |>   FsValueColor.Cmyk
        static member CMYK_MAGENTA = FsDeviceCmyk.MAGENTA |> FsValueColor.Cmyk
        static member CMYK_YELLOW = FsDeviceCmyk.YELLOW |> FsValueColor.Cmyk

        static member IsEqual (color1: FsValueColor, color2: FsValueColor, valueEqualOptions: ValueEqualOptions) =
            let colorSpaces =
                [ color1.ColorSpace
                  color2.ColorSpace ]
                |> List.distinct

            match colorSpaces with 
            | [colorSpace] ->
                match valueEqualOptions with 
                | ValueEqualOptions.Exactly -> 
                    let colorValue1 =
                        color1.GetColorValue()

                    let colorValue2 =
                        color2.GetColorValue()

                    colorValue1 = colorValue2

                | ValueEqualOptions.RoundedValue fTolearance ->
                    let colorValue1 =
                        color1.GetColorValue()
                        |> List.map (fun v -> NearbyColorValue(float v, fTolearance.[colorSpace]) )

                    let colorValue2 =
                        color2.GetColorValue()
                        |> List.map (fun v -> NearbyColorValue(float v, fTolearance.[colorSpace]) )
                    colorValue1 = colorValue2
            | _ :: _ -> false
            | [] -> failwith "Invalid token"


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
            | FsValueColor.Cmyk cmyk -> new DeviceCmyk(cmyk.C, cmyk.M, cmyk.Y, cmyk.K) :> Color

            | FsValueColor.Gray (FsGray v) ->
                let grayColor = new DeviceGray(v)
                grayColor :> Color

            | FsValueColor.Rgb rgb ->
                new DeviceRgb(rgb.R, rgb.G, rgb.B) :> Color

            | FsValueColor.Lab lab -> lab.ToItextColor()

        static member private OfItextColor_Result (color: Color) = 
            match color with 
            | :? DeviceCmyk as color -> 
                let colorValues = color.GetColorValue()
                { C = colorValues.[0] 
                  M = colorValues.[1] 
                  Y = colorValues.[2]
                  K = colorValues.[3] }
                |> FsValueColor.Cmyk
                |> Result.Ok

            | :? DeviceRgb as color ->
                let colorValues = color.GetColorValue()
                { R = colorValues.[0] 
                  G = colorValues.[1] 
                  B = colorValues.[2] }
                |> FsValueColor.Rgb
                |> Result.Ok

            | :? DeviceGray as color ->
                let colorValues = color.GetColorValue()
                FsGray colorValues.[0]
                |> FsValueColor.Gray
                |> Result.Ok

            | :? Lab as color ->
                let colorValues = color.GetColorValue()
                { 
                    L = colorValues.[0]
                    a = colorValues.[1]
                    b = colorValues.[2]
                }
                |> FsValueColor.Lab
                |> Result.Ok
            | _ -> 
                sprintf "Cannot convert %s to fsValueColor" (color.GetType().FullName)
                |> Result.Error

        static member OfItextColor(color: Color) =
            FsValueColor.OfItextColor_Result(color)
            |> Result.getOrFail

        member x.IsEqualTo(y: Color, valueEqualOptions: ValueEqualOptions) =
            match FsValueColor.OfItextColor_Result y with 
            | Result.Ok y -> x.IsEqualTo(y, valueEqualOptions)
            | Result.Error _ -> false

    [<RequireQualifiedAccess>]
    module FsValueColor =
        let fromKnownColor (knownColor: KnownColor) =
            match knownColor with 
            | KnownColor.Black -> (FsGray.BLACK) |> FsValueColor.Gray
            | KnownColor.White -> FsGray.WHITE |> FsValueColor.Gray  
            | KnownColor.Gray -> FsGray.GRAY |> FsValueColor.Gray  
            | _ -> 
                let color = Color.FromName(knownColor.ToString())
                FsDeviceRgb.Create(int color.R, int color.G, int color.B)
                |> FsValueColor.Rgb
    

    type FsSeparation =
        { Name: string 
          Color: FsValueColor
          Transparency: float }
    with 

        member x.LoggingText = x.Name

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

    let private getAlterColor (alterColorSpace: ColorSpace) (colorValue: float32 list) =
        match alterColorSpace with 
        | ColorSpace.Lab -> 
            { L = colorValue.[0]
              a = colorValue.[1]
              b = colorValue.[2] }
            |> FsValueColor.Lab

        | ColorSpace.Rgb ->
            { R = colorValue.[0] 
              G = colorValue.[1]
              B = colorValue.[2] }
            |> FsValueColor.Rgb

        | ColorSpace.Gray ->
            FsValueColor.Gray(FsGray colorValue.[0])

        | ColorSpace.Cmyk ->
            { C = colorValue.[0] 
              M = colorValue.[1]
              Y = colorValue.[2] 
              K = colorValue.[3] }
            |> FsValueColor.Cmyk

        

    type private PdfCieBasedCs.IccBased with 
        member colorSpace.GetICC() = 

            let pdfStream = 
                (colorSpace.GetPdfObject() :?> PdfArray).Get(1).GetIndirectReference().GetRefersTo() :?> PdfStream

            let byarrays = pdfStream.GetBytes()
            
            let streamText = System.Text.Encoding.UTF8.GetString(byarrays.[0..500])

            let icc = Icc.OfStreamText streamText
                
            icc



    type private PdfFunction0SamplesRange(values: byte list) =
        
        let head = values.[0]
        let last = List.last values
        let length = values.Length
        let range = int last - int head
        member x.Values = values
        member x.Start = head
        member x.Last = last
        member x.Range = range
        member x.Length = length

        override x.ToString() = 
            sprintf "%d-%d" head last

    [<RequireQualifiedAccess>]
    type private PdfFunction0SamplesKind =
        /// 0	0	0
        /// 255	255	255
        | Default
        | Valid of PdfFunction0SamplesRange list


    type private PdfFunction0Samples(bytes: byte [], alterColorSpace: ColorSpace) =
        let chunkSize =
            match alterColorSpace with 
            | ColorSpace.Lab -> 3
            | ColorSpace.Cmyk -> 4
            | ColorSpace.Gray -> 1
            | ColorSpace.Rgb -> 3

        let ranges =
            bytes
            |> List.ofArray
            |> List.splitIntoGroup chunkSize
            |> List.map PdfFunction0SamplesRange


        let kind =
            match ranges |> List.forall (fun m -> m.Range = 255) with 
            | true -> PdfFunction0SamplesKind.Default
            | false ->
                match ranges |> List.forall (fun m -> m.Length = 255) with 
                | true -> PdfFunction0SamplesKind.Valid(ranges)
                | false -> failwithf "Cannot determine PdfFunction0SamplesKind from %A" ranges
                
        member x.Ranges = ranges

        member x.Kind = kind

    type private PdfSpecialCs.Separation with 
        member internal x.GetAlternateSpace() =
            let pdfName = 
                let colorSpacePdfArray = x.GetPdfObject() :?> PdfArray
                match colorSpacePdfArray.Get(2) with
                | :? PdfArray as pdfArray -> pdfArray.GetAsName(0)
                | :? PdfName as pdfName -> pdfName
                | _ -> failwith "Invalid token "

            match pdfName with 
            | PdfName PdfName.DeviceCMYK -> ColorSpace.Cmyk
            | PdfName PdfName.DeviceRGB 
            | PdfName PdfName.CalRGB -> ColorSpace.Rgb
            | PdfName PdfName.DeviceGray -> ColorSpace.Gray
            | PdfName PdfName.Lab -> ColorSpace.Lab
            | _ -> failwithf "Cannot convert %A to colorSpace" pdfName

        member x.GetAlternateColorValue() =
            let colorSpacePdfArray = x.GetPdfObject() :?> PdfArray

            let colorSpacePdfFunction = 
                match colorSpacePdfArray.GetAsDictionary(3) with
                | null ->
                    let stream = colorSpacePdfArray.GetAsStream(3)
                    stream :> PdfDictionary
                | v -> v


            let pdfFunctionType = colorSpacePdfFunction.GetAsInt(PdfName.FunctionType)

            match pdfFunctionType.Value with
            | 0 ->

                let alterColorSpace = x.GetAlternateSpace()

                let dictionaryRange = 
                    colorSpacePdfFunction.GetAsArray(PdfName.Range)
                    |> List.ofSeq
                    |> List.map (fun m -> (m :?> PdfNumber).FloatValue())

                let samples =
                    let stream = colorSpacePdfFunction :?> PdfStream
                    PdfFunction0Samples(stream.GetBytes(), alterColorSpace)

                match samples.Kind with 
                | PdfFunction0SamplesKind.Default ->
                    match alterColorSpace with 
                    | ColorSpace.Lab ->
                        let getColorValue valueGroup = 
                            match valueGroup with 
                            | [0.f; v] -> v
                            | [v; 0.f] -> v
                            | _ -> failwith "Invalid token"
                        [ dictionaryRange.[0]; getColorValue dictionaryRange.[2..3]; getColorValue dictionaryRange.[4..5] ]

                    | ColorSpace.Cmyk ->
                        [ dictionaryRange.[1]; dictionaryRange.[3]; dictionaryRange.[5]; dictionaryRange.[7] ]

                    | ColorSpace.Rgb ->
                        [ dictionaryRange.[0]; dictionaryRange.[2]; dictionaryRange.[4] ]

                    | _ -> failwith "Not implemnted"

                | PdfFunction0SamplesKind.Valid ranges ->
                    match alterColorSpace with 
                    | ColorSpace.Lab ->
                        let __checkdictionaryRangeValid = 
                            match dictionaryRange with 
                            | [0.f; 100.f; -128.f; 128.f; -128.f; 128.f] -> ()
                            | _ -> failwith "Not implemented"

                        let l =
                            let headRange = ranges.[0]
                            if headRange.Start = 255uy 
                            then float32 headRange.Last / 255f * 100.f
                            elif headRange.Start >= headRange.Last 
                            then float32 headRange.Last / float32 headRange.Start * 100.f
                            else failwithf "Cannot parse PdfFunction0SamplesKind %A to lab" samples.Kind

                        let a = 
                            let range = ranges.[1]
                            int range.Last - int range.Start

                        let b = 
                            let range = ranges.[2]
                            int range.Last - int range.Start 


                        [ l; float32 a; float32 b ]

                    | ColorSpace.Cmyk ->
                        let __checkdictionaryRangeValid = 
                            match dictionaryRange with 
                            | [0.f; 1.f; 0.f; 1.f; 0.f; 1.f; 0.f; 1.f] -> ()
                            | _ -> failwith "Not implemented"
                        let getValue (range: PdfFunction0SamplesRange) =
                            match range.Start with 
                            | 0uy -> float32 range.Last / 255.f
                            | _ -> failwithf "Cannot parse PdfFunction0SamplesKind %A to cmyk" samples.Kind

                        ranges
                        |> List.map getValue

                    | ColorSpace.Rgb ->
                        let __checkdictionaryRangeValid = 
                            match dictionaryRange with 
                            | [0.f; 1.f; 0.f; 1.f; 0.f; 1.f] -> ()
                            | _ -> failwith "Not implemented"
                        let getValue (range: PdfFunction0SamplesRange) =
                            match range.Start with 
                            | 255uy -> float32 range.Last / 255.f
                            | _ -> failwithf "Cannot parse PdfFunction0SamplesKind %A to cmyk" samples.Kind
                        ranges
                        |> List.map getValue

                    | _ -> failwith "Not implemnted"

            | 2 ->
                colorSpacePdfFunction.GetAsArray(PdfName.C1)
                |> List.ofSeq
                |> List.map (fun m -> (m :?> PdfNumber).FloatValue())
            | 4 ->
                let dictionaryRange = 
                    colorSpacePdfFunction.GetAsArray(PdfName.Range)
                    |> List.ofSeq
                    |> List.map (fun m -> (m :?> PdfNumber).FloatValue())

                let alterColorSpace = x.GetAlternateSpace()

                let values = 
                    let pdfStream = colorSpacePdfFunction :?> PdfStream
                    let bytes = pdfStream.GetBytes()
                    let result = System.Text.Encoding.UTF8.GetString(bytes)
                    match result with 
                    | ParseRegex.NamedMany1F ("(?<token>\d+\.?\d*)\s+mul", "token") values -> 
                        values
                        |> List.map System.Single.Parse

                match alterColorSpace with 
                | ColorSpace.Lab ->
                    let __checkdictionaryRangeValid = 
                        match dictionaryRange with 
                        | [0.f; 100.f; -128.f; 128.f; -128.f; 128.f] -> ()
                        | _ -> failwith "Not implemented"

                    failwith "Not implemented"

                | ColorSpace.Cmyk ->
                    let __checkdictionaryRangeValid = 
                        match dictionaryRange with 
                        | [0.f; 1.f; 0.f; 1.f; 0.f; 1.f; 0.f; 1.f] -> ()
                        | _ -> failwith "Not implemented"
                    values

                | _ -> failwith "Not implemnted"

            | _ -> failwith "Not implemnted"


        member x.GetAlterateColor() =
            getAlterColor (x.GetAlternateSpace()) (x.GetAlternateColorValue())

    let private createSeparation (name: PdfName, alternateSpace: PdfColorSpace, tintTransform: PdfFunction) =
        if not (tintTransform.CheckCompatibilityWithColorSpace(alternateSpace))
        then
            let ex = new PdfException(KernelExceptionMessageConstant.FUNCTION_IS_NOT_COMPATIBLE_WITH_COLOR_SPACE, (alternateSpace, tintTransform))
            raise ex

        new PdfSpecialCs.Separation(name, alternateSpace.GetPdfObject(), tintTransform.GetPdfObject())


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
            let colorSpace = createSeparation(name, new PdfDeviceCs.Cmyk(), separationPdfFunction)
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

            let colorSpace = 
                let alternateSpace = PdfDeviceCs.Rgb()
                //if (not (separationPdfFunction.CheckCompatibilityWithColorSpace(alternateSpace))) 
                //then 
                //    let ex = new PdfException(KernelExceptionMessageConstant.FUNCTION_IS_NOT_COMPATIBLE_WITH_COLOR_SPACE, ((separationPdfFunction, alternateSpace)))
                //    raise ex

                createSeparation(name, alternateSpace, separationPdfFunction)
            
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
            let colorSpace = createSeparation(name, new PdfDeviceCs.Gray(), separationPdfFunction)
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

            let colorSpace = createSeparation(name, color.GetColorSpace(), separationPdfFunction)
            new Separation(colorSpace, float32 transparency)


        static member Create(name, color: Color, ?transparency: float) =
            match color with 
            | :? DeviceRgb as color -> Separation.Create(name, color, ?transparency = transparency)
            | :? DeviceCmyk as color ->  Separation.Create(name, color, ?transparency = transparency)
            | :? DeviceGray as color -> Separation.Create(name, color, ?transparency = transparency)
            | :? Lab as color -> Separation.Create(name, color, ?transparency = transparency)
            | _ -> failwithf "cannot create separation from color %s" (color.GetType().FullName)


    let private fsSeparationCache = new ConcurrentDictionary<PdfSpecialCs.Separation, FsSeparation>()

    type FsSeparation with 

        static member OfSeparation(separation: Separation) =
            let transparency = 
                separation.GetColorValue() 
                |> List.ofArray 
                |> List.exactlyOne_DetailFailingText

            let colorSpace = separation.GetColorSpace() :?> PdfSpecialCs.Separation
            
            let v = 
                fsSeparationCache.GetOrAdd(colorSpace, fun _ ->
                    let colorSpacePdfArray = 
                        colorSpace.GetPdfObject() :?> PdfArray
                    
                    match colorSpacePdfArray.IsFlushed() with 
                    | true -> failwithf "%A  is alrealy flushed" separation
                    | false ->
                        let colorName = 
                            let uri = 
                                (colorSpacePdfArray.Get(1)
                                 |> string)

                            DecodedPdfName.Create(uri)

    
                        try 
                            FsSeparation.Create(colorName.ReadableName, separation.GetAlterateColor())
                        with ex ->
                            raise (new AccumulatedException(sprintf "Error when parsing separation color %s" colorName.ReadableName, ex))
                )

            { v with Transparency = float transparency }

       

        member separation1.IsEqualTo(separation0: FsSeparation, ?valueEqualOptions) =
            FsSeparation.IsEqual(separation0, separation1, defaultArg valueEqualOptions ValueEqualOptions.DefaultRoundedValue)


        member separation1.IsEqualTo(color: Color, valueEqualOptions) =
            match color with 
            | :? Separation as separation ->
                let separation0 = FsSeparation.OfSeparation separation
                FsSeparation.IsEqual(separation0, separation1, valueEqualOptions)

            | _ -> false



    let private fsIccBasedCache = new ConcurrentDictionary<PdfCieBasedCs.IccBased, Icc>()
    type FsIccBased =
        { Icc: Icc
          Color: FsValueColor }
    with 
        member x.LoggingText = x.Icc.LoggingText + " " + x.Color.LoggingText

        member x.IsEqualTo(y, valueEqualOptions) =
            x.Icc = y.Icc
            &&
                x.Color.IsEqualTo(y.Color, valueEqualOptions)


        static member OfICCBased(color: IccBased) =
            
            let colorSpace = color.GetColorSpace() :?> PdfCieBasedCs.IccBased 
            let icc = 
                fsIccBasedCache.GetOrAdd(colorSpace, fun _ ->
                    colorSpace.GetICC()
                )

            { Icc = icc
              Color = getAlterColor (icc.ColorSpace) (List.ofSeq (color.GetColorValue())) }

    [<RequireQualifiedAccess>]
    type AlternativeFsColor =
        | Separation of FsSeparation
        | IccBased of FsIccBased
        | ValueColor of FsValueColor
    with 
        member x.AlterColor =
            match x with 
            | AlternativeFsColor.ValueColor v -> v
            | AlternativeFsColor.IccBased v ->   v.Color
            | AlternativeFsColor.Separation v -> v.Color

        member x.LoggingText =
            match x with 
            | AlternativeFsColor.ValueColor v -> v.LoggingText
            | AlternativeFsColor.IccBased v ->   v.LoggingText
            | AlternativeFsColor.Separation v -> v.LoggingText

        static member BLACK = FsValueColor.BLACK |> AlternativeFsColor.ValueColor
        
        static member WHITE = FsValueColor.WHITE |> AlternativeFsColor.ValueColor

        static member Whites = 
            [ FsValueColor.WHITE
              FsValueColor.CMYK_WHITE
              FsValueColor.RGB_WHITE ]
            |> List.map AlternativeFsColor.ValueColor

        override x.ToString() = x.GetType().Name + " " + x.LoggingText
            


    [<RequireQualifiedAccess>]
    type FsColor =
        | Separation of FsSeparation
        | IccBased of FsIccBased
        | ValueColor of FsValueColor
        | PatternColor of PatternColor
        | ShadingColor of PdfShadingColor
    with 
        member x.AsAlternativeFsColor =
            match x with 
            | FsColor.ValueColor v -> v  |> AlternativeFsColor.ValueColor  |> Some
            | FsColor.IccBased v ->   v  |> AlternativeFsColor.IccBased    |> Some
            | FsColor.Separation v -> v  |> AlternativeFsColor.Separation  |> Some
            | FsColor.PatternColor _ 
            | FsColor.ShadingColor _ -> None

        static member OfAlternativeFsColor color =
            match color with 
            | AlternativeFsColor.IccBased v -> FsColor.IccBased v
            | AlternativeFsColor.Separation v -> FsColor.Separation v
            | AlternativeFsColor.ValueColor v -> FsColor.ValueColor v

        member x.AlterColor = 
            x.AsAlternativeFsColor
            |> Option.map(fun m -> m.AlterColor)

        member x.IsInColorSpace(colorSpace, ?includingAlterColor: bool) =
            let value =
                match defaultArg includingAlterColor true with 
                | true -> x.AlterColor
                | false ->
                    match x with 
                    | FsColor.ValueColor v -> Some v
                    | FsColor.IccBased _
                    | FsColor.Separation _  
                    | FsColor.PatternColor _
                    | FsColor.ShadingColor _ -> None

            match value with 
            | Some value -> value.IsInColorSpace(colorSpace)
            | None -> false



        member x.LoggingText =
            match x with 
            | FsColor.Separation v -> v.LoggingText
            | FsColor.IccBased v -> v.LoggingText
            | FsColor.ValueColor v -> v.LoggingText
            | FsColor.PatternColor _ -> "PatternColor"
            | FsColor.ShadingColor _ -> "ShadingColor"

        override x.ToString() = x.GetType().Name + " " + x.LoggingText

        member x.IsEqualTo(y, ?valueEqualOptions) =
            let valueEqualOptions = defaultArg valueEqualOptions ValueEqualOptions.DefaultRoundedValue
            let FALSE = false
            match x with 
            | FsColor.IccBased x -> 
                match y with 
                | FsColor.IccBased y -> x.IsEqualTo(y, valueEqualOptions)
                | _ -> FALSE
            | FsColor.Separation x -> 
                match y with 
                | FsColor.Separation y -> x.IsEqualTo(y, valueEqualOptions)
                | _ -> FALSE

            | FsColor.ValueColor x -> 
                match y with 
                | FsColor.ValueColor y -> x.IsEqualTo(y, valueEqualOptions)
                | _ -> FALSE
            | FsColor.PatternColor x -> 
                match y with 
                | FsColor.PatternColor y -> x.Equals(y)
                | _ -> FALSE

            | FsColor.ShadingColor x ->     
                match y with 
                | FsColor.ShadingColor y -> x.Equals(y)
                | _ -> FALSE

        static member RGB_BLACK = FsValueColor.RGB_BLACK  |> FsColor.ValueColor
        static member RGB_WHITE = FsValueColor.RGB_WHITE  |> FsColor.ValueColor
        static member RGB_RED = FsValueColor.RGB_RED  |> FsColor.ValueColor
        static member RGB_BLUE = FsValueColor.RGB_BLUE  |> FsColor.ValueColor
        static member RGB_MAGENTA = FsValueColor.RGB_MAGENTA  |> FsColor.ValueColor

        static member CMYK_WHITE = FsValueColor.CMYK_WHITE  |> FsColor.ValueColor
        static member CMYK_BLACK = FsValueColor.CMYK_BLACK  |> FsColor.ValueColor
        static member CMYK_CYAN = FsValueColor.CMYK_CYAN  |> FsColor.ValueColor
        static member CMYK_MAGENTA = FsValueColor.CMYK_MAGENTA  |> FsColor.ValueColor
        static member CMYK_YELLOW = FsValueColor.CMYK_YELLOW  |> FsColor.ValueColor


        static member BLACK = FsValueColor.BLACK |> FsColor.ValueColor
        static member WHITE = FsValueColor.WHITE |> FsColor.ValueColor
        static member GRAY = FsValueColor.GRAY |> FsColor.ValueColor

        static member CreateRGB(r, g, b: int) =
            FsValueColor.CreateRGB(r, g, b)
            |> FsColor.ValueColor

        static member CreateRGB(r, g, b: float32) =
            FsValueColor.CreateRGB(r, g, b)
            |> FsColor.ValueColor

        static member valueColor(valueColor: FsValueColor) =
            FsColor.ValueColor valueColor

        static member valueColor(valueColor: FsDeviceRgb) =
            FsColor.ValueColor (FsValueColor.Rgb valueColor)

        static member valueColor(valueColor: FsDeviceCmyk) =
            FsColor.ValueColor (FsValueColor.Cmyk valueColor)

        static member valueColor(valueColor: FsGray) =
            FsColor.ValueColor (FsValueColor.Gray valueColor)


    type FsSeparation with 
        member x.IsEqualTo(color, valueEqualOptions) =
            match color with 
            | FsColor.Separation separation ->
                FsSeparation.IsEqual(x, separation, valueEqualOptions)

            | _ -> false

        static member Contains(valueEqualOptions) =
            fun (color: FsColor) (fsSeparations: FsSeparation list) ->
                fsSeparations
                |> List.exists (fun m -> m.IsEqualTo(color, valueEqualOptions))


    [<RequireQualifiedAccess>]
    module FsColor =
        let private fsColorCache = new ConcurrentDictionary<SerializableCustomComparable<Color, int>, FsColor>()

        let OfItextColor(color: Color) =
            let color = SerializableCustomComparable(color, fun color -> color.GetHashCode())
            fsColorCache.GetOrAdd(color, fun color ->
                let color = color.Value
                match color with 
                | :? Separation as separation -> 
                    FsSeparation.OfSeparation separation
                    |> FsColor.Separation
                | :? IccBased as iccBased -> 
                    FsIccBased.OfICCBased iccBased
                    |> FsColor.IccBased
                | :? PatternColor as patternColor -> FsColor.PatternColor patternColor
                | :? PdfShadingColor as shadingColor -> 
                    FsColor.ShadingColor shadingColor

                | _ -> 
                    FsValueColor.OfItextColor color
                    |> FsColor.ValueColor
            )


        let equal (c1: FsColor) (c2: FsColor) =
            c1.IsEqualTo(c2)

        let equalToItextB  (c2: FsColor) (c1: Color) =
            equal (OfItextColor c1) c2

        let (|EqualTo|_|) color1 color2 =
            if equal color1 color2 then Some ()
            else None

        let isSeparation (c: FsColor) =
            match c with 
            | FsColor.Separation _ -> true 
            | _ -> false
    
        let asSeparation (c: FsColor) =
            match c with 
            | FsColor.Separation sepa -> Some sepa
            | _ -> None

        
        let isCmyk (color: FsColor) =
            match color with 
            | FsColor.ValueColor (FsValueColor.Cmyk _) -> true
            | _ -> false
    
        let isGray (color: FsColor) =
            match color with 
            | FsColor.ValueColor (FsValueColor.Gray _) -> true
            | _ -> false

        let isRgb (color: FsColor) =
            match color with 
            | FsColor.ValueColor (FsValueColor.Rgb _) -> true
            | _ -> false




    [<RequireQualifiedAccess>]
    module FsColors =
        let tryFindIndex (color: FsColor) colors =
            colors 
            |> List.tryFindIndex (fun color' -> color.IsEqualTo(color'))

        let contains (color: FsColor) colors =
            colors |> List.exists (fun color' -> color.IsEqualTo(color'))

        let containsWith valueEqualOptions (color: FsColor) colors =
            colors |> List.exists (fun color' -> color.IsEqualTo(color', valueEqualOptions = valueEqualOptions))


        let containsItext (color: Color) colors =
            let color = FsColor.OfItextColor color
            colors |> List.exists (fun color' -> color.IsEqualTo(color'))

        let private comparer =
            { new IEqualityComparer<FsColor> with 
                member __.Equals(x,y) = 
                    FsColor.equal x y
    
                member __.GetHashCode(_) = 0
            }

        let distinct (colors: FsColor seq) =
            colors.Distinct(comparer)
  
    
        let except colors1 colors2 =
            colors2 |> List.filter (fun c -> 
                contains c colors1 
                |> not
            )

    [<AutoOpen>]
    module _FsColorExtensions =
        [<RequireQualifiedAccess>]
        module FsValueColors =
            let tryFindIndex (color: FsValueColor) colors =
                colors 
                |> List.tryFindIndex (fun color' -> color.IsEqualTo(color'))

            let contains (color: FsValueColor) colors =
                colors |> List.exists (fun color' -> color.IsEqualTo(color'))
      
            let private comparer =
                { new IEqualityComparer<FsValueColor> with 
                    member __.Equals(x,y) = 
                        FsValueColor.IsEqual(x, y, ValueEqualOptions.DefaultRoundedValue)
    
                    member __.GetHashCode(_) = 0
                }

            let distinct (colors: FsValueColor seq) =
                colors.Distinct(comparer)
    
            let except colors1 colors2 =
                colors2 |> List.filter (fun c -> 
                    contains c colors1 
                    |> not
                )
            



    [<RequireQualifiedAccess>]
    module Separation =
        let equal valueEqualOptions (c1: Separation) (c2: Separation) =
            FsSeparation.OfSeparation(c1).IsEqualTo(c2, valueEqualOptions)
    
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
    

    
        let pantoneSolidCoated (pantoneEnum: PantoneColorEnum) writer =
            PdfDocument.obtainSperationColorFromResources (@"Pantone+ Solid Coated/" + pantoneEnum.ToString()) writer
    
        let pantoneTPX (tpxColorEnum: TPXColorEnum) writer =
            PdfDocument.obtainSperationColorFromResources (@"TPX/" + tpxColorEnum.ToString()) writer
    
        let fromKnownColor (knownColor: KnownColor) =
            match knownColor with 
            | KnownColor.Black -> DeviceGray.BLACK :> Color
            | KnownColor.White -> DeviceGray.WHITE :> Color
            | _ -> DeviceRgb.fromKnownColor knownColor :> Color

        //[<System.ObsoleteAttribute("Using FsColor instead")>]
        //let (|EqualTo|_|) color1 color2 =
        //    if equal color1 color2 then Some ()
        //    else None

    [<RequireQualifiedAccess>]
    module Colors =
        let private equal (c1: Color) (c2: Color) =
            let fsColor1 = FsColor.OfItextColor c1
            let fsColor2 = FsColor.OfItextColor c2
            fsColor1.IsEqualTo(fsColor2)

        let private comparer =
            { new IEqualityComparer<Color> with 
                member __.Equals(x,y) = 
                    equal x y
    
                member __.GetHashCode(_) = 0
            }

        let distinct (colors: Color seq) =
            colors.Distinct(comparer)
            
        let private contains color colors =
            colors |> List.exists (equal color)
    
        let private tryFindIndex color colors =
            colors |> List.tryFindIndex (equal color)

        let except colors1 colors2 =
            colors2 |> List.filter (fun c -> 
                contains c colors1 
                |> not
            )



    [<RequireQualifiedAccess>]
    type PdfCanvasColor = 
        | Value of FsValueColor
        | Separation of FsSeparation
        | ColorCard of ColorCard
        | Registration
    with 
        member x.LoggingText =
            match x with 
            | PdfCanvasColor.Value v -> v.LoggingText
            | PdfCanvasColor.Separation v -> v.LoggingText
            | PdfCanvasColor.ColorCard colorCard ->
                match colorCard with 
                | ColorCard.KnownColor knownColorEnum -> knownColorEnum.ToString()
                | ColorCard.Pantone colorEnum -> colorEnum.ToString()
                | ColorCard.TPX tpxColorEnum -> tpxColorEnum.ToString()

            | PdfCanvasColor.Registration -> "Registration"

        override x.ToString() = 
            x.GetType().Name + " " + x.LoggingText

        static member valueColor(valueColor: FsValueColor) =
            PdfCanvasColor.Value valueColor

        static member valueColor(valueColor: FsDeviceRgb) =
            PdfCanvasColor.Value (FsValueColor.Rgb valueColor)

        static member valueColor(valueColor: FsDeviceCmyk) =
            PdfCanvasColor.Value (FsValueColor.Cmyk valueColor)

        static member valueColor(valueColor: FsGray) =
            PdfCanvasColor.Value (FsValueColor.Gray valueColor)


        static member CreateRGB(r, g, b: int) =
            FsValueColor.CreateRGB(r, g, b)
            |> PdfCanvasColor.Value

        static member CreateRGB(r, g, b: float32) =
            FsValueColor.CreateRGB(r, g, b)
            |> PdfCanvasColor.Value

        static member OfFsColor(color: FsColor) =
            match color with 
            | FsColor.Separation separation -> 
                separation
                |> PdfCanvasColor.Separation
            | FsColor.IccBased _ -> failwithf "Currently conversion of icc based color to PdfCanvasColor is not supported" 
            | FsColor.PatternColor _ -> failwithf "Currently conversion of pattern color to PdfCanvasColor is not supported" 
            | FsColor.ShadingColor _ -> failwithf "Currently conversion of shading color to PdfCanvasColor is not supported" 
            | FsColor.ValueColor valueColor ->
                valueColor
                |> PdfCanvasColor.Value



        static member BLACK = FsValueColor.BLACK |> PdfCanvasColor.Value
        static member WHITE = FsValueColor.WHITE |> PdfCanvasColor.Value
        static member GRAY = FsValueColor.GRAY |> PdfCanvasColor.Value

        static member OfITextColor(color: Color) =
            match FsColor.OfItextColor color with 
            | FsColor.Separation separation -> 
                PdfCanvasColor.Separation separation
            | FsColor.IccBased _ -> failwithf "Currently conversion of icc based color to PdfCanvasColor is not supported" 
            | FsColor.PatternColor _ -> failwithf "Currently conversion of pattern to PdfCanvasColor is not supported" 
            | FsColor.ShadingColor _ -> failwithf "Currently conversion of shading color to PdfCanvasColor is not supported" 
            | FsColor.ValueColor color -> PdfCanvasColor.Value color

        member pdfCanvasColor.IsEqualTo(fsColor: FsColor) =
            match pdfCanvasColor,fsColor with 
            | PdfCanvasColor.Value color1, FsColor.ValueColor color2 -> color1.IsEqualTo(color2)
            | PdfCanvasColor.Separation separation1, FsColor.Separation separation2 -> separation1.IsEqualTo(separation2)
            | PdfCanvasColor.ColorCard colorCard1, _ ->
    
                match colorCard1 with 
                | ColorCard.Pantone _ 
                | ColorCard.TPX _ ->
                    let separation1 =
                        match colorCard1 with 
                        | ColorCard.Pantone pantoneColor1 -> FsSeparation.OfPantone pantoneColor1
                        | ColorCard.TPX tpxColor1 -> FsSeparation.OfTpx tpxColor1
                        | ColorCard.KnownColor knownColor -> failwith "Invalid token"
                    
                    match fsColor with 
                    | FsColor.Separation separation2 ->
                        separation1.IsEqualTo(
                            separation2,
                            ValueEqualOptions.RoundedValue ValueEqualOptionsTolerance.Rough
                        )
                    | _ -> false

                | ColorCard.KnownColor knownColor1 ->
                    let knownColor1 = 
                        (FsValueColor.fromKnownColor knownColor1)
                        |> PdfCanvasColor.Value

                    knownColor1.IsEqualTo(fsColor)
    
            | PdfCanvasColor.Registration, _ ->
                 PdfCanvasColor.Separation(FsSeparation.Registration).IsEqualTo(fsColor)

            | _, _ -> false

        member pdfCanvasColor.IsEqualToItextColor(color: Color) =
            pdfCanvasColor.IsEqualTo(FsColor.OfItextColor color)

        static member Contains(color: FsColor) (pdfCanvasColor: PdfCanvasColor list) =
            pdfCanvasColor
            |> List.exists(fun pdfCanvasColor -> pdfCanvasColor.IsEqualTo(color))
  
        static member Lab(color) = 
            FsValueColor.Lab color
            |> PdfCanvasColor.Value


    [<RequireQualifiedAccess>]
    type NullablePdfCanvasColor =
        | N 
        | Value of FsValueColor
        | Separation of FsSeparation
        | ColorCard of ColorCard
        | Registration
    with 

        static member OfPdfCanvasColor color =
            match color with 
            | PdfCanvasColor.Value v ->                 NullablePdfCanvasColor.Value         v
            | PdfCanvasColor.Separation v ->            NullablePdfCanvasColor.Separation    v
            | PdfCanvasColor.ColorCard v ->             NullablePdfCanvasColor.ColorCard     v
            | PdfCanvasColor.Registration ->            NullablePdfCanvasColor.Registration  
                             
        static member BLACK = PdfCanvasColor.BLACK |> NullablePdfCanvasColor.OfPdfCanvasColor
        static member WHITE = PdfCanvasColor.WHITE |> NullablePdfCanvasColor.OfPdfCanvasColor
        static member GRAY =  PdfCanvasColor.GRAY  |> NullablePdfCanvasColor.OfPdfCanvasColor

        static member valueColor(valueColor: FsValueColor) =
            NullablePdfCanvasColor.Value valueColor

        static member valueColor(valueColor: FsDeviceRgb) =
            NullablePdfCanvasColor.Value (FsValueColor.Rgb valueColor)

        static member valueColor(valueColor: FsDeviceCmyk) =
            NullablePdfCanvasColor.Value (FsValueColor.Cmyk valueColor)

        static member valueColor(valueColor: FsGray) =
            NullablePdfCanvasColor.Value (FsValueColor.Gray valueColor)


        static member OfFsColor color =
            PdfCanvasColor.OfFsColor color
            |> NullablePdfCanvasColor.OfPdfCanvasColor

        static member OfITextColor(color) =
            PdfCanvasColor.OfITextColor(color) 
            |> NullablePdfCanvasColor.OfPdfCanvasColor

        static member Lab(color) =
            FsValueColor.Lab color
            |> NullablePdfCanvasColor.Value

    [<RequireQualifiedAccess>]
    module NullablePdfCanvasColor =
        let (|Non|PdfCanvasColor|) = function
            | NullablePdfCanvasColor.N -> Non ()
            | NullablePdfCanvasColor.Value           v   -> PdfCanvasColor(PdfCanvasColor.Value        v)
            | NullablePdfCanvasColor.Separation      v   -> PdfCanvasColor(PdfCanvasColor.Separation   v)
            | NullablePdfCanvasColor.ColorCard       v   -> PdfCanvasColor(PdfCanvasColor.ColorCard    v)
            | NullablePdfCanvasColor.Registration        -> PdfCanvasColor(PdfCanvasColor.Registration  )


    type NullablePdfCanvasColor with 
        member x.LoggingText =
            match x with 
            | NullablePdfCanvasColor.Non -> "N"
            | NullablePdfCanvasColor.PdfCanvasColor v -> v.LoggingText

        override x.ToString() = x.GetType().Name + x.LoggingText

    type Color with 
        member x.IsEqualTo(fsSeparation: FsSeparation, valueEqualOptions) =
            fsSeparation.IsEqualTo(x, valueEqualOptions)


         

    [<RequireQualifiedAccess>]
    type FsItextPersistableColor =
        | Value of FsValueColor
        | Separation of FsSeparation

    with 
        static member OfItextColor(color: Color) =
            match color with 
            | :? Separation as separationColor ->
                FsSeparation.OfSeparation separationColor
                |> FsItextPersistableColor.Separation

            | _ -> FsItextPersistableColor.Value (FsValueColor.OfItextColor color)

        
        static member IsEqual (color1: FsItextPersistableColor, color2: FsItextPersistableColor, valueEqualOptions) =
            match color1, color2 with 
            | FsItextPersistableColor.Value color1, FsItextPersistableColor.Value color2 ->
                FsValueColor.IsEqual(color1, color2, valueEqualOptions)
         
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