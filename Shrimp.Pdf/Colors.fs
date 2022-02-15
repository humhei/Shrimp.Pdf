namespace Shrimp.Pdf.Colors
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
open System.Collections.Concurrent
open Shrimp.FSharp.Plus
open Shrimp.Pdf.icms2
open Shrimp.FSharp.Plus

[<AutoOpen>]
module _Colors =


    type DeviceRgb with 
        static member OfHex(hex: int) =
            let color = System.Drawing.Color.FromArgb(hex)
            DeviceRgb(int color.R, int color.G, int color.B)

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

    type Icc with 
        member x.ColorSpace = 
            match x with 
            | Icc.Rgb _-> ColorSpace.Rgb
            | Icc.Cmyk _-> ColorSpace.Cmyk
            | Icc.Lab _-> ColorSpace.Lab
            | Icc.Gray _-> ColorSpace.Gray

    let private whitePoint = 
        lazy 
            config.Value.GetFloatList("shrimp.pdf.colors.labWhitePoint")
            |> Array.ofSeq

    type FsDeviceRgb =
        { R: float32 
          G: float32 
          B: float32 }
    with 
        member x.Range255 =
            {| R = x.R * 255.f 
               G = x.G * 255.f
               B = x.B * 255.f |}

        static member RED = { R = 1.0f; G = 0.0f; B = 0.0f }
        static member GREEN = { R = 0.0f; G = 1.0f; B = 0.0f }
        static member BLUE = { R = 0.0f; G = 0.0f; B = 1.0f }
        static member MAGENTA = { R = 1.0f; G = 0.0f; B = 1.0f }
        static member YELLOW = { R = 1.0f; G = 1.0f; B = 0.0f }
        static member BLACK = { R = 0.0f; G = 0.0f; B = 0.0f }
        static member WHITE = { R = 1.0f; G = 1.0f; B = 1.0f }
        static member GRAY = { R = 0.5f; G = 0.5f; B = 0.5f }

        member x.LoggingText = 
            let colorName = 
                match x with 
                | EqualTo FsDeviceRgb.RED -> "RED"
                | EqualTo FsDeviceRgb.GREEN -> "GREEN"
                | EqualTo FsDeviceRgb.BLUE -> "BLUE"
                | EqualTo FsDeviceRgb.MAGENTA -> "MAGENTA"
                | EqualTo FsDeviceRgb.YELLOW -> "YELLOW"
                | EqualTo FsDeviceRgb.BLACK -> "BLACK"
                | EqualTo FsDeviceRgb.WHITE -> "WHITE"
                | EqualTo FsDeviceRgb.GRAY -> "GRAY"
                | _ ->
                    let range255 = x.Range255
                    sprintf "%.1f %.1f %.1f" (range255.R) range255.G range255.B

            "RGB " + colorName

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


    type FsLab =
        { L: float32 
          a: float32
          b: float32 }
    with 

        member x.LoggingText = 
            sprintf "LAB %.1f %.1f %.1f" (x.L) x.a x.b

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
    with 
        member x.Range100 =
            { C = x.C * 100.f; M = x.M * 100.f; Y = x.Y * 100.f; K = x.K *100.f}



            

        static member CYAN = { C = 1.0f; M = 0.0f; Y = 0.0f; K = 0.0f }
        static member MAGENTA = { C = 0.0f; M = 1.0f; Y = 0.0f; K = 0.0f }
        static member YELLOW = { C = 0.0f; M = 0.0f; Y = 1.0f; K = 0.0f }
        static member BLACK = { C = 0.0f; M = 0.0f; Y = 0.0f; K = 1.0f }
        static member WHITE = { C = 0.0f; M = 0.0f; Y = 0.0f; K = 0.0f }
        static member GRAY = { C = 0.0f; M = 0.0f; Y = 0.0f; K = 0.5f }
        static member RED =  { C = 0.0f; M = 1.0f; Y = 1.0f; K = 0.0f }
        static member GREEN =  { C = 1.0f; M = 0.0f; Y = 1.0f; K = 0.0f }

        member x.LoggingText = 
            let colorName = 
                match x with 
                | EqualTo FsDeviceCmyk.CYAN -> "CYAN"
                | EqualTo FsDeviceCmyk.RED -> "RED"
                | EqualTo FsDeviceCmyk.GREEN -> "GREEN"
                | EqualTo FsDeviceCmyk.MAGENTA -> "MAGENTA"
                | EqualTo FsDeviceCmyk.YELLOW -> "YELLOW"
                | EqualTo FsDeviceCmyk.BLACK -> "BLACK"
                | EqualTo FsDeviceCmyk.WHITE -> "WHITE"
                | EqualTo FsDeviceCmyk.GRAY -> "GRAY"
                | _ ->
                    sprintf "%.1f %.1f %.1f %.1f" (x.C) x.M x.Y x.K

            "CMYK " + colorName

    type FsGray = FsGray of float32
    with 
        member x.LoggingText = 
            let (FsGray v) = x
            sprintf "K %.1f" v

        static member BLACK = FsGray 0.0f
        static member WHITE = FsGray 1.0f
        static member GRAY = FsGray 0.5f

    [<RequireQualifiedAccess>]
    type FsValueColor =
        | Rgb of FsDeviceRgb
        | Cmyk of FsDeviceCmyk
        | Lab of FsLab
        | Gray of FsGray
    with 

        static member CreateRGB(r, g, b: int) =
            FsDeviceRgb.Create(r, g, b)
            |> FsValueColor.Rgb

        static member CreateRGB(r, g, b: float32) =
            FsDeviceRgb.Create(r, g, b)
            |> FsValueColor.Rgb

        member x.LoggingText = 
            match x with
            | FsValueColor.Rgb rgbColor -> rgbColor.LoggingText

            | FsValueColor.Cmyk cmykColor -> cmykColor.LoggingText

            | FsValueColor.Gray grayColor -> grayColor.LoggingText

            | FsValueColor.Lab (labColor) -> labColor.LoggingText

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
            | FsValueColor.Cmyk cmyk -> new DeviceCmyk(cmyk.C, cmyk.M, cmyk.Y, cmyk.K) :> Color

            | FsValueColor.Gray (FsGray v) ->
                let grayColor = new DeviceGray()
                grayColor.SetColorValue([|v|]) 
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

            let icc =
                match streamText with 
                | String.Contains "sRGB IEC61966-2.1" -> Icc.Rgb RgbIcc.``SRGB Color Space Profile``
                | String.Contains "Adobe RGB (1998)" -> Icc.Rgb RgbIcc.AdobeRGB1998
                | String.Contains "Apple RGB" -> Icc.Rgb RgbIcc.AdobeRGB1998
                | String.Contains "CIE LAB" -> Icc.Lab LabIcc.``CIE Lab``
                | String.Contains "Dot Gain 15%" -> Icc.Gray GrayIcc.``Dot Gain 15%``
                | String.Contains "Dot Gain 20%" -> Icc.Gray GrayIcc.``Dot Gain 20%``
                | String.Contains "Dot Gain 25%" -> Icc.Gray GrayIcc.``Dot Gain 25%``
                | String.Contains "Dot Gain 30%" -> Icc.Gray GrayIcc.``Dot Gain 30%``
                | String.Contains "Japan Color 2001 Coated" -> Icc.Cmyk CmykIcc.JapanColor2001Coated
                | String.Contains "U.S. Web Coated (SWOP) v2" -> Icc.Cmyk CmykIcc.USWebCoatedSWOP
                | _ -> failwith "Not implemented"

            icc

    

    type private PdfSpecialCs.Separation with 
        member x.GetAlternateSpace() =
            let pdfName = 
                let colorSpacePdfArray = x.GetPdfObject() :?> PdfArray
                match colorSpacePdfArray.Get(2) with
                | :? PdfArray as pdfArray -> pdfArray.GetAsName(0)
                | :? PdfName as pdfName -> pdfName
                | _ -> failwith "Invalid token "

            match pdfName with 
            | PdfName PdfName.DeviceCMYK -> ColorSpace.Cmyk
            | PdfName PdfName.DeviceRGB -> ColorSpace.Rgb
            | PdfName PdfName.DeviceGray -> ColorSpace.Gray
            | PdfName PdfName.Lab -> ColorSpace.Lab
            | _ -> failwithf "Cannot convert %A to colorSpace" pdfName

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
                | ColorSpace.Lab ->

                    let getColorValue valueGroup = 
                        match valueGroup with 
                        | [0.f; v] -> v
                        | [v; 0.f] -> v
                        | _ -> failwith "Invalid token"

                    [ range.[0]; getColorValue range.[2..3]; getColorValue range.[4..5] ]

                | ColorSpace.Cmyk ->
                    [ range.[1]; range.[3]; range.[5]; range.[7] ]

                | _ -> failwith "Not implemnted"
            | 2 ->
                colorSpacePdfFunction.GetAsArray(PdfName.C1)
                |> List.ofSeq
                |> List.map (fun m -> (m :?> PdfNumber).FloatValue())

            | _ -> failwith "Not implemnted"


        member x.GetAlterateColor() =
            getAlterColor (x.GetAlternateSpace()) (x.GetAlternateColorValue())

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
    
                    let colorName = 
                        let uri = 
                            (colorSpacePdfArray.Get(1)
                             |> string).TrimStart('/')

                        uri.Replace("#20", " ")
    

                    FsSeparation.Create(colorName, separation.GetAlterateColor())
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

        static member Contains(valueEqualOptions) =
            fun (color: Color) (fsSeparations: FsSeparation list) ->
                fsSeparations
                |> List.exists (fun m -> m.IsEqualTo(color, valueEqualOptions))


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
    type FsColor =
        | Separation of FsSeparation
        | IccBased of FsIccBased
        | ValueColor of FsValueColor
        | PatternColor of PatternColor
    with 
        member x.LoggingText =
            match x with 
            | FsColor.Separation v -> v.LoggingText
            | FsColor.IccBased v -> v.LoggingText
            | FsColor.ValueColor v -> v.LoggingText
            | FsColor.PatternColor _ -> "PatternColor"

        member x.IsEqualTo(y, ?valueEqualOptions) =
            let valueEqualOptions = defaultArg valueEqualOptions ValueEqualOptions.DefaultRoundedValue
            match x, y with 
            | FsColor.IccBased x, FsColor.IccBased y -> x.IsEqualTo(y, valueEqualOptions)
            | FsColor.Separation x, FsColor.Separation y -> x.IsEqualTo(y, valueEqualOptions)
            | FsColor.ValueColor x, FsColor.ValueColor y -> x.IsEqualTo(y, valueEqualOptions)
            | FsColor.PatternColor x, FsColor.PatternColor y -> false
            | _, _ -> false

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
        | Lab of FsLab
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

            | PdfCanvasColor.Lab labColor -> labColor.LoggingText
            | PdfCanvasColor.Registration -> "Registration"

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
            | FsColor.ValueColor valueColor ->
                valueColor
                |> PdfCanvasColor.Value



        static member BLACK = FsValueColor.BLACK |> PdfCanvasColor.Value
        static member WHITE = FsValueColor.WHITE |> PdfCanvasColor.Value
        static member GRAY = FsValueColor.GRAY |> PdfCanvasColor.Value

        static member OfITextColor(color: Color) =
            match color with 
            | :? Separation as separation -> 
                FsSeparation.OfSeparation separation
                |> PdfCanvasColor.Separation
            | :? IccBased -> failwithf "Currently conversion of icc based color to PdfCanvasColor is not supported" 
            | _ ->
                FsValueColor.OfItextColor color
                |> PdfCanvasColor.Value

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
                        separation1.IsEqualTo(separation2, ValueEqualOptions.RoundedValue 0)
                    | _ -> false

                | ColorCard.KnownColor knownColor1 ->
                    let knownColor1 = 
                        (FsValueColor.fromKnownColor knownColor1)
                        |> PdfCanvasColor.Value

                    knownColor1.IsEqualTo(fsColor)
    
            | PdfCanvasColor.Lab labColor1, _ -> 
                (PdfCanvasColor.Value (FsValueColor.Lab labColor1)).IsEqualTo(fsColor)
    
            | PdfCanvasColor.Registration, _ ->
                 PdfCanvasColor.Separation(FsSeparation.Registration).IsEqualTo(fsColor)

            | _, _ -> false

        member pdfCanvasColor.IsEqualToItextColor(color: Color) =
            pdfCanvasColor.IsEqualTo(FsColor.OfItextColor color)

        static member Contains(color: FsColor) (pdfCanvasColor: PdfCanvasColor list) =
            pdfCanvasColor
            |> List.exists(fun pdfCanvasColor -> pdfCanvasColor.IsEqualTo(color))
  
        

    [<RequireQualifiedAccess>]
    type NullablePdfCanvasColor =
        | N 
        | Value of FsValueColor
        | Separation of FsSeparation
        | ColorCard of ColorCard
        | Registration
        | Lab of FsLab
    with 

        static member OfPdfCanvasColor color =
            match color with 
            | PdfCanvasColor.Value v ->                 NullablePdfCanvasColor.Value         v
            | PdfCanvasColor.Separation v ->            NullablePdfCanvasColor.Separation    v
            | PdfCanvasColor.ColorCard v ->             NullablePdfCanvasColor.ColorCard     v
            | PdfCanvasColor.Lab v ->                   NullablePdfCanvasColor.Lab           v
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

    [<RequireQualifiedAccess>]
    module NullablePdfCanvasColor =
        let (|Non|PdfCanvasColor|) = function
            | NullablePdfCanvasColor.N -> Non ()
            | NullablePdfCanvasColor.Value           v   -> PdfCanvasColor(PdfCanvasColor.Value        v)
            | NullablePdfCanvasColor.Separation      v   -> PdfCanvasColor(PdfCanvasColor.Separation   v)
            | NullablePdfCanvasColor.ColorCard       v   -> PdfCanvasColor(PdfCanvasColor.ColorCard    v)
            | NullablePdfCanvasColor.Lab             v   -> PdfCanvasColor(PdfCanvasColor.Lab          v)
            | NullablePdfCanvasColor.Registration        -> PdfCanvasColor(PdfCanvasColor.Registration  )


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