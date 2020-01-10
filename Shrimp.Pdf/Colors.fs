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
open iText.Kernel.Pdf.Function
open iText.Kernel.Pdf.Colorspace

[<AutoOpen>]
module _Colors =

    let private whitePoint = 
        lazy 
            config.GetFloatList("shrimp.pdf.colors.whitePoint")
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
        static member ToItextColor = function
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

    [<RequireQualifiedAccess>]
    module DeviceRgb =
    
        let fromKnownColor(knownColor: KnownColor) =
            let color = Color.FromKnownColor(knownColor)
            new DeviceRgb(int color.R, int color.G, int color.B)
    
        let MAGENTA = new DeviceRgb(1.f, 0.f, 1.f) :> Color
    
    [<RequireQualifiedAccess>]
    module DeviceCmyk =
        let WHITE = DeviceCmyk(0,0,0,0)
    
    
    
    
    type Separation with 
        static member private ColorText(color: Color) =
            color.GetColorValue()
            |> Array.map string
            |> String.concat " "
            |> sprintf "{%s}"

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

            | _ -> failwith ""
          
        static member private Decode(color: Color) =
            let colorValues = color.GetColorValue()
            match color with 
            | :? Lab ->
                [| 100.f; colorValues.[0];  0.f;  colorValues.[1]; 0.f; colorValues.[2];|]
            | _ ->  Separation.Range(color)
        

        static member Create(name, color: DeviceCmyk, ?transparency: float) =
            let transparency = defaultArg transparency 1.
            let range = Separation.Range color

            let separationPdfFunction =
                new PdfFunction.Type2(
                    new PdfArray([| 0; 1 |]), 
                    new PdfArray(range),
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
                //new PdfFunction.Type0(
                //    new PdfArray([| 0; 1 |]), 
                //    new PdfArray(range),
                //    PdfArray[| 2 |],
                //    PdfNumber(8),
                //    PdfNumber(1),
                //    new PdfArray([| 0; 1 |]),
                //    PdfArray(Separation.Decode color),
                //    [| 255uy; 255uy; 255uy; 0uy; 0uy; 0uy;  |]
                //)
                new PdfFunction.Type2(
                    new PdfArray([| 0; 1 |]), 
                    new PdfArray(range),
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
                    new PdfArray([| 0; 1 |]), 
                    new PdfArray(range),
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
                    new PdfArray([| 0; 1 |]), 
                    new PdfArray(range),
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
    
    

