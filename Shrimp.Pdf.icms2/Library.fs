namespace Shrimp.Pdf.icms2
open Akkling
open Akka.Configuration
open Shrimp.Akkling.Cluster
open Shrimp.Akkling.Cluster.Intergraction
open Shrimp.Akkling.Cluster.Intergraction.Configuration
open Shrimp.FSharp.Plus
open System.IO
open System.Drawing.Imaging
open System.Drawing

[<AutoOpen>]
module Core =
    type BitmapColorValuesStorage =
        { File: RawFile 
          Stride: int 
          Size: Size
          PixelFormat: PixelFormat }
    with 
        member x.Path = x.File.Path

    type BitmapColorValues with 
        member x.GetAsRgbValues() =
            let depth = 
                Bitmap.GetPixelFormatSize(x.PixelFormat) / 8
                
            x.Values
            |> Array.map(fun value ->
                float32 value / 255.f
            )
            |> Array.chunkBySize x.Stride
            |> Array.collect (fun strideValues ->
                Array.take (x.Size.Width * depth) strideValues
                |> Array.chunkBySize depth
            )
            |> Array.map (Array.take 3 >> Array.rev)

        member x.GetAsGrayValues() =
            x.Values
            |> Array.map(fun value ->
                float32 value / 255.f
            )
            |> Array.chunkBySize 4
            |> Array.map (Array.take 1)

        member x.GetAsCMYKValues() =
            x.Values
            |> Array.map(fun value ->
                float32 value / 255.f * 100.f
            )
            |> Array.chunkBySize 4

        member x.ToStorage() =
            let rawFile = System.IO.Path.ChangeExtension(Path.GetTempFileName(), ".raw")
            File.WriteAllBytes(rawFile, x.Values)
            { File = RawFile rawFile 
              Stride = x.Stride
              Size = x.Size
              PixelFormat = x.PixelFormat }

        static member OfStorage(v: BitmapColorValuesStorage) =
            let bytes = System.IO.File.ReadAllBytes v.File.Path
            BitmapColorValues(bytes, v.Stride, v.Size, v.PixelFormat)

    [<AutoOpen>]
    module _Image =
        open System.Drawing
        type Bitmap with 
            member x.GetPixelsList() =
                [0.. x.Height-1 ]
                |> List.map(fun i ->
                    [0 .. x.Width-1 ]
                    |> List.map(fun j ->
                        x.GetPixel(j, i)
                    )
                )
                


            member x.GetPixels() =
                [|
                    for i = 0 to (x.Height-1) do 
                        for j = 0 to (x.Width-1) do
                            yield x.GetPixel(j, i)
                |]
                

            member x.MapPixel(mapping) =
                for i = 0 to (x.Height-1) do 
                    for j = 0 to (x.Width-1) do
                        mapping <| x.GetPixel(j, i)


    [<RequireQualifiedAccess>]
    type GrayIcc =
        | ``Dot Gain 15%`` = 0
        | ``Dot Gain 20%`` = 1
        | ``Dot Gain 25%`` = 2
        | ``Dot Gain 30%`` = 3

    [<RequireQualifiedAccess>]
    module GrayIcc =
        let ofStreamText (streamText) = 
            match streamText with 
            | String.Contains "Dot Gain 15%" -> Some GrayIcc.``Dot Gain 15%``
            | String.Contains "Dot Gain 20%" -> Some GrayIcc.``Dot Gain 20%``
            | String.Contains "Dot Gain 25%" -> Some GrayIcc.``Dot Gain 25%``
            | String.Contains "Dot Gain 30%" -> Some GrayIcc.``Dot Gain 30%``
            | _ -> None


    [<RequireQualifiedAccess>]
    type CmykIcc =
        | JapanColor2001Coated = 0 
        | ``USWebCoatedSWOP`` = 1

    [<RequireQualifiedAccess>]
    module CmykIcc =
        let ofStreamText (streamText) = 
            match streamText with 
            | String.Contains "Japan Color 2001 Coated" -> Some CmykIcc.JapanColor2001Coated
            | String.Contains "U.S. Web Coated (SWOP) v2" -> Some CmykIcc.USWebCoatedSWOP
            | _ -> None


    [<RequireQualifiedAccess>]
    type RgbIcc =
        | ``SRGB Color Space Profile`` = 0
        | AdobeRGB1998 = 1
        | AppleRGB = 2

    [<RequireQualifiedAccess>]
    module RgbIcc =
        let ofStreamText (streamText) =
            match streamText with 
            | String.Contains "sRGB IEC61966-2.1" -> Some RgbIcc.``SRGB Color Space Profile``
            | String.Contains "Adobe RGB (1998)" -> Some RgbIcc.AdobeRGB1998
            | String.Contains "Apple RGB" -> Some RgbIcc.AdobeRGB1998
            | _ -> None


    [<RequireQualifiedAccess>]
    type LabIcc =
        | ``CIE Lab`` = 0

    [<RequireQualifiedAccess>]
    module LabIcc =
        let ofStreamText (streamText) =
            match streamText with 
            | String.Contains "CIE LAB" -> Some LabIcc.``CIE Lab``
            | _ -> None


    
    [<RequireQualifiedAccess>]
    type Icc =
        | Gray of GrayIcc 
        | Cmyk of CmykIcc
        | Rgb of RgbIcc
        | Lab of LabIcc
    with 
        member x.LoggingText =
            match x with 
            | Icc.Gray v -> v.ToString()
            | Icc.Cmyk v -> v.ToString()
            | Icc.Rgb  v -> v.ToString()
            | Icc.Lab  v -> v.ToString()

        static member OfStreamText(streamText: string) =
            match streamText with 
            | Try GrayIcc.ofStreamText icc -> Icc.Gray icc
            | Try RgbIcc.ofStreamText icc -> Icc.Rgb icc
            | Try CmykIcc.ofStreamText icc -> Icc.Cmyk icc
            | Try LabIcc.ofStreamText icc -> Icc.Lab icc
            | _ -> failwithf "Not implemented: Cannot read iccEnum from %A" streamText

        member x.ColorValuesCount =
            match x with 
            | Icc.Gray _ -> 1
            | Icc.Cmyk _ -> 4
            | Icc.Rgb _ -> 3
            | Icc.Lab _ -> 3

    type Intent = 
        | INTENT_PERCEPTUAL = 0u
        | INTENT_RELATIVE_COLORIMETRIC = 1u
        | INTENT_SATURATION = 2u
        | INTENT_ABSOLUTE_COLORIMETRIC = 3u
        | INTENT_PRESERVE_K_ONLY_PERCEPTUAL = 10u
        | INTENT_PRESERVE_K_ONLY_RELATIVE_COLORIMETRIC = 11u
        | INTENT_PRESERVE_K_ONLY_SATURATION = 12u
        | INTENT_PRESERVE_K_PLANE_PERCEPTUAL = 13u
        | INTENT_PRESERVE_K_PLANE_RELATIVE_COLORIMETRIC = 14u
        | INTENT_PRESERVE_K_PLANE_SATURATION = 15u
    

    [<RequireQualifiedAccess>]
    type ServerMsg =
        | CalcColor of inputIcc: Icc * inputValues: float32 []  * outputIcc: Icc * indent: Intent
        | ConvertImageColorSpace of inputIcc: Icc * bmpFile: BitmapColorValuesStorage * outputIcc: Icc * indent: Intent

    type private AssemblyFinder = AssemblyFinder

    let private referenceConfig = 
        lazy
            ConfigurationFactory.FromResource<AssemblyFinder>("Shrimp.Pdf.icms2.reference.conf")
            |> Configuration.fallBackByApplicationConf


    let [<Literal>] private SERVER = "server"
    let [<Literal>] private CLIENT = "client"


    let [<Literal>] private SHRIMP_PDF_ICMS2 = "shrimpPdfIcms2"

    let private seedPort = 
        lazy
            referenceConfig.Value.GetInt("shrimp.pdf.icms2.port")

    [<RequireQualifiedAccess>]
    module Server =
        let createAgent (receive): Server<unit, ServerMsg> =
            Server(SHRIMP_PDF_ICMS2, SERVER, CLIENT, seedPort.Value, seedPort.Value, setParams_Loggers_Nlog, receive)


    [<RequireQualifiedAccess>]
    module Routed =
        let port = 
            lazy
                referenceConfig.Value.GetInt("shrimp.pdf.icms2.port")

    [<RequireQualifiedAccess>]
    module Client =
        let create(): Client<unit, ServerMsg>  =
            Client(SHRIMP_PDF_ICMS2, CLIENT, SERVER, 0, seedPort.Value, Behaviors.ignore, setParams_Loggers_Nlog)



