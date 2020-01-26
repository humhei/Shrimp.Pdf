namespace Shrimp.Pdf.icms2
open Akkling
open Akka.Configuration
open Shrimp.Akkling.Cluster
open Shrimp.Akkling.Cluster.Intergraction
open Shrimp.Akkling.Cluster.Intergraction.Configuration

[<AutoOpen>]
module Core =

    [<RequireQualifiedAccess>]
    type GrayIcc =
        | ``Dot Gain 15%`` = 0
        | ``Dot Gain 20%`` = 1
        | ``Dot Gain 25%`` = 2
        | ``Dot Gain 30%`` = 3

    [<RequireQualifiedAccess>]
    type CmykIcc =
        | JapanColor2001Coated = 0 
        | ``USWebCoatedSWOP`` = 1

    [<RequireQualifiedAccess>]
    type RgbIcc =
        | ``SRGB Color Space Profile`` = 0
        | AdobeRGB1998 = 1
        | AppleRGB = 2

    [<RequireQualifiedAccess>]
    type LabIcc =
        | ``CIE Lab`` = 0


    [<RequireQualifiedAccess>]
    type Icc =
        | Gray of GrayIcc 
        | Cmyk of CmykIcc
        | Rgb of RgbIcc
        | Lab of LabIcc

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



