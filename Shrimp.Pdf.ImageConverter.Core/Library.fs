namespace Shrimp.Pdf.ImageConverter.Core
open Akkling
open Akka.Configuration
open Shrimp.Akkling.Cluster
open Shrimp.Akkling.Cluster.Intergraction
open Shrimp.Akkling.Cluster.Intergraction.Configuration
open Shrimp.FSharp.Plus
open System.IO
open System.Drawing


module Cluster =
    
    [<RequireQualifiedAccess>]
    type ServerMsg =
        | ConvertPdfFileToImage of PdfFile * dpi: float32 * targetDir: FsFullPath

    type private AssemblyFinder = AssemblyFinder

    let private referenceConfig = 
        lazy
            ConfigurationFactory.FromResource<AssemblyFinder>("Shrimp.Pdf.ImageConverter.Core.reference.conf")
            |> Configuration.fallBackByApplicationConf



    let [<Literal>] private SERVER = "server"
    let [<Literal>] private CLIENT = "client"


    let [<Literal>] private SHRIMP_PDF_IMAGE_CONVERTER = "ShrimpPdfImageConverter"

    let private seedPort = 
        lazy
            referenceConfig.Value.GetInt("Shrimp.Pdf.ImageConverter.Core.port")

    [<RequireQualifiedAccess>]
    module Server =
        let createAgent (receive): Server<unit, ServerMsg> =
            Server(SHRIMP_PDF_IMAGE_CONVERTER, SERVER, CLIENT, seedPort.Value, seedPort.Value, setParams_Loggers_Nlog, receive)



    [<RequireQualifiedAccess>]
    module Client =
        let create(): Client<unit, ServerMsg>  =
            Client(SHRIMP_PDF_IMAGE_CONVERTER, CLIENT, SERVER, 0, seedPort.Value, Behaviors.ignore, setParams_Loggers_Nlog)





