namespace Shrimp.Pdf
open iText.Kernel.Geom
open iText.IO.Font
open iText.IO.Font.Otf
open iText.Kernel.Font
open Akka.Configuration
open System.Reflection
open System.IO
open System

[<AutoOpen>]
module Operators =

    type private AssemblyFinder = AssemblyFinder

    let private possibleFolders = 
        [ "../Assets"(*UWP*) ] 

    /// application.conf should be copied to target folder
    let private fallBackByApplicationConf config =
        let folder = System.IO.Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)
        let folders = 
            [ folder ] 
            @ possibleFolders
                |> List.map (fun m -> Path.Combine(folder, m))

        folders 
        |> List.map (fun folder -> Path.Combine(folder, "application.conf"))
        |> List.tryFind (fun file -> File.Exists(file))
        |> function
            | Some file ->
                let texts = File.ReadAllText(file, Text.Encoding.UTF8)
                let applicationConfig = ConfigurationFactory.ParseString(texts)
                applicationConfig.WithFallback(config)
            | None -> config

    let private config = 
        ConfigurationFactory
            .FromResource<AssemblyFinder>("Shrimp.Pdf.Extensions.reference.conf")
        |> fallBackByApplicationConf

    let tolerance = config.GetDouble("shrimp.pdf.tolerance")

    /// approximately equal to 
    /// benchmark by (CONFIG: shrimp.pdf.tolerance (default is 0.1))
    let inline internal  (@=) a b =
        float (abs (a - b)) < tolerance

    /// mm to px
    let inline mm mm =
        let l = float mm
        l / 25.4 * 72.

