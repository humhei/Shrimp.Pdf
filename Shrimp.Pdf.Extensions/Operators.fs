namespace Shrimp.Pdf
open iText.Kernel.Geom
open Akka.Configuration
open System.Reflection
open System.IO
open System
open Shrimp.Akkling.Cluster.Intergraction.Configuration


[<AutoOpen>]
module Operators =

    type private AssemblyFinder = AssemblyFinder

    let private config = 
        lazy
            ConfigurationFactory
                .FromResource<AssemblyFinder>("Shrimp.Pdf.Extensions.reference.conf")
            |> Configuration.fallBackByApplicationConf

    let tolerance = 
        lazy
            config.Value.GetDouble("shrimp.pdf.tolerance")

    /// approximately equal to 
    /// benchmark by (CONFIG: shrimp.pdf.tolerance (default is 0.1))
    let (@=) a b =
        (abs (a - b)) < tolerance.Value

    /// defaultConversion: mm to user unit
    let mm (mm: float) =
        mm / 25.4 * 72.

    /// defaultConversion: userUnit to mm
    let userUnitToMM (userUnit: float) =
        userUnit / 72. * 25.4

