namespace Shrimp.Pdf.icms2
open Akkling
open Akka.Configuration
open Shrimp.Akkling.Cluster
open Shrimp.Akkling.Cluster.Intergraction
open Shrimp.Akkling.Cluster.Intergraction.Configuration
open Shrimp.FSharp.Plus
open Fake.Core
open System.IO
open System.Drawing.Imaging
open System.Drawing

[<AutoOpen>]
module internal Config =
   
    type private AssemblyFinder = AssemblyFinder

    let referenceConfig = 
        ConfigurationFactory.FromResource<AssemblyFinder>("Shrimp.Pdf.icms2.reference.conf")
        |> Configuration.fallBackByApplicationConf

    //let resourceDirectory = 
        
    //    match Environment.environVarOrNone "ShrimpPdfResources" with 
    //    | Some dir ->
    //        match FsDirectoryInfo.tryCreate dir with 
    //        | Some dir -> dir.Path
    //        | None ->
    //            Path.GetFullPath (referenceConfig.GetString("shrimp.pdf.resourcesDirectory"))
    //    | None -> Path.GetFullPath (referenceConfig.GetString("shrimp.pdf.resourcesDirectory"))



