[<AutoOpen>]
module Types
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Fake.IO
open System.IO
open System.Threading
open iText.Kernel.Pdf
open Fake.IO.FileSystemOperators

open Shrimp.FSharp.Plus
open iText.Kernel.Geom
open Shrimp.Pdf
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Colors
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Parser
open iText.Kernel.Colors
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.Kernel.Pdf.Colorspace


let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"

let cuttingLineSeparation =
    { FsSeparation.Name = "CuttingLine"
      BaseColor = FsValueColor.RGB_BLUE
      Transparency = 1.0 }

let cuttingLineSeparationZH =
    { FsSeparation.Name = "刀版"
      BaseColor = FsValueColor.RGB_BLUE
      Transparency = 1.0 }

let createTestPath file = 
    Path.changeExtension ".tests.pdf" file
    

let runTest file flow =
    let newPath = createTestPath file
    File.Copy(file, newPath, true)
        
    let r = run newPath flow   
    r                              
     
let runManyWithBackup (files: string list) outputDir flow =
    Directory.ensure outputDir
    let newPaths =
        files 
        |> List.map (fun path ->
            let newPath = 
                outputDir </> Path.GetFileName(path)
            File.Copy(path, newPath, true)
            newPath
        )

    runMany Configuration.DefaultValue newPaths flow