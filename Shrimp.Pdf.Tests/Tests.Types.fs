[<AutoOpen>]
module Types
open Expecto
open Shrimp.Pdf.Colors
open Shrimp.Pdf.Extensions
open Fake.IO



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