// Learn more about F# at http://fsharp.org

open System
open Expecto
open Expecto.Logging
open System.Threading
open Shrimp.Pdf.icms2
open Shrimp.Pdf.Colors
open Shrimp.Pdf.DSL
open Shrimp.Pdf
open Shrimp.Pdf.Image
open Shrimp.FSharp.Plus
open System.Drawing
open Shrimp.Pdf.IntegratedInfos
open System.IO
open iText.IO.Image

[<RequireQualifiedAccess>]
type MaterialBorders =
    | All
    | Outer
    | OuterAndHorizontal
    | None

let testConfig =  
    { Expecto.Tests.defaultConfig with 
         verbosity = LogLevel.Debug
         parallelWorkers = 1 }

let allTests =
    testList "All Tests"
        [
            ReuseTests.reuseTests
            FileOperationTests.fileOperationTests
            ManipulateTests.manipulateTests
            RealSampleTests.realSamplesTests
            FunctionTests.functionTests
            FlowNameTests.flowNameTests
            BugFixmentTests.bugFixmentTests
            FlowTests.flowTests
            icms2Tests.icmsTests
            ImageTests.imageTests
        ]

[<EntryPoint>]
let main argv =
    //let bmp = @"D:\VsCode\Workspace\Shrimp.Pdf\Shrimp.Pdf.Tests\datas\image\convert cmyk image to gray1.jpg"
    //let bytes = File.ReadAllBytes bmp
    
    //let imageData = ImageDataFactory.Create(bytes)

    //let m =  System.Text.Encoding.Unicode.GetString(bytes)


    //BitmapUtils.ReadColorValues(new Bitmap(bmp))
    //|> BitmapColorValues.toIndexableStorage ({ColorSpace = ColorSpace.Cmyk; IndexTable = None})
    //|> ignore
    runTests testConfig allTests
    Console.Read()
    0
