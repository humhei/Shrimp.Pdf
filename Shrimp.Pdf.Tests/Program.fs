// Learn more about F# at http://fsharp.org

open System
open Expecto
open Expecto.Logging
open System.Threading
open Shrimp.Pdf.icms2
open Shrimp.Pdf.Colors
open Shrimp.Pdf.DSL
open Shrimp.FSharp.Plus
open System.Drawing
open System.IO

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
    runTests testConfig allTests
    Console.Read()
    0
