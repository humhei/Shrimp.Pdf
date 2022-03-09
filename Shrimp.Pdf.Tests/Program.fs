// Learn more about F# at http://fsharp.org

open System
open Expecto
open Expecto.Logging
open System.Threading
open Shrimp.Pdf.icms2


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
        ]

[<EntryPoint>]
let main argv =
    //SetClientContext()
    runTests testConfig allTests
    Console.Read()
    0
