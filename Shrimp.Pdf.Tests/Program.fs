// Learn more about F# at http://fsharp.org

open System
open Expecto
open Expecto.Logging
open System.Threading


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
        ]

[<EntryPoint>]
let main argv =
    runTests testConfig allTests
