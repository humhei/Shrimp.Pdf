// Learn more about F# at http://fsharp.org

open System
open Expecto
open Expecto.Logging
open System.Threading


let testConfig =  
    { Expecto.Tests.defaultConfig with 
         parallelWorkers = 1
         verbosity = LogLevel.Debug }

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
    Thread.Sleep(2000)
    Console.Read()
