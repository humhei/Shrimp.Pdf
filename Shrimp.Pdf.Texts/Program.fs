// Learn more about F# at http://fsharp.org

open System
open Expecto
open Expecto.Logging


let testConfig =  
    { Expecto.Tests.defaultConfig with 
         parallelWorkers = 1
         verbosity = LogLevel.Debug }

let allTests =
    testList "All Tests"
        [
            ReuseTests.reuseTests
        ]

[<EntryPoint>]
let main argv =
    runTests testConfig allTests
