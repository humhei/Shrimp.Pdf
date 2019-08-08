// Learn more about F# at http://fsharp.org

module Tests.Runner
open System
open Expecto.Logging
open Expecto
open MyTests
let testConfig =  
    { Expecto.Tests.defaultConfig with 
         verbosity = LogLevel.Debug
         parallelWorkers = 1
         }
[<EntryPoint>]
let main argv =
    runTests testConfig myTests
