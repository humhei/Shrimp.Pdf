// Learn more about F# at http://fsharp.org

open Expecto.Logging
open Expecto
open MyTests
let testConfig =  
    { Expecto.Tests.defaultConfig with 
         parallelWorkers = 1
         verbosity = LogLevel.Debug }


let Tests = 
    testList "All tests" [  
        MyTests
    ]

[<EntryPoint>]
let main argv = 
    runTests testConfig Tests