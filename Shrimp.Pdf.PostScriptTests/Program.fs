﻿// Learn more about F# at http://fsharp.org

open System
open Expecto
open Expecto.Logging
open System.Threading
open Shrimp.Pdf
open Shrimp.FSharp.Plus
open System.Drawing
open System.Collections.Generic
open System.IO

let testConfig =  
    { Expecto.Tests.defaultConfig with 
         verbosity = LogLevel.Debug
         parallelWorkers = 1 }

let allTests =
    testList "All Tests"
        [
            MyTests.myTests
        ]

[<EntryPoint>]
let main argv =
    runTests testConfig allTests
    Console.Read()
    0
