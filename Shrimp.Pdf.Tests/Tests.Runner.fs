module Runner
open Expecto
open Expecto.Logging
open System
open Tests.Pdf
open Tests.Composite
open Tests.AcrobatIac
open Tests.FParsec
open Tests.AiIac
open Atrous.Pdf.Targets.Remoting
open Tests.Types.Input
open Hello.XLParser

let testConfig =  
    { Expecto.Tests.defaultConfig with 
         verbosity = LogLevel.Debug
         parallelWorkers = 1
         }



[<EntryPoint>]
let main argv =
    //run (通用贴标.Build.dm)    
    //runTests testConfig PdfTests
    runTests testConfig ReuseTests
    //runTests testConfig RegionTests-