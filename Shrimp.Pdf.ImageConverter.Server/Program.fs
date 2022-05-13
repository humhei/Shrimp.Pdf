// Learn more about F# at http://fsharp.org

open System
open Shrimp.Pdf.ImageConverter.Server
open Shrimp.FSharp.Plus
open System.Threading

[<EntryPoint>]
let main argv =
    //let pdfFile =
    //    PdfFile 
    //        @"D:\Users\Jia\Documents\MyData\Docs\2017\健耐\CORSO\.flow\VerifyDocuments\#2022-02-10#(健耐, 巨丰2022-2-10)\健耐 巨丰2022-2-10\吊牌改1.pdf"

    //let targetDir =
    //    FsFullPath @"D:\Users\Jia\Documents\MyData\Docs\2017\健耐\CORSO\.flow\VerifyDocuments\#2022-02-10#(健耐, 巨丰2022-2-10)\健耐 巨丰2022-2-10"

    //let r = RemoteServer.convertPdfFileToJpegs(pdfFile, 300.f, targetDir).Result
    RemoteServer.createServer() |> ignore
    (new ManualResetEventSlim(false)).Wait()

    printfn "Hello World from F#!"
    0 // return an integer exit code
