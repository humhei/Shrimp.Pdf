// Learn more about F# at http://fsharp.org

open System
open Shrimp.Pdf.ImageConverter.Server
open Shrimp.FSharp.Plus
open System.Threading

[<EntryPoint>]
let main argv =
    //let pdfFile =
    //    PdfFile 
    //        @"C:\Users\Administrator\Desktop\1234\新建文档1.pdf"

    //let targetDir =
    //    FsFullPath @"C:\Users\Administrator\Desktop\1234"

    //let r = RemoteServer.convertPdfFileToJpegs(pdfFile, 72.f, targetDir).Result
    RemoteServer.createServer() |> ignore

    (new ManualResetEventSlim(false)).Wait()

    printfn "Hello World from F#!"
    0 // return an integer exit code
