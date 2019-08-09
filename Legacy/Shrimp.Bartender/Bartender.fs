namespace Shrimp.Pdf

[<RequireQualifiedAccess>]
module Bartender =

    open Shrimp.Utils.AsyncUtils
    open Seagull.BarTender.Print
    open Fake.IO
    open Microsoft.Win32
    open Seagull.BarTender.Print.Database
    open System

    let monitor = new Object()
    let printWithXlsx (dbPath: string) paths (printerName: string) = 
        lock monitor (fun _ ->
            use btEngine = new Engine()
            try 
                paths |> List.map(fun (btwPath,pdfPath) ->
                    File.delete pdfPath
                    use printerJobControl = Registry.CurrentUser.OpenSubKey(@"Software\Adobe\Acrobat Distiller\PrinterJobControl",true)
                    printerJobControl.SetValue(@"C:\Program Files\Seagull\BarTender Suite\bartend.exe",pdfPath)
                    //printerJobControl.SetValue(@"C:\Windows\splwow64.exe",pdfPath)
                    btEngine.Start()
                    let btFormat = btEngine.Documents.Open(btwPath,printerName)
                    let dbconn = btFormat.DatabaseConnections.[0]
                    let originDB = dbconn.FileName
                    let originFileName = dbconn.FileName
                    dbconn.FileName <- dbPath
                    dbconn.SQLStatement <- "Select * from `Sheet1$`"
                    btFormat.Save()
                    if Path.getDirectory originDB <>  Path.getDirectory dbPath || originDB.Substring(originDB.Length - 7,7) <> "rd.xlsx"
                    then 
                        failwith "Please set btw data binding in designer"
                    else
                        let r = btFormat.Print()
                        waitUntil (fun _ -> File.exists pdfPath)
                        originFileName
                )
            with ex ->
                btEngine.Stop()
                failwithf "%A" ex
        )