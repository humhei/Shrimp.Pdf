[<AutoOpen>]
module Types
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Fake.IO
open System.IO
open System.Threading
open iText.Kernel.Pdf
open Fake.IO.FileSystemOperators


let runWithBackup path flow =
    let newPath = Path.changeExtension ".tests.pdf" path
    File.Copy(path, newPath, true)

    run { UserState = (); File = newPath } flow

let runManyWithBackup (paths: string list) outputDir flow =
    Directory.ensure outputDir
    let flowModels =
        paths 
        |> List.map (fun path ->
            let newPath = 
                outputDir </> Path.GetFileName(path)
            File.Copy(path, newPath, true)
            { UserState = (); File = newPath }
        )

    runMany flowModels flow