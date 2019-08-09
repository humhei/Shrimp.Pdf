namespace Shrimp.Pdfargets.ImposeThermal
[<RequireQualifiedAccess>]
module Iac =
    open Fake.IO
    open System.IO
    open Fake.IO.FileSystemOperators
    open Shrimp.Pdf
    open iText.Kernel.Geom
    open System.IO.Compression
    open Shrimp.Pdf.Targets

    let saveToEps (srcs: string list) = 
        srcs |> List.map (fun src ->
            async {
                let dest = src |> Path.changeExtension ".eps"
                Iac.saveToEPS src dest
            }
        )
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore

    let saveToEpsWithZip (srcs: string list) = 

        srcs |> List.map (fun src ->
            async {
                let dir = Path.getDirectory src </> "tmp"
                let zipPath,name = 
                    let dir = Path.getDirectory src
                    let dirName = dir |> Path.GetFileName
                    let zipName = sprintf "%s.zip" dirName
                    dir </> zipName,dirName
                File.delete zipPath
                ensureDir dir
                let dest = 
                    let fileName = sprintf "%s.eps" name
                    dir </> fileName
                Iac.saveToEPS src dest
                ZipFile.CreateFromDirectory(dir,zipPath)
                Directory.delete dir
            }
        )
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore