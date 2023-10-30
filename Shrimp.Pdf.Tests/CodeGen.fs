module FunctionTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
open Shrimp.FSharp.Plus
open System.IO
open Fake.IO
open Shrimp.Pdf.icms2
open Fake.IO.Globbing.Operators
open Shrimp.Pdf.Colors

let functionTests =
  testList "Function Tests" [
    testCase "IndexExpr tests" <| fun _ -> 
        let root = @"D:\Users\Jia\Documents\MyData\Config\pitstop\2016\色彩库\PANTONE® Solid Coated-V4\替代空间：CIE L_a_b_"
        let files = 
            !!(root + "\*.elc")
            |> Seq.iter(fun file ->
                let targetFile = Path.changeExtension ".pdf" file
                File.Move(file, targetFile)
            )

            !!(root + "\*.pdf")
            |> List.ofSeq
            |> List.map PdfFile

        let output = root </> (Path.GetFileName root) + ".txt"
        printfn "BEGIN Writing colors to %s" output
        //let files = !!(root + "\Colors\TPX\*.elc")
        //let output = root </> "Colors" </> "TPX.fsx"
        let strings = 
            files 
            |> List.map (fun file ->
                async {
                    let colors = PdfRunner.ReadColors(file, pageSelector = PageSelector.Last, inShadowMode = false)
                    let colorText = 
                        colors
                        |> List.choose(fun color ->
                            match color with 
                            | FsColor.Separation color ->
                                match color.Color with 
                                | FsValueColor.Lab lab ->
                                    let colorName = color.Name
                                    let colorHexLiteral = lab.HexLiteral
                                    sprintf "    | ``%s`` = %s" colorName colorHexLiteral 
                                    |> Some

                                | _ -> None

                            | _ -> None
                        )


                    return colorText
                }
            ) 
            |> List.ofSeq
            |> Async.Parallel
            |> Async.RunSynchronously
            |> List.concat
        File.WriteAllLines(output, strings)
        printfn "End Writing colors to %s" output
        System.Console.Read()
        ()
  ]
