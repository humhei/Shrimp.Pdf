module ManipulateTests
open Expecto
open Shrimp.Pdf.Reuses
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Fake.IO
open System.IO

let runWithBackup path =
    let newPath = Path.changeExtension ".tests.pdf" path
    File.Copy(path, newPath, true)
    run newPath

let manipulateTests =
  testList "Manipulates Tests" [
    testCase "change stroke color b255 to m100" <| fun _ -> 
        [
            manipulates [
                (
                    Selector.Path (fun pathRenderInfo -> 
                        pathRenderInfo.GetStrokeColor() = DeviceRgb.BLUE),
                    Modifier.Actions [
                        PdfCanvas.setStrokeColor DeviceCmyk.MAGENTA
                    ]
                )
                    
            ]
        ]
        |> runWithBackup "datas/manipulate/change stroke color b255 to m100.pdf" 
        |> ignore
  ]