namespace Shrimp.Pdf

open iText.Kernel.Pdf
open Shrimp.Pdf.Image
open iText.Kernel.Colors

#nowarn "0104"
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open Shrimp.Pdf
open System.IO
open iText.Kernel.Pdf.Xobject
open Shrimp.Pdf.Parser
open System.Drawing
open Shrimp.FSharp.Plus
open Shrimp.Pdf.Colors
open Akkling
open Shrimp.Pdf.icms2
open iText.Layout.Element
open Fake.IO
open Shrimp.Pdf.icms2.client
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.IO.Image
open FSharp.Data
open System.Collections.Concurrent
open Shrimp.Pdf.js.shared

[<AutoOpen>]
module _Image_Modify  =
    type ModifyIM with
        static member AddBoundToImages(?selector, ?canvasAddRectangleArgs) =
            let canvasAddRectangleArgs =
                match canvasAddRectangleArgs with 
                | None -> fun (args: PdfCanvasAddRectangleArguments) -> 
                    { args with StrokeColor = NullablePdfCanvasColor.OfPdfCanvasColor(PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA)}
                | Some args -> args
                    
            Modify.Create_RecordIM(
                PageSelector.All,
                [
                    { SelectorAndModifiersRecordIM.Name = "add bound to images"
                      Selector = ImageX (defaultArg selector (fun _ _ -> true))
                      Modifiers = [
                        ModifierIM.AddImageBorder(canvasAddRectangleArgs)
                      ]
                    }
                ]
            )
        

