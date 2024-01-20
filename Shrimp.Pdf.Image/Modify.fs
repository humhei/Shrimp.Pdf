namespace Shrimp.Pdf

open Shrimp.Pdf.Image
open iText.Kernel.Colors

#nowarn "0104"
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open Akkling

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
        

