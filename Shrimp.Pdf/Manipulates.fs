namespace Shrimp.Pdf
open Shrimp.Pdf.Parser


[<AutoOpen>]
module ManipulateOperator =
    let manipulates (operators: list<Selector * Modifier>) =
        operators
        |> List.map (fun (selector,modifier) ->
            fun (model: FlowModel) (document: IntegralDocument) ->
                IntegralDocument.modify selector modifier document
                model
        ) |> Flow.Manipulates

//module Manipulates =
//    open Shrimp.Pdf.Extensions
    
