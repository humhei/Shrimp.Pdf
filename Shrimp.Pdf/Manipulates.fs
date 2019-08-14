namespace Shrimp.Pdf
open Shrimp.Pdf.Parser
open iText.Layout



[<AutoOpen>]
module ManipulateOperator =
    let manipulates (pageSelector: PageSelector) pageModifiers (operators: list<RenderInfoSelector * SelectionModifier>)  =
        (
            pageModifiers
            |> List.map (fun (pageBoxKind: PageBoxKind, actions: list<Canvas -> Canvas>) ->
                fun (model: FlowModel) (document: IntegralDocument) ->
                    IntegralDocument.addNew pageSelector pageBoxKind actions document
                    model
            )
        )
        @
        (
            operators
            |> List.map (fun (renderInfoSelector, modifier) ->
                fun (model: FlowModel) (document: IntegralDocument) ->
                    IntegralDocument.modify (pageSelector) renderInfoSelector modifier document
                    model
            ) 
        ) |> Flow.Manipulates




//module Manipulates =
//    open Shrimp.Pdf.Extensions
    
