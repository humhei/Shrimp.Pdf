namespace Bench
[<RequireQualifiedAccess>]
module 吊牌贴标 =
    open Shrimp.Pdf
    open System.IO
    open Shrimp.Pdf.Targets.Types
    open Shrimp.Entities.Types
    open Shrimp.Pdf.Types
    open Shrimp.Pdf.Utils

    let private name = __SOURCE_FILE__ |> Path.GetFileNameWithoutExtension
    let productInput item =
        ProductInput.create (fun pi ->
            { pi with 
                Name = name
                Kind = ProductKind.setBleed (Margin.createWith (fun mg -> { mg with Left = mm 3.})) (ProductKind.createStickerWithItem item)
                Filter = None
            }
        )
