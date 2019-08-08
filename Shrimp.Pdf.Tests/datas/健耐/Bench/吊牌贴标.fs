namespace Bench
[<RequireQualifiedAccess>]
module 吊牌贴标 =
    open Atrous.Pdf
    open System.IO
    open Atrous.Pdf.Targets.Types
    open Atrous.Entities.Types
    open Atrous.Pdf.Types
    open Atrous.Pdf.Utils

    let private name = __SOURCE_FILE__ |> Path.GetFileNameWithoutExtension
    let productInput item =
        ProductInput.create (fun pi ->
            { pi with 
                Name = name
                Kind = ProductKind.setBleed (Margin.createWith (fun mg -> { mg with Left = mm 3.})) (ProductKind.createStickerWithItem item)
                Filter = None
            }
        )
