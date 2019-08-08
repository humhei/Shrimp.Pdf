namespace Bench
[<RequireQualifiedAccess>]
module 男鞋吊牌 =
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
                Kind = 
                    {
                        Front = DocType.Pdf {Bleed = Margin.empty; DesiredColor = DesiredColor.black }
                        Back = None
                        CuttingLine = false
                        HangtagTemplate = None
                    } |> ProductKind.Hangtag
                Filter = 
                    Some (fun item ->
                        match item with 
                        | :? ISex as isex -> Sex.isMan isex.Sex
                        | _ -> false
                    )

            }
        )
