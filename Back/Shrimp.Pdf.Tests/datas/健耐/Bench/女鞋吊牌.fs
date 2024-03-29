﻿namespace Bench
[<RequireQualifiedAccess>]
module 女鞋吊牌 =
    open Shrimp.Pdf
    open System.IO
    open Shrimp.Pdf.Targets.Types
    open Shrimp.Entities.Types
    open Shrimp.Pdf.Types

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
                        | :? ISex as isex -> Sex.isWoman isex.Sex
                        | _ -> false
                    )

            }
        )
