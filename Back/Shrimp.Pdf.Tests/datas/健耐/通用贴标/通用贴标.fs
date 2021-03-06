﻿namespace 通用贴标
[<RequireQualifiedAccess>]
module 通用贴标 =
    open Shrimp.Entities.Types
    open System.IO
    open Shrimp.Pdf.Targets.Types
    open 通用贴标.Orders.Order
    let private name = __SOURCE_FILE__ |> Path.GetFileNameWithoutExtension
    let productInput item =
        ProductInput.create (fun pi ->
            { pi with 
                Name = name
                Kind = ProductKind.createInnerSticker item
                Filter = None
            }
        )
