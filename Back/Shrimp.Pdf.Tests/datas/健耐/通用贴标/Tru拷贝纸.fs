﻿namespace 通用贴标
[<RequireQualifiedAccess>]
module Tru拷贝纸 =
    open Shrimp.Entities.Types
    open System.IO
    open Shrimp.Pdf.Targets.Types
    open 通用贴标.Orders.Order

    let private name = __SOURCE_FILE__ |> Path.GetFileNameWithoutExtension
    let productInput item =
        ProductInput.create (fun pi ->
            { pi with 
                Name = name
                Kind = ProductKind.createCopyPaper
                Filter = 
                    Some (fun item -> 
                        match item with 
                        | :? Item as item -> item.Brand + "拷贝纸" = name
                        | _ -> false 
                    )
            }
        )