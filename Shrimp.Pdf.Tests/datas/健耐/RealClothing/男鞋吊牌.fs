namespace RealClothing
[<RequireQualifiedAccess>]
module 男鞋吊牌 =
    open Atrous.Pdf
    open System.IO
    open Atrous.Pdf.Targets.Types
    open Atrous.Entities.Types
    open Atrous.Pdf.Colors

    let private name = __SOURCE_FILE__ |> Path.GetFileNameWithoutExtension
    let productInput item =
        ProductInput.create (fun pi ->
            { pi with 
                Name = name
                Kind = 
                    {
                        Front = DocType.Pdf {Bleed = None;}
                        Back = None
                        CuttingLine = None
                        AutoSetHangtagTemplate =
                            {
                                RetrieveFrom = RetrieveFrom.Fsx
                                MA = MA.Auto
                            }
                    } |> ProductKind.Hangtag
                Filter = 
                    Some (fun item ->
                        match item with 
                        | :? ISex as isex -> Sex.isMan isex.Sex
                        | _ -> false
                    )

            }
        )
