namespace RealClothing
[<RequireQualifiedAccess>]
module 吊牌 =
    open Shrimp.Pdf
    open System.IO
    open Shrimp.Pdf.Targets.Types
    open Shrimp.Entities.Types
    open Shrimp.Pdf.Colors
    open Shrimp.Pdf.Types
    open Shrimp.Pdf.Utils

    let private name = __SOURCE_FILE__ |> Path.GetFileNameWithoutExtension
    let productInput item =
        ProductInput.create (fun pi ->
            { pi with 
                Name = name
                Kind = 
                    {
                        Front = DocType.Pdf {Bleed = Some Margin.empty}
                        Back = 
                            {Variable = Variable.createSimple item; Bleed = None} 
                            |> DocType.Btw
                            |> Some
                        CuttingLine = Some CuttingLine.Impress
                        AutoSetHangtagTemplate = 
                            {
                                RetrieveFrom = RetrieveFrom.Fsx
                                MA = MA.Auto
                            }
                    } |> ProductKind.Hangtag
                Filter = None
            }
        )
