namespace RealClothing
[<RequireQualifiedAccess>]
module 吊牌 =
    open Atrous.Pdf
    open System.IO
    open Atrous.Pdf.Targets.Types
    open Atrous.Entities.Types
    open Atrous.Pdf.Colors
    open Atrous.Pdf.Types
    open Atrous.Pdf.Utils

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
