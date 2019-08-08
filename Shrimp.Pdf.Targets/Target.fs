
namespace Atrous.Pdf.Targets



[<RequireQualifiedAccess>]
module Target =
    open iText.Kernel.Font
    open LiteDB.FSharp.Extensions
    open Types
    open Fake.IO.FileSystemOperators
    open Atrous.Entities.Types
    open Atrous.Entities
    open Atrous.Pdf
    open Atrous.Pdf.Extensions
    open Atrous.Extensions
    open Atrous

    let private execute (tarm: TargetModel) f = 
        let globalDirs = tarm.GlobalConfig.GlobalDirs
        PdfFontFactory.RegisterDirectory globalDirs.FontDir |> ignore

        let db = tarm.Database
        let tag = db |> LiteRepository.retrieveTag tarm.CompanyName tarm.TagName
        match tag with 
        | Some tag ->
            let dir = TargetModel.tagDir tarm </> bin </> pk  
            let products = 
                db 
                |> LiteRepository.retrieveProducts tag 
                |> List.map (fun prod -> 
                    { prod with 
                        Orders = 
                            let filter = prod |> Product.getFilter dir
                            prod.Orders |> List.map (fun o ->
                                { o with 
                                    Items =
                                        let items = o.Items |> List.filter filter
                                        items 
                                }
                            )
                            |> List.filter tarm.OrderQuery
                            |> List.filter (fun o -> o.Items.Length <> 0)
                    }
                )
                |> List.filter (fun prod -> prod.Orders.Length <> 0)

            products |> List.iterAsync (fun p -> 
                f tarm p
            )

        | None -> Logger.notImplemented()


    let makeSure tarm =
        execute tarm (Product.makeSure)

    let proof tarm =
        execute tarm (Product.proof)

    let printingOutput tarm =
        execute tarm (Product.commercialOutput)

    let imposeThermal tarm =
        execute tarm (Product.imposeThermal)

    let hangtagOutput (tarm: TargetModel)= 
        execute tarm (Product.hangtagOutput)

    let output tarm =
        execute tarm (Product.output)
