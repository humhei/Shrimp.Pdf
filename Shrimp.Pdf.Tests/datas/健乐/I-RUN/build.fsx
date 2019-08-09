#load @"../../../packages.fsx"
open iText.Kernel.Colors
open iText.Kernel.Geom
open Shrimp.Pdf
open Fake.IO.FileSystemOperators
open Fake.Core
open LiteDB
open LiteDB.FSharp
open LiteDB.FSharp.Extensions
open Shrimp.Entities.Types
open Shrimp.Pdf.Targets
open Tests.Types
open LiteDB.FSharp.Linq
open System
open System.Threading
let addDataToDB = 
    useNewDatabase <| fun db ->
        let file = new IRunDataType()
        let orders =
            file.Data 
            |> Seq.filter (fun d -> not <| String.IsNullOrEmpty d.ORDER)
            |> Seq.groupBy (fun d -> d.ORDER)
            |> Seq.map (fun (k,v) -> 
                {
                    Id = 0
                    Name = k
                    Date = v |>  Seq.map (fun r -> r.DATE) |> Seq.distinct |> Seq.exactlyOne
                    Items = v |> Seq.map (fun r ->
                        try 
                            Item.create(fun it ->
                                { it with 
                                    Brand = None
                                    Art = r.ART
                                    Barcode = None
                                    Color = None
                                    EUR = r.EUR
                                    US = Some r.US
                                    Number = int r.NUMBER
                                    Material = 
                                        {
                                            Top = MaterialMiddleWare.fromString  materialDir r.TOP
                                            Middle = MaterialMiddleWare.fromString materialDir r.MIDDLE
                                            Bottom = MaterialMiddleWare.fromString materialDir r.BOTTOM
                                        } |> Some                   
                                    Image = None
                                    Template = Some "模板 1"
                                }
                            )
                        with ex ->
                            printf "invaid input %A" r
                            failwithf "%A" ex
                    ) |> List.ofSeq 
                }
            )
            |> List.ofSeq

        let items = orders |> List.collect (fun o -> o.Items)
        let products =
            [
                {
                    Id = 0
                    Name = "热转印"
                    Kind = ProductKind.Thermal
                    Variable = 
                        {
                            PrinterKeys = [Item.Art; Item.EUR; Item.US; Item.Material; Item.Template]
                            ImposerKeys = [Item.Art]
                        } |> Some
                    Orders = orders
                    UnitPrice = 0.12
                }
            ]
        let tfBk = 
            {
                Id = 0
                Size = { Width = mm 35.; Height = mm 35. }
                HSpace = mm 5.
                VSpace = mm 5.
                ColNum = 7
                RowNum = 9
                Margin = { Left = mm 10.517; Top = mm 10.164; Right = 0.; Bottom = 0. }
            }

        let r =
            db 
            |> LiteRepository.insertItems<Item> items
            |> LiteRepository.insertItems<Order> orders
            |> LiteRepository.insertItems<Product> products
            |> LiteRepository.insertItem<Tag>
                {
                    Id = 0
                    Company = "健乐"
                    Name = "I-RUN"
                    Products = products
                } 
            |> LiteRepository.insertItem<ThermalBackground> tfBk
            |> LiteRepository.query<Order>
            |> LiteQueryable.expand (Expr.prop (fun t -> t.Items))
            |> LiteQueryable.first
        ()

useDatabase <| fun db ->
    {
        Database = db
        GlobalDirs = globalDirs
        OrderPrediate = 
            Expr.prop (fun (o:Order) ->
                [
                    o.Name = "Z17"
                ]
                |> List.forall id )
        CompanyName = "健乐"
        TagName = "I-RUN"
    } |> ImposeThermal.exec
Console.ReadLine()
Thread.Sleep(200000)
printfn "Completed target"