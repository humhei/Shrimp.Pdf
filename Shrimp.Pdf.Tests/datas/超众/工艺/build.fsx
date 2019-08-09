#load @"../../../packages.fsx"
open Fake.Runtime
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
open FSharp.ExcelProvider
open ExcelProcess
open ArrayParser
open FParsec.CharParsers
open Shrimp.Pdf.Entities

let xlTecPath =  root + @"\超众\工艺\工艺1814-1816 口舌转印标（阿达）.xlsx"
useNewDatabase <| fun db ->
    let orderName = "工艺1814-1816"

    let sheet = 
        xlTecPath
        |>Excel.getWorksheetByIndex 1 

    let items = XLParser.items materialDir sheet
    let order = 
        {
            Id = 0 
            Items = items
            Name = orderName
            Date = DateTime.UtcNow.Date
        }

    let product = 
        {
            Id = 0
            Name = "热转印"
            Orders = [order]
            Paper = None
            UnitPrice = 0.1
            Kind = ProductKind.createThermalWithItem items.[0]
        }

    let r =
        db 
        |> LiteRepository.insertItems<Item> items
        |> LiteRepository.insertItems<Order> [order]
        |> LiteRepository.insertItems<Product> [product]
        |> LiteRepository.insertItem<Tag>
            {
                Id = 0
                Company = "超众"
                Name = "工艺"
                Products = [product]
            } 
        |> LiteRepository.query<Tag>
        |> LiteQueryable.expand (Expr.prop (fun t -> t.Products))
        |> LiteQueryable.first
    ()

useDatabase <| fun db ->
    {
        Database = db
        GlobalDirs = globalDirs
        OrderPrediate = 
            Expr.prop (fun (o:Order) ->
                [
                    o.Date = new DateTime(2018,6,30)
                ]
                |> List.forall id
            )
        CompanyName = "超众"
        TagName = "工艺"
    } |> Target.makeSure

    Console.ReadLine() |> ignore