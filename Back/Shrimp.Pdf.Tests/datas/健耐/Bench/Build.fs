namespace Bench
open Expecto
open System
open Tests.Types
open Input
open Bench.Orders.Order

module Build =
    open Shrimp.Pdf.Targets.Types

    let productInputs =
        [
            男鞋吊牌.productInput
            女鞋吊牌.productInput
            //吊牌贴标.productInput
            //鞋图贴标.productInput
        ]

    let parts = __SOURCE_DIRECTORY__.Split([|"\\"|],StringSplitOptions.None)

    let dm: DBModel = 
        {
            CompanyName = parts |> Seq.item (parts.Length - 2)
            TagName = parts |> Seq.last
            OrderNames = ["18SPL86"]
            OrderXLSXName = xlsxName
            SheetGenerator = SheetGenerator.Automatic
            ProductsGenerator = DBModel.generateProducts productInputs
            ItemsGenerator = itemGenerator
        }


