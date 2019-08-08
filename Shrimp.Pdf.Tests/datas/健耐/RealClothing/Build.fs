namespace RealClothing
open Expecto
open System
open Tests.Types
open Input
open RealClothing.Orders.Order
open RealClothing.Orders

module Build =
    open Atrous.Pdf.Targets.Types

    let productInputs =
        [
            吊牌.productInput
            //男鞋吊牌.productInput
            //女鞋吊牌.productInput
            //吊牌贴标.productInput
            //鞋图贴标.productInput
        ]

    let parts = __SOURCE_DIRECTORY__.Split([|"\\"|],StringSplitOptions.None)

    let dm: DBModel = 
        {
            CompanyName = parts |> Seq.item (parts.Length - 2)
            TagName = parts |> Seq.last
            OrderNames = ["18SPN35"]
            OrderXLSXName = xlsxName
            SheetGenerator = SheetGenerator.Automatic
            ProductsGenerator = DBModel.generateProducts productInputs
            ItemsGenerator = itemGenerator
        }


