namespace 通用贴标
open Expecto
open System
open ExcelProcess
open Atrous.Entities
open Tests.Types
open Input
open 通用贴标.Orders.Order

module Build =
    open Atrous.Pdf.Targets.Types

    let productInputs =
        [
            通用贴标.productInput
            //Tru拷贝纸.productInput
            //Mosaic拷贝纸.productInput
        ]

    let parts = __SOURCE_DIRECTORY__.Split([|"\\"|],StringSplitOptions.None)
    let dm: DBModel = 
        {
            CompanyName = parts |> Seq.item (parts.Length - 2)
            TagName = parts |> Seq.last
            OrderNames = ["18SPL98"]
            OrderXLSXName = xlsxName
            SheetGenerator = SheetGenerator.Automatic
            ProductsGenerator = DBModel.generateProducts productInputs
            ItemsGenerator = itemGenerator
        }


