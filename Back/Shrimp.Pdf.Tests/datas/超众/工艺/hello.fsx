#I @"D:\VsCode\Gitee\Shrimp\Shrimp.Pdf.Tests\bin\x86\Debug\net462"
#r @"EPPlus.Core.dll"
#r @"ExcelProcesser.dll"
open ExcelProcess
open Shrimp.Pdf.Entities
let xlTecPath =  @"D:\VsCode\Gitee\Shrimp\Shrimp.Pdf.Tests\datas\超众\工艺\工艺1814-1816 口舌转印标（阿达）.xlsx"

let sheet = 
    xlTecPath
    |>Excel.getWorksheetByIndex 1 

let items = XLParser.items "" sheet