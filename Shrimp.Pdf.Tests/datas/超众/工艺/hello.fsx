#I @"D:\VsCode\Gitee\Atrous\Atrous.Pdf.Tests\bin\x86\Debug\net462"
#r @"EPPlus.Core.dll"
#r @"ExcelProcesser.dll"
open ExcelProcess
open Atrous.Pdf.Entities
let xlTecPath =  @"D:\VsCode\Gitee\Atrous\Atrous.Pdf.Tests\datas\超众\工艺\工艺1814-1816 口舌转印标（阿达）.xlsx"

let sheet = 
    xlTecPath
    |>Excel.getWorksheetByIndex 1 

let items = XLParser.items "" sheet