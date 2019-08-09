namespace Shrimp.Pdfargets.Hangtag
[<RequireQualifiedAccess>]
module internal ReportingScene =
    open Shrimp.Pdf.Targets.Reports
    open Shrimp.Pdf.Targets.Types
    open Shrimp.Entities.Types
    open Shrimp.Pdf.Utils

    let house_benefit =
        ReportingScene.PlateScene
            {
                Name = "house"
                Header = ["版数";"名称"; "数量(双)"; "印张数量(张)"; "买价(元)"; "卖价(元)"; "收入(元)"; "利用率"]
                Content = fun (ru: FinanceUnit<_>) -> 
                    [
                        string ru.CurrentIndex
                        ru.Name
                        string ru.OrderNumber
                        string ru.PieceNumber
                        sprintf "%.2f" ru.Cost
                        sprintf "%.2f" ru.SalePrice
                        sprintf "%.2f" (ru.SalePrice - ru.Cost)
                        sprintf "%.2f%%" (ru.Utilization * 100.)
                    ] 
                Footer = fun arg ->
                    let reportUnits = arg.ReportUnits
                    let orderNumber,paperNumber,cost,salePrice = 
                        reportUnits |> List.fold (fun acc ru ->
                            let orderNumber,paperNumber,cost,salePrice = acc
                            ru.OrderNumber + orderNumber,
                            ru.PieceNumber + paperNumber,
                            ru.Cost + cost,
                            ru.SalePrice + salePrice
                        ) (0.,0.,0.,0.)
                    let average = reportUnits |> List.averageBy (fun ru -> ru.Utilization)
                    [
                        "合计"
                        sprintf "%s_%s" arg.TableName arg.ProductName
                        string orderNumber
                        string paperNumber
                        sprintf "%.2f" cost
                        sprintf "%.2f" salePrice
                        sprintf "%.2f" (salePrice - cost)
                    ]
            }


    let house_partition : ReportingScene<HangtagReportInfo,HangtagUnit> =
        ReportingScene.TableScene <|
            {
                Simplify = TypedImposerDataRow.simplify
                Name = "house_partition"
                Header = fun row ->
                    let headers = 
                        row
                        |> TypedImposerDataRow.toZH

                    headers @ ["数量"]
                Content = fun tables ->
                    let table = tables |> List.exactlyOne
                    let accumRows = 
                        table.Rows |> List.groupBy (fun r ->
                            r.Id
                        )
                        |> List.map (fun (id,rows) ->
                            let head = List.head rows
                            { head with 
                                Number = rows |> List.sumBy (fun r -> r.Number)
                                DuplicatedNumber = rows.Length }
                        ) 
                        |> List.sortBy (fun r -> r.Id)

                    accumRows 
                    |> List.map (fun r -> 
                        let headers = r |> TypedImposerDataRow.getValues |> List.map string
                        let number = r.Number
                        headers @ [string number]
                    )

                Footer = fun tables ->
                    let table = tables |> List.exactlyOne
                    let placeHolder =
                        table.Rows.[0] 
                        |> TypedImposerDataRow.getValues 
                        |> List.map string 
                        |> List.map (fun _ -> "")
                        |> List.skip 1

                    let number = table.Rows |> List.sumBy (fun r -> r.Number) |> string
                    ["合计: " + table.Name] @ placeHolder @ [number]
            }
    let house_priting : ReportingScene<HangtagReportInfo,HangtagUnit> =
        ReportingScene.PlateScene <| 
            {
                Name = "house_priting"
                Header = ["版面"; "印张数量(张)"]
                Content = fun (ru: FinanceUnit<_>) -> 
                    let specfic = ru.Specfic
                    let role = specfic.PlateRole
                    [
                        PlateRole.toString role
                        PlateRole.paperInfo role (fun _ -> 
                            sprintf "%s%s%d张(切%s)" 
                                (OpenSize.openTypeText ru.Specfic.OpenSize) 
                                (Paper.toStringZH specfic.Paper) 
                                (roundUp ru.PieceNumber)
                                (OpenSize.openNumText ru.Specfic.OpenSize)
                        )
                    ] 
                Footer = fun arg ->
                    let reportUnits = arg.ReportUnits
                    [
                        sprintf "%s_%s" arg.TableName arg.ProductName
                        HangtagUnit.paperText reportUnits
                    ]
            }
    let reconciliation =
        ReportingScene.PlateScene
            {
                Name = "reconciliation"
                Header = ["版数"; "名称"; "数量(双)"; "印张数量"]
                Content = fun (ru: FinanceUnit<_>) -> 
                    [
                        string ru.CurrentIndex
                        ru.Name
                        string ru.OrderNumber
                        string ru.PieceNumber
                    ] 
                Footer = fun arg ->
                    let reportUnits = arg.ReportUnits
                    let orderNumber,paperNumber = 
                        reportUnits |> List.fold (fun acc ru ->
                            let orderNumber,paperNumber = acc
                            ru.OrderNumber + orderNumber,
                            ru.PieceNumber + paperNumber
                        ) (0.,0.)
                    [
                        "合计"
                        sprintf "%s_%s" arg.TableName arg.ProductName
                        string orderNumber
                        string paperNumber
                    ]
            }


