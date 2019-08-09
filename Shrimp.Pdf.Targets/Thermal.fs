
namespace Shrimp.Pdf.Targets

[<RequireQualifiedAccess>]
module Thermal =
    open Shrimp.Pdf
    open iText.Kernel.Pdf.Canvas.Parser.Data
    open LiteDB.FSharp.Extensions
    open LiteDB.FSharp.Linq
    open Shrimp.Entities.Types
    open Fake.IO.FileSystemOperators
    open Shrimp.Pdf.Types
    open FParsec
    open Actions
    open Shrimp.Pdf.Utils
    open Types
    open Reuses
    open Shrimp.Pdf.Reuses
    open Shrimp.Pdf.Manipulates
    open FromState
    open Shrimp.Pdf.Extensions
    open Shrimp.Pdf.Targets.Reports

    [<RequireQualifiedAccess>]
    module ReportingScene =

        let house =
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

    let makeSure (p: Product) tarm targetName (thermal: Thermal) =

        match thermal.DocType with 
        | DocType.Btw btw -> 
            Btw.simpleMakesure p tarm targetName btw.Variable

        | _ -> ()

    let imposeThermal (p: Product) (tarm: TargetModel) targetName (thermal: Thermal) =
        let globalDirs = tarm.GlobalConfig.GlobalDirs
        let background db (tr : TextRenderInfo) (state: ReportInfo<_,_>) =
            let sizeString = tr.GetText()
            let size = sizeString |> FsSize.fromString

            let thBk = 
                db 
                |> LiteRepository.query<ThermalBackground>
                |> LiteQueryable.find (Expr.prop(fun thBk -> FsSize.isRotatedEqual thBk.Size size || thBk.Size = size))

            ReportInfo.fillTemplate thBk tr state (fun arg ->
                let fileName = 
                    match arg.RotateXObjectDegree with 
                    | Some rotation when Rotation.isRotated rotation -> 
                        let parser = pipe2 pint8 (pstring "×" >>. pint8) (fun width height -> sprintf "%d×%d.pdf" height width) 
                        runToResult parser sizeString
                    | _ ->
                        sprintf "%s.pdf" sizeString
                let fileName = globalDirs.TfBackgroundDir </> fileName
                { arg with Background = Background.File fileName |> Some }
            )

        match thermal.DocType with 
        | DocType.Btw btw -> 
            let path = TargetModel.file p ".btw" tarm
            let pdf,tables = Btw.printWithNewer p targetName tarm btw.Variable path
            let selectors = btw.Variable.ImposerSelectors
            let produceReport productName : Flow<ReportInfo<_,_>> =
                Flow.TransformUserStates (fun state ->
                    let reportUnits =
                        tables |> List.zip state.ReportUnits |> List.mapi (fun i tb ->
                            let reportUnit,tb = tb
                            let orderNumber = tb.Rows |> List.map (fun r -> r.Number) |> List.sum 
            
                            let paperNumber,utilization = 
                                let rawPaperNumber = 
                                    tb.Rows |> List.map (fun r -> r.Number) |> List.max |> fun n -> 2. * n 
                                let paperNumber = 
                                    let numberStragety (number: float) = 
                                        float number * 1.1
                                        + 10.
                                    rawPaperNumber |> numberStragety

                                let utilization = 
                                    let unitsNumber = tb.Rows.Length
                                    float orderNumber * 2. / (float unitsNumber * rawPaperNumber)

                                paperNumber,utilization


                            let cost = (float paperNumber) * 1.8 + 20.
                            let salePrice = p.UnitPrice * (float orderNumber)
                            { reportUnit with 
                                PaperNumber = paperNumber
                                OrderNumber = orderNumber
                                Utilization = utilization
                                Cost = cost
                                SalePrice = salePrice
                                Name = tb.Name
                                CurrentIndex = i + 1
                                TotalIndex = tables.Length
                                UnitPrice = p.UnitPrice
                            }
                        )
                    let info =
                        let orderNum = reportUnits |> List.sumBy (fun u -> u.OrderNumber)
                        let printNum = reportUnits |> List.map (fun u -> u.OrderNumber / u.Utilization) |> List.sum
                        { state with 
                            ReportUnits = reportUnits
                            Utilization = orderNum / printNum
                            Cost = reportUnits |> List.sumBy (fun u -> u.Cost)
                            SalePrice = reportUnits |> List.sumBy (fun u -> u.SalePrice)
                            PaperNumber = reportUnits |> List.sumBy (fun u -> u.PieceNumber)
                            OrderNumber = orderNum }

                    info
                )

            let thermalShuffle = 
                single (fun doc (state: FlowState<ReportInfo<_,_>>) ->
                    let userState = state.UserState
                    let arg = userState.Arg

                    let imposeNum = userState.ImposeMatrix ||> (*)

                    ImposerSelectors.cata selectors (fun _ ->
                        let tables =  userState.Tables
                        let typeOrders = OSTypedOrder.init tables doc.Src

                        let tbName = typeOrders |> List.map (fun tpOrder -> tpOrder.TpTable.Name) |> String.concat "_"

                        let typeOrders = 
                            typeOrders 
                            |> List.collect (TypedOrder.splitBySelectors selectors.Value) 
                            |> List.map (TypedOrder.shuffleToImposeNumber arg imposeNum)

                        typeOrders |> List.collect (fun tpOrder -> tpOrder.Pages) |> List.iter (PdfPage.addToDoc doc.Dest)
                        let tables = typeOrders |> List.map (fun tpOrder -> tpOrder.TpTable)
                        let userState = { userState with Tables = tables; TableName = tbName }
                        { state with UserState = userState }
                    )
                )


            let mds = 
                [
                    ImposerSelectors.cata selectors (fun _ -> assignPagesToOrdersOfTarm p tarm tables targetName)
                    manipulates [trimToStrokeBlue]
                    Read.btwSize[background tarm.Database]
                    resizeWithStateOfSize
                    Read.extractFirstPageToBrandPage
                    thermalShuffle
                    imposeWithStateOfArgs
                    produceReport p.Name
                    Flow.FromState (fun state ->
                        let reportUnits = state.UserState.ReportUnits
                        manipulates [
                            fun arg ->
                                let reportUnit = reportUnits.[arg.PageNum - 1]
                                let text = 
                                    sprintf "%s至少印%d张     %s" reportUnit.Name (roundUp reportUnit.PaperNumber) todayMark
                                addTextToLeftBottom text arg
                        ]
                    )
                ]
                |> Shrimp.Pdf.Targets.Operators.run pdf
            let scenes =
                [
                    ReportingScene.house
                    ReportingScene.reconciliation
                ]

            mds |> List.iter (fun md -> scenes |> List.iter (Shrimp.Pdf.Targets.Reports.ReportingScene.report md))

        | _ -> ()

