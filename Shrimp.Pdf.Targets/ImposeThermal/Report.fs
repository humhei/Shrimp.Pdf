namespace Shrimp.Pdfargets.ImposeThermal
open Fake.IO
open iText.Kernel.Pdf
open Shrimp.Pdf
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open Shrimp.Entities.Types
open iText.Layout
open iText.Layout.Element
[<RequireQualifiedAccess>]
module Report =
    open Shrimp.Pdf.Targets
    open iText.Layout.Layout
    open Types

    type Unit =
        {
            OrderNumber: int
            Utilization: float
            PaperNumber: int
            Name: string
            Cost: float
            SalePrice: float
            CurrentIndex: int
            TotalIndex: int
            UnitPrice: float
            Canvas: PdfCanvas
            Page: PdfPage
        }

    type Augument<'parserResult> = 
        {
            Path: string
            TableName: string
            Product: Product
            Thumbnail: ImposerCell
            ReportUnits: Unit list
            ParserResult: 'parserResult
        }
    type FooterArgument =
        {
            ReportUnits: Unit list
            TableName: string
            ProductName: string
        }
    type Scene = 
        {
            Name: string
            Header: string list
            Content: Unit -> string list
            Footer: FooterArgument -> string list
        }
    [<RequireQualifiedAccess>]
    module Scene =
        let house =
            {
                Name = "house"
                Header = ["版数";"名称"; "数量(双)"; "印张数量(张)"; "买价(元)"; "卖价(元)"; "收入(元)"; "利用率"]
                Content = fun (ru: Unit) -> 
                    [
                        string ru.CurrentIndex
                        ru.Name
                        string ru.OrderNumber
                        string ru.PaperNumber
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
                            ru.PaperNumber + paperNumber,
                            ru.Cost + cost,
                            ru.SalePrice + salePrice
                        ) (0,0,0.,0.)
                    let average = reportUnits |> List.averageBy (fun ru -> ru.Utilization)
                    [
                        "合计"
                        sprintf "%s_%s" arg.TableName arg.ProductName
                        string orderNumber
                        string paperNumber
                        sprintf "%.2f" cost
                        sprintf "%.2f" salePrice
                        sprintf "%.2f" (salePrice - cost)
                        sprintf "%.2f%%" (average * 100.)
                    ]
            }
        let reconciliation =
            {
                Name = "reconciliation"
                Header = ["版数"; "名称"; "数量(双)"; "印张数量"]
                Content = fun (ru: Unit) -> 
                    [
                        string ru.CurrentIndex
                        ru.Name
                        string ru.OrderNumber
                        string ru.PaperNumber
                    ] 
                Footer = fun arg ->
                    let reportUnits = arg.ReportUnits
                    let orderNumber,paperNumber = 
                        reportUnits |> List.fold (fun acc ru ->
                            let orderNumber,paperNumber = acc
                            ru.OrderNumber + orderNumber,
                            ru.PaperNumber + paperNumber
                        ) (0,0)
                    [
                        "合计"
                        sprintf "%s_%s" arg.TableName arg.ProductName
                        string orderNumber
                        string paperNumber
                    ]
            }
    let produce (product: Product) (zips: list<TypedImposerDataTable * ImposerTable<_>>)= 
        zips |> List.mapi (fun i (dataTable,imposerTable) -> 

            let orderNumber = dataTable.Rows |> List.map (fun r -> r.Number) |> List.sum |> int
            
            let paperNumber,utilization = 
                let rawPaperNumber = 
                    dataTable.Rows |> List.map (fun r -> r.Number) |> List.max |> int |> fun n -> 2 * n
                let paperNumber = 
                    let numberStragety (number: float) = 
                        roundup (float number * 1.1)
                        + 10
                    float rawPaperNumber |> numberStragety      
                let utilization = 
                    let unitsNumber = imposerTable.Rows |> List.collect (fun r -> r.Units) |> List.length
                    float orderNumber * 2. / float (unitsNumber * rawPaperNumber )
                paperNumber,utilization


            let cost = (float paperNumber) * 1.8 + 20.
            let salePrice = product.UnitPrice * (float orderNumber)
            {
                PaperNumber = paperNumber
                OrderNumber = orderNumber
                Utilization = utilization
                Cost = cost
                SalePrice = salePrice
                Name = dataTable.Name
                CurrentIndex = i + 1
                TotalIndex = zips.Length
                Canvas = imposerTable.Canvas
                Page = imposerTable.Page
                UnitPrice = product.UnitPrice
            }
        )
    let exec (reportArgs: Augument<ParserResult>) = 
        let reportUnits = reportArgs.ReportUnits
        let thumbnail = reportArgs.Thumbnail

        let reportWithScence (scence: Scene) =
            let dest = reportArgs.Path |> Path.changeExtension (sprintf "%s_report.pdf" scence.Name)
            let writer = new PdfWriter(dest)
            let pdf = new PdfDocument(writer)
            let pageSize = PageSize.A4.Rotate()
            let margin = float32 20
            let document = new Document(pdf, pageSize)
            document.SetMargins(margin, margin, margin, margin)

            let table = 
                let args = Array.replicate scence.Header.Length 4.f
                let tb = new Table(args)
                tb.SetWidth(pageSize.GetWidth() - 70.f) |> ignore
                tb
            let dahei = Fonts.daHei()
            let heiti = Fonts.heiti()
            let addBoldCell (s: string) = 
                let c = new Cell()
                c.Add(new Paragraph(s)).SetFont(dahei) |> ignore
                table.AddCell c |> ignore
            let addHeader() = 
                scence.Header |> List.iter addBoldCell

            let addContent() = 
                let addCell (s: string) = 
                    let c = new Cell()
                    c.Add(new Paragraph(s)).SetFont(heiti) |> ignore
                    table.AddCell c |> ignore

                reportUnits |> List.iter (scence.Content >> List.iter addCell)

            let addFooter() = 
                {
                    ReportUnits = reportUnits
                    TableName = reportArgs.TableName
                    ProductName = reportArgs.Product.Name
                } 
                |> scence.Footer 
                |> List.iter addBoldCell
            let addThumbnail() = 
                let height = 
                    let render = table.CreateRendererSubTree().SetParent(document.GetRenderer())
                    let layout = render.Layout(new LayoutContext(new LayoutArea(0, new Rectangle(pageSize.GetWidth(),1000.f))))
                    let totalHeight = layout.GetOccupiedArea().GetBBox().GetHeight()
                    totalHeight + margin + thumbnail.BBox.GetHeight()
                let y = pageSize.GetHeight() - height - margin |> float
                let x = margin |> float
                let xobject = thumbnail.Page.CopyAsFormXObject(pdf) 
                let page = pdf.GetFirstPage()
                let canvas = new PdfCanvas(page)
                let addSizeText = 
                    let width = thumbnail.BBox.GetWidth() |> float
                    let height = thumbnail.BBox.GetHeight() |> float
                    let position = 
                        let x = x + width + float margin
                        let y = y + height / 2.
                        {
                            Box = PageBoxKind.CropBox
                            Orientation = Orientation.LeftBottom (x,y)
                        }

                    let text = reportArgs.ParserResult.SizeText |> sprintf "%smm"
                    let font = heiti
                    PdfCanvas.addText position text font page canvas
                ImposerCell.addToCanvas canvas xobject x y thumbnail

            addHeader()
            addContent()
            addFooter()
            document.Add table |> ignore
            addThumbnail()

            document.Close()
            pdf.Close()

        Scene.house |> reportWithScence
        Scene.reconciliation |> reportWithScence
