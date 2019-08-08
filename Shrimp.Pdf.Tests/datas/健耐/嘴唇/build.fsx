#load @"../../../packages.fsx"
open Atrous.Pdf.Entities
open Expecto.Logging
open Atrous.Pdf
open Fake.IO.FileSystemOperators
open Fake.Core
open LiteDB.FSharp.Extensions
open Atrous.Entities.Types
open Atrous.Pdf.Targets
open Tests.Types
open LiteDB.FSharp.Linq
open System
open ExcelProcess
open ArrayParser
open FParsec.CharParsers
open Expecto
open MatrixParsers
open FParsec.Primitives
open Input
open System.Collections.Generic

type Lipper =
    {
        DefaultMaterials: Material
        ImageMap: IDictionary<string,string>
        BrandMap: IDictionary<string,string>
        MaterialMap: IDictionary<string,MaterialEnum list>
        DefaultBrand: string
    }

let testConfig =  
    { Expecto.Tests.defaultConfig with 
         parallelWorkers = 1
         verbosity = LogLevel.Debug }
let pass() = Expect.isTrue true "passed"

let lipperDm =
    let itemGenerator =
        fun (dm: DBModel<Lipper>,sheet) ->
            let tagDir = DBModel.tagDir dm
            let lipper = dm.Specfic 
            let materialMap = lipper.MaterialMap
            let brandMap = lipper.BrandMap
            let defaultBrand = lipper.DefaultBrand
            let imageMap = lipper.ImageMap
            let items = 
                let parser = 
                    let pZip =
                        let skip = skipAnyOf [',';'，';'/']
                        tuple4
                            (FParsec.art .>> skip) 
                            (pint32 .>> pstring "双" .>> skip) 
                            (pint32 .>> pstring "箱" .>> skip) 
                            (pstring "条形码" >>. skipAnyOf ['：';':'] >>. pint64)
                        |> (!^)
                    r3 pZip (!!(xPlaceholder 1) <==> XLParser.pSize) (mxRowMany (XLParser.pColor <==> XLParser.pFraction))

                runMatrixParser parser sheet
                |> Seq.collect 
                    ( fun 
                        (
                            (art,number,cartonNumber,barcode),
                            ((),sizes),
                            colorFractionsZips
                        ) ->
                            let barcodeText = barcode.ToString()
                            if not <| Barcode.isValidEan13 barcodeText then 
                                failwithf "invalid ean13 %A" barcodeText
                            let items = 
                                colorFractionsZips |> List.collect (fun (color,fractions) ->
                                    fractions |> List.map2 (fun size fraction -> 
                                        Item.create (fun item ->
                                            { item with
                                                Color = Some color
                                                Number = fraction * cartonNumber
                                                Barcode =  barcodeText |> Some
                                                Art = art
                                                EUR = size |> float
                                                Fraction = Some fraction
                                                Image = 
                                                    let path =
                                                        if imageMap.ContainsKey art then
                                                            tagDir </> ShoeImagesFolderName </> imageMap.[art]
                                                        else 
                                                            defaultShoeImage
                                                    path |> Some
                                                Material = 
                                                        if materialMap.ContainsKey art then 
                                                            materialMap.[art] |> Material.fromEnums materialDir
                                                        else 
                                                            deafultMaterials
                                                        |> Some
                                                Brand = 
                                                        if brandMap.ContainsKey art then 
                                                            brandMap.[art]
                                                        else 
                                                            defaultBrand
                                                        |> Some

                                            }
                                        ) 
                                    ) sizes
                                )

                            let isvalidData =
                                let accum = items |> List.sumBy (fun item -> item.Number)
                                let b = accum = number
                                assert b

                            items
                    ) |> List.ofSeq

            let isValidData =
                let accum = items |> List.sumBy (fun item -> item.Number)
                let calCartonNum = 
                    items 
                    |> List.groupBy (fun item -> item.Art) 
                    |> List.sumBy (fun (_,items) -> 
                        let fractionSum = items |> List.sumBy (fun item -> item.Fraction.Value)
                        let num = items |> List.sumBy (fun item -> item.Number)
                        num / fractionSum
                    ) 

                let _,(number,cartonNum) = 
                    let parser = 
                        !^(pstring "合计") 
                        <==> !^ ((pint32 .>> (pstring "双" <|> pstring "") .>> pstring "/") .>>. (pint32 .>> pstring "箱")) 
                    runMatrixParser parser sheet
                    |> Seq.exactlyOne
                let p = accum = number && cartonNum = calCartonNum
                assert p
            items
    let productGen =
        fun (items: Item list,orders) ->
            [
                {
                    Id = 0
                    Name = "鞋图贴标"
                    Orders = orders
                    Paper = None
                    UnitPrice = 0.1
                    Kind = ProductKind.createStickerWithItem items.[0]
                }
                {
                    Id = 0
                    Name = "成分条码标"
                    Orders = orders
                    Paper = None
                    UnitPrice = 0.1
                    Kind = 
                        ProductKind.createStickerWithItem2 items.[0] [Item.Color]
                }
                {
                    Id = 0
                    Name = "外箱条码"
                    Orders = orders
                    Paper = None
                    UnitPrice = 0.1
                    Kind = 
                        ProductKind.createStickerOfCarton [Item.Barcode]
                }
                {
                    Id = 0
                    Name = "吊牌"
                    Orders = orders
                    Paper = Some Paper.``300gmsCoated``
                    UnitPrice = 0.1
                    Kind = 
                        { 
                            Front = DocType.Pdf {DesiredColor = DesiredColor.Double (ColorMap.Magenta,ColorMap.Black);Bleed = 0.}
                            Back = 
                                {
                                    PrinterKeys = [Item.Art;Item.Barcode]
                                    MakesureSelectors = []
                                    ImposerSelectors = 
                                        {
                                            IsOrderMerged = true
                                            Value = [Selector.Art]
                                        }
                                } |> DocType.simpleBtw |> Some
                            CuttingLine = false
                            HangtagTemplate = 
                                { 
                                    Rotation = Some Rotation.Clockwise
                                    Background =
                                        {
                                            Id = 0
                                            Size = { Width = mm 30; Height = mm 40 }
                                            HSpace = 0.
                                            VSpace = 0.
                                            ColNum = 12
                                            RowNum = 6
                                            Margin = Margin.createSimple (mm 6.)
                                        } 
                                }
                                |> Some
                                    
                        } |> ProductKind.Hangtag 
                }
            ]
    let lipper =
        {
            DefaultMaterials = [MaterialEnum.Textil; MaterialEnum.Textil; MaterialEnum.Rubber] |> Material.fromEnums materialDir
            ImageMap = dict []
            BrandMap = dict []
            MaterialMap = dict []
            DefaultBrand = "LIP LOVER"
        }
    {
        CompanyName = "健耐"
        TagName = "嘴唇"
        OrderName = "18SPX36"
        OrderXLSXName = sprintf "%s合同生产细节"
        Specfic = lipper
        SheetGenerator = SheetGenerator.Sheet1
        ProductsGenerator = productGen
        ItemsGenerator = itemGenerator
    }
run lipperDm