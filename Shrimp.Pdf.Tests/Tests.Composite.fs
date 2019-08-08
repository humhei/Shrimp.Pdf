module Tests.Composite
//open Expecto
//open System
//open Atrous.Pdf
//open LiteDB
//open Types
//open FSharp.ExcelProvider
//open Fake.IO.FileSystemOperators
//open Types
//open LiteDB.FSharp.Extensions
//open LiteDB.FSharp.Linq
//open Atrous.Entities.Types
//open Atrous.Pdf.Targets
//open ExcelProcess
//open FParsec
//open ArrayParsers
//open Atrous.Entities
//open MatrixParsers
//open SheetParsers
//open System.Diagnostics
//open Input
//open System.Collections.Generic
//open OfficeOpenXml
//open XLParser
//let pass() = Expect.isTrue true "passed"
//let fail() = Expect.isTrue false "failed"
//type Lipper =
//    {
//        DefaultMaterials: Material
//        ImageMap: IDictionary<string,string>
//        BrandMap: IDictionary<string,string>
//        MaterialMap: IDictionary<string,MaterialEnum list>
//        DefaultBrand: string
//    }
//let lipperDm =
//    let itemGenerator =
//        fun (dm: DBModel<Lipper>,sheet) ->
//            let tagDir = DBModel.tagDir dm
//            let lipper = dm.Specfic 
//            let materialMap = lipper.MaterialMap
//            let brandMap = lipper.BrandMap
//            let defaultBrand = lipper.DefaultBrand
//            let imageMap = lipper.ImageMap
//            let items = 
//                let parser = 
//                    let pZip =
//                        let skip = skipAnyOf [',';'，';'/']
//                        tuple4
//                            (FParsec.art .>> skip) 
//                            (pint32 .>> pstring "双" .>> skip) 
//                            (pint32 .>> pstring "箱" .>> skip) 
//                            (pstring "条形码" >>. skipAnyOf ['：';':'] >>. pint64)
//                        |> (!^)
//                    r3 pZip (!!(xPlaceholder 1) <==> SeqBySpace.pSize) (mxRowMany (Single.pColor <==> SeqBySpace.pFraction))

//                runMatrixParser parser sheet
//                |> Seq.collect 
//                    ( fun 
//                        (
//                            (art,number,cartonNumber,barcode),
//                            ((),sizes),
//                            colorFractionsZips
//                        ) ->
//                            let barcodeText = barcode.ToString()
//                            if not <| Barcode.isValidEan13 barcodeText then 
//                                failwithf "invalid ean13 %A" barcodeText
//                            let items = 
//                                colorFractionsZips |> List.collect (fun (color,fractions) ->
//                                    fractions |> List.map2 (fun size fraction -> 
//                                        Item.create (fun item ->
//                                            { item with
//                                                Color = Some color
//                                                Number = fraction * cartonNumber
//                                                Barcode =  barcodeText |> Some
//                                                Art = art
//                                                EUR = size
//                                                Fraction = Some fraction
//                                                Image = 
//                                                    let path =
//                                                        if imageMap.ContainsKey art then
//                                                            tagDir </> ShoeImagesFolderName </> imageMap.[art]
//                                                        else 
//                                                            defaultShoeImage
//                                                    path |> Some
//                                                Material = 
//                                                        if materialMap.ContainsKey art then 
//                                                            materialMap.[art] |> Material.fromEnums materialDir
//                                                        else 
//                                                            deafultMaterials
//                                                        |> Some
//                                                Brand = 
//                                                        if brandMap.ContainsKey art then 
//                                                            brandMap.[art]
//                                                        else 
//                                                            defaultBrand
//                                                        |> Some

//                                            }
//                                        ) 
//                                    ) sizes
//                                )

//                            let isvalidData =
//                                let accum = items |> List.sumBy (fun item -> item.Number)
//                                let b = accum = number
//                                Debug.Assert(b)

//                            items
//                    ) |> List.ofSeq

//            let isValidData =
//                let accum = items |> List.sumBy (fun item -> item.Number)
//                let calCartonNum = 
//                    items 
//                    |> List.groupBy (fun item -> item.Art) 
//                    |> List.map (fun (_,items) -> 
//                        let fractionSum = items |> List.sumBy (fun item -> item.Fraction.Value)
//                        let num = items |> List.sumBy (fun item -> item.Number)
//                        num / fractionSum
//                    ) 
//                    |> List.sum 

//                let number,cartonNum = 
                    
//                    runMatrixParser Single.pTotal sheet
//                    |> Seq.exactlyOne
//                let p = accum = number && cartonNum = calCartonNum
//                Debug.Assert (p)
//            items
//    let productGen =
//        fun (items: Item list,orders) ->
//            [
//                Product.createWith (fun p ->
//                    {
//                        p with 
//                            Id = 0
//                            Name = "鞋图贴标"
//                            Orders = orders
//                            Paper = None
//                            UnitPrice = 0.1
//                            Kind = ProductKind.createStickerWithItem items.[0]
//                    }
//                )
//                Product.createWith (fun p ->
//                    {
//                        p with 
//                            Id = 0
//                            Name = "成分条码标"
//                            Orders = orders
//                            Paper = None
//                            UnitPrice = 0.1
//                            Kind = 
//                                ProductKind.createStickerWithItem2 items.[0] [Item.Color]
//                    }
//                )
//                Product.createWith (fun p ->
//                    {
//                        p with 
//                            Id = 0
//                            Name = "外箱条码"
//                            Orders = orders
//                            Paper = None
//                            UnitPrice = 0.1
//                            Kind = 
//                                ProductKind.createStickerOfCarton [Item.Barcode]
//                    }
//                )
//                Product.createWith (fun p ->
//                    {
//                        p with 
//                            Id = 0
//                            Name = "吊牌"
//                            Orders = orders
//                            Paper = Some Paper.``300gmsCoated``
//                            UnitPrice = 0.1
//                            Kind = 
//                                { 
//                                    Front = DocType.Pdf {DesiredColor = DesiredColor.Double (ColorMap.Magenta,ColorMap.Black);Bleed = 0.}
//                                    Back = 
//                                        {
//                                            PrinterKeys = [Item.Art;Item.Barcode]
//                                            MakesureSelectors = []
//                                            ImposerSelectors = 
//                                                {
//                                                    IsOrderMerged = true
//                                                    Value = [Selector.Art]
//                                                }
//                                        } |> DocType.simpleBtw |> Some
//                                    CuttingLine = false
//                                    HangtagTemplate = 
//                                        { 
//                                            Rotation = Some Rotation.Clockwise
//                                            Background =
//                                                {
//                                                    Id = 0
//                                                    Size = { Width = mm 30; Height = mm 40 }
//                                                    HSpace = 0.
//                                                    VSpace = 0.
//                                                    ColNum = 12
//                                                    RowNum = 6
//                                                    Margin = Margin.imposingDest
//                                                } 
//                                        }
//                                        |> Some
                                    
//                                } |> ProductKind.Hangtag 
//                    }
//                )
//            ]
//    let lipper =
//        {
//            DefaultMaterials = [MaterialEnum.Textil; MaterialEnum.Textil; MaterialEnum.Rubber] |> Material.fromEnums materialDir
//            ImageMap = dict []
//            BrandMap = dict []
//            MaterialMap = dict []
//            DefaultBrand = "LIP LOVER"
//        }
//    {
//        CompanyName = "健耐"
//        TagName = "嘴唇"
//        OrderName = "18SPX36"
//        OrderXLSXName = sprintf "%s合同生产细节"
//        Specfic = lipper
//        SheetGenerator = SheetGenerator.Sheet1
//        ProductsGenerator =  fun _ ->  productGen
//        ItemsGenerator = itemGenerator
//    }


////let IRUNTests =
////  testList "IRUNTests" [
////    ftestCase "add data to liteDB" <| fun _ -> 
////        useNewDatabase <| fun db ->
////            let file = new IRunDataType()
////            let orders =
////                file.Data 
////                |> Seq.filter (fun d -> not <| String.IsNullOrEmpty d.ORDER)
////                |> Seq.groupBy (fun d -> d.ORDER)
////                |> Seq.map (fun (k,v) -> 
////                    {
////                        Id = 0
////                        Name = k
////                        Date = v |>  Seq.map (fun r -> r.DATE) |> Seq.distinct |> Seq.exactlyOne
////                        Items = v |> Seq.map (fun r ->
////                            try 
////                                Item.create(fun it ->
////                                    { it with 
////                                        Brand = None
////                                        Art = r.ART
////                                        Barcode = None
////                                        Color = None
////                                        EUR = r.EUR
////                                        US = Some r.US
////                                        Number = int r.NUMBER
////                                        Material = 
////                                            {
////                                                Top = MaterialMiddleWare.fromString  materialDir r.TOP
////                                                Middle = MaterialMiddleWare.fromString materialDir r.MIDDLE
////                                                Bottom = MaterialMiddleWare.fromString materialDir r.BOTTOM
////                                            } |> Some                   
////                                        Image = None
////                                        Template = Some "模板 1"
////                                    }
////                                )
////                            with ex ->
////                                printf "invaid input %A" r
////                                failwithf "%A" ex
////                        ) |> List.ofSeq 
////                    }
////                )
////                |> List.ofSeq

////            let items = orders |> List.collect (fun o -> o.Items)
////            let products =
////                [
////                    {
////                        Id = 0
////                        Name = "热转印"
////                        Kind = ProductKind.Thermal None
////                        Variable = 
////                            {
////                                PrinterKeys = [Item.Art; Item.EUR; Item.US; Item.Material; Item.Template]
////                                MakesureSelectors = [Selector.Art]
////                                ImposerSelectors = []
////                            } |> Some
////                        Orders = orders
////                        UnitPrice = 0.12
////                    }
////                ]
////            let tfBk = 
////                {
////                    Id = 0
////                    Size = { Width = mm 35.; Height = mm 35. }
////                    HSpace = mm 5.
////                    VSpace = mm 5.
////                    ColNum = 7
////                    RowNum = 9
////                    Margin = { Left = mm 10.517; Top = mm 10.164; Right = 0.; Bottom = 0. }
////                }

////            let r =
////                db 
////                |> LiteRepository.insertItems<Item> items
////                |> LiteRepository.insertItems<Order> orders
////                |> LiteRepository.insertItems<Product> products
////                |> LiteRepository.insertItem<Tag>
////                    {
////                        Id = 0
////                        Company = "健乐"
////                        Name = "I-RUN"
////                        Products = products
////                    } 
////                |> LiteRepository.insertItem<ThermalBackground> tfBk
////                |> LiteRepository.query<Order>
////                |> LiteQueryable.expand (Expr.prop (fun t -> t.Items))
////                |> LiteQueryable.first

////            pass()

////    testCase "makeSure" <| fun _ -> 
////        useDatabase <| fun db ->
////            {
////                Database = db
////                GlobalDirs = globalDirs
////                OrderQuery = 
////                    Expr.prop (fun (o:Order) ->
////                        [
////                            o.Name = "Z01"
////                            //o.Date = new DateTime(2018,4,8)
////                        ]
////                        |> List.forall id )
////                CompanyName = "健乐"
////                TagName = "I-RUN"
////            } |> Makesure.exec

////            Console.ReadLine() |> ignore
////    ftestCase "imposing transfer" <| fun _ -> 
////        useDatabase <| fun db ->
////            {
////                Database = db
////                GlobalDirs = globalDirs
////                OrderQuery = 
////                    Expr.prop (fun (o:Order) ->
////                        [
////                            o.Name = "Z17"
////                        ]
////                        |> List.forall id )
////                CompanyName = "健乐"
////                TagName = "I-RUN"
////            } |> ImposeThermal.exec
////            Console.ReadLine() |> ignore
////  ]

//let [<Literal>] private xlHangtenPath =  root + @"\双优星\Hangten\数据库.xlsx"
//type HangtenDataType = ExcelFile<xlHangtenPath,"Sheet1">
//let HangtenTests =
//  testList "HangtenTests" [
//    ftestCase "add data to liteDB" <| fun _ -> 
//        useNewDatabase <| fun db ->
//            let file = new HangtenDataType()
//            let orders =
//                file.Data 
//                |> Seq.filter (fun d -> not <| String.IsNullOrEmpty d.ORDER)
//                |> Seq.groupBy (fun d -> d.ORDER)
//                |> Seq.map (fun (k,v) -> 
//                    {
//                        Id = 0
//                        Name = k
//                        Date = v |>  Seq.map (fun r -> r.DATE) |> Seq.distinct |> Seq.exactlyOne
//                        Items = v |> Seq.map (fun r ->
//                            try 
//                                Item.create(fun it ->
//                                    { it with 
//                                        Brand = None
//                                        Art = r.ART
//                                        Barcode = Some (string r.BARCODE)
//                                        Color = Some (ColorEnum.fromString r.COLOR)
//                                        EUR = int r.EUR
//                                        UK = Some r.UK
//                                        US = Some r.US
//                                        Number = int r.NUMBER
//                                        Material = 
//                                            {
//                                                Top = MaterialMiddleWare.fromString  materialDir r.TOP
//                                                Middle = MaterialMiddleWare.fromString materialDir r.MIDDLE
//                                                Bottom = MaterialMiddleWare.fromString materialDir r.BOTTOM
//                                            } |> Some          
//                                        Image = None
//                                        Template = None
//                                    }
//                                )
//                            with ex ->
//                                printf "invaid input %A" r
//                                failwithf "%A" ex
//                        ) |> List.ofSeq 
//                    }
//                )
//                |> List.ofSeq

//            let items = orders |> List.collect (fun o -> o.Items)
//            let products =
//                [
//                    Product.createWith (fun p ->
//                        {
//                            p with 
//                                Id = 0
//                                Name = "热转印"
//                                Kind = 
//                                    ProductKind.Thermal
//                                        (
//                                            {
//                                                PrinterKeys = [Item.Art; Item.EUR; Item.UK; Item.US; Item.Barcode; Item.Material]
//                                                MakesureSelectors = [Selector.Art]
//                                                ImposerSelectors = 
//                                                    { 
//                                                        Value =[{Selector.Art with Pattern = [|'-'|] |> Pattern.Before |> Some }]
//                                                        IsOrderMerged = true
//                                                    }
//                                            } |> DocType.simpleBtw
//                                        )
//                                Orders = orders
//                                UnitPrice = 0.1
//                                Paper = None
//                        }
//                    )
//                    //{
//                    //    Id = 0
//                    //    Name = "内盒贴标"
//                    //    Kind = 
//                    //        ProductKind.Sticker 
//                    //            (                         
//                    //                {
//                    //                    PrinterKeys = [Item.Art; Item.EUR; Item.UK; Item.US; Item.Barcode; Item.Color]
//                    //                    MakesureSelectors = [Selector.Art]
//                    //                    ImposerSelectors = []
//                    //                } |> DocType.Btw
//                    //            )
//                    //    Variable = 
//                    //        None
//                    //    Orders = orders
//                    //    UnitPrice = 0.1
//                    //}
//                ]

//            let tfBk = 
//                {
//                    Id = 0
//                    Size = { Width = mm 30.; Height = mm 25. }
//                    HSpace = mm 4.
//                    VSpace = mm 5.
//                    ColNum = 8
//                    RowNum = 12
//                    Margin = { Left = mm 10.746; Top = mm 11.469; Right = 0.; Bottom = 0. }
//                }

//            let r =
//                db 
//                |> LiteRepository.insertItems<Item> items
//                |> LiteRepository.insertItems<Order> orders
//                |> LiteRepository.insertItems<Product> products
//                |> LiteRepository.insertItem<Tag>
//                    (
//                        Tag.createWith (fun tag ->
//                            {
//                                tag with 
//                                    Id = 0
//                                    Company = "双优星"
//                                    Name = "Hangten"
//                                    Products = products
//                            } 
//                        )
//                    )
//                |> LiteRepository.insertItem<ThermalBackground> tfBk
//                |> LiteRepository.query<Order>
//                |> LiteQueryable.expand (Expr.prop (fun t -> t.Items))
//                |> LiteQueryable.first

//            pass()

//    testCase "makeSure" <| fun _ -> 
//        useDatabase <| fun db ->
//            {
//                Database = db
//                GlobalDirs = globalDirs
//                OrderQuery = 
//                    OrderQuery.prediate (
//                        Expr.prop (fun (o:Order) ->
//                            [
//                                o.Name = "BK169"
//                            ]
//                            |> List.forall id)
//                    )
//                CompanyName = "双优星"
//                TagName = "Hangten"
//            } |> Target.makeSure

//            Console.ReadLine() |> ignore

//    ftestCase "imposing transfer" <| fun _ -> 
//        useDatabase <| fun db ->
//            {
//                Database = db
//                GlobalDirs = globalDirs
//                OrderQuery = 
//                    OrderQuery.prediate(
//                        Expr.prop (fun (o:Order) ->
//                            [
//                                o.Name = "BK169"
//                            ]
//                            |> List.forall id)
//                    )
//                CompanyName = "双优星"
//                TagName = "Hangten"
//            } |> Target.imposeThermal
//            Console.ReadLine() |> ignore

//    //ftestCase "printing output" <| fun _ -> 
//    //    useDatabase <| fun db ->
//    //        {
//    //            Database = db
//    //            GlobalDirs = globalDirs
//    //            OrderQuery = 
//    //                Expr.prop (fun (o:Order) ->
//    //                    [
//    //                        o.Name = "BK169"
//    //                    ]
//    //                    |> List.forall id)
//    //            CompanyName = "双优星"
//    //            TagName = "Hangten"
//    //        } |> Target.printingOutput
//    //        Console.ReadLine() |> ignore
//  ]
//let [<Literal>] private xlExistPath =  root + @"\健乐\Exist\数据库.xlsx"
//type ExistDataType = ExcelFile<xlExistPath,"Sheet1">

//let ExistTests =
//  testList "ExistTests" [
//    ftestCase "add data to liteDB" <| fun _ -> 
//        useNewDatabase <| fun db ->
//            let file = new ExistDataType()
//            let orders =
//                file.Data 
//                |> Seq.filter (fun d -> not <| String.IsNullOrEmpty d.ORDER)
//                |> Seq.groupBy (fun d -> d.ORDER)
//                |> Seq.map (fun (k,v) -> 
//                    {
//                        Id = 0
//                        Name = k
//                        Date = v |>  Seq.map (fun r -> r.DATE) |> Seq.distinct |> Seq.exactlyOne
//                        Items = v |> Seq.map (fun r ->
//                            try 
//                                Item.create(fun it ->
//                                    { it with 
//                                        Brand = None
//                                        Art = r.ART
//                                        Barcode = Some (string r.BARCODE)
//                                        Color = Some (ColorEnum.fromString r.COLOR)
//                                        EUR = int r.EUR
//                                        UK = Some r.UK
//                                        US = Some r.US
//                                        CM = Some r.CM
//                                        Number = int r.NUMBER
//                                        Material = None         
//                                        Image = None
//                                        Template = Some r.Template
//                                        Code = r.CODE |> int |> Some
//                                    }
//                                )
//                            with ex ->
//                                printf "invaid input %A" r
//                                failwithf "%A" ex
//                        ) |> List.ofSeq 
//                    }
//                )
//                |> List.ofSeq

//            let items = orders |> List.collect (fun o -> o.Items)
//            let products =
//                [
//                    //{
//                    //    Id = 0
//                    //    Name = "吊牌1"
//                    //    Kind = {Front = DocType.AI {IsCmyk = false;Bleed = 0.}; Back = None; CuttingLine = false} |> ProductKind.Hangtag 
//                    //    Variable = 
//                    //        {
//                    //            PrinterKeys = [Item.Art; Item.EUR; Item.UK; Item.US; Item.CM; Item.Barcode; Item.Color]
//                    //            MakesureSelectors = [Selector.Art]
//                    //            ImposerSelectors = ImposerSelectors.empty
//                    //        } |> Some
//                    //    Orders = orders
//                    //    UnitPrice = 0.1
//                    //}
//                    Product.createWith (fun p ->
//                        {
//                            p with 
//                                Id = 0
//                                Name = "吊牌2"
//                                Kind = 
//                                    { 
//                                        Front = DocType.AI {DesiredColor = DesiredColor.Double (ColorMap.Red,ColorMap.Black);Bleed = 0.}
//                                        Back = 
//                                            {
//                                                PrinterKeys = [Item.Template; Item.Art; Item.Color; Item.EUR; Item.UK; Item.US; Item.CM; Item.Code; Item.Barcode]
//                                                MakesureSelectors = [Selector.Template; Selector.Art; Selector.Color]
//                                                ImposerSelectors = 
//                                                    {
//                                                        IsOrderMerged = true
//                                                        Value = [Selector.Art]
//                                                    }
//                                            } |> DocType.simpleBtw |> Some
//                                        CuttingLine = false
//                                        HangtagTemplate = None
//                                    } |> ProductKind.Hangtag 

//                                Orders = orders
//                                UnitPrice = 0.1
//                                Paper = Some Paper.``300gmsCoated``
//                        }
//                    )
//                ]

//            let r =
//                db 
//                |> LiteRepository.insertItems<Item> items
//                |> LiteRepository.insertItems<Order> orders
//                |> LiteRepository.insertItems<Product> products
//                |> LiteRepository.insertItem<Tag>
//                    (
//                        Tag.createWith (fun tag ->
//                            {
//                                tag with 
//                                    Company = "建乐"
//                                    Name = "Exists"
//                                    Products = products
//                            } 
//                        )
//                    )
//                |> LiteRepository.query<Order>
//                |> LiteQueryable.expand (Expr.prop (fun t -> t.Items))
//                |> LiteQueryable.first
//            pass()

//    testCase "makeSure" <| fun _ -> 
//        useDatabase <| fun db ->
//            {
//                Database = db
//                GlobalDirs = globalDirs
//                OrderQuery = 
//                    OrderQuery.prediate (
//                        Expr.prop (fun (o:Order) ->
//                            [
//                                o.Date = new DateTime(2018,6,2)
//                            ]
//                            |> List.forall id)
//                    )
//                CompanyName = "健乐"
//                TagName = "Exist"
//            } |> Target.makeSure

//            Console.ReadLine() |> ignore


//    testCase "proof output" <| fun _ -> 
//        useDatabase <| fun db ->
//            {
//                Database = db
//                GlobalDirs = globalDirs
//                OrderQuery = 
//                    OrderQuery.prediate (
//                        Expr.prop (fun (o:Order) ->
//                            [
//                                o.Date = new DateTime(2018,6,2)
//                            ]
//                            |> List.forall id)
//                    )
//                CompanyName = "健乐"
//                TagName = "Exist"
//                //Selectors = Expr.prop(fun (i: Item) -> i.)
//            } |> Target.proof

//            Console.ReadLine() |> ignore
//    ftestCase "hangtag output" <| fun _ -> 
//        useDatabase <| fun db ->
//            {
//                Database = db
//                GlobalDirs = globalDirs
//                OrderQuery = 
//                    OrderQuery.prediate (
//                        Expr.prop (fun (o:Order) ->
//                            [
//                                o.Date = new DateTime(2018,6,2)
//                            ]
//                            |> List.forall id)
//                    )
//                CompanyName = "健乐"
//                TagName = "Exist"
//            } |> Target.hangtagOutput

//            Console.ReadLine() |> ignore
//  ]

////let TecTests =
////  testList "ExistTests" [
////    ftestCase "add data to liteDB" <| fun _ -> 
////        let xlTecPath =  root + @"\超众\工艺\工艺1814-1816 口舌转印标（阿达）.xlsx"
////        useNewDatabase <| fun db ->

////            let orderName = "工艺1814-1816"

////            let sheet = 
////                xlTecPath
////                |>Excel.getWorksheetByIndex 1 

////            let items = XLParser.items materialDir sheet
////            let order = 
////                {
////                    Id = 0 
////                    Items = items
////                    Name = orderName
////                    Date = DateTime.UtcNow.Date
////                }

////            let product = 
////                {
////                    Id = 0
////                    Name = "热转印"
////                    Orders = [order]
////                    Paper = None
////                    UnitPrice = 0.1
////                    Kind = ProductKind.createThermalWithItem items.[0]
////                }

////            let r =
////                db 
////                |> LiteRepository.insertItems<Item> items
////                |> LiteRepository.insertItems<Order> [order]
////                |> LiteRepository.insertItems<Product> [product]
////                |> LiteRepository.insertItem<Tag>
////                    {
////                        Id = 0
////                        Company = "超众"
////                        Name = "工艺"
////                        Products = [product]
////                    } 
////                |> LiteRepository.query<Tag>
////                |> LiteQueryable.expand (Expr.prop (fun t -> t.Products))
////                |> LiteQueryable.first
////            pass()
////            ()

////    ftestCase "makeSure" <| fun _ -> 
////        useDatabase <| fun db ->
////            {
////                Database = db
////                GlobalDirs = globalDirs
////                OrderQuery = 
////                    Expr.prop (fun (o:Order) ->
////                        [
////                            o.Date = new DateTime(2018,6,30)
////                        ]
////                        |> List.forall id
////                    )
////                CompanyName = "超众"
////                TagName = "工艺"
////            } |> Target.makeSure

////            Console.ReadLine() |> ignore
////  ]