module Tests.Types
open Fake.IO
open Fake.IO.FileSystemOperators
open Shrimp.Pdf.Targets
open LiteDB
open LiteDB.FSharp
open Shrimp.Entities.Types
open LiteDB.FSharp.Extensions
open System
open Shrimp.Pdf.Targets.Types
open Shrimp.Entities
open System.Collections.Generic
open Shrimp.Pdf
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System.Collections.Concurrent
open Microsoft.FSharp.Reflection
open Shrimp.Utils
open Shrimp.Pdf.Colors
open Shrimp.Pdf.Types
open Shrimp.Pdf.Targets.Types
open Shrimp

let workingDirectory = __SOURCE_DIRECTORY__ </> @"bin\Debug\netcoreapp2.1"
let [<Literal>] root = __SOURCE_DIRECTORY__ + @"\datas"

[<RequireQualifiedAccess>]
module HomePrinter =
    let konicaC652 =
        {
            Name = "KONICA MINOLTA C652SeriesPS"
            Size = PrinterSize.A3
            Color = PrinterColor.FourColor
            RecommandedSize = PrinterSize.A4
            UnitCost = fun _ printerColor ->
                /// 一个粉盒8000张 200元 
                /// 1张0.025
                match printerColor with 
                | PrinterColor.FourColor -> 0.4
                | PrinterColor.Monochrome -> 0.1
            PieceNumberTransform = fun raw ->
                roundUpToInt raw + 1
        }

[<RequireQualifiedAccess>]
module Printer =
    let konicC652 = HomePrinter.konicaC652 

[<RequireQualifiedAccess>]
module Profession =
    let trader =
        {
            QQAccount = 857701188L
            QQAccountName = "188577"
            PhoneNumber = Known 18857711888L
            Organization = Organization.Insider {Name = "吴庆达"}
        } |> Trader

    let prepressDesigner =
        {
            QQAccount = 857701188L
            QQAccountName = "188577"
            PhoneNumber = Known 18857711077L
            Organization = Organization.Insider {Name = "吴乃甲"}
        } |> PrepressDesigner

    let filmPublisher =
        {
            QQAccount = 86061258L
            QQAccountName = "龙腾菲林输出"
            PhoneNumber = Known 13958753851L
            Organization = Organization.OutSourcer {Address = Unknown}
        } |> FilmPublisher

    let 星瑞达CTP =
        {
            QQAccount = 3454206963L
            QQAccountName = "星瑞达CTP"
            PhoneNumber = Unknown
            Organization = Organization.OutSourcer {Address = Unknown}
        } |> CTPPublisher

    let 万达CTP =
        {
            QQAccount = 1260787799L
            QQAccountName = "万达CTP"
            PhoneNumber = Unknown
            Organization = Organization.OutSourcer {Address = Unknown}
        } |> CTPPublisher

    let proofer: Proofer =
        { Profession = 
            {
                QQAccount = 100620579L
                QQAccountName = "真彩打样1"
                PhoneNumber = Unknown
                Organization = Organization.OutSourcer {Address = Unknown}
            }
          Printer = 
            { RecommandedSize = PrinterSize.A3
              UnitCost = fun printerColor pieceNumber ->
                match printerColor with 
                | PrinterColor.FourColor -> 3.
                | PrinterColor.Monochrome -> 0.25

              PieceNumberTransform = fun raw ->
                roundUpToInt raw + 1
            }
          Coater = fun _ pieceNumber -> float pieceNumber * 0.75
          Cutter = 
            { Cost = 
                fun _ pieceNumber ->
                    pieceNumber * 8 |> float
            }
        }


[<RequireQualifiedAccess>]
module OffsetPress =
    let pieceNumberTransform raw = roundUpToInt (raw * 1.1)
    let 亿通四色 =  FourColorEightOpenPress.create "亿通" [Profession.万达CTP] (fun _ -> 240) pieceNumberTransform
    let 镇前路单色 = MonochromeEightOpenPress.create "镇前路56号" [Profession.星瑞达CTP] (fun _ -> 60) pieceNumberTransform


let orderRoot dir = dir </> "Orders"
let testDir = root </> "TestFiles"
//let btwPath = root </> "成分条码标.btw" 
//let csvPath = xlPath |> Path.changeExtension ".csv"
let materialDir = root + @"\global\material"
let GlobalShoeImagesDir = root + @"\global\shoeImages"
let ShoeImagesFolderName = "shoeImages"
let defaultShoeImage = GlobalShoeImagesDir </> "Normal_6.pdf"
let deafultMaterials = [MaterialEnum.Textil; MaterialEnum.Textil; MaterialEnum.Sintetico] |> Material.ofEnums materialDir
let private tfBackgroundDir = root + @"\global\transfer"
let private fontDir = root + @"\global\fonts"
let private dbPath = root </> "simple.db"

let globalConfig = 
    {
        GlobalDirs =
            {
                Root = root
                TfBackgroundDir = tfBackgroundDir
                FontDir = fontDir
            }
        FilmPublisher = Profession.filmPublisher
        Trader = Profession.trader
        PrepressDesigner = Profession.prepressDesigner
        Proofer = Profession.proofer
        FourColorEightOpenPress = OffsetPress.亿通四色
        MonochromeEightOpenPress = OffsetPress.镇前路单色
        HomePrinter = Printer.konicC652
        FinanceConfig =
            { Mold = fun _ -> 60.
              MoldPress = fun pieceNumber -> float pieceNumber * 0.1 |> min 40.
              HangtagSlice = fun pieceNumber -> float pieceNumber * 0.05 |> min 20.
              StickerSlice = fun pieceNumber -> float pieceNumber * 0.05 |> min 20.
              WastingPercent = fun stragety productKind ->
                match stragety,productKind with 
                | PrintingStrategy.Digital digital,ProductKind.Commercial commercial ->
                    match digital,commercial with 
                    | DigitalPrinting.HomePrinter _, Commercial.Sticker _ -> 1.05
                    | DigitalPrinting.Proofing _, _ -> 1.
                    | _ -> Logger.notImplemented()

                | PrintingStrategy.Press press, _ -> 
                    match press with 
                    | Press.WebPress -> 1.
                    | Press.Flat flat -> 
                        match flat with 
                        | FlatPress.Offset _ -> 1.
                        | FlatPress.Manual _ -> 1.05
                | _ -> Logger.notImplemented()

              SalePrice = fun arg -> Logger.notImplemented()

              ChunkPrice = fun arg ->
                match arg with 
                | ChunkPriceArg.Sticker arg ->
                    assert (arg.A4ChunkNumber <> 0)
                    let totalNumber = arg.TpDataTables |> List.sumBy (TypedImposerDataTable.totalNumber)
                    let printingColor = PrinterColor.ofDesiredColor arg.DesiredColor
                    let piecePrice = 
                        match arg.Paper.Material, printingColor with 
                        | UnderTone.White, PrinterColor.FourColor -> 1.2
                        | UnderTone.White, PrinterColor.Monochrome -> 0.7
                        | UnderTone.Transparent, PrinterColor.Monochrome -> 1.2
                        | UnderTone.Transparent, PrinterColor.FourColor -> Logger.notImplemented()
                    piecePrice / float arg.A4ChunkNumber
                    |> roundUpBy 200

              CoatingCost = fun _ pieceNumber ->
                float pieceNumber * 0.03

              ManualCost = fun _ pieceNumber ->
                float pieceNumber * 0.1 + 25.

              ManualRecommendPieceSize = function
                | Commercial.Sticker st ->
                    match st.Paper.Material with
                    | UnderTone.White -> FsSize.A4
                    | UnderTone.Transparent -> FsSize.create (toInche 180) (toInche 260)
                | Commercial.Hangtag ht -> Logger.notImplemented()

              }

              
    }

let baseConverter = new FSharpJsonConverter()

type MyJsonConverter() =
    inherit FSharpJsonConverter()

    let advance(reader: JsonReader) =
        reader.Read() |> ignore

    let readElements(reader: JsonReader, itemTypes: Type[], serializer: JsonSerializer) =
        let rec read index acc =
            match reader.TokenType with
            | JsonToken.EndArray -> acc
            | JsonToken.PropertyName -> 
                advance reader
                read index acc
            | _ when index = itemTypes.Length ->
                acc
            | _ ->
                let value = serializer.Deserialize(reader, itemTypes.[index])
                advance reader
                read (index + 1) (acc @ [value])
        advance reader
        read 0 List.empty


    let registeredTypes = ConcurrentDictionary<Type,obj -> obj>()
    member this.RegisterType<'T>(deserializeMapper : 'T -> 'T) =
        let t = typeof<'T>
        let boxedDeserializeMapper value = value |> unbox |> deserializeMapper |> box
        registeredTypes.[t] <- boxedDeserializeMapper

    override this.CanConvert(t) =
        if base.CanConvert(t) then true
        else
            registeredTypes.ContainsKey(t) 

    override this.WriteJson(writer,value,serializer) =
        let t = value.GetType()
        match registeredTypes.TryGetValue(t) with 
        | true, deserializeMapper ->
            if FSharpType.IsRecord(t) then
                writer.WriteStartObject()
                
                t.GetProperties() |> Array.iter(fun prop -> 
                    writer.WritePropertyName(prop.Name)
                    let itemValue = FSharpValue.GetRecordField(value,prop)
                    serializer.Serialize(writer,itemValue)
                )
                writer.WriteEndObject()

            else 
                Logger.notImplemented()

        | false, _ -> 
            base.WriteJson(writer,value,serializer)

    override this.ReadJson(reader, t, existingValue, serializer) =
        match registeredTypes.TryGetValue(t) with 
        | true, deserializeMapper ->
            if FSharpType.IsRecord(t) then 
                let itemTypes = t.GetProperties() |> Array.map(fun prop -> prop.PropertyType)
                let values = readElements (reader,itemTypes,serializer)
                let value = FSharpValue.MakeRecord(t,values |> List.toArray)
                deserializeMapper value
            else 
                let value = base.ReadJson(reader, t, existingValue, serializer)
                deserializeMapper value

        | false, _ -> 
            let value = base.ReadJson(reader, t, existingValue, serializer)
            value

module Input =
    open ExcelProcess
    open OfficeOpenXml
    open Expecto
    open Expecto.Logging
    open MBrace.FsPickler

    let binarySerializer = FsPickler.CreateBinarySerializer()
    let mapper = FSharpBsonMapper()
    let converters = 
        let cv = new MyJsonConverter()
        cv.RegisterType<AutoSetTemplate>(fun v ->
            {v with RetrieveFrom = RetrieveFrom.DB}
        )
        cv

    FSharpBsonMapper.UseCustomJsonConverters [|converters|]
    mapper.DbRef<Tag,_>(fun t -> t.Products)
    mapper.DbRef<Product,_>(fun t -> t.Orders)


    let useDatabase (f: LiteRepository -> unit) = 

        use db = new LiteRepository(dbPath, mapper)
        db.EnsureIndex<Tag,_>(fun t -> t.Name)
        db.EnsureIndex<Order,_>(fun o -> o.Name)
        f db

    let useNewDatabase (f: LiteRepository -> unit) =
        File.delete dbPath
        useDatabase f


    type DBModel =
        {
            CompanyName: string
            TagName: string
            OrderNames: string list
            OrderXLSXName: string -> string
            SheetGenerator: SheetGenerator
            ItemsGenerator: DBModel -> string -> ExcelWorksheet -> IItem list
            ProductsGenerator: DBModel -> IItem -> Product list
        }



    [<RequireQualifiedAccess>]
    module DBModel =
        open FileSystem
        let tagDir (dm: DBModel) =
            root </> dm.CompanyName </> dm.TagName

        let getSheet xlsxName orderName (dm: DBModel) =
            let docDir = tagDir dm
            let orderDir = orderRoot docDir 
            let xlPath =  orderDir </> (xlsxName orderName + ".xlsx")
            SheetGenerator.getSheet xlPath dm.SheetGenerator

        let generateProducts (productInputs: (IItem-> ProductInput.ProductInput) list) =
            (fun (dm: DBModel) ((item: IItem)) ->
                productInputs |> List.map (fun f ->
                    let input = f item
                    Product.createWith (fun p ->
                        {
                            p with 
                                Id = 0
                                Name = input.Name
                                Orders = []
                                UnitPrice = input.UnitPrice
                                Kind = input.Kind
                                Filter = 
                                    match input.Filter with
                                    | Some filter -> 
                                        let dir = tagDir dm </> bin </> pk
                                        ensureDir dir
                                        let path = dir </> (sprintf "%s.pk" input.Name)
                                        let bytes = binarySerializer.Pickle filter
                                        File.writeBytes path bytes
                                        true
                                    | None -> false
                        }
                    )
                )
            )
    let pass() = Expect.isTrue true "passed"
    let fail() = Expect.isTrue false "failed" 

    let run (dm:DBModel) =
        Environment.CurrentDirectory <- workingDirectory
        let testConfig =  
            { Expecto.Tests.defaultConfig with 
                 verbosity = LogLevel.Debug }

        let tests = 
            [
            testCase "add data to liteDB" <| fun _ -> 
                useDatabase <| fun db ->


                    let companyName = dm.CompanyName
                    let tagName = dm.TagName
                    
                    let orders = 
                        dm.OrderNames |> List.map (fun orderName ->
                            let sheet = DBModel.getSheet dm.OrderXLSXName orderName dm
                            let items = dm.ItemsGenerator dm orderName sheet
                            {
                                    Id = 0 
                                    Items = items
                                    Name = orderName
                                    Date = DateTime.UtcNow.Date
                                    TagName = tagName
                            }
                        )
                    let items = orders |> List.collect (fun order -> order.Items)

                    match List.tryHead items with 
                    | Some item ->
                        let products = dm.ProductsGenerator dm item |> List.map (fun prod -> 
                            { prod with Orders = orders }
                        )

                        let tag =
                            Tag.createWith (fun tag -> 
                                { tag with 
                                    Company = Company.createSimple companyName
                                    Name = tagName
                                    Products = products }
                            )
                        LiteRepository.upsertTag tag db |> ignore
                            
                        pass()
                    | None -> failwith "No datas existed"
      
            testList "Target" [
                test "makeSure"  {
                    useDatabase <| fun db ->
                        {
                            Database = db
                            GlobalConfig = globalConfig
                            OrderQuery = fun (order: Order) ->
                                List.contains order.Name dm.OrderNames
                            CompanyName = dm.CompanyName
                            TagName = dm.TagName
                        } |> Target.makeSure
                    printfn "make Sure "
                }

                test "output" { 
                    useDatabase <| fun db ->
                        {
                            Database = db
                            GlobalConfig = globalConfig
                            OrderQuery = fun (order: Order) ->
                                List.contains order.Name dm.OrderNames
                            CompanyName = dm.CompanyName
                            TagName = dm.TagName
                        } |> Target.output
                    printfn "output"
                }
                //test "proof" { 
                //    useDatabase <| fun db ->
                //        {
                //            Database = db
                //            GlobalConfig = globalConfig 
                //            OrderQuery = fun (order: Order) ->
                //                List.contains order.Name dm.OrderNames
                //            CompanyName = dm.CompanyName
                //            TagName = dm.TagName
                //        } |> Target.proof
                //    printfn "proof"
                //}
            ]
            ]
        tests |> List.sumBy (runTests testConfig)