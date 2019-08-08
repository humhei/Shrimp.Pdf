namespace Atrous.Pdfargets

module Types = 
    open System
    open System.IO
    open LiteDB
    open FParsec
    open iText.Kernel.Pdf
    open iText.Kernel.Colors
    open iText.Kernel.Pdf.Canvas.Parser.Data
    open iText.Kernel.Geom
    open Fake.IO
    open Fake.IO.FileSystemOperators
    open Atrous.Pdf.Extensions
    open Atrous.Pdf
    open Atrous.Pdf.Operators
    open Atrous.Pdf.Colors
    open Atrous.Utils
    open Atrous.Pdf.Types
    open Atrous.Entities.Types
    open Atrous.Entities.Csv
    open Atrous
    open Atrous.Extensions

    [<AutoOpen>]
    module InfixExtensions =
        [<RequireQualifiedAccess>]
        module TextArgument =
            let createWith transfrom =
                {
                    Text = "Text"
                    FontGen = FontGen.heiti
                    FontColor = DeviceGray.BLACK
                    FontSize = 9.
                } |> transfrom

            let simpleText text = 
                createWith (fun arg ->
                    { arg with Text = text }
                )

        [<RequireQualifiedAccess>]
        module AI =
            let sizeParser: Parser<FsSize,unit> =
                let pWidth = pfloat .>> (pstring "×")
                let pHeight = pfloat
                pipe3 pWidth pHeight (pstring "cm" <|> pstring "mm") (fun f1 f2 unit -> 
                    match unit with 
                    | "mm" ->
                        {Width = f1 / 10.; Height = f2 / 10.}
                    | "cm" -> {Width = f1; Height = f2}
                    | _ -> failwith "Not implemented"
                )

        [<RequireQualifiedAccess>]
        module Btw =
            let sizeParser: Parser<FsSize,unit> =
                let pWidth = pfloat .>> (pstring "×")
                let pHeight =  pfloat
                pipe2 pWidth pHeight (fun f1 f2 -> {Width = toInche f1; Height = toInche f2})

        [<RequireQualifiedAccess>]
        module FsSize =

            let fromStringBtw s =
                runToResult Btw.sizeParser s

            let fromStringAI s =
                runToResult AI.sizeParser s

            let fromString docType s =
                match docType with 
                | DocType.AI _ | DocType.Pdf _ -> fromStringAI s
                | DocType.Btw _ -> fromStringBtw s

            let initUnit = FsSize.A4
        

        type ExplicitImposingArgument = ImposingArgumentBase<FsSize,FsSize>

        [<RequireQualifiedAccess>]
        module ExplicitImposingArgument =
            let dummy = 
                ImposingArgumentBase.createDummy 
                    (FsSize.initUnit)
                    (FsSize.A4)

            let imposeMatrix (arg: ExplicitImposingArgument) =
                match arg.ColNums,arg.RowNum with 
                | colNums,_ when colNums |> List.forall ((=) 0) -> 
                    failwithf "ColNums should be set first%A" arg
                | _,0 -> 
                    failwithf "RowNum should be set first%A" arg
                | colNums,_ when colNums.Length > 1 -> Logger.notImplemented()
                | colNums,rowNum ->
                    let colNum = colNums |> List.exactlyOne
                    colNum,rowNum

            let imposeMatrixMulitiply = 
                imposeMatrix >> (fun (colNum,rowNum) ->
                    colNum * rowNum
                )

            let toImplicit (arg: ExplicitImposingArgument) : ImposingArgument =
                ImposingArgumentBase.map (Some arg.DesiredSize) (Background.Size arg.Background) arg

            let clockwiseBackground (arg: ExplicitImposingArgument) =
                { arg with 
                    Background = FsSize.clockwise arg.Background }

            let clearMargin (arg: ExplicitImposingArgument) =
                { arg with 
                    Margin = Margin.empty }

            let preImpose (arg: ExplicitImposingArgument) =
                let src = 
                    let writer = Path.GetTempFileName()
                    let doc = new PdfDocument(new PdfWriter(writer))
                    doc.AddNewPage(PageSize.ofFsSize arg.Background) |> ignore
                    doc.Close()
                    writer

                let model = 
                    [
                        Reuses.impose (toImplicit arg)
                    ] |> preflight src |> List.exactlyOne

                let tb = model.FlowState.Tables |> List.exactlyOne
                
                let colNum,rowNum = tb.Rows.[0].Units.Length, tb.Rows.Length
            
                { arg with ColNums = [colNum]; RowNum = rowNum},tb

            let mostPreimpose transforms arg = 
                transforms |> List.mapAsync (fun transform ->
                    transform arg |> preImpose
                ) |> Array.maxBy (fst >> imposeMatrixMulitiply)


        [<RequireQualifiedAccess>]
        module Logger =
            let redirect arg =
                printfn "Warning: short hand impletation %A" arg
                arg

        [<RequireQualifiedAccess>]
        module Margin =
            let imposingDest = Margin.createSimple (toInche 6.)
        
        [<RequireQualifiedAccess>]
        module Paper =
            let ``300gmsCoated`` =
                {
                    Weight = 300
                    Material = ()
                    TonPrice = 6200
                    Brand = "Untitled"
                }

        [<RequireQualifiedAccess>]
        module BkTemplate =
            let ofImposingArgument (arg: ExplicitImposingArgument) =
                {
                    Size = arg.DesiredSize
                    HSpaces = arg.HSpaces
                    VSpaces = arg.VSpaces
                    ColNums = arg.ColNums
                    RowNum = arg.RowNum
                    Margin = arg.Margin
                }

            let applyTo (arg:ExplicitImposingArgument) (thBk: BkTemplate) =
                { arg with 
                    DesiredSize = thBk.Size
                    ColNums = thBk.ColNums
                    RowNum = thBk.RowNum
                    HSpaces = thBk.HSpaces
                    VSpaces = thBk.VSpaces
                    Margin = thBk.Margin
                }


    let todayMark =
        DateTime.Today.ToString("yyyy年M月d日")

    let todayFileName =
        DateTime.Today.ToString("yyyy-M-d")

    let [<Literal>] bin = ".bin"
    let [<Literal>] pk = ".pk"



    type GlobalDirs =
        {
            Root: string
            TfBackgroundDir: string
            FontDir: string
        }



    [<RequireQualifiedAccess>]
    type FlatPress =
        | Offset of OffsetPress
        | Manual

    [<RequireQualifiedAccess>]
    type Press =
        | Flat of FlatPress
        | WebPress

    type Proofing =
        {
            Proofer: Proofer
            IsCoating: bool
            IsCutting: bool
        }
    [<RequireQualifiedAccess>]
    module Proofing =
        let ofProofer coating cuttingLine proofer =
            let simple = {IsCoating = false; IsCutting = false; Proofer = proofer}
            let withCoating = { simple with IsCoating = true }
            let withCutting = {simple with IsCutting = true}
            let allFeature = {simple with IsCoating = true; IsCutting = true}
            match coating,cuttingLine with 
            | Coating.None,CuttingLine.Slice ->
                [simple]
            | _,CuttingLine.Slice ->
                [simple; withCoating]
            | Coating.None,_ -> 
                [simple; withCutting]
            | _ -> [simple; withCutting; withCoating; allFeature]


    [<RequireQualifiedAccess>]
    type DigitalPrinting =
        | HomePrinter of HomePrinter
        | Proofing of Proofing

    [<RequireQualifiedAccess>]
    module DigitalPrinting = 
        let asPrinter = function
            | DigitalPrinting.HomePrinter homePrinter -> Printer.Home homePrinter
            | DigitalPrinting.Proofing proofing -> Printer.OutSource proofing.Proofer.Printer

    [<RequireQualifiedAccess>]
    type PrintingStrategy =
        | Press of Press
        | Digital of DigitalPrinting



    [<RequireQualifiedAccess>] 
    module PrintingStrategy = 

        let (|FlatPress|_|) = function
            | PrintingStrategy.Press press -> 
                match press with 
                | Press.Flat flatPress -> Some flatPress 
                | _ -> None
            | _ -> None

        let (|Proofing|_|) = function
            | PrintingStrategy.Digital digital -> 
                match digital with 
                | DigitalPrinting.Proofing proofing -> Some proofing 
                | _ -> None
            | _ -> None


        let isProofer = function 
            | PrintingStrategy.Digital digital -> 
                match digital with 
                | DigitalPrinting.Proofing _ -> true
                | DigitalPrinting.HomePrinter _ -> false
            | _ -> false

        let ofFlat flat =
            flat
            |> Press.Flat
            |> PrintingStrategy.Press

        let manual = 
            FlatPress.Manual
            |> ofFlat

        let ofOffset offSetPress =
            offSetPress 
            |> FlatPress.Offset
            |> ofFlat

    type TypedImposerDataRow =
        {
            Id: int
            Number: float
            Cells: ImposerDataCell list
            DuplicatedNumber: int
        }

    [<RequireQualifiedAccess>]
    module TypedImposerDataRow =
        let getCellFromKey (key: string) (row:TypedImposerDataRow) = 
            row.Cells |> List.find (fun data -> data.Header = key)
    
        let getValues (row:TypedImposerDataRow) = 
            row.Cells |> List.map ((fun cell -> cell.Value.ToString()))

        let getValueFromKey (key: string) (row:TypedImposerDataRow) = 
            let data = row |> getCellFromKey key 
            data.Value

        let getValueFromSelector (selector: Selector) (row:TypedImposerDataRow) = 
            getValueFromKey selector.Name row |> string |> String.applySelector selector

        let simplify row =
            { row with 
                Cells = row.Cells |> List.filter (fun cell ->
                    List.contains cell.Header [Item.Art;Item.Barcode;Item.Color;Item.EUR;Item.UK;Item.US]
                )
            }

        let toZH row =
            row.Cells 
            |> List.map (fun cell -> cell.Header)
            |> List.map Item.toZh

    type TypedImposerDataTable = 
        {
            Name: string
            Rows: TypedImposerDataRow list
        }

    [<RequireQualifiedAccess>]
    module TypedImposerDataTable =
        let toDataTable (tpTable: TypedImposerDataTable) :ImposerDataTable =
            tpTable.Rows |> List.map (fun tpRow -> 
                [
                    {
                        Value = tpRow.Id 
                        Type = typeof<int>
                        Header = "Id"
                    }
                    {
                        Value = tpTable.Name
                        Type = typeof<string>
                        Header = "Order"
                    }
                ]
                @ tpRow.Cells 
                @ [
                        {
                            Value = tpRow.Number
                            Type = typeof<int>
                            Header = "Number"
                        }
                  ]
            )

        let totalNumber (tpTable: TypedImposerDataTable) = 
            tpTable.Rows |> List.sumBy (fun row ->
                row.Number
            )

    type StickerPriceArg =
        {
            PieceNumber : int
            TpDataTables : TypedImposerDataTable list
            PrintingColor : PrinterColor
            Paper : StickerPaper
            A4ChunkNumber: int
        }

    [<RequireQualifiedAccess>]
    type SalePriceArg =
        | Sticker of StickerPriceArg


    type StickerChunkPriceArg =
        {
            TpDataTables : TypedImposerDataTable list
            DesiredColor : DesiredColor
            Paper : StickerPaper
            A4ChunkNumber: int
        }

    [<RequireQualifiedAccess>]
    type ChunkPriceArg = 
        | Sticker of StickerChunkPriceArg


    type FinanceConfig =
        {
            Mold: ImposerTable -> float
            MoldPress: int -> float (*pieceNumber -> price*)
            HangtagSlice: int -> float (*pieceNumber -> price*)
            StickerSlice: int -> float (*pieceNumber -> price*)

            CoatingCost: Coater (*pieceSize -> pieceNumber -> price*)

            ManualCost: FsSize -> int -> float
            ManualRecommendPieceSize: Commercial -> FsSize

            WastingPercent: PrintingStrategy -> ProductKind -> float
            SalePrice: SalePriceArg -> float
            ChunkPrice: ChunkPriceArg -> float
        }

    type GlobalConfig =
        {
            GlobalDirs: GlobalDirs
            FilmPublisher: FilmPublisher
            Trader: Trader
            PrepressDesigner: PrepressDesigner
            Proofer: Proofer
            HomePrinter: HomePrinter
            FourColorEightOpenPress: FourColorEightOpenPress
            MonochromeEightOpenPress: MonochromeEightOpenPress
            FinanceConfig: FinanceConfig
        }

    [<RequireQualifiedAccess>]
    module GlobalConfig =
        let getPresses gc =
            let press1 = FourColorEightOpenPress.value gc.FourColorEightOpenPress
            let press2 = MonochromeEightOpenPress.value gc.MonochromeEightOpenPress
            [press1;press2]


        let getPrinters gc =
            let printer1 = Printer.Home gc.HomePrinter
            let printer2 = Printer.OutSource gc.Proofer.Printer
            [printer1;printer2]

        let getWastingPercent printingStrategy productKind gc =
            gc.FinanceConfig.WastingPercent printingStrategy productKind 

        let getProofings coating cuttingLine gc = 
            let proofer = gc.Proofer
            Proofing.ofProofer coating cuttingLine proofer


    type TargetModel =
        {
            GlobalConfig: GlobalConfig
            CompanyName: string
            TagName: string
            OrderQuery: Order -> bool
            Database: LiteRepository
        }

    [<RequireQualifiedAccess>]
    module TargetModel =
        open FileSystem

        let getManualCost (tarm: TargetModel) =
            tarm.GlobalConfig.FinanceConfig.ManualCost

        let manualCost tarm =
            tarm.GlobalConfig.FinanceConfig.ManualCost

        let getWastingPercent printingStrategy productKind (tarm: TargetModel) =
            GlobalConfig.getWastingPercent printingStrategy  productKind tarm.GlobalConfig
        


        let getMoldCost table tarm =
            tarm.GlobalConfig.FinanceConfig.Mold table

        let getPressCost pieceNumber tarm =
            tarm.GlobalConfig.FinanceConfig.MoldPress pieceNumber

        let getSliceCost pieceNumber tarm =
            tarm.GlobalConfig.FinanceConfig.StickerSlice pieceNumber

        let getSalePrice arg tarm =
            tarm.GlobalConfig.FinanceConfig.SalePrice arg

        let getChunkPrice arg tarm =
            tarm.GlobalConfig.FinanceConfig.ChunkPrice arg

        let getCoatingCost pieceSize pieceNumber tarm =
            tarm.GlobalConfig.FinanceConfig.CoatingCost pieceSize pieceNumber

        let tagDir (tarm: TargetModel) = 
            let globalFloders = tarm.GlobalConfig.GlobalDirs
            globalFloders.Root </> tarm.CompanyName </> tarm.TagName 

        let randomCsv tarm =
            let dir = tagDir tarm </> ".bin" </> ".xlsx"
            ensureDir dir
            let rd = Path.GetRandomFileName()
            dir </> sprintf "数据库%srd.csv" rd

        let randomXlsx tarm =
            randomCsv tarm |> Path.changeExtension ".xlsx"

        let file productName ext tarm =
            let dir = tagDir tarm
            dir </> sprintf "%s%s" productName ext

        let fileInBin productName ext tarm =
            let dir = tagDir tarm </> bin
            ensureDir dir
            dir </> sprintf "%s%s" productName ext

        let fileInBinTarget targetName productName ext tarm =
            let dir = tagDir tarm </> bin </> targetName
            ensureDir dir
            dir </> sprintf "%s%s" productName ext

        let filesInTargetOrder p targetName ext tarm = 
            let dir = tagDir tarm
            p.Orders |> List.map (fun o ->
                let dir = dir </> targetName </> o.Name
                ensureDir dir
                dir </> sprintf "%s%s" p.Name ext
            )

        let fileInTarget productName targetName ext tarm = 
            let dir = tagDir tarm </> targetName
            ensureDir dir
            dir </> sprintf "%s%s" productName ext




    type TypedOrder<'page> =
        {
            TpTable: TypedImposerDataTable
            Pages: 'page list
        }

    type DataLength = private DataLength of int
    type PageLength = private PageLength of int

    [<RequireQualifiedAccess>]
    module TypedOrder =
        let init (tables: TypedImposerDataTable list) (doc: PdfDocument) validate pageBundle =
            let dataLength = tables |> List.sumBy (fun tb -> tb.Rows.Length) |> DataLength
            let pages = PdfDocument.getAllPages doc
            let pageLength = pages.Length |> PageLength
            if validate dataLength pageLength then 
                let PageZip: 'a list = pageBundle pages
                tables
                |> List.mapi (fun i table -> 
                    let datas = table.Rows
                    let upper = datas.Length-1+i
                    {
                        TpTable = table
                        Pages = PageZip.[i .. upper]
                    }
                ) 
            else Logger.notImplemented()

        /// iAtrousne IdSaveTransferMulti
        let splitBySelectors selectors (tpOrder: TypedOrder<_>) =
            List.zip tpOrder.Pages tpOrder.TpTable.Rows 
            |> List.groupBy (fun (page,row) ->
                selectors |> List.map (fun selector -> TypedImposerDataRow.getValueFromSelector selector row)
            )
            |> List.map (fun (names,zips) ->
                let pages,rows = zips |> List.unzip
                let name = (tpOrder.TpTable.Name :: names) |> String.concat "_"
                {
                    TpTable = 
                        {
                            Name = name
                            Rows = rows
                        }
                    Pages = pages
                }
            )

        let mapResolved (tpOrder: TypedOrder<_>) mapping =
            let rows = tpOrder.TpTable.Rows
            let pages = tpOrder.Pages
            mapping (List.zip rows pages) 

        let transformResolved (tpOrder: TypedOrder<_>) transition =
            let newRows,newPages = 
                mapResolved tpOrder transition |> List.unzip

            let tpTable = { tpOrder.TpTable with Rows = newRows }
            { tpOrder with TpTable = tpTable; Pages = newPages }


        let sortById (tpOrder: TypedOrder<_>) =
            transformResolved tpOrder (fun zips ->
                zips
                |> List.sortBy (fun (row,page) -> row.Id)
            )

        let sortByNumber (tpOrder: TypedOrder<_>) =
            transformResolved tpOrder (fun zips ->
                zips
                |> List.sortByDescending (fun (row,page) -> row.Number)
            )

        let shuffleToImposeNumber (args: ExplicitImposingArgument) imposeNum (tpOrder: TypedOrder<_>) =
            transformResolved tpOrder (fun zips ->
                if zips.Length > imposeNum then Logger.notImplemented()
                else
                    let rec loop (zips) = 
                        let accum = zips |> List.sumBy (fun (r,_) -> r.DuplicatedNumber)
                        if accum = imposeNum
                        then 
                            let zips =
                                zips
                                |> List.collect (fun (r,page) ->
                                    let dupNum = r.DuplicatedNumber
                                    let r = { r with DuplicatedNumber = 1}
                                    List.replicate dupNum (r,page)
                                )
                            match args.RotateXObjectDegree with 
                            | Rotation.Clockwise | Rotation.Counterclockwise->
                                let colNum = args.ColNums |> Seq.head
                                zips
                                |> List.chunkBySize colNum
                                |> List.collect List.rev
                            | Rotation.None -> zips
                            | _ -> Logger.notImplemented()
                        else 
                            let zips = 
                                zips 
                                |> List.transformByMax (fun (row,_) -> row.Number) (fun (row,pageBundle) ->
                                        let orNum = float row.DuplicatedNumber * row.Number
                                        let dupNum = row.DuplicatedNumber + 1
                                        { row with Number = (orNum / float dupNum); DuplicatedNumber = dupNum },pageBundle
                                )
                            loop zips
                    loop zips 
            )






    type OSTypedOrder = TypedOrder<int * PdfPage>

    [<RequireQualifiedAccess>]
    module OSTypedOrder =
        let init (tables: TypedImposerDataTable list) (doc: PdfDocument) : OSTypedOrder list =
            let validate (DataLength dataLength) (PageLength pageLength) = 
                dataLength = pageLength
            TypedOrder.init tables doc validate (fun pages -> pages |> List.mapi (fun i page -> i,page))

    type FBTypedOrder = TypedOrder<int * PdfPage * PdfPage>
    
    [<RequireQualifiedAccess>]
    module FBTypedOrder =
        let init (tables: TypedImposerDataTable list) (doc: PdfDocument) : FBTypedOrder list =
            let validate (DataLength dataLength) (PageLength pageLength) = 
                dataLength * 2 = pageLength

            let pageBundle =
                List.chunkBySize 2 >> List.mapi (fun i fb -> i,fb.[0],fb.[1])

            TypedOrder.init tables doc validate pageBundle
   
    [<RequireQualifiedAccess>]
    module ProductInput =
        type ProductInput =
            {
                UnitPrice: float
                Kind: ProductKind
                Name: string
                Filter: (IItem -> bool) option
            }

        let create (tranform: ProductInput -> ProductInput) =
            {
                UnitPrice = 0.1
                Kind = ProductKind.createCopyPaper
                Name = "Untitled"
                Filter = None
            } |> tranform

    [<RequireQualifiedAccess>]
    type SheetGenerator =
        | Sheet1
        | Index0
        | Automatic

    [<RequireQualifiedAccess>]
    module SheetGenerator =
        open ExcelProcess
        open OfficeOpenXml

        let getSheet xlPath sheetGenerator : ExcelWorksheet = 
            match sheetGenerator with 
            | SheetGenerator.Sheet1 ->
                Excel.getWorksheetByName "Sheet1" xlPath
            | SheetGenerator.Index0 ->
                Excel.getWorksheetByIndex 0 xlPath
            | SheetGenerator.Automatic ->
                let sheets = Excel.getWorksheets xlPath
                if Seq.length sheets = 1 then 
                    Seq.exactlyOne sheets
                else Seq.head sheets 



    type SimpleFinanceUnit =
        {
            OrderNumber: int
            PieceNumber: int
            Name: string
            TotalIndex: int
            CurrentIndex: int
        }

    type ComplexFinanceUnitCost =
        {
            Paper: float
            Coating: float
            Printing: float
        }
    with 
        member x.Total = x.Paper + x.Coating + x.Printing

    [<RequireQualifiedAccess>]
    module ComplexFinanceUnitCost =
        let dummy = 
            {
                Paper = 0.
                Coating = 0.
                Printing = 0.
            }

    type ComplexFinanceUnit =
        {
            OrderNumber: int
            Utilization: float
            PieceNumber: int
            Name: string
            Cost: ComplexFinanceUnitCost
            TotalIndex: int
            CurrentIndex: int
            ChunkPrice: float
        }
    with
        member x.SalePrice = x.ChunkPrice * float x.OrderNumber
        member x.Benifit = x.SalePrice - x.Cost.Total

    [<RequireQualifiedAccess>]
    module ComplexFinanceUnit =
        let dummy = 
            {
                OrderNumber = 0
                Utilization = 0.
                PieceNumber = 0
                Name = "untitled"
                Cost = ComplexFinanceUnitCost.dummy
                CurrentIndex = 0
                TotalIndex = 0
                ChunkPrice = 0.
            }


        let asSimpleFinanceUnit (complex: ComplexFinanceUnit) =
            {
                OrderNumber = complex.OrderNumber
                PieceNumber = complex.PieceNumber
                Name = complex.Name
                TotalIndex = complex.TotalIndex
                CurrentIndex = complex.CurrentIndex
            }

        let private validSetter = 
            lazy (
                typeof<SimpleFinanceUnit>.GetProperties()
                |> Array.map (fun property -> property.Name)
                |> function
                    | [|"OrderNumber";"PieceNumber";"Name";"TotalIndex";"CurrentIndex"|] -> true
                    | _ -> false
            )

        let private setWithSimpleFinanceUnit (simple: SimpleFinanceUnit) (complex: ComplexFinanceUnit) =
            if validSetter.Value then 
                { complex with 
                    OrderNumber = simple.OrderNumber 
                    PieceNumber = simple.PieceNumber
                    Name = simple.Name
                    TotalIndex = simple.TotalIndex
                    CurrentIndex = simple.CurrentIndex }
            else Logger.inCompletedMapping complex

        let transformVirtualInnerSimpleFinanceUnit (transform: SimpleFinanceUnit -> SimpleFinanceUnit) (complex: ComplexFinanceUnit) =
            let newSimple = transform (asSimpleFinanceUnit complex)
            setWithSimpleFinanceUnit newSimple complex

    //[<RequireQualifiedAccess>]
    //type FinanceUnit =
    //    | Simple of SimpleFinanceUnit
    //    | Complex of ComplexFinanceUnit


    [<RequireQualifiedAccess>]
    type FinanceUnit = 
        | Simple of SimpleFinanceUnit
        | Complex of ComplexFinanceUnit
    
    [<RequireQualifiedAccess>]
    module FinanceUnit =
        let pieceNumber = function 
            | FinanceUnit.Complex unit -> unit.PieceNumber
            | FinanceUnit.Simple unit -> unit.PieceNumber

        let name = function 
            | FinanceUnit.Complex unit -> unit.Name
            | FinanceUnit.Simple unit -> unit.Name


    type SimpleFinanceUnits = SimpleFinanceUnit list

    type ComplexFinanceUnits = ComplexFinanceUnit list

    [<RequireQualifiedAccess>]
    type FinanceUnits =
        | Simple of SimpleFinanceUnits
        | Complex of ComplexFinanceUnits
    with 
        member this.Item index = 
            match this with 
            | FinanceUnits.Simple simples -> FinanceUnit.Simple simples.[index] 
            | FinanceUnits.Complex complexes -> FinanceUnit.Complex complexes.[index]
        member this.Length =
            match this with 
            | FinanceUnits.Simple simples -> simples.Length
            | FinanceUnits.Complex complexes -> complexes.Length

    [<RequireQualifiedAccess>]
    module FinanceUnits =
        let orderNumber = function 
            | FinanceUnits.Simple units -> units |> List.sumBy(fun unit -> unit.OrderNumber)
            | FinanceUnits.Complex units -> units |> List.sumBy(fun unit -> unit.OrderNumber)

        let pieceNumbers = function 
            | FinanceUnits.Simple units -> units |> List.map(fun unit -> unit.PieceNumber)
            | FinanceUnits.Complex units -> units |> List.map(fun unit -> unit.PieceNumber)

        let pieceNumber = pieceNumbers >> List.sum

        let setIndexer (financeUnits: FinanceUnits) = 
            let length = financeUnits.Length
            let setIndexerForSimples i (unit: SimpleFinanceUnit) = 
                { unit with 
                    CurrentIndex = i + 1
                    TotalIndex = length }

            match financeUnits with 
            | FinanceUnits.Simple units -> 
                units |> List.mapi setIndexerForSimples |> FinanceUnits.Simple
            | FinanceUnits.Complex units ->
                units |> List.mapi (fun i unit ->
                    ComplexFinanceUnit.transformVirtualInnerSimpleFinanceUnit (setIndexerForSimples i) unit
                )
                |> FinanceUnits.Complex

    type CostBundleArg =
        { PrintingCost: int -> FsSize -> float
          PaperCost: int -> FsSize -> float 
          CuttingLineCost: int -> ImposerTable -> float
          CoatingCost: int -> FsSize -> float }

        //let cost arg = function
        //| FinanceUnits.Simple units -> 
    
    //[<RequireQualifiedAccess>]
    //module FinanceUnit =
    //    open System.Numerics

    //    let asSimple = function 
    //        | FinanceUnit.Simple simple -> Some simple
    //        | FinanceUnit.Complex complex -> None

    //    let asComplex = function 
    //        | FinanceUnit.Simple simple -> None
    //        | FinanceUnit.Complex complex -> Some complex

    //    let pieceNumber = function 
    //        | FinanceUnit.Simple simple -> simple.PieceNumber
    //        | FinanceUnit.Complex complex -> complex.PieceNumber

    //    let name = function 
    //        | FinanceUnit.Simple simple -> simple.Name
    //        | FinanceUnit.Complex complex -> complex.Name

    //    let orderNumber = function 
    //        | FinanceUnit.Simple simple -> simple.OrderNumber
    //        | FinanceUnit.Complex complex -> complex.OrderNumber

    //    let private dummy =
    //        {
    //            OrderNumber = 0
    //            Utilization = 0.
    //            PieceNumber = 0
    //            Name = "untitled"
    //            Cost = 0.
    //            SalePrice = 0.
    //            CurrentIndex = 0
    //            TotalIndex = 0
    //        }

        //let create unitPrice index rows =
        //    let printNum = 
        //        let maxNum = rows |> List.map (fun row -> row.Number) |> List.max
        //        float rows.Length * maxNum
        //    let orderNum = rows |> List.map (fun row -> row.Number) |> List.sum |> int
        //    let utilization = float orderNum / float printNum

        //    let salePrice = float orderNum * unitPrice
        //    { dummy with 
        //        OrderNumber = orderNum
        //        Utilization = utilization
        //        Cost = 0.
        //        SalePrice = salePrice 
        //        CurrentIndex = index + 1 }


    //[<RequireQualifiedAccess>]
    //module FinanceUnits =
    //    let (|Simple|Complex|) financeUnits =
    //        let simples = financeUnits |> List.choose FinanceUnit.asSimple
    //        let complexs = financeUnits |> List.choose FinanceUnit.asComplex
    //        match simples.Length,complexs.Length with
    //        | 0, i when i = financeUnits.Length -> Complex complexs
    //        | i, 0 when i = financeUnits.Length -> Simple simples
    //        | _ -> Logger.invalidToken()

    type FinanceCost =
        {
            Printing: float
            CuttingLine: float
            Paper: float
            Coating: float
        }
    with 
        member x.Total = x.Printing + x.CuttingLine + x.Paper + x.Coating

    [<RequireQualifiedAccess>]
    module FinanceCost =
        let dummy =
            {
                Printing = 0.
                CuttingLine = 0.
                Paper = 0.
                Coating = 0.
            }

    type Finance =
        {
            Units: FinanceUnits
            OrderNumber: int
            Utilization: float
            PieceNumber: int
            Cost: FinanceCost
            ChunkPrice: float
        }
    with
        member x.SalePrice = x.ChunkPrice * float x.OrderNumber
        member x.Benifit = x.SalePrice - x.Cost.Total

    [<RequireQualifiedAccess>]
    module Finance =
        let dummy = 
            {
                Units = FinanceUnits.Simple []
                OrderNumber = 0
                Utilization = 0.
                PieceNumber = 0
                Cost = FinanceCost.dummy
                ChunkPrice = 0.
            }

        //let fill (financeUnits: FinanceUnits) cst salePrice  finance =
        //    { finance with
        //        Units = financeUnits
        //        OrderNumber = orderNumber
        //        Utilization = utilization
        //        PieceNumber = pieceNumber
        //        Cost = cost
        //        SalePrice = salePrice
        //    }

    type ReportInfo<'productSpecific,'unitSpecific> =
        {
            ReportUnits: FinanceUnits
            TableName: string
            ProductName: string
            Size: TextRenderInfo
            Arg: ExplicitImposingArgument
            ImposeMatrix: int * int
            BrandPage: string
            Tables: TypedImposerDataTable list
            Specific: 'productSpecific
            OrderNumber: float
            Utilization: float
            PaperNumber: float
            PrintingStrategy: PrintingStrategy
            Cost: float
            SalePrice: float
            Template : Template
        }
        with 
            member this.Benifit = this.SalePrice - this.Cost 
            member this.ImposeNum = this.ImposeMatrix ||> (*)

    //[<RequireQualifiedAccess>]
    //module ReportInfo =
    //    open System.IO
    //    open Atrous.Pdf.Utils
    //    open Atrous.Pdf.Operators

    //    let customDummy v =
    //        {
    //            ReportUnits = []
    //            TableName = "untitled"
    //            ProductName = "untitled"
    //            Size = null
    //            Arg = ExplicitImposingArgument.dummy
    //            ImposeMatrix = 0,0
    //            BrandPage = "untitled"
    //            Tables = []
    //            Specific = v
    //            OrderNumber = 0.
    //            Utilization = 0.
    //            PaperNumber = 0.
    //            Cost = 0.
    //            SalePrice = 0.
    //            Template = None
    //            PrintingStrategy = PrintingStrategy.manual

    //        }

    //    let dummy : ReportInfo<unit,unit> =
    //        customDummy ()

        //let fillTemplate docType (trInfo: TextRenderInfo) (template: BkTemplate) (state: ReportInfo<_,_>) =
        //    let size = FsSize.fromString docType (trInfo.GetText())
        //    let setRotation arg =
        //        let isRotated = FsSize.isRotatedEqual size template.Size 
        //        { arg with 
        //            RotateXObjectDegree = if isRotated then Rotation.Clockwise else Rotation.None } 

        //    let arg = BkTemplate.applyTo (setRotation state.Arg) template

        //    let colNums,rowNum = template.ColNums,template.RowNum
        //    let colNum = colNums |> List.max

        //    { state with Arg = arg; ImposeMatrix = (colNum,rowNum); Template = Some template }

        //let fillFinance 
        //    (p: Product) 
        //    (tpTable : TypedImposerDataTable) 
        //    (state: ReportInfo<'productSpecfic,'unitSpecfic>) =
        //    let imposeNum = state.ImposeMatrix ||> (*)
        //    let plates = tpTable.Rows |> List.chunkBySize imposeNum
        //    let reportUnits = plates |> List.mapi (FinanceUnit.create p.UnitPrice)

        //    let reportUnits = 
        //        reportUnits |> List.map (fun ru -> 
        //            { ru with 
        //                TotalIndex = reportUnits.Length }
        //        )

            //let info =
            //    let orderNum = reportUnits |> List.sumBy (fun u -> u.OrderNumber)
            //    let printNum = reportUnits |> List.map (fun u -> u.OrderNumber / u.Utilization) |> List.sum
            //    { state with 
            //        ProductName = p.Name
            //        ReportUnits = reportUnits
            //        Utilization = orderNum / printNum
            //        Cost = reportUnits |> List.sumBy (fun u -> u.Cost)
            //        SalePrice = reportUnits |> List.sumBy (fun u -> u.SalePrice)
            //        PaperNumber = reportUnits |> List.sumBy (fun u -> u.PieceNumber)
            //        TableName = tpTable.Name
            //        OrderNumber = orderNum 
            //    }
            //info
            //()

        //let preImpose (state: ReportInfo<_,_>) =
        //    let arg = state.Arg
        //    let src = 
        //        let writer = Path.GetTempFileName()
        //        let doc = new PdfDocument(new PdfWriter(writer))
        //        doc.AddNewPage(PageSize.ofFsSize arg.Background) |> ignore
        //        doc.Close()
        //        writer

        //    let model = 
        //        [
        //            Reuses.impose (ExplicitImposingArgument.toImplicit arg)
        //        ] |> preflight src |> List.exactlyOne

        //    let tb = model.FlowState.Tables |> List.exactlyOne

        //    let colNum,rowNum = tb.Rows.[0].Units.Length,tb.Rows.Length
            
        //    let arg = { arg with ColNums = [colNum]; RowNum = rowNum }

        //    { state with Arg = arg; ImposeMatrix = (colNum,rowNum) }



    //type PlateScene<'a,'b> = 
    //    {
    //        Name: string
    //        Header: string list
    //        Content: FinanceUnit -> string list
    //        Footer: ReportInfo<'a,'b> -> string list
    //    }

    //[<RequireQualifiedAccess>]
    //module PlateScene =
    //    let contentStringss userState scene =
    //        userState.ReportUnits |> List.map scene.Content 

    //    let headerStrings scene =
    //        scene.Header

    //    let footerStrings userState scene =
    //        scene.Footer userState

    //type TableScene = 
    //    {
    //        Name: string
    //        Header: TypedImposerDataRow -> string list
    //        Content: TypedImposerDataTable list -> string list list
    //        Footer: TypedImposerDataTable list -> string list
    //        Tables: TypedImposerDataTable list
    //    }

    //[<RequireQualifiedAccess>]
    //module TableScene =
    //    let contentStringss scene =
    //        scene.Content scene.Tables

    //    let headerStrings scene =
    //        let tables = scene.Tables
    //        let tpTable = tables.[0]
    //        let tpRow = tpTable.Rows.[0]
    //        scene.Header tpRow

    //    let footerStrings scene =
    //        scene.Footer scene.Tables

    //[<RequireQualifiedAccess>]
    //type ReportingScene<'a,'b> =
    //    | PlateScene of PlateScene<'a,'b>
    //    | TableScene of TableScene
   
    //[<RequireQualifiedAccess>]
    //module ReportingScene =
    //    open System.Reflection
    //    open iText.Kernel.Colors
    //    open iText.Layout.Layout

    //    let private addThumbnail (brandPage: string,size: FsSize,sizeText: string,font,margin,pageSize: PageSize,document: Document,table: Table,pdf:PdfDocument) = 
        
    //        let reader = new PdfDocument(new PdfReader(brandPage))
    //        let thumbnail = reader.GetFirstPage()
    //        let lastTableHeight = 
    //            let lastPageNum = pdf.GetNumberOfPages()
    //            let render = table.CreateRendererSubTree().SetParent(document.GetRenderer())
    //            let layoutHeight = float32 lastPageNum * pageSize.GetHeight()
    //            render.Layout(new LayoutContext(new LayoutArea(0, new Rectangle(pageSize.GetWidth(),layoutHeight)))) |> ignore
    //            let rowHeights = 
    //                render.GetType().GetField("heights",BindingFlags.NonPublic|||BindingFlags.Instance).GetValue(render) :?> seq<float32>
    //                |> List.ofSeq
    //                |> List.chunkByMax (pageSize.GetHeight() - margin * 2.f)

    //            rowHeights 
    //            |> List.last
    //            |> List.sum

    //        let y = pageSize.GetHeight() - lastTableHeight - (margin * 2.f) - (thumbnail.GetTrimBox().GetHeight()) |> float
    //        let x = margin |> float
    //        let xobject = thumbnail.CopyAsFormXObject(pdf) 
    //        let page = pdf.GetLastPage()
    //        let canvas = new PdfCanvas(page)
    //        PdfCanvas.addXObjectOfTrimBox xobject x y size.Width size.Height canvas

    //        let addSizeText = 
    //            let width = size.Width
    //            let height = size.Height
    //            let position = 
    //                let x = x + width + float margin
    //                let y = y + height / 2.
    //                Orientation.LeftBottom (x,y)

    //            let canvas = new Canvas(canvas,pdf,page.GetBBox())
    //            Canvas.addTextToRect position sizeText font 12. (DeviceGray.BLACK) canvas 
    //        ()

    //    let headerStrings = function
    //        | ReportingScene.PlateScene ps -> PlateScene.headerStrings ps
    //        | ReportingScene.TableScene tb -> TableScene.headerStrings tb

    //    let contentStringss userState = function
    //        | ReportingScene.PlateScene ps -> PlateScene.contentStringss userState ps
    //        | ReportingScene.TableScene tb -> TableScene.contentStringss  tb

    //    let footerStrings userState = function
    //        | ReportingScene.PlateScene ps -> PlateScene.footerStrings userState ps
    //        | ReportingScene.TableScene tb -> TableScene.footerStrings tb

    //    let name = function
    //        | ReportingScene.PlateScene ps -> ps.Name
    //        | ReportingScene.TableScene tb -> tb.Name

    //    let report (md: FlowModel<ReportInfo<_,_>>) (scene: ReportingScene<_,_>) =
    //        let userState = md.FlowState.UserState
    //        let size,sizeText,brandPage = 
    //            let size = userState.Size
    //            userState.Arg.DesiredSize,
    //            size.GetText(),
    //            userState.BrandPage

    //        let reportWithScence (scene: ReportingScene<_,_>) =

    //            let writer = 
    //                let dest = 
    //                    let name = name scene
    //                    md.Path |> Path.changeExtension (sprintf "%s_report.pdf" name)
    //                let writer = new PdfWriter(dest)
    //                new PdfDocumentWithResources(writer)

    //            let pageSize = PageSize.A4.Rotate()
    //            let margin = float32 20
    //            let document = new Document(writer, pageSize)
    //            document.SetMargins(margin, margin, margin, margin)

    //            let headerStrings = headerStrings scene

    //            let table = 
    //                let arg = Array.replicate headerStrings.Length 4.f
    //                let tb = new Table(arg)
    //                tb.SetWidth(pageSize.GetWidth() - 70.f)

    //            let dahei = PdfDocumentWithResources.getOrAddFont FontGen.daHei writer
    //            let heiti = PdfDocumentWithResources.getOrAddFont FontGen.heiti writer
                
    //            let addBoldCell (s: string) = 
    //                let c = new Cell()
    //                c.Add(new Paragraph(s)).SetFont(dahei) |> ignore
    //                table.AddCell c |> ignore

    //            let addHeader() = 
    //                headerStrings |> List.iter addBoldCell

    //            let addContent() = 
    //                let addCell (s: string) = 
    //                    let c = new Cell()
    //                    c.Add(new Paragraph(s)).SetFont(heiti) |> ignore
    //                    table.AddCell c |> ignore
    //                contentStringss userState scene
    //                |> (List.iter (List.iter addCell))

    //            let addFooter() = 
    //                footerStrings userState scene
    //                |> List.iter addBoldCell


    //            addHeader()
    //            addContent()
    //            addFooter()
    //            document.Add table |> ignore
    //            addThumbnail (brandPage,size,sizeText,heiti,margin,pageSize,document,table,writer)

    //            document.Close()
    //            writer.Close()
    //        scene |> reportWithScence



    [<AutoOpen>]
    module Extensions =
        [<RequireQualifiedAccess>]
        module Order =
            open System.Linq
            let toTypedDataTable p (variable:Variable) (order: Order) : TypedImposerDataTable =
                let toTypedDataCell key (it:IItem) = 
                    let tp = it.GetType()
                    let field = tp.GetProperty(key)
                    {
                        Header = key
                        Value = field.GetValue(it)
                        Type = field.PropertyType
                    }

                let groupedRows : TypedImposerDataRow list = 

                    let items = order.Items.OrderBy(fun item -> 
                        variable.PrinterKeys |> List.map (fun k -> 
                            let c = toTypedDataCell k item
                            c.Value
                        )
                    )
                    items
                    |> List.ofSeq
                    |> List.groupBy (fun item ->
                        variable.PrinterKeys |> List.map (fun k -> toTypedDataCell k item)
                    )
                    |> List.map (fun (gds,items) -> 
                        {
                            Id = items |> List.map (fun it -> it.Id) |> List.max
                            Cells = 
                                gds |> List.sortBy (fun gd -> 
                                    Item.toPrecedence gd.Header
                                )
                            Number = ProductKind.getNumber items p.Kind |> float
                            DuplicatedNumber = 1
                        }
                    )
                    |> List.sortBy(fun row -> row.Id)
                {
                    Rows = groupedRows
                    Name = order.Name
                }


        [<RequireQualifiedAccess>]
        module Product =
            let toTypedDataTables variable (p: Product) =
                p.Orders |> List.map (Order.toTypedDataTable p variable)

        [<RequireQualifiedAccess>]
        module FlowModel = 

            //let initDummy pdf =
            //    {
            //        Path = pdf
            //        FlowState =  initFlowState ReportInfo.dummy
            //    }

            let initCustom v pdf =
                {
                    Path = pdf
                    FlowState = initFlowState v
                }

        [<RequireQualifiedAccess>]
        module private Background =
            let preImpose arg spawns (background: FsSize) =
                let f1 = 
                    (fun (arg: ExplicitImposingArgument) -> 
                        {arg with 
                            Background = background
                            IsRepeated = true
                            Margin = Margin.imposingDest })
                
                let transforms =
                    [f1] 
                    |> List.spawn spawns

                let newArg,tb = ExplicitImposingArgument.mostPreimpose transforms arg
                { newArg with 
                    Cropmark = Some Cropmark.simple
                    Background = FsSize.maximalBackground
                    Margin = Margin.imposingDest },tb

        [<RequireQualifiedAccess>]
        module HomePrinter =
            let preImpose arg (homePrinter: HomePrinter) =
                let background = PrinterSize.value homePrinter.RecommandedSize
                Background.preImpose 
                    arg 
                    [ ExplicitImposingArgument.clearMargin
                      ExplicitImposingArgument.clockwiseBackground ] 
                    background
        
        [<RequireQualifiedAccess>]
        module OutSourcePrinter =
            let preImpose arg (outSourcePrinter: OutSourcePrinter) =
                let background = PrinterSize.value outSourcePrinter.RecommandedSize
                Background.preImpose 
                    arg 
                    [ExplicitImposingArgument.clockwiseBackground] 
                    background

        [<RequireQualifiedAccess>]
        module Printer = 
            let preImpose arg = function 
                | Printer.OutSource outSourcePrinter -> OutSourcePrinter.preImpose arg outSourcePrinter
                | Printer.Home homePrinter -> HomePrinter.preImpose arg homePrinter

            let unitCost desiredColor printer =
                let printerColor = PrinterColor.ofDesiredColor desiredColor
                match printer with 
                | Printer.OutSource outSourcePrinter -> Some (fun _ pieceNumber -> outSourcePrinter.UnitCost printerColor pieceNumber)
                | Printer.Home homePrinter -> 
                    match desiredColor,homePrinter.Color with 
                    | desiredColor,PrinterColor.Monochrome when (not (DesiredColor.isBlack desiredColor)) -> None
                    | _ -> Some (fun pieceSize _ -> homePrinter.UnitCost pieceSize printerColor)

            let getFinanceUnits arg tables printer =
                let multiply = ExplicitImposingArgument.imposeMatrixMulitiply arg
                let pieceNumberTransform = Printer.getPieceNumberTransform printer
                let totalIndex = tables |> List.collect (fun tb -> tb.Rows) |> List.length
                tables |> List.mapFold (fun indexAccum table ->
                    table.Rows |> List.map (fun row ->
                        let orderNumber = row.Number
                        let pieceNumber = 
                            float orderNumber / float multiply 
                            |> pieceNumberTransform

                        {
                            Name = table.Name
                            PieceNumber = pieceNumber
                            OrderNumber = int orderNumber
                            CurrentIndex = indexAccum + 1
                            TotalIndex = totalIndex
                        }
                    ),table.Rows.Length + indexAccum
                ) 0
                |> fst
                |> List.concat
                |> FinanceUnits.Simple

        [<RequireQualifiedAccess>]
        module Proofer =
            let cuttingLineCost imposerTable pieceNumber (proofer: Proofer) = 
                proofer.Cutter.Cost imposerTable pieceNumber 

        [<RequireQualifiedAccess>]
        module DigitalPrinting =
            let printingCost wastingPercent desiredColor digital =
                let printer = DigitalPrinting.asPrinter digital
                Printer.unitCost desiredColor printer
                |> Option.map (fun unitCost ->
                    fun pieceNumber pieceSize ->
                        float pieceNumber * wastingPercent * (unitCost pieceSize pieceNumber)
                )

            let getFinanceUnits arg tables digital = 
                let printer = DigitalPrinting.asPrinter digital
                Printer.getFinanceUnits arg tables printer

        [<RequireQualifiedAccess>]
        module PlateOpenSize = 
            let preImpose arg plateOpenSize =
                let background = PlateOpenSize.getOriginFsSize plateOpenSize
                let newArg,tb = 
                    Background.preImpose 
                        arg 
                        [ExplicitImposingArgument.clockwiseBackground]
                        background
                { newArg with IsRepeated = false },tb

        [<RequireQualifiedAccess>]
        module OffsetPress =
            let printingCost desiredColor offSetPress =
                DesiredColor.curriedPressPrice offSetPress desiredColor
                |> Option.map (fun pressPrice ->
                    fun pieceNumber _ ->
                        pressPrice pieceNumber
                        |> float
                )
        

        [<RequireQualifiedAccess>]
        module ManualPress = 
            let preImpose arg (pieceSize: FsSize) =
                let arg,tb = 
                    Background.preImpose 
                        arg 
                        [ ExplicitImposingArgument.clearMargin
                          ExplicitImposingArgument.clockwiseBackground ] 
                        pieceSize

                { arg with IsRepeated = false }, tb

            let printingCost desiredColor tarm = 
                DesiredColor.toPressColors desiredColor
                |> function
                    | [] | [PressColor.FourColor] -> None
                    | manualPressColors ->
                        let manualCost = TargetModel.getManualCost tarm
                        fun pieceNumber pieceSize ->
                            manualCost pieceSize pieceNumber * float manualPressColors.Length
                        |> Some
        

        [<RequireQualifiedAccess>]
        module Flow = 
            let rec bindM (getProp: 'userState -> 'propUserState) (setProp: 'propUserState -> 'userState -> 'userState) (flow: Flow<'propUserState>) =
                match flow with 
                | Flow.Reuse reuse -> 
                    fun (flowModel: FlowModel<'userState>) ->
                        let propFlowModel = flowModel |> FlowModel.mapUserState getProp
                        let flows =
                            reuse propFlowModel |> List.map (FlowModel.mapUserState (fun userState ->
                                setProp userState flowModel.FlowState.UserState
                            ))
                        flows
                    |> Flow.Reuse
                | Flow.Composite flows ->
                    flows |> List.map (bindM getProp setProp) |> Flow.Composite
                | Flow.Read read ->
                    fun doc userState ->
                        let propUserState = getProp userState
                        let newPropUserState = read doc propUserState
                        setProp newPropUserState userState
                    |> Flow.Read
                | Flow.Manipulate manipulate ->
                    fun (arg: ManipulateArgument<'userState>) ->
                        let propArg = ManipulateArgument.mapUserState getProp arg
                        let propFlowState = manipulate propArg
                        propFlowState |> FlowState.mapUserState (fun propUserState ->
                            setProp propUserState arg.FlowState.UserState
                        )
                    |> Flow.Manipulate
                | Flow.TransformUserState transform ->
                    fun (userState: 'userState) ->
                        let propUserState = getProp userState
                        let newPropUserState = transform propUserState
                        setProp newPropUserState userState
                    |> Flow.TransformUserState

                | Flow.FromState fromState ->
                    fun (flowState: FlowState<'userState>) ->
                        let propFlowState = FlowState.mapUserState getProp flowState
                        let propFlow = fromState propFlowState
                        bindM getProp setProp propFlow
                    |> Flow.FromState
                | _ -> Logger.notImplemented()

            let mapM ofLeaf asLeaf (flow: Flow<'userState>) : Flow<'newUserState> = bindM ofLeaf (fun userState _ -> asLeaf userState) flow
        
        [<RequireQualifiedAccess>]
        module Commercial =
            let getManualPieceSize tarm (commercial: Commercial) = 
                tarm.GlobalConfig.FinanceConfig.ManualRecommendPieceSize commercial

            let getWastingPercent printingStrategy tarm (commercial: Commercial) =
                TargetModel.getWastingPercent printingStrategy (ProductKind.Commercial commercial) tarm
        
        [<RequireQualifiedAccess>]
        module Sticker =
            let getManualPieceSize tarm (sticker: Sticker) =
                Commercial.getManualPieceSize tarm (Commercial.Sticker sticker)

            let getWastingPercent printingStrategy tarm (sticker: Sticker) =
                Commercial.getWastingPercent printingStrategy tarm (Commercial.Sticker sticker)