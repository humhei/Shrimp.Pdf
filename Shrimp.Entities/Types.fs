namespace Atrous.Entities


module Types = 
    open System
    open Fake.IO.FileSystemOperators
    open Atrous.Pdf
    open Atrous.Utils
    open iText.Kernel.Geom
    open Atrous.Pdf.Colors
    open Atrous.Extensions
    open Atrous.Pdf.Types
    open Atrous.Pdf.Extensions
    open Atrous

    [<RequireQualifiedAccess>]
    type RetrieveFrom =
        | Code = 0
        | DB = 1

    [<RequireQualifiedAccess>]
    type Settable<'T> =
        | Auto
        | Manual of 'T

    [<RequireQualifiedAccess>]
    module Settable =
        let cata fAuto fManual settable =
            match settable with 
            | Settable.Auto -> fAuto()
            | Settable.Manual t -> fManual t

    [<CustomEquality;NoComparison>]
    type AutoSet<'T when 'T : equality> =
        {
            Settable: Settable<'T>
            RetrieveFrom: RetrieveFrom
        }

        override x.Equals(y) =
            match y with 
            | :? AutoSet<'T> as y -> 
                let equalWith compareWhenNew compareWhenExists =
                    match x.RetrieveFrom,y.RetrieveFrom with 
                    | RetrieveFrom.Code,RetrieveFrom.DB -> compareWhenExists(x,y)
                    | RetrieveFrom.DB,RetrieveFrom.Code -> compareWhenExists(y,x)
                    | _, _ -> compareWhenNew(x,y)

                let compareWhenNew (x,y) = x.Settable = y.Settable

                let compareWhenExists (fromFsx,fromDB) =
                    match fromFsx.Settable,fromDB.Settable with 
                    | Settable.Auto _, Settable.Auto -> false
                    | Settable.Manual _, Settable.Auto -> false
                    | Settable.Auto, Settable.Manual _ -> true
                    | Settable.Manual m1, Settable.Manual m2 -> m1 = m2

                equalWith compareWhenNew compareWhenExists

            | _ -> false

        override x.GetHashCode() = 
            Logger.notImplemented()
    

    type Known<'T> =
        | Unknown
        | Known of 'T
    
    type Email = private Email of string
    
    [<RequireQualifiedAccess>]
    module Email =
        let create = runWithInputReturn FParsec.email >> Email

    type Insider =
        {
            Name: string
        }

    type OutSourcer =
        {
            Address: Known<string>
        }

    type Upward = OutSourcer

    [<RequireQualifiedAccess>]
    type Organization =
        | Insider of Insider
        | OutSourcer of OutSourcer

    type Prefession<'organization> =
        {
            QQAccount: int64
            QQAccountName: string
            PhoneNumber: Known<int64>
            Organization: 'organization
        }
        
    type DownwardPrefession = Prefession<Organization>

    type PrepressDesigner = PrepressDesigner of DownwardPrefession
    
    type Trader = Trader of DownwardPrefession

    type CTPPublisher = CTPPublisher of DownwardPrefession

    type FilmPublisher = FilmPublisher of DownwardPrefession
    //type MoldMaker = MoldMaker of DownwardPrefession

    //type MoldPresser = MoldPresser of DownwardPrefession
    //type PaperCutter = PaperCutter of DownwardPrefession
    //type StickerSlicer = StickerSlicer of DownwardPrefession
    
    [<RequireQualifiedAccess>]
    type PlatePublisher =
        | FilmPublisher of CTPPublisher
        | CTPPublisher of FilmPublisher

    type UpwardOrganization = OutSourcer

    type UpwardPrefession = Prefession<UpwardOrganization>
    type OrderOffer = OrderOffer of UpwardPrefession

    type OpenKind =
        | Big
        | Positive

    type OpenSize =
        {
            Width: float
            Height: float
            OpenNum: int
            OpenKind: OpenKind
        }

    [<RequireQualifiedAccess>]
    module OpenSize =
        open Atrous.Utils
        open Atrous.Pdf.Extensions


        let inline create width height openKind openNum =

            let w = toInche width
            let h = toInche height

            {
                Width = w
                Height = h
                OpenKind = openKind
                OpenNum = openNum
            }

        let Four = create 540 390 Positive 4
        let BigFour = create 590 440 Positive 4
        let Six = create 390 360 Positive 6
        let BigSix = create 440 394 Big 6
        let Eight = create 390 270 Positive 8
        let BigEight = create 440 295 Big 8
        let Nine = create 360 260 Positive 9
        let BigNine = create 394 294 Big 9
        let Ten = create 390 216 Positive 10
        let BigTen = create 440 236 Big 10
        let Twelve = create 270 260 Positive 12
        let BigTwelve = create 295 294 Big 12
        let FourTeen = create 216 270 Positive 14
        let BigFourTeen = create 236 323 Big 14
        let FiveTeen = create 216 260 Positive 15
        let BigFiveTeen = create 236 294 Big 15
        let SixTeen = create 270 195 Positive 16
        let BigSixTeen = create 295 220 Big 16

        let allOpenSizes = 
            [
                Four
                BigFour
                Six
                BigSix
                Eight
                BigEight
                Nine
                BigNine
                Ten
                BigTen
                Twelve
                BigTwelve
                SixTeen
                BigSixTeen
            ]

        let isPositive openSize = 
            match openSize.OpenKind with
            | Positive -> true
            | Big -> false

        let getWidth openSize = openSize.Width
        let getHeight openSize = openSize.Height

        let openTypeTextZH (openSize: OpenSize) =
            match openSize.OpenKind with 
            | Positive -> "正度"
            | Big -> "大度"

        let openNumTextZH (openSize: OpenSize) = 
            sprintf "%d开" openSize.OpenNum

        let openSizeTextZH o =
            sprintf "%s%s" (openTypeTextZH o) (openNumTextZH o) 
        
        let tryFindMinimun (size: FsSize) (openSizes: OpenSize list) =
            openSizes |> FsSize.tryFindMinSizeBiggerThanFsSizeContinuously size getWidth getHeight

        let tryFindMinimunByBackground background openSizes =
            match background with 
            | Background.Size size -> tryFindMinimun size openSizes
            | _ -> None
        
        let toFsSize openSize =
            {
                Width = openSize.Width
                Height = openSize.Height
            }

    [<RequireQualifiedAccess>]
    type PrintingColor =
        | FourColor
        | Monochrome

    type PrinterColor = PrintingColor

    [<RequireQualifiedAccess>]
    module PrinterColor = 
        let ofDesiredColor desiredColor =
            match desiredColor with 
            | DesiredColor.Black _ -> PrinterColor.Monochrome
            | _ -> PrinterColor.FourColor

    type PressColor = PrinterColor

    [<RequireQualifiedAccess>]
    module PressColor =
        let isFourColor = function
            | PressColor.FourColor -> true
            | PressColor.Monochrome -> false

        let isMonochrome = function
            | PressColor.FourColor -> true
            | PressColor.Monochrome -> false

        let ofDesiredColor desiredColor = PrinterColor.ofDesiredColor desiredColor


    type MaxiumPressOpenSize =
        {
            OriginSize: OpenSize
            ClippedSize: FsSize
        }

    [<RequireQualifiedAccess>]
    type PlateOpenSize =
        | OpenSize of OpenSize
        | MaxiumPressOpenSize of MaxiumPressOpenSize

    [<RequireQualifiedAccess>] 
    module PlateOpenSize =
        let getOpenNum = function
            | PlateOpenSize.MaxiumPressOpenSize mos -> mos.OriginSize.OpenNum
            | PlateOpenSize.OpenSize os -> os.OpenNum

        let getOpenKind = function
            | PlateOpenSize.MaxiumPressOpenSize mos -> mos.OriginSize.OpenKind
            | PlateOpenSize.OpenSize os -> os.OpenKind

        let getOriginFsSize = function
            | PlateOpenSize.MaxiumPressOpenSize mos -> mos.OriginSize |> OpenSize.toFsSize
            | PlateOpenSize.OpenSize os -> os |> OpenSize.toFsSize

        let getClippedFsSize = function
            | PlateOpenSize.MaxiumPressOpenSize mos -> mos.ClippedSize
            | PlateOpenSize.OpenSize os -> os |> OpenSize.toFsSize

        let getOriginOpenSize = function
            | PlateOpenSize.MaxiumPressOpenSize mos -> mos.OriginSize
            | PlateOpenSize.OpenSize os -> os

    type PressSize = 
        {
            Number: int
            Width: float
            Height: float
        }

    [<RequireQualifiedAccess>]
    module PressSize =
        open Atrous.Pdf.Extensions
        open Atrous.Utils

        let eight = {Number = 8; Width = toInche 510.;Height = toInche 390.}

        let allPressSize =
            [
                eight
            ]

        let getWidth pressSize = pressSize.Width

        let getHeight pressSize = pressSize.Height

        let toFsSize pressSize = 
            {
                Width = pressSize.Width
                Height = pressSize.Height
            }

        let isBiggerThan (size: FsSize) pressSize =
            pressSize.Width > size.Width - tolerance
            && pressSize.Height > size.Height - tolerance

        let isBiggerThanOfOpenSize (size: OpenSize) pressSize =
            isBiggerThan (OpenSize.toFsSize size) pressSize

        let tryGetMaxiumOpenSize pressSize =
            let fsSize = toFsSize pressSize
            OpenSize.tryFindMinimun fsSize OpenSize.allOpenSizes |> Option.map (fun openSize ->
                {
                    OriginSize = openSize
                    ClippedSize = fsSize
                }
            )

    type OffsetPress = 
        {
            Tag: string
            Size: PressSize
            Color: PressColor
            DeliverFroms: CTPPublisher list
            CostUnit: int -> int (*peiceNumber -> priceUnit*)
            MaxiumOpenSize: MaxiumPressOpenSize option
            PieceNumberTransform: float -> int
        }

    [<RequireQualifiedAccess>]
    module OffsetPress =

        let tryGetMaxiumOpenSize press =
            PressSize.tryGetMaxiumOpenSize press.Size

        let isFourColor press =
            PressColor.isFourColor press.Color

        let isMonochrome press =
            PressColor.isMonochrome press.Color

        let isEightOpen press =
            press.Size.Number = 8

        let isFourColorEightOpen press =
            isFourColor press
            && isEightOpen press

        let isMonochromeEightOpen press =
            isMonochrome press
            && isEightOpen press

        let getSize press =
            press.Size

        let filterByColor (color:PressColor) presses =
            presses |> List.filter (fun press ->
                press.Color = color
            )

        let tryFindBySizeAndColorIn (pageSize:FsSize) (color:PressColor) presses =
            filterByColor color presses
            |>  List.tryFind ( fun press -> press.Size |> PressSize.isBiggerThan pageSize)

        let getPlateOpenSizes (press: OffsetPress) =
            let withMaxium plateSizes = 
                match press.MaxiumOpenSize with 
                | Some mos -> PlateOpenSize.MaxiumPressOpenSize mos :: plateSizes
                | None -> plateSizes

            OpenSize.allOpenSizes |> List.filter (fun pageSize ->
                press.Size |> PressSize.isBiggerThanOfOpenSize pageSize 
            ) |> List.map PlateOpenSize.OpenSize |> withMaxium


    type FourColorEightOpenPress = private FourColorEightOpenPress of OffsetPress
    [<RequireQualifiedAccess>]
    module FourColorEightOpenPress =
        let create tag deliverFroms costUnit pieceNumberTransform =
            {
                Tag= tag
                Size= PressSize.eight
                Color= PressColor.FourColor
                DeliverFroms = deliverFroms
                CostUnit = costUnit
                MaxiumOpenSize = PressSize.tryGetMaxiumOpenSize PressSize.eight
                PieceNumberTransform = pieceNumberTransform
            }
            |> FourColorEightOpenPress

        let value (FourColorEightOpenPress press) = press

    type MonochromeEightOpenPress = private MonochromeEightOpenPress of OffsetPress
    [<RequireQualifiedAccess>]
    module MonochromeEightOpenPress =
        let create tag deliverFroms costUnit pieceNumberTransform =
            {
                Tag= tag
                Size= PressSize.eight
                Color= PressColor.Monochrome
                DeliverFroms = deliverFroms
                CostUnit = costUnit
                MaxiumOpenSize = PressSize.tryGetMaxiumOpenSize PressSize.eight
                PieceNumberTransform = pieceNumberTransform
            }
            |> MonochromeEightOpenPress

        let value (MonochromeEightOpenPress press) =
            press


    type PrinterSize = private PrinterSize of FsSize

    [<RequireQualifiedAccess>]
    module PrinterSize = 
        let A4 = PrinterSize (FsSize.ofPageSize PageSize.A4)
        let A3 = PrinterSize (FsSize.ofPageSize PageSize.A3)
        let value (PrinterSize pageSize) = pageSize
        let toOpenSize (PrinterSize pageSize) =
            OpenSize.tryFindMinimun pageSize OpenSize.allOpenSizes

    type HomePrinter =
        {
            Name: string
            Size: PrinterSize
            Color: PrinterColor
            RecommandedSize: PrinterSize
            UnitCost: FsSize -> PrinterColor -> float
            PieceNumberTransform: float -> int
        }


    type OutSourcePrinter =
        {
            RecommandedSize: PrinterSize
            UnitCost: PrinterColor -> int -> float
            PieceNumberTransform: float -> int
        }

    [<RequireQualifiedAccess>]
    type Printer = 
        | Home of HomePrinter
        | OutSource of OutSourcePrinter

    [<RequireQualifiedAccess>]
    module Printer =
        let isHomePrinter = function 
            | Printer.Home _ -> true
            | Printer.OutSource _ -> false
        
        let isOutSourcePrinter = function 
            | Printer.Home _ -> false
            | Printer.OutSource _ -> true


        let getRecommandedSize = function 
            | Printer.Home homePrinter -> homePrinter.RecommandedSize
            | Printer.OutSource outSourcePrinter -> outSourcePrinter.RecommandedSize
            
        let getPieceNumberTransform = function 
            | Printer.Home homePrinter -> homePrinter.PieceNumberTransform
            | Printer.OutSource outSourcePrinter -> outSourcePrinter.PieceNumberTransform

    type Coater = FsSize -> int -> float

    type DigitalCutter =
        {
            Cost: ImposerTable -> int -> float
        }

    type Proofer =
        { Printer: OutSourcePrinter
          Profession: DownwardPrefession
          Cutter: DigitalCutter
          Coater: Coater }



    [<RequireQualifiedAccess>]
    type MaterialEnum =
        | Sintetico = 0
        | Textile = 1
        | Rubber = 2
        | PU = 3
        | PVC = 4
        | Poliuretano = 5
        | Mesh = 6
        | Goma = 7
        | Lona = 8
        | Textil = 9
        | Caucho = 10
        | Sintética = 11

    [<RequireQualifiedAccess>]
    module MaterialEnum =
        let ofString = Enum.ofString<MaterialEnum>

    type MaterialMiddleWare =
        {
            Enum: MaterialEnum
            Icon: string
        }

    [<RequireQualifiedAccess>]
    module MaterialMiddleWare =
        let ofEnumZH materialDir kind =
            {
                Enum = kind
                Icon = 
                    match kind with 
                    | MaterialEnum.Textile | MaterialEnum.Textil -> 
                        materialDir </> "布.pdf"
                    | _ -> materialDir </> "革.pdf"
            }
        let ofStringZH materialDir (s: string) =
            let kind = MaterialEnum.ofString s
            ofEnumZH materialDir kind

    type Material =
        {
            Top: MaterialMiddleWare list
            Middle: MaterialMiddleWare list
            Bottom: MaterialMiddleWare list
        }
    [<RequireQualifiedAccess>]
    module Material =

        let ofEnums materialDir (enums: MaterialEnum list) =
            match enums with 
            | [top;middle;bottom] ->
                {
                    Top = [MaterialMiddleWare.ofEnumZH materialDir top]
                    Middle = [MaterialMiddleWare.ofEnumZH materialDir middle]
                    Bottom = [MaterialMiddleWare.ofEnumZH materialDir bottom]
                }
            | _ -> Logger.invalidArg enums
        
    [<RequireQualifiedAccess>]
    type Sex = 
        | Man 
        | Woman
        | Children


    [<RequireQualifiedAccess>]
    module Sex =
        let isMan sex =
            match sex with | Sex.Man -> true | _ -> false

        let isWoman sex =
            match sex with | Sex.Woman -> true | _ -> false

    type ColorEnum =
        | Blanco = 0
        | Black = 2
        | Blue = 3
        | Burgundy = 4
        | ``Dusty blue`` = 5
        | Cafe = 6
        | Dorado (*金色*) = 6
        | ``Denim black`` = 7
        | ``Denim gray`` = 8
        | Grey = 9
        | Pink = 10
        | Red = 11
        | Plateado (*银色*) = 12
        | Negro = 13
        | Navy = 14
        | Rose = 15
        | Rojo = 16
        | Rosado = 17 
        | ``Rose gold`` = 18
        | Verde = 19
        | Jeans = 20
        | ``Rojo de vino`` = 21
        | White = 22
        | Wht = 23
        | Mint = 24
        | Maroon = 25 
        | Beige = 26
        | Mar = 27
        | Green = 28

    type SizeZip =
        {
            EUR: int
            UK: float
            US: float
        }

    [<RequireQualifiedAccess>]
    module SizeZip =
        let create (eur,uk,us) =
            {
                EUR = eur
                UK = uk
                US = us
            }

        let getUK sizeZip =
            sizeZip.UK
        let getEUR sizeZip =
            sizeZip.EUR

        let private findByUS us sizeZips =
            sizeZips |> List.find (fun sz ->
                sz.US = us
            )

        let getEURFromUS us sizeZips =
            findByUS us sizeZips |> getEUR

        let getUKFromUS us sizeZips =
            findByUS us sizeZips |> getUK

    [<RequireQualifiedAccess>]
    module ColorEnum =
        let toZH = function 
            | ColorEnum.Green ->
                "绿色"
            | _ -> "Not implemented"

        let ofString = Enum.ofString<ColorEnum>

        let enums =
            Enum.GetNames(typeof<ColorEnum>) |> Seq.sortDescending

        let map =
            enums |> Seq.map (fun s -> 
                let enum = ofString s
                enum,toZH enum
            ) |> dict

        let fromZH s =
            map.Keys 
            |> Seq.filter (fun key -> map.[key] = s)
            |> List.ofSeq
            |> function 
                | [item] -> item
                | _ -> failwithf "cannot find colorEnum form %s" s

    [<RequireQualifiedAccess>]
    type Language =
        | Spanish
        | English
        | Chinese

    type IPrice =
        abstract member Price: float

    type IVendorCode =
        abstract member VendorCode: int

    type IBrand =
        abstract member Brand : string

    type IStyle =
        abstract member Style : string

    type IColorCard =
        abstract member ColorCard : ColorCard

    type IColor =
        abstract member Color : ColorEnum
    
    type IEUR =
        abstract member EUR : int

    type IUK =
        abstract member UK : float

    type IUS =
        abstract member US : float
    
    type ISex =
        abstract member Sex: Sex
    
    type ISize =
        inherit IUS
        inherit IEUR
        inherit IUK

    type IShippingDate =
        abstract member ShippingDate : DateTime

    type IBarcode =
        abstract member Barcode : string 

    type ICartonBarcode =
        abstract member CartonBarcode : string 
    
    type IItem = 
        abstract member Id : int
        abstract member Art : string
        abstract member Number : int

    type IFraction =
        abstract member Fraction : int

    type IImage =
        abstract member Image : string

    [<RequireQualifiedAccess>]
    module Item = 

        let [<Literal>] Id =  "Id"
        let [<Literal>] Art =  "Art"
        let [<Literal>] Style =  "Style"
        let [<Literal>] Brand =  "Brand"
        let [<Literal>] Color =  "Color"
        //let [<Literal>] ShortColor =  "ShortColor"
        let [<Literal>] EUR =  "EUR"
        let [<Literal>] UK =  "UK"
        let [<Literal>] Number =  "Number"
        //let [<Literal>] Material =  "Material"
        //let [<Literal>] Image =  "Image"
        let [<Literal>] Barcode =  "Barcode"
        //let [<Literal>] Date =  "Date"
        let [<Literal>] Sex =  "Sex"
        //let [<Literal>] SizeRange =  "SizeRange"
        let [<Literal>] US =  "US"
        //let [<Literal>] CM =  "CM"
        //let [<Literal>] Code =  "Code"
        //let [<Literal>] Template =  "Template"
        let [<Literal>] Price =  "Price"
        let [<Literal>] ColorCard =  "ColorCard"
        let [<Literal>] Fraction =  "Fraction"
        let [<Literal>] FontColor =  "FontColor"

        //let [<Literal>] ShippingDate =  "ShippingDate"
    
        let toZh string =
            match string with 
            | Art -> "货号"
            | Brand -> "商标"
            | Color -> "颜色"
            | EUR -> "欧码"
            | US -> "美码"
            | UK -> "英码"
            | Price -> "价格"
            | Barcode -> "条码"
            | ColorCard -> "色卡"
            | Style -> "款式"
            | Sex -> "性别"
            | FontColor -> "文字颜色"
            | Fraction -> "配码"
            | Number -> "数量"
            | _ -> failwith "Not implemented"
    
        let toPrecedence string =
            let literals = [Art;Style;Color;Barcode;EUR;US;UK] 
            List.tryFindIndex ((=) string) literals |> Option.defaultValue literals.Length

        //let toKeys (x: Item) =
        //    [
        //        if x.Template <> None then yield Template
        //        if x.Art <> "Untitiled" then yield Art
        //        if x.Style <> None then yield Style
        //        if x.Brand <> None then yield Brand
        //        if x.Color <> None then yield Color
        //        if x.ShortColor <> None then yield ShortColor
        //        if x.Material <> None then yield Material
        //        if x.Image <> None then yield Image
        //        if x.Sex <> None then yield Sex
        //        if x.Barcode <> None then yield Barcode
        //        if x.Code <> None then yield Code
        //        if x.SizeRange <> None then yield SizeRange
        //        if x.EUR <> None then yield EUR
        //        if x.US <> None then yield US
        //        if x.UK <> None then yield UK 
        //        if x.CM <> None then yield CM
        //        if x.Fraction <> None then yield Fraction
        //        if x.Price <> None then yield Price
        //        if x.Number <> 0 then yield Number
        //        if x.ShippingDate <> None then yield ShippingDate
        //    ]

    [<RequireQualifiedAccess>]
    type Pattern = 
        | Before of char[]

    type Selector =
        {
            Name: string
            Pattern: Pattern option
        }

    [<RequireQualifiedAccess>]
    module Selector = 
        let apply (s:string) (selector:Selector) = 
            match selector.Pattern with 
            | Some pt -> 
                match pt with
                | Pattern.Before separator -> 
                    s.Split separator
                    |> function
                        | [|s;_|] -> s
                        | _ -> failwithf "%s should be separate to two parts %A" s separator
            | None -> s

        let internal createSimple s =
            {
                Name = s
                Pattern = None
            }

        let Id =  createSimple Item.Id
        let Art =  createSimple Item.Art
        let Number =  createSimple Item.Number

        //let Brand =  createSimple Item.Brand
        //let Color =  createSimple Item.Color
        //let EUR =  createSimple Item.EUR
        //let UK =  createSimple Item.UK
        //let Material =  createSimple Item.Material
        //let Image =  createSimple Item.Image
        //let Barcode =  createSimple Item.Barcode
        //let Date =  createSimple Item.Date
        //let Sex =  createSimple Item.Sex
        //let SizeRange =  createSimple Item.SizeRange
        //let US =  createSimple Item.US
        //let Template =  createSimple Item.Template
        //let Price =  createSimple Item.Price
    [<RequireQualifiedAccess>]
    module String =
        let applySelector selector s =
            Selector.apply s selector

    [<CLIMutable>]
    type Order =
        {
            Id: int
            Name: string
            Items: IItem list
            Date: DateTime
            TagName: string
        }

    [<RequireQualifiedAccess>]
    type Splice =
        | Merged
        | Splited

    type ImposerSelectors = 
        {
            Splice: Splice
            Value: Selector list
        }

    [<RequireQualifiedAccess>] 
    module ImposerSelectors =
        let cata (selectors:ImposerSelectors) f =
            match selectors.Splice with 
            | Splice.Merged _ -> Logger.notImplemented()
            | Splice.Splited -> f()

        let empty =
            {
                Splice = Splice.Splited
                Value = []
            }

    type Variable =
        {
            PrinterKeys: string list
            MakesureSelectors: Selector list
            ImposerSelectors: ImposerSelectors
        }

    [<RequireQualifiedAccess>]
    module Variable =
        let createSimple (item:IItem) =
            let keys = item.GetType().GetProperties() |> Seq.map(fun p -> p.Name) |> List.ofSeq |> List.except [Item.Id;Item.Number]
            let escapeCarton (keys: string list) = keys |> List.filter (fun key -> key.Contains "Carton" |> not)
            
            {
                PrinterKeys = keys |> escapeCarton
                MakesureSelectors = 
                    match item with 
                    | :? IColor -> [Item.Art;Item.Color]
                    | _ -> [Item.Art]
                    |> List.map Selector.createSimple
                ImposerSelectors = ImposerSelectors.empty
            }

    
    [<RequireQualifiedAccess>]
    module DesiredColor =

        let toPressColors = function
            | DesiredColor.CMYK -> [PressColor.FourColor] 
            | DesiredColor.Double _ -> [PressColor.Monochrome;PressColor.Monochrome]
            | DesiredColor.Single _ -> [PressColor.Monochrome]

        let curriedPressPrice press dc = 
            toPressColors dc |> List.collect (fun color ->
                OffsetPress.filterByColor color [press]
            ) 
            |> function
                | [] -> None
                | presses ->
                    fun pieceNumber ->
                        List.sumBy (fun press -> press.CostUnit pieceNumber) presses
                    |> Some


        let ofColors colors =
            let colorCards = colors |> List.map ColorCard.ofColor
            match colorCards with 
            | [c] -> DesiredColor.Single c
            | [c1;c2] ->
                (c1,c2) |> DesiredColor.Double
            | colors when colors.Length > 2 ->
                DesiredColor.CMYK
            | _ -> Logger.notImplemented()

    [<RequireQualifiedAccess>]
    type Bleed = 
        | Some of Margin
        | None
    
    [<RequireQualifiedAccess>]
    module Bleed = 
        open iText.Kernel.Pdf
        let isSome = function 
            | Bleed.Some _ -> true
            | Bleed.None -> false

        let isNone = function
            | Bleed.Some _ -> false
            | Bleed.None -> true

        let usualVaild = 
            Bleed.Some (Margin.createSimple (toInche 1.5))

        let toHSpaces = function
            | Bleed.Some m ->
                if m.Left = 0. && m.Right = 0. 
                then [0.]
                else 
                    assert (m.Left = m.Right)
                    [m.Left * 2.]
            | Bleed.None -> [0.]

        let toVSpaces  = function
            | Bleed.Some m ->
                if m.Top = 0. && m.Bottom = 0. 
                then [0.]
                else 
                    assert (m.Top = m.Bottom)
                    [m.Top * 2.]
            | Bleed.None -> [0.]

        let getTrimbox (page: PdfPage) (bleed: Bleed) =
            match bleed with 
            | Bleed.Some _ -> page.GetTrimBox()
            | Bleed.None -> PdfPage.getBBox page


    type Btw =
        {
            Variable : Variable
            Bleed: Bleed
        }

    type AI =
        {
            Bleed: Bleed
        }


    type Pdf = AI

    [<RequireQualifiedAccess>]
    type DocType =
        | AI of AI
        | Btw of Btw
        | Pdf of Pdf

    [<RequireQualifiedAccess>]
    module DocType =
        let simpleAI = {Bleed = Bleed.None} |> DocType.AI

        let simpleBtw item =
             {Variable = Variable.createSimple item; Bleed = Bleed.None}
             |> DocType.Btw

        let bleed (d:DocType) =
            match d with 
            | DocType.AI a -> a.Bleed 
            | DocType.Btw a -> a.Bleed
            | DocType.Pdf a -> a.Bleed
        
        let internal setBleed bleed (d:DocType) =
            match d with 
            | DocType.AI a -> { a with Bleed = Bleed.Some bleed } |> DocType.AI
            | DocType.Btw a -> { a with Bleed = Bleed.Some bleed } |> DocType.Btw
            | DocType.Pdf a -> { a with Bleed = Bleed.Some bleed } |> DocType.Pdf
        
    type BkTemplate =
        {
            Size: FsSize
            HSpaces: float list
            VSpaces: float list
            ColNums: int list
            RowNum: int
            Margin: Margin
        }

    type Template = BkTemplate option
    type AutoSetTemplate = AutoSet<Template>

    [<RequireQualifiedAccess>]
    module AutoSetTemplate =
        let shouldHaveValue (autoSetTemplate: AutoSetTemplate) =
            match autoSetTemplate.Settable with 
            | Settable.Auto -> true
            | Settable.Manual template ->
                match template with 
                | Some _ -> true
                | _ -> false

    [<RequireQualifiedAccess>]
    type CuttingLine =
        | Impress
        | DieCutting
        | Both
        | Slice

    [<RequireQualifiedAccess>]
    module CuttingLine =

        let toBleed cuttingLine whenNotHaveDieCutting = 
            match cuttingLine with
            | CuttingLine.DieCutting | CuttingLine.Both -> Bleed.usualVaild
            | CuttingLine.Impress | CuttingLine.Slice -> whenNotHaveDieCutting()

        let moldCost autoSetTemplate cost = function
            | CuttingLine.DieCutting
            | CuttingLine.Both 
            | CuttingLine.Impress -> 
                if AutoSetTemplate.shouldHaveValue autoSetTemplate then cost()
                else 0.
            | CuttingLine.Slice -> 0.

        let pressCost cost = function
            | CuttingLine.DieCutting
            | CuttingLine.Both 
            | CuttingLine.Impress -> cost()
            | CuttingLine.Slice -> 0.

        let sliceCost cost = function
            | CuttingLine.DieCutting
            | CuttingLine.Both -> 0.
            | CuttingLine.Impress 
            | CuttingLine.Slice -> cost()

    [<RequireQualifiedAccess>]
    type Coating =
        | None
        | Matte
        | Shiny

    [<RequireQualifiedAccess>]
    module Coating =
        let cost value = function 
            | Coating.None -> 0.
            | Coating.Matte | Coating.Shiny -> value()

    type Paper<'material> =
        {
            Brand: string
            Weight: int
            Material: 'material
            TonPrice: int
        }

    [<RequireQualifiedAccess>]
    module Paper =
        let priceEveryPieceByArea (size:FsSize) (p:Paper<_>) =
            let area = (toMM size.Height / 1000.) * (toMM size.Width / 1000.) / 1.<mm> / 1.<mm>
            area * float p.Weight * float p.TonPrice / float 1000000

        let priceEveryPaper (openSize:OpenSize) (p:Paper<_>) =
            match openSize.OpenKind with 
            | Positive ->
                0.787 * 1.092 * float p.Weight * float p.TonPrice / float 1000000
            | Big ->
                0.889 * 1.194 * float p.Weight * float p.TonPrice / float 1000000

    [<RequireQualifiedAccess>]
    type UnderTone =
        | White
        | Transparent

    type StickerPaper = Paper<UnderTone>
    type Coated = Paper<unit>

    type Hangtag =
        {
            Front: DocType
            Back: DocType option
            CuttingLine: CuttingLine
            AutoSetTemplate: AutoSetTemplate
            Paper: Coated
            Coating: Coating
        }

    [<RequireQualifiedAccess>]
    module Hangtag =
        let getBleed (h:Hangtag) = 
            let cuttingLine = h.CuttingLine
            CuttingLine.toBleed cuttingLine (fun _ ->
                let maxOfBleed b1 b2 =
                    match b1,b2 with 
                    | Bleed.Some b,Bleed.None -> Bleed.Some b
                    | Bleed.None, Bleed.Some b -> Bleed.Some b
                    | Bleed.None,Bleed.None -> Bleed.None
                    | Bleed.Some b1,Bleed.Some b2 -> Margin.max b1 b2 |> Bleed.Some

                let backBleed = h.Back |> Option.map DocType.bleed |> Option.defaultValue Bleed.None
                maxOfBleed (DocType.bleed h.Front) (backBleed)
            )

    type Sticker = 
        {
            DocType: DocType
            IsCarton: bool
            Multiple: int
            CuttingLine: CuttingLine
            Paper: StickerPaper
            AutoSetTemplate: AutoSetTemplate
            Coating: Coating
        }

    [<RequireQualifiedAccess>]
    module Sticker =
        let private dummyPaper =
            {
                Brand = "Untitled"
                Weight = 128
                Material = UnderTone.White
                TonPrice = 12000
            }

        let createInner (item: IItem) =
            {
                DocType = 
                    DocType.simpleBtw item
                IsCarton = false
                Multiple = 1
                CuttingLine = CuttingLine.Slice
                AutoSetTemplate = 
                    {
                        Settable = Settable.Manual None
                        RetrieveFrom = RetrieveFrom.Code
                    }
                Paper = dummyPaper
                Coating = Coating.None
            }

        let createCarton item =
            {
                Paper = dummyPaper
                DocType = DocType.simpleBtw item
                IsCarton = true
                Multiple = 2
                CuttingLine = CuttingLine.Slice
                AutoSetTemplate = 
                    {
                        Settable = Settable.Manual None
                        RetrieveFrom = RetrieveFrom.Code
                    }
                Coating = Coating.None
            }
    
        let getBleed (sticker: Sticker) =
            let cuttingLine = sticker.CuttingLine
            CuttingLine.toBleed cuttingLine (fun _ ->
                DocType.bleed sticker.DocType
            )

        let getNumber (items: IItem list) (sticker: Sticker) =
            let sumNum = items |> List.sumBy (fun item -> item.Number)
            let baseNum = 
                if sticker.IsCarton then 
                    let sumFrac = items |> List.sumBy(fun item -> 
                        let item = item :?> IFraction
                        item.Fraction)
                    sumNum / sumFrac
                else sumNum
            baseNum * sticker.Multiple

        let priceEveryPiece (pageSize: FsSize) (sticker: Sticker)=
            let paper = sticker.Paper
            Paper.priceEveryPieceByArea pageSize paper

    [<RequireQualifiedAccess>]
    type Commercial =
        | Sticker of Sticker
        | Hangtag of Hangtag

    [<RequireQualifiedAccess>]
    module Commercial =
        let getAutoSetTemplate = function 
            | Commercial.Hangtag ht -> ht.AutoSetTemplate
            | Commercial.Sticker sk -> sk.AutoSetTemplate

        let getCuttingLine = function 
            | Commercial.Hangtag ht -> ht.CuttingLine
            | Commercial.Sticker sk -> sk.CuttingLine

        let getCoating = function 
            | Commercial.Hangtag ht -> ht.Coating
            | Commercial.Sticker sk -> sk.Coating

        let createInnerSticker item =
            Sticker.createInner item |> Commercial.Sticker

        let createCartonSticker item =
            Sticker.createCarton item |> Commercial.Sticker

        let isHangtag = function
            | Commercial.Hangtag hangtag -> true
            | _ -> false

        let isSticker = function
            | Commercial.Sticker sticker -> true
            | _ -> false

        let getBleed = function
            | Commercial.Hangtag hangtag -> Hangtag.getBleed hangtag
            | Commercial.Sticker sticker -> Sticker.getBleed sticker

    type Thermal = 
        {
            DocType: DocType
            AutoSetTemplate: AutoSetTemplate
        }

    [<RequireQualifiedAccess>]
    module Thermal =
        let createSimple item =
            { 
                DocType = DocType.simpleBtw item
                AutoSetTemplate = 
                    {
                        Settable = Settable.Manual None
                        RetrieveFrom = RetrieveFrom.Code
                    }
            }

    type CopyPaper =
        {
            Pdf: Pdf
        }

    [<RequireQualifiedAccess>]
    type ProductKind =
        | Commercial of Commercial
        | Thermal of Thermal
        | CopyPaper of CopyPaper

    [<RequireQualifiedAccess>]
    module ProductKind =

        let createInnerSticker item =
            Commercial.createInnerSticker item |> ProductKind.Commercial

        let createCartonSticker item =
            Commercial.createCartonSticker item |> ProductKind.Commercial

        let getBleed (productKind: ProductKind) =
            match productKind with
            | ProductKind.Commercial cm -> Commercial.getBleed cm
            | ProductKind.Thermal _ | ProductKind.CopyPaper _ -> Bleed.None

        let createCopyPaper =
            {
                Pdf = {Bleed = Bleed.None}
            } |> ProductKind.CopyPaper

        let getNumber (items: IItem list) productKind =
            match productKind with 
            | ProductKind.Thermal _ | ProductKind.CopyPaper _ ->
                items |> List.sumBy (fun item -> item.Number)
            | ProductKind.Commercial cm -> 
                match cm with 
                | Commercial.Hangtag _ ->
                    items |> List.sumBy (fun item -> item.Number)
                | Commercial.Sticker sk ->
                    Sticker.getNumber items sk
    
    [<CLIMutable>]
    type Product =
        {
            Id: int
            Name: string
            Kind: ProductKind
            Orders: Order list
            UnitPrice: float
            Filter: bool
        }

    [<RequireQualifiedAccess>]
    module Product =
        open MBrace.FsPickler
        open Fake.IO
        let private binarySerializer = FsPickler.CreateBinarySerializer()
        
        let getFilter dir p =
            if p.Filter then 
                let path = dir </> sprintf "%s.pk" p.Name
                let bytes = File.readAsBytes path
                binarySerializer.UnPickle<IItem -> bool> bytes
            else fun _ -> true

        let createWith (f: Product -> Product) =
            {
                Id = 0 
                Name = "Untitled"
                Kind = ProductKind.createCopyPaper
                Orders = []
                UnitPrice = 0.1
                Filter = false
            } |> f

        let isSticker p =
            match p.Kind with 
            | ProductKind.Commercial cm when Commercial.isSticker cm -> true
            | _ -> false

        let isThermal p =
            match p.Kind with 
            | ProductKind.Thermal _ -> true
            | _ -> false

        let isHangtag p =
            match p.Kind with 
            | ProductKind.Commercial cm when Commercial.isHangtag cm -> true
            | _ -> false
    

    [<CLIMutable>]
    type Company =
        {
            Id: int
            Name: string
            OrderOffers: OrderOffer list
        }

    [<RequireQualifiedAccess>]
    module Company =
        let createWith transform =
            { Id = 0
              Name = "Untitled"
              OrderOffers = 
              [
                  { QQAccount = 0L
                    QQAccountName = "Untitled"
                    PhoneNumber = Unknown
                    Organization = {Address = Unknown} } |> OrderOffer
              ]
            } |> transform

        let createSimple name =
            createWith (fun company ->
                { company with Name = name}
            )

    [<CLIMutable>]
    type Tag =
        {
            Id: int
            Company: Company
            Name: string
            Products: Product list
            OrderOfferIndex: int
        }

    [<RequireQualifiedAccess>]
    module Tag = 
        let createWith transform =
            {
                Id = 0 
                Company = Company.createWith id
                Name = "Untitled"
                OrderOfferIndex = 0
                Products = []
            } |> transform


    [<AutoOpen>]
    module Extensions =

        [<RequireQualifiedAccess>]
        module OrderQuery =
            open LiteDB
            open LiteDB.FSharp.Extensions
            open System.Linq.Expressions

            let prediate prediate query =
                query 
                |> LiteQueryable.where prediate 
                |> LiteQueryable.toList

            let lastest (query:LiteQueryable<Order>) =
                query.ToEnumerable() 
                |> Seq.sortBy(fun o -> o.Date)
                |> Seq.head 
                |> List.singleton

            [<RequireQualifiedAccess>]
            type LiteQueryable =
       
                static member tryFirst (query: LiteQueryable<'a>) =
                    query.ToEnumerable() |> Seq.tryHead

                static member tryFind (exp: Expression<Func<'a,bool>>) (query: LiteQueryable<'a>) =
                    query |> LiteQueryable.where exp |> LiteQueryable.tryFirst