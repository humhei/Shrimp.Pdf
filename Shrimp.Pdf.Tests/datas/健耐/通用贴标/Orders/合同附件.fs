namespace 通用贴标.Orders
open 通用贴标
module Order =
    open Shrimp.Entities.Types
    open System.IO
    open ExcelProcess
    open MatrixParsers
    open ArrayParsers
    open SheetParsers
    open FParsec
    open Shrimp.Entities
    open Tests.Types.Input
    open System
    open LiteDB.FSharp
    open Shrimp.Entities.XLParser

    [<CLIMutable>]
    type Item =
        {
            Art : string
            Id : int
            Number : int
            EUR: int
            UK : float
            US: float
            Color : ColorEnum
            Barcode : string
            ShippingDate: DateTime
            Brand: string
            //CartonBarcode : string
            //Sex : Sex
            //Price : float
            //Fraction : int
            //FontColor: ColorEnum
            Style: string
            //CartonArt: string
        }

        interface IItem with 
            member this.Art = this.Art
            member this.Id = this.Id
            member this.Number = this.Number 

        interface ISize with 
            member this.EUR = this.EUR
            member this.US = this.US
            member this.UK = this.UK

        //interface IEUR with 
        //    member this.EUR = this.EUR

        //interface IStyle with 
        //    member this.Style = this.Style

        //interface IUK with 
        //    member this.UK = this.UK

        interface IColor with 
            member this.Color = this.Color

        interface IBarcode with
            member this.Barcode = this.Barcode

        //interface ICartonBarcode with
        //    member this.CartonBarcode = this.CartonBarcode

        //interface IColorCard with 
        //    member this.ColorCard = this.ColorCard

        interface IShippingDate with
            member this.ShippingDate = this.ShippingDate

        interface IBrand with
            member this.Brand = this.Brand

        interface IStyle with 
            member this.Style = this.Style

        //interface ISex with
        //    member this.Sex = this.Sex

        //interface IPrice with
        //    member this.Price = this.Price

        //interface IFraction with
        //    member this.Fraction = this.Fraction

    //type Item =
    //    inherit IItem
    //    inherit ISize
    //    inherit IColor
    //    inherit IBarcode
    //    inherit IShippingDate
    //    inherit IBrand
    //    inherit IStyle

    let xlsxName s = 
        sprintf "%s%s" s (__SOURCE_FILE__ |> Path.GetFileNameWithoutExtension)

    let itemGenerator (dm:DBModel) orderName sheet =
        FSharpBsonMapper.RegisterInheritedConverterType<IItem,Item>()
        let artsMap = Maps.artMap
        let sizeMap = Maps.sizeMap
        let pStyle = 
            artsMap |> Seq.map (fun pair -> pair.Style) |> Seq.map pstring |> choice |> (!^)
        let pColor = 
            Single.pColor
        let pSize =
            mxMany (Single.pUS)

        let items = 

            let styleColorParser = 
                r2
                    (c2 (!^= (pstring "货号")) (mxUntil (fun i -> i < 2) (!^= (pstring "颜色"))))
                    (c2 pStyle (mxUntil ((fun i -> i < 2))pColor))

            let usNumbersParser =
                r2
                    (c2 (!!(xPlaceholder 1)) pSize)
                    (
                        mxRowUntil (fun i -> i < 7) 
                            (c2 (!^=(pstring "总计")) (mxMany (!^ pint32)))
                    )

            let p = 
                sp3 
                    (styleColorParser >=> snd)
                    (usNumbersParser >=> id)
                    (Single.pTotal >=> id)
                    sheet
                |> fun (p1,p2,p3) ->
                    let (style,color) = p1 |> Seq.exactlyOne
                    let (_,uses),(_,numbers) = p2 |> Seq.exactlyOne
                    let isValidData = 
                        let (sum,carton) = p3 |> Seq.exactlyOne
                        let sum2 = numbers |> List.take uses.Length |> List.sum
                        assert (sum = sum2)
                    uses |> List.mapi(fun i us ->
                        let matchedZip = artsMap |> List.find (fun zip -> zip.Style = style) 
                        let item = 
                            { 
                                Id = i
                                Art = matchedZip.Art
                                Number = numbers.[i]
                                Style = style
                                Color = color 
                                US = us
                                UK = (SizeZip.getUKFromUS us sizeMap) 
                                EUR = (SizeZip.getEURFromUS us sizeMap)
                                ShippingDate = 示意图.shippingDateMap.[orderName]
                                Barcode =
                                    (
                                    sprintf 
                                        "%s%s%s" 
                                        matchedZip.Art 
                                        matchedZip.ShortColor 
                                        (sprintf "%d0" (int us))) 
                                Brand = matchedZip.Brand  
                            }
                        item
                    )
            p
        items |> List.map (fun it -> it :> IItem)