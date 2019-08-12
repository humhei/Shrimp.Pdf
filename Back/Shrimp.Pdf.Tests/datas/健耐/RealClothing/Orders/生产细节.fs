namespace RealClothing.Orders
module Order =
    open Shrimp.Entities.Types
    open System.IO
    open ExcelProcess
    open MatrixParsers
    open SheetParsers
    open FParsec
    open Shrimp.Entities
    open XLParser
    open Tests.Types.Input
    open LiteDB.FSharp
    open Shrimp.Pdf.Colors

    [<CLIMutable>]
    type Item =
        {
            Art : string
            Id : int
            Number : int
            UK : float
            Color : ColorEnum
            Barcode : string
            CartonBarcode : string
            ColorCard : ColorCard
            Sex : Sex
            Price : float
            Fraction : int
            FontColor: ColorEnum
            Style: string
            CartonArt: string
        }

        interface IItem with 
            member this.Art = this.Art
            member this.Id = this.Id
            member this.Number = this.Number 

        //interface IEUR with 
        //    member this.EUR = this.EUR

        interface IStyle with 
            member this.Style = this.Style

        interface IUK with 
            member this.UK = this.UK

        interface IColor with 
            member this.Color = this.Color

        interface IBarcode with
            member this.Barcode = this.Barcode

        interface ICartonBarcode with
            member this.CartonBarcode = this.CartonBarcode

        interface IColorCard with 
            member this.ColorCard = this.ColorCard

        //val mutable ShippingDate : DateTime
        //interface IShippingDate with
        //    member this.ShippingDate = this.ShippingDate

        //val mutable Brand : string
        //interface IBrand with
        //    member this.Brand = this.Brand

        interface ISex with
            member this.Sex = this.Sex

        interface IPrice with
            member this.Price = this.Price

        interface IFraction with
            member this.Fraction = this.Fraction

    let xlsxName s = 
        sprintf "%s%s" s (__SOURCE_FILE__ |> Path.GetFileNameWithoutExtension)

    let itemGenerator (dm:DBModel) orderName sheet =
        FSharpBsonMapper.RegisterInheritedConverterType<IItem,Item>()
        let hangtag = 挂牌.hangtagGenerator dm orderName
        //let metaTable = package.MetaTable
        let items = 
            let pArt =
                r3
                    (c2 (mxPText "货号") (mxPText "款式"))
                    (mxPText "段")
                    (mxRowManySkipSpace 1 !^art)
                >=> fun (_,_,arts) -> arts 
                    >> Seq.exactlyOne

            let pColor = 
                r2
                    (c2 (mxPText "款式") (mxPText "颜色"))
                    (mxRowManySkipWith mxSkipOrigin 2 (c2 mxSkipOrigin Single.pColor))
                >=> snd >> Seq.exactlyOne >> List.map snd
            let pSizeAndFraction =
                r2
                    (c2 (mxPText "英码") (mxMany Single.pUK))
                    (mxRowManySkipOrigin 2 (c2 (mxSkipOrigin) (mxMany Single.pFraction)))
                >=> (fun ((_,sizes),fractions) ->
                    sizes,fractions |> List.map snd
                    )
                >> Seq.exactlyOne

            let pNumberAndCartonNumber =
                r2
                    (c2 (mxPText "数量") (mxPText "箱数"))
                    (mxRowManySkipSpace 2 (c2 (!^pint32) (!^pint32)))
                >=> snd >> Seq.exactlyOne

            let items = 
                sp4 pArt pColor pSizeAndFraction pNumberAndCartonNumber sheet
                |> fun (arts,colors,(uks,fractionss),p4) ->
                    let colorSumNumbers,cartonNumbers = p4 |> List.unzip
                    let l = colorSumNumbers.Length - 1
                    assert (List.sum colorSumNumbers.[0..l - 1] = colorSumNumbers.[l])
                    assert (List.sum cartonNumbers.[0..l - 1] = cartonNumbers.[l])
                    colors |> List.mapi (fun i color ->
                        let art = arts.[i]
                        let fractions = fractionss.[i]
                        let cartonNumber = cartonNumbers.[i]
                        let colorSumNumber = colorSumNumbers.[i]
                        assert (cartonNumber * (List.sum fractions) = colorSumNumber)
                        let sex = Sex.ofUKS (List.map float uks) 
                        uks |> List.mapi (fun j uk ->
                            let fraction = fractions.[j]
                            let number = fraction * cartonNumber
                            let colorCard,fontColor = 挂牌.colorCardGenerator sex uk
                            let price = hangtag.Price
                            let style = hangtag.Style
                            let barcode = hangtag.BarcodeTable.[color,uk]
                            let cartonDetail = 外箱正唛细节.cartonDetailMap.[orderName] |> List.find (fun z ->
                                z.Color = color
                            )
                            {
                                Art = art
                                Id = 0
                                Number = number
                                UK = uk
                                Color = color
                                Barcode = barcode
                                CartonBarcode = cartonDetail.CartonBarcode
                                ColorCard = ColorCard.Pantone colorCard 
                                Sex = sex
                                Price = price
                                Style = style
                                FontColor = fontColor
                                CartonArt = cartonDetail.CartonArt
                                Fraction = fraction
                            }
                        )
                    )
                    |> List.concat

            items |> List.mapi(fun i (item: Item) -> {item with Id = i + 1})

        items |> List.map (fun it -> it :> IItem)