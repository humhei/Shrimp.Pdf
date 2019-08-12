namespace Bench.Orders
module Order =
    open Shrimp.Pdf
    open Shrimp.Entities.Types
    open System.IO
    open ExcelProcess
    open MatrixParsers
    open SheetParsers
    open FParsec
    open Shrimp.Entities
    open XLParser
    open Tests.Types.Input
    open Bench.Orders
    open System 
    open LiteDB.FSharp
    open Tests.Types
    type Item =
        val mutable Art : string
        val mutable Id : int
        val mutable Number : int

        interface IItem with 
            member this.Art = this.Art
            member this.Id = this.Id
            member this.Number = this.Number 

        val mutable EUR : int
        interface IEUR with 
            member this.EUR = this.EUR

        val mutable UK : float
        interface IUK with 
            member this.UK = this.UK

        val mutable Color : ColorEnum
        interface IColor with 
            member this.Color = this.Color

        val mutable Barcode : string
        interface IBarcode with
            member this.Barcode = this.Barcode

        val mutable ColorCard : ColorEnum
        interface IColorCard with 
            member this.ColorCard = this.ColorCard

        val mutable ShippingDate : DateTime
        interface IShippingDate with
            member this.ShippingDate = this.ShippingDate

        val mutable Brand : string
        interface IBrand with
            member this.Brand = this.Brand

        val mutable Sex : Sex
        interface ISex with
            member this.Sex = this.Sex

        val mutable Price : float
        interface IPrice with
            member this.Price = this.Price

        val mutable VendorCode : int
        interface IVendorCode with
            member this.VendorCode = this.VendorCode

        val mutable Fraction : int
        interface IFraction with
            member this.Fraction = this.Fraction


        val mutable Image : string
        interface IImage with 
            member this.Image = this.Image

        val mutable CompositeArtKey : string

        new (art,id,number,eur,uk,color,barcode,colorCard,shippingDate,brand,sex,price,vendorCode,fraction,image,compositeArtKey) =
            { Art = art; Id = id; Number = number; EUR = eur; UK = uk; Color = color; Barcode = barcode 
              ColorCard = colorCard; ShippingDate = shippingDate; Brand = brand; Sex = sex; Price = price
              VendorCode = vendorCode; Fraction = fraction; Image = image; CompositeArtKey = compositeArtKey }


    let xlsxName s = 
        sprintf "%s%s" s (__SOURCE_FILE__ |> Path.GetFileNameWithoutExtension)

    let itemGenerator (dm:DBModel) orderName sheet =
        FSharpBsonMapper.RegisterInheritedConverterType<IItem,Item>()
        let imageMap =
            Map []
        let package = 包装.packageGenerator dm orderName
        let metaTable = package.MetaTable
        let items = 
            let pArt = 
                r2
                    (c2 (mxPText "货号") (mxPText "颜色"))
                    (mxRowManySkipSpace 4 mxOrigin)
                >=> snd
            let pSize =
                mxMany
                    (!^^ 
                        ((pfloat .>> spaces .>> FParsec.parentheses) .>>. (pint32 .>> FParsec.parentheses))
                        (fun (uk,eur) ->
                            Shoes.isUK uk && Shoes.isEur eur
                        ))
                >=> id

            let pColorNumbers =
                c2 !^ FParsec.color (mxMany !^ pint32)
                >=> id
            let items = 
                sp3 pArt pSize pColorNumbers sheet
                |> fun (p1,p2,p3) ->
                    let art = p1 |> Seq.exactlyOne |> Seq.exactlyOne
                    let uk_eurs = p2 |> Seq.exactlyOne
                    let sex = Sex.fromEur (uk_eurs |> List.map snd)
                    p3 
                    |> Seq.map (fun (color,numbers) ->
                        numbers |> List.mapi (fun i number ->
                            let uk,eur = uk_eurs.[i]
                            let findedRecord = 包装.findRecord (art,color,uk) package

                            let barcode = findedRecord.Barcode
                            let brand = metaTable.Brand
                            let colorCard = metaTable.ColorCard

                            new Item (
                                art = art,
                                barcode = barcode,
                                brand= brand,
                                colorCard = colorCard,
                                eur = eur,
                                id = 0,
                                number = number,
                                sex = sex,
                                uk = uk,
                                shippingDate = metaTable.ShippingDate,
                                color = color,
                                price = metaTable.Price,
                                vendorCode = metaTable.VendorCode,
                                fraction = 0,
                                image = 
                                    match Map.tryFind art imageMap with 
                                    | Some image -> image
                                    | None -> defaultShoeImage
                                    ,
                                compositeArtKey = findedRecord.CompositeArtKey
                            )
                        )
                    )
                    |> Seq.concat
                    |> List.ofSeq 
            items |> List.iteri(fun i item -> item.Id <- i)
            items

        items |> List.map (fun it -> it :> IItem)