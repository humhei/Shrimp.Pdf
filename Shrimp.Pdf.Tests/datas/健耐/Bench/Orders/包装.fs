namespace Bench.Orders

open Shrimp.Entities.Types

[<RequireQualifiedAccess>]
module 包装 = 
    open Shrimp.Pdf
    open System.IO
    open System
    open Tests.Types.Input
    open ExcelProcess
    open MatrixParsers
    open FParsec
    open Shrimp.Entities.XLParser
    open ExcelProcess.SheetParsers
    open Shrimp.Pdf.Utils

    let xlsxName s = 
        sprintf "%s%s" s (__SOURCE_FILE__ |> Path.GetFileNameWithoutExtension)

    type MetaTable =
        {
            ColorCard: ColorEnum
            ShippingDate: DateTime
            VendorCode: int
            Price: float
            Brand: string
        }

    type BarcodeRecord =
        {
            UK: float
            Art: string
            Color: ColorEnum
            CompositeArtKey: string
            Barcode: string
        }

    type Package =
        {
            MetaTable: MetaTable
            BarcodeTable: BarcodeRecord list
        }


    let findRecord (art,color,uk) package = 
        package.BarcodeTable |> List.filter(fun record ->
            (record.Art,record.Color,record.UK) = (art,color,uk)
        )
        |> List.exactlyOne


    let packageGenerator (dm:DBModel) orderName =
        let sheet = DBModel.getSheet xlsxName orderName dm
        let metaTable = 
            let pShippingDate = c2 (!^= (pstring "交货期")) (Single.pDate "yyMMdd")
            let pColorCard = c3 (!^= (pstring "颜色编码:")) (mxXPlaceHolder 1) (Single.pColorWith Language.Chinese)
            let pCode = c2 (!^= (pstring "供应商编码")) (!^pint32)
            let pPrice = c2 (!^= (pstring "零售价")) (!^pfloat)
            let pBrand = c2 (!^= (pstring "商标")) (mxOrigin)
            let sheetParser = 
                r6 pShippingDate pColorCard pCode pPrice (mxYPlaceHolder 1) pBrand >=> 
                    (
                        fun ((_,date),(_,_,color),(_,code),(_,price),_,(_,brand)) -> 
                            {
                                ShippingDate = date
                                ColorCard = color
                                VendorCode = code
                                Price = price
                                Brand = brand
                            }  
                    )

            runSheetParser sheet sheetParser |> Seq.exactlyOne

        let barcodeTable =
            let parser = 
                r2
                    ( c4 (mxPText "货号") (mxPText "颜色") (mxPText "货号编码") (mxPText "条形码编码") )
                    ( mxRowManySkipSpace 3
                        (c4 mxOrigin Single.pColor mxOrigin mxOrigin)
                    ) 
                >=> (snd >>
                        List.map (fun (art,color,artKey,barcode) ->
                            let uk = Int.returnLast123 artKey |> float
                            {
                                UK = uk
                                Art = art
                                Color = color
                                CompositeArtKey = artKey
                                Barcode = barcode
                            }    
                        )
                    )
            runSheetParser sheet parser |> Seq.exactlyOne

        {
            MetaTable = metaTable
            BarcodeTable = barcodeTable
        }
