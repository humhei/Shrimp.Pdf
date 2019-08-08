namespace RealClothing.Orders
module 挂牌 =
    open FParsec
    open Atrous.Entities
    open ExcelProcess
    open MatrixParsers
    open XLParser.FParsec
    open Atrous.Entities.XLParser
    open SheetParsers
    open Tests.Types.Input
    open System.IO
    open Atrous.Entities.Types
    open Atrous.Pdf.ColorBook
    open Atrous.Pdf

    //type BarcodeRow = 
    //    {
    //        Color: ColorEnum
    //        Barcode: string
    //        UK: float
    //    }
    type Hangtag =
        {
            BarcodeTable : Map<ColorEnum * float,string>
            Style : string
            Price : float
        }

    let xlsxName s = 
        sprintf "%s%s" s (__SOURCE_FILE__ |> Path.GetFileNameWithoutExtension)

    let pstyle  = 
        pstring "改成" >>. colon >>. spaces >>. pstring "REAL" .>>. (manyCharsTill anyChar eof) |> (!^)
        >=>
            fun (s1,s2) -> (s1 + s2).Trim()
            >> Seq.exactlyOne

    let pPrice = 
        pstring "改成R" >>.  spaces >>. pfloat |> (!^)
        >=>
            id >> Seq.exactlyOne

    let pBarcodeTable =
        r2 
            (c3 (mxPText "颜色") (mxPText "条形码") (mxPText "尺码"))
            (mxRowMany 
                ( c3 Single.pColor mxOrigin Single.pUK ))
        >=> 
            snd
            >> Seq.exactlyOne >> List.map (fun (color,barcode,uk) ->
                (color,uk),barcode
            ) >> Map
            
    let colorCardGenerator (sex: Sex) uk =
        match sex with 
        | Sex.Man -> 
            match uk with 
            | 6. -> PantoneColorEnum.``PANTONE 2597 C``,ColorEnum.White
            | 7. -> PantoneColorEnum.``PANTONE 803 C``,ColorEnum.Black
            | 8. -> PantoneColorEnum.``PANTONE 293 C``,ColorEnum.White
            | 9. -> PantoneColorEnum.``PANTONE 485 C``,ColorEnum.White
            | 10. -> PantoneColorEnum.``PANTONE 802 C``,ColorEnum.White
            | 11. -> PantoneColorEnum.``PANTONE 165 C``,ColorEnum.White
            | 12. -> PantoneColorEnum.``PANTONE 165 C``,ColorEnum.White
            | _ -> Logger.invalidToken()
        | Sex.Woman ->
            match uk with 
            | 3. -> PantoneColorEnum.``PANTONE 170 C``,ColorEnum.Black
            | 4. -> PantoneColorEnum.``PANTONE 7471 C``,ColorEnum.Black
            | 5. -> PantoneColorEnum.``PANTONE Medium Purple C``,ColorEnum.White
            | 6. -> PantoneColorEnum.``PANTONE 7486 C``,ColorEnum.Black
            | 7. -> PantoneColorEnum.``PANTONE Blue 0821 C``,ColorEnum.Black
            | 8. -> PantoneColorEnum.``PANTONE 106 C``,ColorEnum.Black
            | _ -> Logger.invalidToken()
        | _ -> Logger.notImplemented()

    let hangtagGenerator (dm : DBModel) orderName =
        let sheet = DBModel.getSheet xlsxName orderName dm
        sp3 pstyle pPrice pBarcodeTable sheet 
        |> fun (style,price,barcodeRows) ->
            {
                Style = style
                Price = price
                BarcodeTable = barcodeRows
            }
