namespace 通用贴标
[<RequireQualifiedAccess>]
module Maps =
    open Shrimp.Pdf
    open Shrimp.Entities.Types
    open System.IO
    open Shrimp.Pdf.Targets
    open System
    type ArtZip =
        {
            Art: string
            Style: string
            Color: ColorEnum 
            ShortColor: string
            Brand: string
        }

    with static member Create(art,style,color,shortColor,brand) =
            {
                Art = art
                Style = style 
                Color = color |> ColorEnum.ofString
                ShortColor = shortColor 
                Brand = brand
            }

    let sizeMap =
            [
                36, 3.5, 5.
                37, 4.,  6.
                38, 5.,  7.
                39, 6.,  8.
                40, 6.5, 9.
            ] |> List.map SizeZip.create
    let artMap =             
        [
            "SH249","COMMUTER","White","WHT","Tru"
            "SH267","AKIKO","Blue","BLU","Tru"
            "SH286","AYURI","Black","BLK","Mosaic"
            "SH259","ANDIE","Beige","BGE","Mosaic"
            "SH313","FONDNESS","Mint","MNT","Tru"
            "SH317","CHARREL","Blue","BLU","Tru"
            "SH316","AMBERLY","Rose","RSE","Mosaic"
            "SH322","FRITZ","White","WHT","Tru"
            "SH327","EUNICE","White","WHT","Mosaic"
            "SH328","DOLBY","Navy","NVY","Tru"
            "SH337","AFRODILLE","Black","BLK","Tru"
            "SH332","HUMPHREY","Maroon","MRN","Mosaic"
        ] |> List.map ArtZip.Create

