namespace Shrimp.Pdf
#nowarn "0104"
open iText.IO.Font
open System.IO
open Resources
open Shrimp.FSharp.Plus

type RegisterableFont =
    { PdfEncodings: string
      File: FsFileInfo 
      FontFamily: string }
with 
    member x.Path = x.File.Path

    member x.LoggingText = x.FontFamily

module FontNames =
    module Query =
        let [<Literal>] ArialMT = "ArialMT"


module RegisterableFonts =

    //module AlibabaPuHuiTi = 
    //    type FontWeight =
    //        | Light = 0
    //        | DemiLight = 6
    //        | Regular = 1
    //        | Medium = 2
    //        | Bold = 3
    //        | Heavy = 4

    //    let private alibabaPuHuiTi (weight: FontWeight) =
    //        let weightText =  weight.ToString()
    //        { PdfEncodings = PdfEncodings.IDENTITY_H
    //          FontFamily = "阿里巴巴普惠体-" + (weightText.Substring(0,1).ToUpper())
    //          File = FsFileInfo.create (Path.Combine(resourceDirectory.Value, @"Fonts/Alibaba-PuHuiTi/Alibaba-PuHuiTi-" + weightText + ".otf")) }

    //    let light = alibabaPuHuiTi FontWeight.Light

    //    let regular = alibabaPuHuiTi FontWeight.Regular

    //    let medium = alibabaPuHuiTi FontWeight.Medium

    //    let bold = alibabaPuHuiTi FontWeight.Bold

    //    let heavy = alibabaPuHuiTi FontWeight.Heavy

    module CommonFonts = 
        let Heiti = 
            { PdfEncodings = PdfEncodings.IDENTITY_H
              FontFamily = "黑体"
              File = FsFileInfo.create (Path.Combine(resourceDirectory.Value, @"Fonts/黑体.ttf")) }

    module Adobe =

        let AdobeHeitiStd_Regular = 
            { PdfEncodings = PdfEncodings.IDENTITY_H
              FontFamily = "AdobeHeitiStd-Regular"
              File = FsFileInfo.create (Path.Combine(resourceDirectory.Value, @"Fonts/AdobeHeitiStd-Regular.otf")) }

    module ArialUnicode =
        
        type FontWeight =
            | Regular = 0
            | Bold = 1

        let arial_unicode (weight: FontWeight) =
            let weightText =  
                match weight with 
                | FontWeight.Regular -> "Arial Unicode MS"
                | FontWeight.Bold -> "Arial Unicode MS Bold"

            { PdfEncodings = PdfEncodings.IDENTITY_H
              FontFamily = weightText
              File = FsFileInfo.create (Path.Combine(resourceDirectory.Value, @"Fonts/Arial-Unicode/" + (weightText.ToString()) + ".ttf")) }


    module Arial =

        type FontWeight =
            | Regular = 0
            | Bold = 1
            | Italic = 2
            | BoldItalic = 3

        let arial (weight: FontWeight) =
            let fontName =  
                match weight with 
                | FontWeight.Regular -> "Arial"
                | FontWeight.Bold -> "Arialbd"
                | FontWeight.BoldItalic -> "Arialbi"
                | FontWeight.Italic -> "Ariali"

            let fontFamily =
                match weight with 
                | FontWeight.Regular -> "Arial"
                | FontWeight.Bold -> "Arial Bold"
                | FontWeight.BoldItalic -> "Arial Bold Italic"
                | FontWeight.Italic -> "Arial Italic"

            { PdfEncodings = PdfEncodings.CP1252
              FontFamily = fontFamily
              File = FsFileInfo.create (Path.Combine(resourceDirectory.Value, @"Fonts/Arial/" + (fontName.ToString()) + ".ttf")) }


    module YaHei =
        type FontWeight =
            | Light = 0
            | Regular = 1
            | Bold = 2

        let yaHei (weight: FontWeight) =
            let weightText = 
                match weight with 
                | FontWeight.Bold 
                | FontWeight.Light ->
                    sprintf $"Microsoft YaHei UI {weight.ToString()}"
                | FontWeight.Regular -> "Microsoft YaHei UI"

            let fontName =
                match weight with 
                | FontWeight.Bold -> "msyhbd.ttc"
                | FontWeight.Light -> "msyhl.ttc"
                | FontWeight.Regular -> "msyh.ttc"

            { PdfEncodings = PdfEncodings.IDENTITY_H
              FontFamily = weightText
              File = FsFileInfo.create (Path.Combine(resourceDirectory.Value, @"Fonts/微软雅黑/" + fontName)) }




    //module SiYuanHeiTi =
    //    type FontWeight =
    //        | Light = 0
    //        | DemiLight = 6
    //        | Regular = 1
    //        | Medium = 2
    //        | Bold = 3
    //        | Black = 5

    //    let siyuanHeiTi (weight: FontWeight) =
    //        let weightText =  
    //            match weight with 
    //            | FontWeight.Bold
    //            | FontWeight.Black -> "Noto Sans S Chinese " + (weight.ToString()) + " Bold"
    //            | _ -> "Noto Sans S Chinese " + (weight.ToString())

    //        { PdfEncodings = PdfEncodings.IDENTITY_H
    //          FontFamily = weightText
    //          File = FsFileInfo.create (Path.Combine(resourceDirectory.Value, @"Fonts/思源黑体/NotoSansHans-" + (weight.ToString()) + ".otf")) }

     


