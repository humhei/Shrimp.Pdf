namespace Shrimp.Pdf

open iText.IO.Font
open iText.Kernel.Font
open System.Collections.Concurrent
open System.Collections.Concurrent
open System.IO





[<RequireQualifiedAccess>]
module RegisterableFonts =

    type private FontWeight =
        | Light = 0
        | Regular = 1
        | Medium = 2
        | Bold = 3
        | Heavy = 4

    let private alibabaPuHuiTi (weight: FontWeight) =
        let weightText =  weight.ToString()
        { PdfEncodings = PdfEncodings.IDENTITY_H
          FontFamily = "阿里巴巴普惠体-" + (weightText.Substring(0,1).ToUpper())
          Path = Path.Combine(resourceDirectory, @"Fonts/Alibaba-PuHuiTi-" + weightText + ".otf") }


    let AlibabaPuHuiTiLight = alibabaPuHuiTi FontWeight.Light

    let AlibabaPuHuiTiRegular = alibabaPuHuiTi FontWeight.Regular

    let AlibabaPuHuiTiMedium = alibabaPuHuiTi FontWeight.Medium

    let AlibabaPuHuiTiBold = alibabaPuHuiTi FontWeight.Bold

    let AlibabaPuHuiTiHeavy = alibabaPuHuiTi FontWeight.Heavy

