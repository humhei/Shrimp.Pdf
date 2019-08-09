namespace Shrimp.Pdfargets
open iText.Kernel.Colors
open Shrimp.Pdf.Colors

[<RequireQualifiedAccess>]
module Color =
    open Shrimp.Pdf.ColorBook
    let _pageNum =  Color._pantone PantoneColorEnum.``PANTONE 1935 C``
    let pageNum writer = Color.pantone PantoneColorEnum.``PANTONE 1935 C`` writer

[<RequireQualifiedAccess>]
module Colors =


    [<RequireQualifiedAccess>]
    module AI =
        let white : Color list = [DeviceGray.WHITE;DeviceCmyk.white]
        let cuttingLine: Color list = [DeviceCmyk.CYAN;DeviceCmyk.MAGENTA]
        let cuttingLineWithRegistion: Color list = Color._registion :> Color :: cuttingLine
        let exceptCuttingLine (cs: Color list) =
            Colors.except cuttingLine cs
        let exceptWhite (cs : Color list) =
            Colors.except white cs 

    [<RequireQualifiedAccess>]
    module Btw =
        let white : Color list = [DeviceGray.WHITE;DeviceCmyk.white]
        let dieCuttingLine: Color list = [DeviceRgb.BLUE]
        let impressLine: Color list = [DeviceRgb.magenta]
        let cuttingLine: Color list = dieCuttingLine @ impressLine
        let size : Color list = [DeviceRgb.RED]
        let exceptCuttingLine (cs: Color list) =
            Colors.except cuttingLine cs
        let exceptWhite (cs : Color list) =
            Colors.except white cs 

    let white : Color list = [DeviceRgb.WHITE;DeviceGray.WHITE;DeviceCmyk.white]

    let cuttingLine: Color list = Btw.cuttingLine @ AI.cuttingLine

    let exceptCuttingLine (cs: Color list) =
        Colors.except cuttingLine cs

    let exceptWhite (cs : Color list) =
        Colors.except white cs 
