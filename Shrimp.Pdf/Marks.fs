namespace Shrimp.Pdf

open Resources


type Mark =
    | CMYK = 0
    | LeftTauge = 1
    | LeftTauge_6 = 11
    | RightTauge = 2
    | RightTauge_6 = 12
    | VerticalRegistering = 3
    | HorizontalRegistering = 4
    | HorizontalRegistering_Top = 440
    | HorizontalRegistering_Bottom = 441
    | VerticalRegistering_Left = 5
    | VerticalRegistering_Right = 6
    | TopTauge = 7
    | BottomTauge = 8

[<RequireQualifiedAccess>]
module Mark =
    let obtainPdfFile (mark: Mark) =
        Resources.obtainMarkFile (mark.ToString())

    let addToDocument writer (mark: Mark) =
        PdfDocument.obtainMarkFromResources (mark.ToString()) writer



     