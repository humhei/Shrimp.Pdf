namespace Shrimp.Pdf

open Resources


type Mark =
    | CMYK = 0
    | LeftTauge = 1
    | RightTauge = 2
    | VerticalRegistering = 3
    | HorizontalRegistering = 4


[<RequireQualifiedAccess>]
module Mark =
    let obtainPdfFile (mark: Mark) =
        Resources.obtainMarkFile (mark.ToString())

    let addToDocument writer (mark: Mark) =
        PdfDocument.obtainMarkFromResources (mark.ToString()) writer



     