namespace Shrimp.Pdf

open Resources

[<RequireQualifiedAccess>]
module Marks = 
    let cmyk writer =
        PdfDocument.obtainMarkFromResources "CMYK" writer
         