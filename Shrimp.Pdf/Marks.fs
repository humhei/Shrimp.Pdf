namespace Shrimp.Pdf

open iText.Kernel.Pdf
open iText.Kernel.Pdf.Xobject

[<RequireQualifiedAccess>]
module Marks =
    open Fake.IO
    let private mark (pageMark:PdfPage) =
        fun (writer:PdfDocument) ->
            lock writer (fun _ ->
                pageMark.CopyAsFormXObject(writer)
            )

    let private _cmyk =
        let file = Path.getFullName "Resources/Marks/CMYK.pdf"
        let doc = new PdfDocument(new PdfReader(file))
        doc.GetFirstPage()

    let cmyk writer =
        mark _cmyk writer
