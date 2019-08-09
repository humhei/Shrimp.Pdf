namespace Shrimp.Pdfargets
[<RequireQualifiedAccess>]        
module FontGen =
    open iText.Kernel.Font
    open iText.IO.Font
    open Shrimp.Pdf.Types
    module private PrivateWrapper = 
        let [<Literal>] Heiti = "AdobeHeitiStd-Regular"
        let [<Literal>] DaHei = "方正大黑简体"
        let createZHFont fontName =
            PdfFontFactory.CreateRegisteredFont(fontName,PdfEncodings.IDENTITY_H,true)

    open PrivateWrapper

    let heiti =
        {
            Name = Heiti
            Gen = createZHFont
        }

    let daHei =
        {
            Name = DaHei
            Gen = createZHFont
        }