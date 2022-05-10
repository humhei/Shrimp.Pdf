module Jpeg
open Fable.Core.JsInterop
open Fable.Import
open Fetch
open PdfJS
open Fable.Core.JsInterop
open Fable.Core
open Node.Api
open Node.Buffer
open Shrimp.Pdf.js.shared
open JpegImport
[<ImportAll("../node_modules/pdfjs-dist/lib/pdf.js")>]
let pdfjs: PdfJS.IExports = jsNative

let readColorValues(input: jpegInput) =
    let bytes = 
  // fs.readFileSync(@"C:\Users\Jia\Desktop\convert cmyk image to gray1.jpg")
        fs.readFileSync(input.rawFile)

    let jpeg = 
        let jpegOptions = input.jpegOptions
        let jpegOptions = 
            // createEmpty<JpegOptions>
            jsOptions<JpegOptions>(fun m ->
                m.colorTransform <- jpegOptions.colorTransform
                match jpegOptions.decodeTransform with 
                | [||] -> ()
                | _ -> m.decodeTransform <- jpegOptions.decodeTransform
            )
        jpegImage.Create(jpegOptions)
    
    jpeg.parse bytes

    let data = 
        jpeg.getData(jsOptions<JpegImageDataGettingOptions>(fun m ->
            m.width <- input.width
            m.height <- input.height
            m.isSourcePDF <- true
            m.forceRGB <- false
            ))
    let m = buffer.Buffer.from(object = data, byteOffset = 0)
    m
    