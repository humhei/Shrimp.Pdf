module JpegImport 
open Fable.Core.JsInterop
open Fable.Import
open Fetch
open PdfJS
open Fable.Core.JsInterop
open Fable.Core
open Node.Api

type JpegImageDataGettingOptions =
    abstract member width: int                with get, set
    abstract member height: int               with get, set
    abstract member forceRGB: bool            with get, set
    abstract member isSourcePDF: bool         with get, set

type [<AllowNullLiteral>] JpegImage =
  abstract member parse: obj -> unit
  abstract member getData: JpegImageDataGettingOptions -> obj



type [<AllowNullLiteral>] JpegOptions =
  abstract member colorTransform: int with get, set
  abstract member decodeTransform: int []  with get, set

type [<AllowNullLiteral>] JpegImageStatic =
  [<EmitConstructor>] abstract Create: JpegOptions -> JpegImage

[<Import("JpegImage", "../node_modules/pdfjs-dist/lib/core/jpg.js")>]
let jpegImage: JpegImageStatic = jsNative