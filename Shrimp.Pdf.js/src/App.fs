module App

open Fable.Core.JsInterop
open Fable.Import
open Fetch
open PdfJS
open Fable.Core.JsInterop
open Fable.Core
open Node.Api
open Fable.Import.Express
open Shrimp.Pdf.js.shared
open Thoth.Json
open Node.Fs
open Node.OS
open Node.Path



let app = 
  let app0 = express.Invoke()
  app0.``use``(express.json())

app.post
  ( U2.Case1 Shared.route, 
    fun (req:express.Request) (res:express.Response) _ ->
      
      let a: jpegInput = req.body :?> jpegInput
      let result = Jpeg.readColorValues a
      let tmpFile = 
        let dirBase = path.join(os.tmpdir(), "express_colors")
        let randomDir = fs.mkdtempSync(dirBase)
        path.join(randomDir, "colors.raw")
      
      fs.writeFileSync(tmpFile, result)
      res.send(tmpFile) |> box)
|> ignore

// Get PORT environment variable or use default
let port = Shared.port


app.listen(port, unbox (fun () ->
  printfn "Server started: http://localhost:%i/" port))
|> ignore
