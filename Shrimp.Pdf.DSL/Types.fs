
namespace Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open System.Runtime.CompilerServices
open System.IO
open Shrimp.FSharp.Plus
open iText.Kernel.Geom
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open Fake.IO
open Fake.IO.FileSystemOperators

[<AutoOpen>]
module private _NewTempEmptyPdfCache =
    let newTempEmptyPdfCache = new System.Collections.Concurrent.ConcurrentDictionary<_, _>()


type PdfUtils =
    static member NewTempEmptyPdf(?pageNumber: int, ?pageSize, ?path, ?boundOptions: PdfCanvasAddRectangleArguments) =   
        let pageNumber = defaultArg pageNumber 1
        let pageSize = defaultArg pageSize FsSize.A4
        let path = defaultArg (path) (Path.GetTempPath() </> "empty.pdf")
        let key = {|PageNumber = pageNumber; PageSize = pageSize; Path = path|}
        newTempEmptyPdfCache.GetOrAdd(key, valueFactory = fun key ->
            File.delete path
            let doc = new PdfDocumentWithCachedResources(writer = path)
            for _ = 0 to (pageNumber-1) do
                let page = doc.AddNewPage(FsSize.toPageSize pageSize) 
                match boundOptions with 
                | None -> ()
                | Some boundOptions ->
                    let canvas = new PdfCanvas(page)
                    let pageBox = page.GetActualBox()
                    PdfCanvas.addRectOrEllipse pageBox (fun _ -> boundOptions) canvas
                    |> ignore

            doc.Close()
            path
        )


        

type PageModifingArguments<'userState> =
    { UserState: 'userState
      Page: PdfPage
      TotalNumberOfPages: int
      PageNum: int }
with 

    member x.MapUserState(fUserState) =
        { UserState = fUserState x.UserState
          Page = x.Page
          TotalNumberOfPages = x.TotalNumberOfPages
          PageNum = x.PageNum }

    member x.DisposeUserState() =   
        { UserState = () 
          Page = x.Page
          TotalNumberOfPages = x.TotalNumberOfPages
          PageNum = x.PageNum }

[<Extension>]
type PageModifingArgumentsExtensions() =
    [<Extension>]
    static member PageUserState(xs: PageModifingArguments<'T list>) = 
        if xs.UserState.Length = xs.TotalNumberOfPages then
            xs.UserState.[xs.PageNum - 1]
        else failwithf "Cannot get current userState because states length %d is not equal to totalNumberOfPages %d" xs.UserState.Length xs.TotalNumberOfPages 



