
namespace Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open iText.Kernel.Pdf
open System.Runtime.CompilerServices
open System.IO
open Shrimp.FSharp.Plus
open iText.Kernel.Geom
open Shrimp.Pdf
open Fake.IO
open Fake.IO.FileSystemOperators
type PdfUtils =
    static member NewTempEmptyPdf(?pageNumber: int, ?pageSize) =   
        let pageNumber = defaultArg pageNumber 1
        let pageSize = defaultArg pageSize FsSize.A4
        let path = Path.GetTempPath() </> "empty.pdf"

        if File.exists path then
            path
        else
            let doc = new PdfDocument(new PdfWriter(path))
            for _ = 0 to (pageNumber-1) do
                doc.AddNewPage(FsSize.toPageSize pageSize) |> ignore

            doc.Close()
            path
        

type PageModifingArguments<'userState> =
    { UserState: 'userState
      Page: PdfPage
      TotalNumberOfPages: int
      PageNum: int }

[<Extension>]
type PageModifingArgumentsExtensions() =
    [<Extension>]
    static member PageUserState(xs: PageModifingArguments<'T list>) = 
        if xs.UserState.Length = xs.TotalNumberOfPages then
            xs.UserState.[xs.PageNum - 1]
        else failwithf "Cannot get current userState because states length %d is not equal to totalNumberOfPages %d" xs.UserState.Length xs.TotalNumberOfPages 



