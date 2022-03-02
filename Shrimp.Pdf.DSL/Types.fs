
namespace Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open iText.Kernel.Pdf
open System.Runtime.CompilerServices


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



