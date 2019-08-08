namespace Atrous.Pdf
[<RequireQualifiedAccess>]
module Iac =
    open Acrobat
    open System.Reflection
    open Fake.IO
    open System.IO
    open Scripts
    open AsyncUtils
    let private closeDoc jsObj = 
        let t = jsObj.GetType()
        t.InvokeMember(
            "closeDoc",
            BindingFlags.InvokeMethod ||| BindingFlags.Public ||| BindingFlags.Instance,
            null,
            jsObj,
            [|"true"|]
        )
        |> ignore

    let saveToEPS src dest =
        let tmp = src |> Path.changeExtension "iac.tmp"
        File.delete tmp
        File.Copy(src,tmp)
        let avDoc = new AcroAVDocClass()
        avDoc.Open(tmp,"") |> ignore
        let pdDoc = avDoc.GetPDDoc() :?> CAcroPDDoc
        let jsObj = pdDoc.GetJSObject()
        usingScript convertToOutlinesScript jsObj (fun t -> 
            t.InvokeMember(
                "saveAs",
                BindingFlags.InvokeMethod ||| BindingFlags.Public ||| BindingFlags.Instance,
                null,
                jsObj,
                [|dest; "com.adobe.acrobat.eps"|]
            )
            |> ignore
        )
        closeDoc jsObj

        waitUntil(fun _ -> 
            try
                File.delete tmp
                true
            with ex -> false
        )
