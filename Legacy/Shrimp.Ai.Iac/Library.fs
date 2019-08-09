namespace Shrimp.Ai
[<RequireQualifiedAccess>]
module ExtendIac =
    open Illustrator
    let saveCopyToPdf src dest =
        let app = new ApplicationClass()
        let doc = app.Open(src)

        let options = 
            let op = new PDFSaveOptionsClass()
            op.PreserveEditability <- false
            op

        doc.SaveAs(dest,options)
        doc.Close()

    let saveCopyToPdfWithbleed (margin:float []) src dest =
        let app = new ApplicationClass()
        let doc = app.Open(src)
        let options = 
            let op = new PDFSaveOptionsClass()
            op.PreserveEditability <- false
            op.BleedOffsetRect <- Array.map box margin 
            op
        doc.SaveAs(dest,options)
        doc.Close()


