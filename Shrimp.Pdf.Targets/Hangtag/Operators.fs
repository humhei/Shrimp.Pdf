namespace Atrous.Pdfargets.Hangtag
[<AutoOpen>]
module internal Operators =
    open Atrous.Entities.Types
    open Atrous.Pdf.Types
    open Atrous.Pdf.Targets.Types
    open Fake.IO
    open Atrous.Pdf.Targets
    open System.IO

    let inline (!@) (f: Hangtag -> Manipulate<_> -> Manipulate<_>) h =
        let s = 
            fun (fl: ManipulateArgument<_> -> unit) ->
                fun arg ->
                    let fl = fl >> fun _ -> arg.FlowState
                    let t = f h fl
                    t arg |> ignore
        s

    let convertToPdf targetName (p: Product) tarm (hangtag: Hangtag) =
        let srcExtToPath ext = TargetModel.file p ext tarm
        let binExtToPath ext = TargetModel.fileInBinTarget targetName p ext tarm

        //let bleed = hangtag |> Hangtag.getBleed |> Option.map Margin.toArray
        match hangtag.Front,hangtag.Back with 
        | DocType.AI _,Some (DocType.AI _) | DocType.AI _,None -> 
            let ai = srcExtToPath ".ai"
            let pdf = srcExtToPath (sprintf "%s.pdf" targetName)
            let destTmp =
                pdf |> Path.changeExtension (sprintf "Ai%sTemp.pdf" targetName)
            failwith "COM interaction is not supported"
            //withNewer targetName ai pdf destTmp (ExtendIac.saveCopyToPdfWithbleed bleed)
            [pdf,[]]
        
        | DocType.AI _,Some (DocType.Btw btw) ->
            let ai = srcExtToPath "正面.ai"
            let pdf = binExtToPath "正面.pdf"
            let destTmp =
                pdf |> Path.changeExtension (sprintf "Ai%sTemp.pdf" targetName)
            failwith "COM interaction is not supported"
            //withNewer targetName ai pdf destTmp (ExtendIac.saveCopyToPdfWithbleed bleed)
            
            let btwPath = srcExtToPath "反面.btw"
            let backPdf,tables = Btw.printWithNewer p targetName tarm btw.Variable btwPath 
            [pdf,[];backPdf,tables]
        | DocType.Pdf _,Some (DocType.Btw btw) ->
            let pdf = srcExtToPath "正面.pdf"
            let pdf3 =  binExtToPath "正面.pdf"
            File.Copy(pdf,pdf3,true)
            let btwPath = srcExtToPath "反面.btw"
            let backPdf,tables = Btw.printWithNewer p targetName tarm btw.Variable btwPath 
            [pdf3,[];backPdf,tables]
        | DocType.Pdf _,None ->
            let pdf = srcExtToPath ".pdf"
            let pdf2 =  binExtToPath ".pdf"
            File.Copy(pdf,pdf2,true)
            [pdf2,[]]
        | _ -> failwith "GoGo"