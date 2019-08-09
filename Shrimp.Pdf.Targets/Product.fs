namespace Shrimp.Pdfargets

[<RequireQualifiedAccess>]
module Product = 
    open Shrimp.Entities.Types
    open Types
    open Shrimp.Pdf
    open Shrimp.Pdf.Extensions
    open Shrimp

    let destExtension (p:Product) = ".pdf"

    let makeSure tarm (p:Product) =
        let targetName = "排版确认"
        match p.Kind with 
        | ProductKind.Commercial commercial  -> 
            Commercial.makeSure p tarm targetName commercial
            //Hangtag.makeSure p tarm targetName hangtag
        | ProductKind.Thermal thermal ->
            Logger.notImplemented()
            //Thermal.makeSure p tarm targetName thermal
        | ProductKind.CopyPaper cp ->
            Logger.notImplemented()
            //CopyPaper.makeSure p tarm targetName cp

    let proof (tarm: TargetModel) p = 
        match p.Kind with 
        | _ -> Logger.notImplemented()

    let commercialOutput (tarm: TargetModel) p = 
        match p.Kind with 
        | ProductKind.Commercial commercial ->
            Commercial.Output.output p tarm commercial
        | _ -> Logger.notImplemented()

    let imposeThermal (tarm: TargetModel) p =
        let targetName = "热转印输出"
        match p.Kind with 
        | ProductKind.Thermal thermal -> 
            Logger.notImplemented()
            //Thermal.imposeThermal p tarm targetName thermal
        | _ -> ()

    let hangtagOutput (tarm: TargetModel) p = 
        Logger.notImplemented()

    let copyPaperOutput (tarm: TargetModel) p = 
        let targetName = "拷贝纸输出"
        match p.Kind with 
        | ProductKind.CopyPaper copyPaper -> 
            Logger.notImplemented()
            //CopyPaper.plateOutput p tarm targetName copyPaper
        | _ -> ()


    let output (tarm: TargetModel) p = 
        [
            match p.Kind with 
            | ProductKind.Thermal thermal -> yield async { imposeThermal tarm p }
            | ProductKind.Commercial commercial -> yield async {commercialOutput tarm p }
            | ProductKind.CopyPaper _ -> yield async { copyPaperOutput tarm p }
        ]
        |> Async.Parallel 
        |> Async.RunSynchronously
        |> ignore

        