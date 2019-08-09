
namespace Shrimp.Pdf.Targets

[<RequireQualifiedAccess>]
module internal CopyPaper =
    open Shrimp.Entities.Types
    open System.IO
    open Shrimp.Pdf.Types
    open Fake.IO
    open Shrimp.Pdf.Colors
    open Types
    open Shrimp.Pdf.Manipulates
    open Shrimp.Pdf.Extensions
    open Shrimp.Pdf
    open FileSystem
    open Fake.IO.FileSystemOperators
    open Shrimp.Pdf.Targets.Actions
    [<AutoOpen>]
    module private Actions =

        [<AutoOpen>]
        module Manipulates =
            let appendSizeText : Flow<ReportInfo<_,_>> =
                Flow.FromState (fun state ->
                    let userState = state.UserState
                    let trInfo = userState.Size

                    let text = 
                        trInfo.GetText()

                    let size = trInfo |> TextRenderInfo.getFontSize
                    let appendedHeight = (trInfo |> TextRenderInfo.getHeight |> float) * Fonts.heightLerance
                    let margin = 
                        { Margin.createSimple (mm 6) with 
                            Top = appendedHeight }
            
                    manipulates
                        [
                            addExceedTextToTopLeft text size
                            applyMargin margin
                        ]
                )
            let appendSizeAndNumberText numberText : Flow<ReportInfo<_,_>> =
                Flow.FromState (fun state ->
                    let userState = state.UserState
                    let trInfo = userState.Size
                    let text = 
                        let sizeText =
                            trInfo.GetText() 
                        sprintf "%s  %s" sizeText numberText

                    let appendedHeight = (trInfo |> TextRenderInfo.getHeight |> float) * Fonts.heightLerance
                    let margin = 
                        { Margin.createSimple (mm 6) with 
                            Top = appendedHeight }
            
                    manipulates
                        [
                            fun arg ->
                                let page = arg.Page
                                let left,top,width = 
                                    let bbox = page.GetBBox()
                                    bbox.GetXF(),bbox.GetTopF(),bbox.GetWidthF()
                                let rect = Rectangle.create left top width appendedHeight
                                addTextToRectWithScale text (DeviceGray.BLACK) 0.8 rect arg
                            applyMargin margin
                        ]
                )
        
        [<RequireQualifiedAccess>]
        module Read =
            let size = Read.aiSize


    let plateOutput (p: Product) tarm targetName (copyPaper: CopyPaper) =
        let (Trader trader) = tarm.GlobalConfig.Trader
        let qqAccountName =trader.QQAccountName
        let ext = ".pdf"
        let src = TargetModel.file p ext tarm
        let cachedFile = TargetModel.fileInBinTarget targetName p ext tarm
        File.Copy (src,cachedFile,true)
        let pdf = TargetModel.fileInTarget p targetName ext tarm
        let states =
            File.Copy(cachedFile,pdf,true)
            [
                Read.size []
                manipulates [
                    removeTextOfFParsec (FParsecParsers.sizeLiteral)
                    trimToStrokeCyanOrMegenta
                    removePathWithStrokeColors Colors.AI.cuttingLine
                    blackOrWhite
                ]
                Flow.Rename (fun (state,s) -> 
                    let size = state.Size
                    let fsSize = size.GetText() |> FsSize.fromStringWithCM |> FsSize.normallize
                    let dir = Path.getDirectory s
                    let newFileName = 
                        [qqAccountName;todayFileName;sprintf "%.0f宽" (fsSize.Height + 1.);p.Name] |> String.concat " " |> sprintf "%s.pdf"
                    dir </> newFileName)
            ] |> run pdf

        let report =
            let pdf = cachedFile
            let orderName = p.Orders |> List.map (fun o -> o.Name) |> String.concat ("_")

            let text = 
                let number = p.Orders |> List.collect (fun o -> o.Items) |> List.sumBy (fun it -> it.Number)
                let addtionalNumber =
                    max (float number * 0.05) 350.
                sprintf "%d+%.0f=%.0f" number addtionalNumber (float number + addtionalNumber)
            let dest =

                let dir = TargetModel.tagDir tarm </> targetName </> "OutSource"
                ensureDir dir
                dir </> sprintf "%s_%s.pdf" p.Name orderName

            File.Copy(pdf,dest,true)
            [
                Read.copyPaperSize []
                manipulates [
                    removeTextOfFParsec (FParsecParsers.sizeLiteral)
                    trimToStrokeCyanOrMegenta
                    clipping (Margin.createSimple 0.)
                ]
                appendSizeAndNumberText text
            ] |> run dest |> ignore
        ()

    let makeSure (p: Product) tarm targetName (copyPaper: CopyPaper) =
        let ext = ".pdf"
        let src = TargetModel.file p ext tarm
        let cachedFile = TargetModel.fileInBinTarget targetName p ext tarm
        File.Copy (src,cachedFile,true)
        let pdf = TargetModel.fileInTarget p targetName ext tarm
        File.Copy(cachedFile,pdf,true)
        [
            Read.copyPaperSize []
            manipulates [
                removeTextOfFParsec (FParsecParsers.sizeLiteral)
                trimToStrokeCyanOrMegenta
                clipping (Margin.createSimple 0.)
            ]
            appendSizeText
        ] |> run pdf |> ignore
        
        TargetModel.filesInTargetOrder p targetName ext tarm
        |> List.iter (fun dest -> File.Copy (pdf,dest,true))
