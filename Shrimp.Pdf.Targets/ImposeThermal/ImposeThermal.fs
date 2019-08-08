namespace Atrous.Pdfargets
[<RequireQualifiedAccess>]
module ImposeThermal =
    open iText.Kernel.Pdf
    open Atrous.Pdf
    open Atrous.Entities.Types
    open System
    open  Atrous.Pdf.Targets.ImposeThermal
    open Types
    open Atrous.Pdf.Targets
    open LiteDB.FSharp.Extensions
    open LiteDB.FSharp.Linq
    open Fake.IO.FileSystemOperators
    open iText.Kernel.Font

    let private common =
        let produceArg thBackgrondDir (parserResult: ParserResult) =
            let thBk,size = parserResult.ThermalBackground,parserResult.SizeText
            {
                ColNums = Seq.repeat [thBk.ColNum]
                RowNum = thBk.RowNum
                HSpaces = Seq.repeat [thBk.HSpace]
                VSpaces = Seq.repeat [thBk.VSpace]
                Margin = thBk.Margin
                Cropmark = None
                Bleed = true
                Background = 
                    let fileName = sprintf "%s.pdf" size
                    thBackgrondDir </> fileName |> Background.File |> Some
            } 

        let parse db srcDoc parser zips = 
            let size = 
                Parsers.size srcDoc parser zips
                |> fst
                |> fst
            let sizeString = size.GetText()
            let size = sizeString |> FsSize.fromString
            let thBk = 
                db 
                |> LiteRepository.query<ThermalBackground>
                |> LiteQueryable.find (Expr.prop(fun thBk -> thBk.Size = size))
            {
                ThermalBackground = thBk
                SizeText = sizeString.Replace("mm","")
                ThermalBackgroundDir = ""
            }

        let nonVarAction (nonVar: NonVar) = 
            failwith "Not implementd"

        let modifiers = []

        let suffixManipulates = 
            [
                blackOrWhite
            ]
        {
            Parse = parse
            Modifiers = modifiers
            TargetName = "热转印输出"
            NonVarAction = nonVarAction
            ProduceArg = produceArg
            ProduceSelectors = fun v -> v.ImposerSelectors
            SuffixManipulates = suffixManipulates
            Iac = Iac.saveToEpsWithZip
        } 

    type ImposeThermal<'parserResult> =
        {
            Shuffle: 'parserResult -> (list<TypedImposerDataRow * PdfPage>) -> (list<TypedImposerDataRow * PdfPage>)
            TableUnfolder: Selector list -> (TypedImposerDataTable * PdfPage list) -> (TypedImposerDataTable * PdfPage list) list
            Report: Report.Augument<'parserResult> -> unit
            ShufferedModifiers: PdfFont -> (Report.Unit -> unit) list
            ProduceReport: Product -> list<TypedImposerDataTable * ImposerTable<'parserResult>> -> Report.Unit list
            ProduceSize: 'parserResult -> FsSize option
        }
    let private imposeThermal =
        let produceSize (parserResult: ParserResult) =
            let thBk,size = parserResult.ThermalBackground,parserResult.SizeText
            thBk.Size |> Some

        let shuffle (parserResult: ParserResult) (zips:list<TypedImposerDataRow * PdfPage>) =
            let thBk,size = parserResult.ThermalBackground,parserResult.SizeText
            let num = thBk.RowNum * thBk.ColNum
            if zips.Length > num then failwithf "failed to make up page number %d to %d" zips.Length num
            let rec loop (zips:list<TypedImposerDataRow * PdfPage>) = 
                let accum = zips |> List.sumBy (fun (r,_) -> r.DuplicatedNumber)
                if accum = num
                then 
                    zips
                    |> List.collect (fun (r,page) ->
                        let dupNum = r.DuplicatedNumber
                        let r = { r with DuplicatedNumber = 1}
                        List.replicate dupNum (r,page)
                    )
                else 
                    let rows = zips |> List.map fst
                    let max = rows |> List.map (fun r -> r.Number) |> List.max
                    let indexOfMax = rows |> List.findIndex (fun r -> r.Number = max)
                    let zips = 
                        zips 
                        |> List.mapi (fun i (r,page) ->
                            if indexOfMax = i
                            then 
                                let orNum = float r.DuplicatedNumber * r.Number
                                let dupNum = r.DuplicatedNumber + 1
                                { r with Number = orNum / float dupNum; DuplicatedNumber = dupNum },page
                            else r,page
                        )
                    loop zips
            loop zips 

        let shufferedModifers font = 

            let addOrderInfo (reportUnit: Report.Unit) = 

                let text = 
                    sprintf "%s至少印%d张" reportUnit.Name reportUnit.PaperNumber
                let position = 
                    {
                        Box = PageBoxKind.CropBox
                        Orientation = (mm 35.,mm 4.2) |> Orientation.LeftBottom
                    }
                reportUnit.Canvas |> PdfCanvas.addText position text font reportUnit.Page

            let addDateInfo (reportUnit: Report.Unit) = 

                let text = 
                    sprintf "%s 第%d版共%d版" (DateTime.Today.ToString("yyyy年M月d日")) reportUnit.CurrentIndex reportUnit.TotalIndex
                let position = 
                    {
                        Box = PageBoxKind.CropBox
                        Orientation = (mm 20.,mm 4.2) |> Orientation.Bottom
                    }
                reportUnit.Canvas |> PdfCanvas.addText position text font reportUnit.Page

            [
                addOrderInfo
                addDateInfo
            ]

        let tableUnfolder (selectors: Selector list) (zip: TypedImposerDataTable * PdfPage list) = 
            let table,pages = zip
            List.zip table.Rows pages
            |> List.groupBy (fun (row,page) ->
                selectors |> List.map (fun k -> TypedImposerDataRow.getValueFromSelector k row)
            )
            |> List.map (fun (k,v) ->
                let rows,pages = v |> List.unzip
                {
                    Name = 
                        k |> List.map string |> List.append [table.Name] |> String.concat "_"
                    Rows = rows
                },pages
            )
        
        {
            Shuffle = shuffle
            TableUnfolder = tableUnfolder
            ProduceSize = produceSize
            ProduceReport = Report.produce
            ShufferedModifiers = shufferedModifers
            Report = Report.exec
        }

    let exec tarm =

        {
            Common = common
            Exec = fun arg ->
                let selectors = arg.Selectors
                let tpTable = arg.TpTable
                let pages = arg.Pages
                let common = arg.Common
                let srcDoc = arg.SrcDoc
                let parser = arg.PdfParser

                imposeThermal.TableUnfolder selectors (tpTable,pages)
                |> List.map (fun (table,pages) ->
                    table,pages,common.Parse tarm.Database srcDoc parser (table,pages)
                )
                |> List.map (fun (table,pages,parserResult) ->
                    let rows,pages = imposeThermal.Shuffle parserResult (List.zip table.Rows pages) |> List.unzip
                    { table with Rows = rows }, pages, parserResult
                )
                |> List.collect (fun (table,pages,parserResult) ->
                    let args = executable.ProduceArg parserResult  (selectors) table.Rows
                    let plates = 
                        assignPlatesWithPages args destDoc pages (imposeThermal.ProduceSize parserResult)
                        |> List.map (ImposerTable.asGeneric parserResult)
                    plates |> List.iter (ImposerTable.draw args)
                    plates |> List.map (fun pl -> table,pl)
                )
                |> fun zips ->
                    let reportUnits = 
                        zips |> imposeThermal.ProduceReport p
                    let font = Fonts.heiti()
                    reportUnits |> List.iter (fun ru ->
                        imposeThermal.ShufferedModifiers font |> List.iter (fun sm ->
                            sm ru
                        )
                    )
                    let unit,parserResult = zips.[0] |> snd |> fun imTable -> 
                        imTable.Rows.[0].Units.[0],imTable.ParserResult
                    {
                        Path = dest
                        Product = p
                        Thumbnail = unit
                        TableName = tpTable.Name
                        ReportUnits = reportUnits
                        ParserResult = parserResult 
                    } |> imposeThermal.Report
                    zips |> List.map snd,dest,destDoc
        }
        |> Target.execute tarm
