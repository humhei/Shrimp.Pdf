namespace Atrous.Pdfargets
[<RequireQualifiedAccess>]
module Btw =
    open System
    open System.IO
    open Fake.IO
    open iText.Kernel.Pdf.Canvas.Parser.Data
    open iText.Kernel.Colors
    open Atrous.Pdf.Extensions

    open Atrous.Pdf
    open Atrous.Utils
    open Atrous.Pdf.Colors
    open Atrous.Pdf.Types
    open Atrous.Pdf.Reuses
    open Atrous.Pdf.PageReader
    open Atrous.Pdf.DocReader
    open Atrous.Pdf.Manipulates
    open Atrous.Pdf.Select
    open Atrous.Entities.Types
    open Atrous.Entities.Csv
    open Atrous.Pdf.Targets.Types
    open Atrous.Pdf.Targets.Actions
    open Reuses
    open Atrous.Extensions
    open Manipulates

    [<RequireQualifiedAccess>]
    module private ColorCard =
        let filters =
            [
                Text.filter(fun s -> s.Contains "PANTONE")
                Text.fill [DeviceGray.pantoneSignature]
            ]


    let private preColor bleed set =

        let processColor arg =
            let textInfos = 
                ManipulateArgument.readTexts ColorCard.filters arg 

            let writer = arg.Page.GetDocument()
            let replaceRgbColors = replaceColor [DeviceRgb.BLACK] DeviceGray.BLACK

            match textInfos with 
            | h::t ->
                textInfos |> List.collect (fun trInfo ->
                    let text = trInfo.GetText()

                    let removePantoneSigText =
                        removeText ((=) text)
                
                    let retrivePantoneColor =
                        let colors = 
                            let trBound = TextRenderInfo.getBound trInfo
                            let filters = 
                                [
                                    (fun prInfo ->
                                        let prBound = PathRenderInfo.getBound prInfo
                                        Rectangle.inbox trBound prBound
                                    )
                                    PathRenderInfo.hasSolidFill
                                ]
                            ManipulateArgument.readPaths filters arg 
                            |> List.map (fun prInfo -> prInfo.GetFillColor()) 
                            |> Seq.disctintByComparer (Color.comparer) |> List.ofSeq
                        let pantoneColor = Color.colorBook text writer 
                        replaceColor colors pantoneColor

                    [
                        removePantoneSigText arg
                        retrivePantoneColor arg
                        replaceRgbColors arg
                    ]

                ) |> List.last
            | [] -> arg.FlowState

        Flow.Composite [manipulate processColor; Read.desiredColor bleed set]

    let private print tpTables tarm btwPath pdfPath =
        let xlsxPath = TargetModel.randomXlsx tarm

        tpTables
        |> List.collect TypedImposerDataTable.toDataTable
        |> Seq.ofList
        |> Seq.xlsxWithProp false (fun s -> s.Replace("Material",""))
        |> Seq.writeXlsx xlsxPath

        let originDBPath: string = 
            let paths = [btwPath,pdfPath]
            Remoting.proxy.call<@fun bartender ->
                bartender.print xlsxPath paths "Adobe PDF For Bartender" 
            @> |> Async.RunSynchronously |> List.exactlyOne

        if originDBPath.Contains ".bin" && originDBPath.Contains ".xlsx" then File.Delete originDBPath

    let btwMonitor = new Object()

    let private printWithNewer (p:Product) tarm btw =
        let btwPath = TargetModel.file p.Name ".btw" tarm

        let tmp = TargetModel.fileInBin p.Name ".pdf" tarm

        let pdfPath = Path.GetTempFileName()
        
        let dataTables = Product.toTypedDataTables btw.Variable p

        let print() = 
            print dataTables tarm btwPath pdfPath 
            File.Copy(pdfPath,tmp,true)
        lock btwMonitor (fun _ ->
            if File.exists tmp then
                if File.GetLastWriteTime(tmp) > File.GetLastWriteTime btwPath then
                    File.Copy(tmp,pdfPath,true)
                else print()
            else 
                print()
        )

        pdfPath,dataTables

    module Makesure =
        type UserState =
            { DesiredColor: DesiredColor
              Tables: TypedImposerDataTable list
              DesiredSize: FsSize
              TrInfo: TextRenderInfo }
        
        [<RequireQualifiedAccess>]
        type Msg =
            | Table of TypedImposerDataTable
            | DesiredColor of DesiredColor
            | Size of TextRenderInfo * FsSize

        let update msg (state:UserState) =
            match msg with 
            | Msg.Table table -> {state with Tables = [table]}
            | Msg.DesiredColor desiredColor -> {state with DesiredColor = desiredColor}
            | Msg.Size (trInfo,fsSize) -> {state with DesiredSize = fsSize; TrInfo = trInfo}

        let init =
            { DesiredColor = DesiredColor.black 
              Tables = []
              TrInfo = null
              DesiredSize = FsSize.initUnit }

        let view
            (product: Product)
            root
            targetName 
            (tables: TypedImposerDataTable list) 
            (btw:Btw) =
                let bleed = btw.Bleed
                
                let impose : Flow<UserState> =
                    let produceArgs rows size =
                        let colNums,rowNum =
                            rows 
                            |> List.groupBy (fun row ->
                                btw.Variable.MakesureSelectors 
                                |> List.map (fun key -> TypedImposerDataRow.getCellFromKey key.Name row)
                            )
                            |> fun group -> 
                                group |> List.map (fun (k,rows) -> rows.Length),
                                group.Length

                        { ImposingArgument.dummy with 
                            ColNums = colNums
                            RowNum = rowNum
                            HSpaces = Bleed.toHSpaces bleed 
                            VSpaces = Bleed.toVSpaces bleed
                            Margin = Margin.imposingDest
                            UseBleed = Bleed.isSome bleed
                            DesiredSize = Some size } 

                    Flow.FromState (fun state ->
                        let userState = state.UserState
                        let table = userState.Tables |> List.exactlyOne
                        let arg = produceArgs table.Rows userState.DesiredSize
                        impose arg
                    )

                Flow.Composite 
                    [ 
                        Read.btwSize (Msg.Size >> update)
                        manipulate (Bleed.bleedOutToStrokeBlue bleed)
                        preColor bleed (Msg.DesiredColor >> update)
                        assignPagesToOrders product root targetName tables (Msg.Table >> update)
                        impose
                    ] 

        let simpleRun 
            p
            tarm
            targetName 
            (btw:Btw) =
                let root = TargetModel.tagDir tarm
                let pdf,tables = 
                    let tmp,tables = printWithNewer p tarm btw
                    let pdfPath = TargetModel.fileInBinTarget targetName p.Name ".pdf" tarm
                    File.Copy(tmp,pdfPath,true)
                    pdfPath,tables
                let addSizeText =
                    (fun (arg: ManipulateArgument<UserState>) ->                    
                        let size = arg.FlowState.UserState.TrInfo
                        addTextToLeftTop (size.GetText()) arg
                    ) 
                [
                    view p root targetName tables btw
                    manipulate addSizeText
                ]
                |> runCustom init pdf
                |> ignore

    module Output = 
        open Atrous.Utils
        
        type UserState =
            { Arg: ExplicitImposingArgument
              TrInfo: TextRenderInfo
              DesiredColor: DesiredColor
              Tables: TypedImposerDataTable list }
        
        let init =
            { Arg = ExplicitImposingArgument.dummy
              TrInfo = null
              DesiredColor = DesiredColor.black
              Tables = [] }

        [<RequireQualifiedAccess>]
        type Msg = 
            | Arg of ExplicitImposingArgument
            | Size of TextRenderInfo * FsSize
            | DesiredColor of DesiredColor
            | Tables of TypedImposerDataTable list

        let update msg (state: UserState) =
            match msg with 
            | Msg.Arg arg -> {state with Arg = arg}
            | Msg.DesiredColor dc -> {state with DesiredColor = dc}
            | Msg.Tables tbs -> {state with Tables = tbs}
            | Msg.Size (trInfo,fsSize) -> 
                {state with 
                    Arg = {state.Arg with DesiredSize = fsSize }
                    TrInfo = trInfo}

        let view product tarm (btw: Btw) =
            let bleed = ProductKind.getBleed product.Kind
            let pdf,tables = printWithNewer product tarm btw
            Flow.Composite
                [
                    Read.btwSize (Msg.Size >> update)
                    manipulate (Bleed.bleedOutToStrokeBlue bleed)
                    preColor bleed (Msg.DesiredColor >> update)
                    Flow.TransformUserState (fun state ->
                        state
                        |> update (Msg.Tables tables) 
                        |> update 
                            (Msg.Arg
                                { state.Arg with
                                    UseBleed = Bleed.isSome bleed
                                    HSpaces = Bleed.toHSpaces bleed
                                    VSpaces = Bleed.toVSpaces bleed
                                    Margin = Margin.createSimple (toInche 6.)}
                            )
                    )
                ],pdf

