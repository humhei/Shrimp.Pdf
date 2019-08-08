namespace Atrous.Pdfargets.FinanceWrapper
open Atrous.Pdf.Targets
open Atrous.Pdf
open Atrous.Pdf.Colors
open Atrous.Pdf.Types
open Atrous.Entities.Types
open Atrous.Pdf.Extensions
open Atrous.Extensions
open Atrous.Utils
open Atrous
open Atrous.Pdf.Targets.Actions
module FinanceWrapper =
    open Atrous.Pdf.Targets.Types

    type UserState =
        { Btw: Btw.Output.UserState
          Finance: Finance
          PrintingStrategy: PrintingStrategy }

    [<RequireQualifiedAccess>]
    type Msg =
        | Btw of Btw.Output.UserState
        | Arg of (ExplicitImposingArgument -> ExplicitImposingArgument)
        | Finance of Finance

    let init =
        { Btw = Btw.Output.init
          Finance = Finance.dummy
          PrintingStrategy = PrintingStrategy.manual }

    let update msg state =
        match msg with 
        | Msg.Btw (newValue) -> {state with Btw = newValue}
        | Msg.Arg transformArg -> 
            { state with 
                Btw = 
                    let transform = Btw.Output.Msg.Arg (transformArg state.Btw.Arg) |> Btw.Output.update
                    transform state.Btw }
        | Msg.Finance finance ->
            { state with 
                Finance = finance }

    let view product tarm btw =
        let flow,pdf = (Btw.Output.view product tarm btw)
        let bindedFlow = Flow.bindM (fun (reportInfo : UserState) -> reportInfo.Btw) (Msg.Btw >> update) flow
        bindedFlow,pdf

open FinanceWrapper
open Atrous.Pdf.Targets.Types

[<RequireQualifiedAccess>]
module private CostBundle =
    open System.Numerics

    let baseCurriedCost pieceSize (costBundle: CostBundleArg) =
        fun pieceNumber ->
            { Printing = costBundle.PrintingCost pieceNumber pieceSize
              Paper = costBundle.PaperCost pieceNumber pieceSize
              Coating = costBundle.CoatingCost pieceNumber pieceSize }

    let printingCost pieceSize costBundle =
        fun financeUnits ->
            match financeUnits with 
            | FinanceUnits.Simple simples -> 
                let pieceNumber = FinanceUnits.pieceNumber financeUnits
                costBundle.PrintingCost pieceNumber pieceSize
            | FinanceUnits.Complex complexes ->
                complexes |> List.sumBy (fun complex ->
                    let pieceNumber = complex.PieceNumber
                    costBundle.PrintingCost pieceNumber pieceSize
                )

    let allCurriedCost pieceSize tb costBundle =
        fun financeUnits ->
            let pieceNumber = FinanceUnits.pieceNumber financeUnits
            let baseCost = baseCurriedCost pieceSize costBundle pieceNumber
            let cuttingLineCost = costBundle.CuttingLineCost pieceNumber tb
            {
                Paper = baseCost.Paper
                Coating = baseCost.Coating
                CuttingLine = cuttingLineCost
                Printing = printingCost pieceSize costBundle financeUnits
            }

[<RequireQualifiedAccess>]
module private PieceSize =
    open System.IO

    let fillFinance financeUnits costBundle chunkPrice tb state (pieceSize: FsSize) =
        let finance = 
            let pieceNumber = FinanceUnits.pieceNumber financeUnits

            let orderNumber = FinanceUnits.orderNumber financeUnits

            let utilization = 
                let multiply = ExplicitImposingArgument.imposeMatrixMulitiply state.Btw.Arg
                float orderNumber / float (multiply * pieceNumber)

            { state.Finance with 
                Cost = CostBundle.allCurriedCost pieceSize tb costBundle financeUnits
                PieceNumber = pieceNumber
                Utilization = utilization 
                Units = financeUnits 
                OrderNumber = orderNumber
                ChunkPrice = chunkPrice }

        state
        |> update (Msg.Finance finance)


[<RequireQualifiedAccess>]
module Printer =
    let fillFinance (costBundle: CostBundleArg) curriedSalePrice state (printer: Printer) =
        let printerSize = Printer.getRecommandedSize printer
        let pieceSize = PrinterSize.value printerSize
        let arg,tb = Printer.preImpose state.Btw.Arg printer
        let state = state |> update (Msg.Arg (fun _ -> arg))
        let financeUnits = 
            let tables = state.Btw.Tables
            Printer.getFinanceUnits arg tables printer

        PieceSize.fillFinance financeUnits costBundle curriedSalePrice tb state pieceSize
    

[<RequireQualifiedAccess>]
module DigitalPrinting =
    open Atrous.Pdf.Targets.Actions.Manipulates
    open Atrous.Pdf.Reuses

    let fillFinance costBundle chunkPrice state digital =
        let printer = DigitalPrinting.asPrinter digital
        Printer.fillFinance costBundle chunkPrice state printer


    let addPageInfo digital =
        Flow.FromState (fun state ->
            let printer = DigitalPrinting.asPrinter digital
            match printer with 
            | Printer.Home homePrinter ->
                let printerSize = PrinterSize.value homePrinter.Size
                let imposeTable = state.Tables |> List.head
                let userState = state.UserState
                let isRotated =
                    let wDis = 
                        let bw = printerSize.Width
                        let pw = ImposerTable.fullWidth imposeTable
                        bw - pw
                    let hDis =
                        let bh = printerSize.Height
                        let ph = ImposerTable.fullHeight imposeTable
                        bh - ph
                    wDis > hDis

                manipulate (fun arg -> 
                    let finance = userState.Finance
                    let financeUnit = finance.Units.[arg.PageNum - 1]
                    let pieceNumber = financeUnit |> FinanceUnit.pieceNumber |> int
                    let tableName = FinanceUnit.name financeUnit
                    let l = finance.Units.Length
                    let text =  sprintf "   %s    %d / %d      共%d张" tableName arg.PageNum l pieceNumber
                    let mp = 
                        if isRotated then addTextToTopRight text arg
                        else
                            addTextToLeftTop text arg
                    mp
                )
            | _ -> Logger.notImplemented()
        )

    let duplicatePagesWithPieceNumbers (_: DigitalPrinting) =
        Flow.FromState (fun (state: FlowState<UserState>) ->
            match state.UserState.Finance.Units with 
            | FinanceUnits.Simple units -> 
                units |> List.mapi (fun i unit ->
                    List.replicate unit.PieceNumber (i + 1)
                )
            | FinanceUnits.Complex _ -> Logger.invalidToken()
            |> List.concat
            |> duplicatePages
        )

[<RequireQualifiedAccess>]
module private TypedImposerDataRowss =
    let getFinanceUnits curriedCost chunkPrice pieceNumberTransform initImposingNum tableName rowss =
        rowss
        |> List.mapi (fun i rows ->
            let orderNumber = rows |> List.sumBy (fun row -> float row.DuplicatedNumber * row.Number)
            let pieceNumber = rows |> List.map (fun row -> row.Number) |> List.max |> pieceNumberTransform
            {
                OrderNumber = int orderNumber
                Utilization = orderNumber / float (pieceNumber * initImposingNum)
                PieceNumber = pieceNumber
                Name = 
                    if rowss.Length = 1 then 
                        sprintf "%s" tableName
                    elif rowss.Length > 1 then 
                        sprintf "%s-%d" tableName (i + 1)
                    else Logger.notImplemented()
                Cost = curriedCost pieceNumber
                ChunkPrice = chunkPrice
                TotalIndex = 0
                CurrentIndex = 0
            }
        )

[<RequireQualifiedAccess>]
module private TypedOrder = 
    let getFinanceUnits curriedCost chunkPrice pieceNumberTransform maxMultiply arg typeOrder =
        let initImposingNum = ExplicitImposingArgument.imposeMatrixMulitiply arg
        [1..maxMultiply] |> List.mapAsync (fun multiply ->
            let multipledNum = multiply * initImposingNum
            let typeOrder = 
                typeOrder 
                |> TypedOrder.shuffleToImposeNumber arg multipledNum
                |> TypedOrder.sortByNumber

            let table = typeOrder.TpTable

            let units = 
                table.Rows 
                |> List.splitBySum 
                    (fun row -> row.DuplicatedNumber) 
                    (fun row duplicatedNumber -> {row with DuplicatedNumber = duplicatedNumber})
                    initImposingNum
                |> TypedImposerDataRowss.getFinanceUnits 
                    curriedCost 
                    chunkPrice 
                    pieceNumberTransform 
                    initImposingNum  
                    table.Name
            units
        ) 
        |> Array.maxBy (fun units -> 
            units |> List.sumBy(fun unit -> unit.Benifit)
        )
        //|> Logger.debugMode

    
type FlatPressSize = private FlatPressSize of FsSize

[<RequireQualifiedAccess>]
module private FlatPressSize = 

    let getFinanceUnits (costBundle: CostBundleArg) chunkPrice pieceNumberTransform maxMultiply userState (FlatPressSize pieceSize) =
        let arg,tables = userState.Btw.Arg, userState.Btw.Tables
        let sampleDoc = createSampleDocument tables arg.DesiredSize
        let curriedCost = CostBundle.baseCurriedCost pieceSize costBundle 
        PdfDocument.useAsReader sampleDoc (fun reader ->
            let tpOrders = OSTypedOrder.init tables reader 
            tpOrders
            |> List.collect (TypedOrder.getFinanceUnits curriedCost chunkPrice pieceNumberTransform maxMultiply arg)
            |> FinanceUnits.Complex
            |> FinanceUnits.setIndexer
        )

    let fillFinance (costBundle: CostBundleArg) chunkPrice tb pieceNumberTransform state (FlatPressSize pieceSize) =
        let financeUnits = 
            //Logger.singleThread (fun _ ->
            getFinanceUnits costBundle chunkPrice pieceNumberTransform 10 state (FlatPressSize pieceSize)
            //)
        PieceSize.fillFinance financeUnits costBundle chunkPrice tb state pieceSize


[<RequireQualifiedAccess>]
module PlateOpenSize =
    open Fake.IO

    let fillFinance costBundle chunkPrice pieceNumberTransform state (plateOpenSize: PlateOpenSize) =
        let arg,tb = PlateOpenSize.preImpose state.Btw.Arg plateOpenSize
        let state = state |> update (Msg.Arg (fun _ -> arg))
        let pieceSize = PlateOpenSize.getOriginFsSize plateOpenSize
        FlatPressSize.fillFinance costBundle chunkPrice tb pieceNumberTransform state (FlatPressSize pieceSize)


    
[<RequireQualifiedAccess>]
module OffsetPress =
    let fillFinance costBundle chunkPrice state offsetPress = 
        let plateOpenSizes = OffsetPress.getPlateOpenSizes offsetPress
        plateOpenSizes 
        |> List.mapAsync (PlateOpenSize.fillFinance costBundle chunkPrice offsetPress.PieceNumberTransform state)
        //|> Logger.debugMode
        |> Seq.maxBy (fun financeUserState -> financeUserState.Finance.Benifit)
[<RequireQualifiedAccess>]
module ManualPress = 
    open Atrous.Utils

    let fillFinance curriedCost chunkPrice state (pieceSize: FsSize) =
        let arg,tb = ManualPress.preImpose state.Btw.Arg pieceSize
        let state = state |> update (Msg.Arg (fun _ -> arg))
        FlatPressSize.fillFinance curriedCost chunkPrice tb roundUpToInt state (FlatPressSize pieceSize)
