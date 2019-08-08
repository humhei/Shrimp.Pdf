namespace Atrous.Pdfargets

[<RequireQualifiedAccess>]
module Sticker =
    open Atrous.Pdf.Extensions
    open Atrous.Pdf
    open Atrous.Pdf.Colors
    open Atrous.Pdf.Types
    open Atrous.Pdf.Reuses
    open Atrous.Entities.Types
    open Atrous.Pdf.Targets.Types
    open Atrous.Pdf.Targets.Actions
    open Manipulates
    open Atrous.Pdf.Targets.FinanceWrapper
    open Atrous

    let makeSure (p: Product) tarm targetName (sticker: Sticker) =
        match sticker.DocType with 
        | DocType.Btw btw -> 
            Btw.Makesure.simpleRun p tarm targetName btw
        | _ -> Logger.notImplemented()
    
    let paperCost pieceSize pieceNumber wastingPercent (sticker: Sticker) =
        let priceEveryPiece = Sticker.priceEveryPiece pieceSize sticker
        priceEveryPiece * float pieceNumber * wastingPercent

    let getPrintingCost printingStrategy desiredColor tarm sticker =
        match printingStrategy with 
        | PrintingStrategy.Digital digital ->
            let wastingPercent = Sticker.getWastingPercent printingStrategy tarm sticker
            DigitalPrinting.printingCost wastingPercent desiredColor digital

        | PrintingStrategy.Press press ->
            match press with 
            | Press.WebPress -> Logger.notImplemented()
            | Press.Flat flatPress ->
                match flatPress with 
                | FlatPress.Manual _ ->
                    ManualPress.printingCost desiredColor tarm
                | FlatPress.Offset offSetPress -> OffsetPress.printingCost desiredColor offSetPress


    let printingStrategies (tarm: TargetModel) (sticker: Sticker) =
        let baseValues = 
            let presses = GlobalConfig.getPresses tarm.GlobalConfig |> List.map PrintingStrategy.ofOffset
            presses @ [PrintingStrategy.manual]
        let digitalPrintings = 
            let proofings = 
                GlobalConfig.getProofings sticker.Coating sticker.CuttingLine tarm.GlobalConfig
                |> List.map DigitalPrinting.Proofing
            match sticker.Paper.Material with 
            | UnderTone.White ->
                DigitalPrinting.HomePrinter tarm.GlobalConfig.HomePrinter :: proofings
            | UnderTone.Transparent ->
                proofings
            |> List.map PrintingStrategy.Digital
        baseValues @ digitalPrintings

    module Output =

        [<RequireQualifiedAccess>]
        type Msg =
            | FinanceWrapper of FinanceWrapper.UserState

        type UserState = 
            {
                FinanceWrapper : FinanceWrapper.UserState
                A4ChunkNumber: int
            }


        let getSalePrice pieceNumber tpTables printingColor tarm userState (sticker: Sticker) =
            let arg = 
                {
                    PieceNumber = pieceNumber
                    TpDataTables = tpTables
                    PrintingColor = printingColor
                    Paper = sticker.Paper
                    A4ChunkNumber = userState.A4ChunkNumber
                } |> SalePriceArg.Sticker
            TargetModel.getSalePrice arg tarm 

        let getChunkPrice userState (sticker: Sticker) =
            let btw = userState.FinanceWrapper.Btw
            let arg =
                {
                    TpDataTables = btw.Tables
                    DesiredColor = btw.DesiredColor
                    Paper = sticker.Paper
                    A4ChunkNumber = userState.A4ChunkNumber
                } |> ChunkPriceArg.Sticker
            TargetModel.getChunkPrice arg

        let init = 
            { FinanceWrapper = FinanceWrapper.init
              A4ChunkNumber = 0 }

        let update msg state = 
            match msg with 
            | Msg.FinanceWrapper financeWrapper -> { state with FinanceWrapper = financeWrapper }

        let bindFinanceWrapper flow =
            flow 
            |> Flow.bindM 
                (fun (reportInfo : UserState) -> reportInfo.FinanceWrapper) 
                (Msg.FinanceWrapper >> update)

        module Actions =
            let getA4ChunkNumber = 
                Flow.TransformUserState (fun (userState: UserState) ->
                    let arg = userState.FinanceWrapper.Btw.Arg
                    let newArg,_ = ManualPress.preImpose arg FsSize.A4
                    let matrix = ExplicitImposingArgument.imposeMatrixMulitiply newArg
                    { userState with A4ChunkNumber = matrix }
                )

        open Actions

        let output product tarm bestBenifit printingStrategyFlow (sticker: Sticker) =
            match sticker.DocType with 
            | DocType.Btw btw -> 
                let flow,pdf = (FinanceWrapper.view product tarm btw)
                let bindedFlow = bindFinanceWrapper flow
                [ 
                    bindedFlow
                    Actions.getA4ChunkNumber
                    bestBenifit
                    printingStrategyFlow
                ],pdf

            | _ -> Logger.notImplemented()

