namespace Atrous.Pdfargets

[<RequireQualifiedAccess>]
module Commercial =
    open Atrous.Pdf
    open Atrous.Pdf.Types
    open Atrous.Entities.Types
    open Atrous.Pdf.Targets.Types
    open Atrous.Pdf.Targets.Actions
    open Atrous.Pdf.Targets.FinanceWrapper
    open Atrous
        

    let makeSure (p: Product) tarm targetName = function
        | Commercial.Hangtag ht -> Logger.notImplemented()
        | Commercial.Sticker st -> Sticker.makeSure p tarm targetName st
    
    let paperCost pieceSize pieceNumber wastingPercent printingStrategy commercial = 
        if PrintingStrategy.isProofer printingStrategy then 0.
        else
            match commercial with 
            | Commercial.Hangtag ht -> Hangtag.paperCost ht
            | Commercial.Sticker st -> Sticker.paperCost pieceSize pieceNumber wastingPercent st

    let getMoldCost tarm (imposerTable: ImposerTable) (commercial: Commercial) =

        let autoSetTemplate = Commercial.getAutoSetTemplate commercial
        let cuttingLine = Commercial.getCuttingLine commercial
        CuttingLine.moldCost autoSetTemplate (fun _ -> TargetModel.getMoldCost imposerTable tarm) cuttingLine
    

    let getPressCost tarm pieceNumber (commercial: Commercial) =
        CuttingLine.pressCost (fun _ -> TargetModel.getPressCost pieceNumber tarm) (Commercial.getCuttingLine commercial)

    let getSliceCost tarm pieceNumber (commercial: Commercial) =
        CuttingLine.sliceCost (fun _ -> TargetModel.getSliceCost pieceNumber tarm) (Commercial.getCuttingLine commercial)

    let cuttingLineCost tarm (imposerTable: ImposerTable) pieceNumber printingStrategy (commercial: Commercial) =
        
        let normalCost =
            getSliceCost tarm pieceNumber commercial
            + getPressCost tarm pieceNumber commercial
            + getMoldCost tarm imposerTable commercial

        match printingStrategy with 
        | PrintingStrategy.Proofing proofing -> 
            if proofing.IsCutting 
            then proofing.Proofer.Cutter.Cost imposerTable pieceNumber
            else normalCost
        | _ -> normalCost

    let coatingCost tarm pieceSize pieceNumber printingStrategy (commercial: Commercial) =
            Coating.cost (fun _ -> 
                let normalCost = TargetModel.getCoatingCost pieceSize pieceNumber tarm
                match printingStrategy with 
                | PrintingStrategy.Proofing proofing -> 
                    if proofing.IsCoating 
                    then 
                        let proofer = proofing.Proofer
                        proofer.Coater pieceSize pieceNumber
                    else 
                        normalCost

                | _ -> normalCost
            )
                (Commercial.getCoating commercial)


    let getPrintingCost printingStrategy desiredColor tarm = function
        | Commercial.Hangtag ht -> Logger.notImplemented()
        | Commercial.Sticker st -> Sticker.getPrintingCost printingStrategy desiredColor tarm st

    let getCostBundle tarm desiredColor printingStrategy (commercial: Commercial) : CostBundleArg option =
        getPrintingCost printingStrategy desiredColor tarm commercial
        |> Option.map (fun getPrintingCost ->
            let wastingPercent = Commercial.getWastingPercent printingStrategy tarm commercial
            {
                PrintingCost = getPrintingCost
                PaperCost = 
                    fun pieceNumber pieceSize -> 
                        paperCost pieceSize pieceNumber wastingPercent printingStrategy commercial
                CuttingLineCost = 
                    fun pieceNumber imposerTable ->
                        cuttingLineCost tarm imposerTable pieceNumber printingStrategy commercial
                CoatingCost = 
                    fun pieceNumber pieceSize -> coatingCost tarm pieceSize pieceNumber printingStrategy commercial
            }
        )



    let printingStrategies (tarm: TargetModel) = function
        | Commercial.Sticker st -> Sticker.printingStrategies tarm st
        | Commercial.Hangtag ht -> Logger.notImplemented()

    module Output =

        [<RequireQualifiedAccess>]
        type UserState = 
            | Hangtag of Hangtag.Output.UserState
            | Sticker of Sticker.Output.UserState
        

        [<RequireQualifiedAccess>]
        module UserState =


            let btw =  function
                | UserState.Hangtag userState -> Logger.notImplemented()
                | UserState.Sticker userState -> userState.FinanceWrapper.Btw

            let printingStrategy = function
                | UserState.Hangtag userState -> Logger.notImplemented()
                | UserState.Sticker userState -> userState.FinanceWrapper.PrintingStrategy

            let financeWrapper = function
                | UserState.Hangtag userState -> Logger.notImplemented()
                | UserState.Sticker userState -> userState.FinanceWrapper
            
            let finance = function
                | UserState.Hangtag userState -> Logger.notImplemented()
                | UserState.Sticker userState -> userState.FinanceWrapper.Finance





        module private Unsafe =

            let asSticker = function
                | UserState.Hangtag userState -> Logger.unsafeToken()
                | UserState.Sticker userState ->  userState

            let ofSticker sticker = UserState.Sticker sticker
            
            [<RequireQualifiedAccess>]
            module WithFlows =
                let (|Sticker|Hangtag|) (commercial,flows) =
                    match commercial with 
                    | Commercial.Sticker st -> 
                        let newFlows = flows |> List.map (Flow.mapM ofSticker asSticker)
                        Sticker (st,newFlows)
                    | _ -> Hangtag
           
            [<RequireQualifiedAccess>]
            module WithUserState =
                let (|Sticker|Hangtag|) (commercial,userState) =
                    match commercial with 
                    | Commercial.Hangtag ht -> Hangtag
                    | Commercial.Sticker st -> Sticker (st,asSticker userState)
        
        open Unsafe

        let getSalePrice pieceNumber tpTables printingColor tarm = function
            | WithUserState.Sticker (st,userState) -> Sticker.Output.getSalePrice pieceNumber tpTables printingColor tarm userState st
            | WithUserState.Hangtag -> Logger.notImplemented()

        let getChunkPrice = function
            | WithUserState.Sticker (st,userState) ->
                Sticker.Output.getChunkPrice userState st
            | WithUserState.Hangtag -> Logger.notImplemented()


        [<RequireQualifiedAccess>]
        type Msg =
            | FinanceUserState of FinanceWrapper.UserState


        let update msg state =
            match msg with 
            | Msg.FinanceUserState financeUserState ->
                match state with 
                | UserState.Sticker sk -> UserState.Sticker { sk with FinanceWrapper = financeUserState }
                | UserState.Hangtag ht -> Logger.notImplemented()

        let fillFinance tarm printingStrategy (state: UserState) commercial =
            let btw = UserState.btw state
            let desiredColor = btw.DesiredColor
            let costBundle = getCostBundle tarm desiredColor printingStrategy commercial
            let financeUserState = { UserState.financeWrapper state with PrintingStrategy = printingStrategy }
            let chunkPrice = getChunkPrice(commercial,state) tarm


            costBundle 
            |> Option.map (fun costBundle ->
                let msg = 
                    match printingStrategy with 
                    | PrintingStrategy.Digital digital -> 
                        DigitalPrinting.fillFinance costBundle chunkPrice financeUserState digital 
                    | PrintingStrategy.Press press -> 
                        match press with 
                        | Press.WebPress -> Logger.notImplemented()
                        | Press.Flat flatPress ->
                            match flatPress with 
                            | FlatPress.Manual ->  
                                let pieceSize = Commercial.getManualPieceSize tarm commercial
                                ManualPress.fillFinance costBundle chunkPrice financeUserState pieceSize
                            | FlatPress.Offset offsetPress ->
                                OffsetPress.fillFinance costBundle chunkPrice financeUserState offsetPress
                    |> Msg.FinanceUserState
                update msg state
            )

        
        let bindFinanceWrapper flow =
            flow 
            |> Flow.bindM 
                UserState.financeWrapper
                (Msg.FinanceUserState >> update)

        let printingStrategyFlow =
            Flow.FromState (fun (state: FlowState<UserState>) ->
                let financeWrapper = UserState.financeWrapper state.UserState
                match financeWrapper.PrintingStrategy with
                | PrintingStrategy.Digital digital ->
                    [
                        DigitalPrinting.addPageInfo
                        DigitalPrinting.duplicatePagesWithPieceNumbers
                    ] 
                    |> List.map (fun flowFoo -> flowFoo digital)
                    |> Flow.Composite
                    |> bindFinanceWrapper
                | _ -> Logger.notImplemented()
            )

        let bestBenifit printingStrategies tarm (commercial: Commercial) =
            Flow.TransformUserState (fun (state: UserState) ->
                printingStrategies 
                |> List.choose (fun printingStrategy -> fillFinance tarm printingStrategy state commercial)
                |> Seq.maxBy (fun userState -> 
                    let finance = UserState.finance userState 
                    finance.Benifit)
                |> Logger.debugMode
            )

        let output product tarm (commercial: Commercial) =
            let printingStrategies = printingStrategies tarm commercial
            let bestBenifit = bestBenifit printingStrategies tarm commercial
            match commercial,[bestBenifit; printingStrategyFlow] with 
            | WithFlows.Sticker (st,[bestBenifit; printingStrategyFlow]) -> 
                let flows,pdf = Sticker.Output.output product tarm bestBenifit printingStrategyFlow st
                runCustom Sticker.Output.init pdf flows |> ignore
            | _ -> Logger.notImplemented()
