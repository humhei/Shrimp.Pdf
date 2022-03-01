namespace Shrimp.Pdf
open Shrimp.FSharp.Plus

module internal rec ManipulateOrReuse =

    [<RequireQualifiedAccess>]
    type SplitOrIntegratedDocument =
        | SplitDocument of SplitDocument
        | IntegratedDocument of IntegratedDocument
    with 
        member x.Open() =
            match x with 
            | SplitOrIntegratedDocument.SplitDocument document -> document.Open()
            | SplitOrIntegratedDocument.IntegratedDocument document -> document.Open()
    
        member x.CloseAndDraft() =
            match x with 
            | SplitOrIntegratedDocument.SplitDocument document -> document.CloseAndDraft()
            | SplitOrIntegratedDocument.IntegratedDocument document -> document.CloseAndDraft()
    
        member x.TryCloseAndDisposeWriter_IfOpened() =
            match x with 
            | SplitOrIntegratedDocument.SplitDocument document -> document.TryCloseAndDisposeWriter_IfOpened()
            | SplitOrIntegratedDocument.IntegratedDocument document -> document.TryCloseAndDisposeWriter_IfOpened()
    

    type FlowModel<'userState> =
        { File: string
          Document: SplitOrIntegratedDocument 
          FlowName: FlowName option
          OperatedFlowNames: FlowName list
          UserState: 'userState }
    with 
        member flowModel.ToPublicFlowModel() =
            { PdfFile = PdfFile flowModel.File 
              UserState = flowModel.UserState }

        member flowModel.ToInternalFlowModel() =
            { File = flowModel.File 
              UserState = flowModel.UserState 
              FlowName = flowModel.FlowName
              OperatedFlowNames = flowModel.OperatedFlowNames }


    [<RequireQualifiedAccess>]
    module FlowModel =
        let mapM mapping (flowModel: FlowModel<_>) =
            { File = flowModel.File 
              UserState = mapping flowModel.UserState
              FlowName = flowModel.FlowName
              Document = flowModel.Document
              OperatedFlowNames = flowModel.OperatedFlowNames }

        let mapTo userState (flowModel: FlowModel<_>) =
            mapM (fun _ -> userState) flowModel

    type Logger with
        static member TryInfoWithFlowModel (flowNameIndex, flowModel: FlowModel<_>, f) =
            Logger.TryInfoWithFlowModel(
                flowNameIndex,
                flowModel.ToInternalFlowModel(),
                f = f
            )

    type  ManipulateOrReuse<'oldUserState, 'newUserState> =
        { Transform: FlowModel<'oldUserState> -> 'newUserState
          OperateDocument: bool }
    with 
        member x.MapTransform(mapping) =
            { Transform = mapping x.Transform 
              OperateDocument = x.OperateDocument }

    [<RequireQualifiedAccess>]
    type Flow<'oldUserState, 'newUserState> =
        | ManipulateOrReuse of ManipulateOrReuse<'oldUserState, 'newUserState>
        | Func of ('oldUserState -> Flow<'oldUserState, 'newUserState>)
        | Factory of (FlowModel<'oldUserState> -> Flow<'oldUserState, 'newUserState>)
        | NamedFlow of FlowName * Flow<'oldUserState, 'newUserState>
        | TupledFlow of ITupledFlow<'oldUserState, 'newUserState>
        | AppendedFlow of (SplitOrIntegratedDocument -> unit) * Flow<'oldUserState, 'newUserState>
    with 
        static member internal Append(appendix: (SplitOrIntegratedDocument -> unit), flow) =
            let rec loop flow =
                match flow with 
                | Flow.ManipulateOrReuse manipulate -> 
                    manipulate.MapTransform(fun transform ->
                        fun (flowModel: FlowModel<_>) ->
                            let userState = transform flowModel
                            appendix flowModel.Document
                            userState
                    )
                    |> Flow.ManipulateOrReuse

                | Flow.Func factory ->
                    fun userState ->
                        userState
                        |> factory
                        |> loop

                    |> Flow.Func

                | Flow.Factory factory ->
                    fun flowModel ->
                        flowModel
                        |> factory
                        |> loop
                    |> Flow.Factory

                | Flow.TupledFlow (tupledFlow) ->
                    tupledFlow.AppendLastManipulate(appendix)
                    |> Flow.TupledFlow

                | Flow.NamedFlow (flowName, flow) ->
                    (flowName, loop flow)
                    |> Flow.NamedFlow

                | Flow.AppendedFlow (appendix, flow) ->
                    (appendix, loop flow)
                    |> Flow.AppendedFlow

            loop flow

        static member internal SetParentFlowName(parentFlowName, flow) =
            let rec loop parentFlowName flow =
                match flow with 
                | Flow.ManipulateOrReuse transform -> 
                    transform
                    |> Flow.ManipulateOrReuse

                | Flow.Func factory ->
                    fun userState ->
                        userState
                        |> factory
                        |> loop parentFlowName

                    |> Flow.Func

                | Flow.Factory factory ->
                    fun flowModel ->
                        flowModel
                        |> factory
                        |> loop parentFlowName
                    |> Flow.Factory

                | Flow.TupledFlow (tupledFlow) ->
                    tupledFlow.SetParentFlowName(parentFlowName)
                    |> Flow.TupledFlow

                | Flow.NamedFlow (flowName, flow) ->
                    let flowName = 
                        match parentFlowName with 
                        | Some parentFlowName ->
                            match parentFlowName with 
                            | FlowName.New _ ->
                                flowName.SetParentFlowName(parentFlowName)
                            | FlowName.Disable _ ->
                                FlowName.Disable

                            | FlowName.Override _ -> FlowName.Disable
                        | None -> flowName

                    (flowName, loop (Some flowName) flow)
                    |> Flow.NamedFlow

                | Flow.AppendedFlow (appendix, flow) ->
                    (appendix, loop parentFlowName flow)
                    |> Flow.AppendedFlow

            loop parentFlowName flow

        static member internal Invoke(flowModel: FlowModel<'userState>) (flow) : FlowModel<'newUserState> =
            let rec loop (flowModel: FlowModel<'userState>) flow =
                match flow with 
                | Flow.ManipulateOrReuse manipulateOrReuse -> 
                    match manipulateOrReuse.OperateDocument with 
                    | false -> 
                        try 
                            let newUserState = (manipulateOrReuse.Transform flowModel)
                            flowModel.Document.TryCloseAndDisposeWriter_IfOpened()
                            FlowModel.mapTo newUserState flowModel
                        with ex ->
                            let ex = 
                                new System.Exception(
                                    sprintf "OperateDocument: false\nError when invoke flow %A to pdfFile %s\nInnerException:\n%A" (flowModel.FlowName, flow) flowModel.File ex, ex) 
                            raise ex

                    | true ->
                        flowModel.Document.Open()
                        try 
                            let newUserState = (manipulateOrReuse.Transform flowModel)
                            flowModel.Document.CloseAndDraft()
                            FlowModel.mapTo newUserState flowModel

                        with ex ->
                            try 
                                flowModel.Document.CloseAndDraft()
                            with _ -> ()
                            
                            let ex = 
                                new System.Exception(
                                    sprintf "Error when invoke flow %A to pdfFile %s\nInnerException:\n%A" (flowModel.FlowName, flow) flowModel.File ex, ex) 
                            
                            raise ex


                | Flow.TupledFlow flow -> flow.Invoke flowModel
                
                | Flow.Func (factory) ->
                    let flow = factory flowModel.UserState
                    loop flowModel flow

                | Flow.Factory (factory) ->
                    flowModel.Document.Open()
                    let flow = factory flowModel
                    loop flowModel flow

                | Flow.NamedFlow (flowName0, flow) ->
                    let index =
                        flowModel.OperatedFlowNames
                        |> List.filter(fun m -> m.RelativeDirectory = flowName0.RelativeDirectory)
                        |> List.length



                    let flowModel =
                        { flowModel with FlowName = Some flowName0; OperatedFlowNames = flowName0 :: flowModel.OperatedFlowNames }

                    Logger.TryInfoWithFlowModel(index, flowModel, fun _ ->
                        let newFlowModel = loop flowModel flow
                        flowModel
                            .ToInternalFlowModel()
                            .TryBackupFile(index)

                        newFlowModel
                    )

                    

                | Flow.AppendedFlow (appendix, flow) ->
                    loop flowModel (Flow<_, _>.Append(appendix, flow))


            loop flowModel flow


        static member Bind 
            (flow1: Flow<'originUserState, 'middleUserState>, flow2: Flow<'middleUserState, 'modifiedUserState>, fMonadState) =
            { Flow1 = flow1 
              Flow2 = flow2 
              FMonadStateBack = id
              FMonadState = fMonadState } :> ITupledFlow<_, _>
            |> Flow.TupledFlow
      
         
        static member MapStateBack (mapping, flow: Flow<_, _>) =
            let rec loop flow = 
                match flow with 
                | Flow.ManipulateOrReuse manipulateOrReuse -> 
                    manipulateOrReuse.MapTransform(fun tranform ->
                        fun flowModel ->
                            FlowModel.mapM mapping flowModel
                            |> tranform
                    )
                    |> Flow.ManipulateOrReuse

                | Flow.Func factory ->
                    fun userState ->
                        userState
                        |> mapping
                        |> factory
                        |> loop

                    |> Flow.Func

                | Flow.Factory factory ->
                    fun flowModel ->
                        FlowModel.mapM mapping flowModel
                        |> factory
                        |> loop
                    |> Flow.Factory

                | Flow.TupledFlow (tupledFlow) ->
                    tupledFlow.MapStateBack (mapping)
                    |> Flow.TupledFlow

                | Flow.NamedFlow (flowName, flow) ->
                    (flowName, loop flow)
                    |> Flow.NamedFlow

                | Flow.AppendedFlow (appendix, flow) ->
                    (appendix, loop flow)
                    |> Flow.AppendedFlow
            
            loop flow

        static member MapState (flow, mapping) =
            let rec loop flow = 
                match flow with 
                | Flow.ManipulateOrReuse manipulateOrReuse -> 
                    manipulateOrReuse.MapTransform(fun transform ->
                        fun flowModel ->
                            flowModel
                            |> transform
                            |> mapping
                    )
                    |> Flow.ManipulateOrReuse

                | Flow.Func factory ->
                    fun userState ->
                        userState
                        |> factory
                        |> loop

                    |> Flow.Func
                    
                | Flow.Factory factory ->
                    fun flowModel ->
                        flowModel
                        |> factory
                        |> loop
                    |> Flow.Factory

                | Flow.TupledFlow (tupledFlow) ->
                    tupledFlow.MapState (mapping)
                    |> Flow.TupledFlow

                | Flow.NamedFlow (flowName, flow) ->
                    (flowName, loop flow)
                    |> Flow.NamedFlow

                | Flow.AppendedFlow (appendix, flow) ->
                    (appendix, loop flow)
                    |> Flow.AppendedFlow
            

            loop flow

        static member Dummy() =
            Flow.ManipulateOrReuse(
                { Transform =
                    fun flowModel -> 
                        match flowModel.Document with 
                        | SplitOrIntegratedDocument.SplitDocument splitDocument ->
                            //flowModel.UserState
                            //splitDocument.Reader.CopyPagesTo(1, splitDocument.Reader.GetNumberOfPages(), splitDocument.Writer)
                            //|> ignore

                            flowModel.UserState 

                        | SplitOrIntegratedDocument.IntegratedDocument _ ->
                            flowModel.UserState

                  OperateDocument = false
                }

            )

        static member Batch (?flowName: FlowName) =
            fun flows ->
                let flow = 
                    Flow.Func(fun userState ->
                        match flows with 
                        | [] -> Flow<_, _>.MapState(Flow<_, _>.Dummy(), fun _ -> [])

                        | flows ->
                            let rec loop (flowAccum: Flow<'originUserState, 'newUserState list> option) (flows: Flow<'originUserState, 'newUserState> list) =
                                match flows with 
                                | [] -> flowAccum.Value

                                | flow :: flows ->
                                    match flowAccum with 
                                    | None -> 
                                        let flowAccum = Flow<_, _>.MapState(flow, List.singleton)
                                        loop (Some flowAccum) flows

                                    | Some flowAccum -> 

                                        let flowAccum =
                                            Flow<_, _>.MapState(flowAccum, (fun a -> userState, a))

                                        let flow = 
                                            Flow<_, _>.MapStateBack(fst, flow)
                    
                                        let flowAccum = 
                                            Flow<_, _>.Bind(flowAccum, flow, fun (a, b) -> snd a @ [b])

                                        loop (Some flowAccum) flows

                            loop None flows
                    )

                match flowName with 
                | None -> flow
                | Some flowName ->
                    Flow.NamedFlow (flowName, flow)



    type ITupledFlow<'oldUserState, 'newUserState> =
        abstract member Invoke: FlowModel<'oldUserState> -> FlowModel<'newUserState>
            
        abstract member MapState: ('newUserState -> 'a) -> ITupledFlow<'oldUserState, 'a>
    
        abstract member MapStateBack: ('a -> 'oldUserState) -> ITupledFlow<'a, 'newUserState>
    
        abstract member AppendLastManipulate: (SplitOrIntegratedDocument -> unit) -> ITupledFlow<'oldUserState, 'newUserState>
        
        abstract member SetParentFlowName: (FlowName option) -> ITupledFlow<'oldUserState, 'newUserState>

    type internal TupledFlow<'a, 'oldUserState, 'middleUserState, 'newUserState, 'finalUserState> =
        { Flow1: Flow<'oldUserState, 'middleUserState>
          Flow2: Flow<'middleUserState, 'newUserState>
          FMonadStateBack : 'a -> 'oldUserState
          FMonadState: 'middleUserState * 'newUserState -> 'finalUserState }
    with 
        interface ITupledFlow<'a, 'finalUserState> with 
            member x.Invoke(flowModel: FlowModel<'a>) =

                let flowModel = FlowModel.mapM x.FMonadStateBack flowModel

                let middleFlowModel = Flow<_, _>.Invoke flowModel x.Flow1

                let newFlowModel = Flow<_, _>.Invoke middleFlowModel x.Flow2
               
                let finalUserState = x.FMonadState (middleFlowModel.UserState, newFlowModel.UserState)

                FlowModel.mapTo finalUserState newFlowModel


            member x.MapState(mapping) =
                { Flow1 = x.Flow1 
                  Flow2 = x.Flow2 
                  FMonadStateBack = x.FMonadStateBack
                  FMonadState = 
                    fun (middleUserState, newUserState) -> 
                        x.FMonadState (middleUserState, newUserState)
                        |> mapping

                } :> ITupledFlow<_, _>

            member x.MapStateBack(mapping) =
                { Flow1 = x.Flow1 
                  Flow2 = x.Flow2 
                  FMonadStateBack = mapping >> x.FMonadStateBack
                  FMonadState = x.FMonadState
                } :> ITupledFlow<_, _>


            member x.AppendLastManipulate(appendix) =
                { Flow1 = x.Flow1 
                  Flow2 = Flow<_, _>.Append(appendix, x.Flow2)
                  FMonadStateBack = x.FMonadStateBack
                  FMonadState = x.FMonadState
                } :> ITupledFlow<_, _>

            member x.SetParentFlowName(parentFlowName) =
                { Flow1 = Flow<_, _>.SetParentFlowName(parentFlowName, x.Flow1)
                  Flow2 = Flow<_, _>.SetParentFlowName(parentFlowName, x.Flow2)
                  FMonadStateBack = x.FMonadStateBack
                  FMonadState = x.FMonadState
                } :> ITupledFlow<_, _>


open ManipulateOrReuse

type Reuse<'oldUserState, 'newUserState> internal 
    (flow: Flow<'oldUserState, 'newUserState>) =

    member internal x.SetParentFlowName(parentFlowName) =
        Flow<_, _>.SetParentFlowName (parentFlowName, flow)
        |> Reuse

    member x.Append(appendix) =
        let appendix = 
            fun document ->
                match document with
                | SplitOrIntegratedDocument.SplitDocument document -> appendix document
                | SplitOrIntegratedDocument.IntegratedDocument _ -> failwith "Invalid token"

        Flow.AppendedFlow(appendix, x.Flow)
        |> Reuse

    member internal x.Flow = flow

    member internal x.Invoke (flowModel: Shrimp.Pdf.InternalFlowModel<_>) (document: SplitDocument) = 
        flow 
        |> Flow<_, _>.Invoke(
            { File = flowModel.File
              Document = SplitOrIntegratedDocument.SplitDocument document
              UserState = flowModel.UserState
              FlowName = flowModel.FlowName
              OperatedFlowNames = flowModel.OperatedFlowNames }
        )
        |> fun flowModel ->
            flowModel.ToInternalFlowModel()
       
  

    static member (||>>) (reuse: Reuse<'oldUserState, 'newUserState>, mapping: 'newUserState -> 'a) =
        Flow<_, _>.MapState(reuse.Flow, mapping)
        |> Reuse

    static member (<<||) (mapping, reuse: Reuse<_, _>) =
        Flow<_, _>.MapStateBack(mapping, reuse.Flow)
        |> Reuse

    
    static member (<+>) (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Bind(reuse1.Flow, reuse2.Flow, snd)
        |> Reuse

    static member (<++>) (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Bind(reuse1.Flow, reuse2.Flow, id)
        |> Reuse

    static member (<.+>) (reuse1: Reuse<'originUserState,'middleUserState>, reuse2: Reuse<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Bind(reuse1.Flow, reuse2.Flow, fst)
        |> Reuse

     
    new (f: Shrimp.Pdf.FlowModel<'oldUserState> -> SplitDocument -> 'newUserState, operateDocument, ?flowName: FlowName) =
        let flow =
            Flow.ManipulateOrReuse (
                { Transform = 
                    fun flowModel ->
                        match flowModel.Document with 
                        | SplitOrIntegratedDocument.SplitDocument document ->
                            f 
                                (flowModel.ToPublicFlowModel())
                                document

                        | SplitOrIntegratedDocument.IntegratedDocument _ ->
                            failwith "Invalid token"
                  OperateDocument = operateDocument
                }

            )
        match flowName with 
        | None -> Reuse(flow)

        | Some flowName ->

            Reuse(
                Flow.NamedFlow(
                    flowName,
                    flow
                )
            )

    new (f, ?flowName) =
        Reuse(f = f, ?flowName = flowName, operateDocument = true)

    new (flowName, reuse: Reuse<_, _>) =
        Reuse(
            Flow.NamedFlow(
                flowName,
                reuse.Flow
            )
        )
    



[<RequireQualifiedAccess>]
module Reuse =
    let dummy() = 
        Reuse(
            f = (fun flowModel splitDocument -> 
                
                //splitDocument.Reader.CopyPagesTo(1, splitDocument.Reader.GetNumberOfPages(), splitDocument.Writer)
                //|> ignore

                flowModel.UserState 
            ),
            operateDocument = false
        )

    let rename (name: string) (paramters: list<string * string>) =
        fun (reuse: Reuse<_, _>) ->
            Reuse(
                Flow.NamedFlow(
                    FlowName.Override(name, paramters),
                    reuse.Flow
                )
            )


type Manipulate<'oldUserState, 'newUserState> internal (flow: Flow<'oldUserState, 'newUserState>) =
    
    member internal x.SetParentFlowName(parentFlowName) =
        Flow<_, _>.SetParentFlowName (parentFlowName, flow)
        |> Manipulate

    member internal x.Flow = flow

    member internal x.Invoke (flowModel: Shrimp.Pdf.InternalFlowModel<_>) (document: IntegratedDocument) = 
        flow 
        |> Flow<_, _>.Invoke(
            { File = flowModel.File
              Document = SplitOrIntegratedDocument.IntegratedDocument document
              UserState = flowModel.UserState
              FlowName = flowModel.FlowName
              OperatedFlowNames = flowModel.OperatedFlowNames }
        ) 
        |> fun flowModel ->
            flowModel.ToInternalFlowModel()


  

    static member (||>>) (manipulate: Manipulate<'oldUserState, 'newUserState>, mapping: 'newUserState -> 'a) =
        Flow<_, _>.MapState(manipulate.Flow, mapping)
        |> Manipulate

    static member (<<||) (mapping, manipulate: Manipulate<_, _>) =
        Flow<_, _>.MapStateBack(mapping, manipulate.Flow)
        |> Manipulate
    
    static member (<+>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Bind(manipulate1.Flow, manipulate2.Flow, snd)
        |> Manipulate

    static member (<++>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Bind(manipulate1.Flow, manipulate2.Flow, id)
        |> Manipulate

    static member (<.+>) (manipulate1: Manipulate<'originUserState,'middleUserState>, manipulate2: Manipulate<'middleUserState,'modifiedUserState>) =
        Flow<_, _>.Bind(manipulate1.Flow, manipulate2.Flow, fst)
        |> Manipulate


    new (f: Shrimp.Pdf.FlowModel<'oldUserState> -> IntegratedDocument -> 'newUserState, operateDocument, ?flowName: FlowName) =
        
        let flow = 
            Flow.ManipulateOrReuse (
                { Transform =
                    fun flowModel ->
                        match flowModel.Document with 
                        | SplitOrIntegratedDocument.SplitDocument document ->
                            failwith "Invalid token"

                        | SplitOrIntegratedDocument.IntegratedDocument document ->
                            f 
                                (flowModel.ToPublicFlowModel())
                                document

                  OperateDocument = operateDocument
                }
            )

        match flowName with 
        | Some flowName ->
            Manipulate(
                Flow.NamedFlow(
                    flowName,
                    flow
                )
            )

        | None -> Manipulate flow

    new (f, ?flowName) =
        Manipulate(f = f, ?flowName = flowName, operateDocument = true)

    new (flowName, manipulate: Manipulate<_, _>) =
        Manipulate(
            Flow.NamedFlow(
                flowName,
                manipulate.Flow
            )
        )
     



[<RequireQualifiedAccess>]
module Manipulate =
    let dummy() = 
        Manipulate(
            f = (fun flowModel doc -> 
                flowModel.UserState),
            operateDocument = false 
        )

    let rename (name: string) (paramters: list<string * string>) =
        fun (reuse: Manipulate<_, _>) ->
            Manipulate(
                Flow.NamedFlow(
                    FlowName.Override(name, paramters),
                    reuse.Flow
                )
            )

[<AutoOpen>]
module ManipulateOrReuseDSL =
    
    [<RequireQualifiedAccess>]
    type ReuseResponse<'originUserState, 'newUserState> =
        | UserState of 'newUserState
        | Reuse of Reuse<'originUserState, 'newUserState>

    type Reuse =
     
        static member Name (flowName) =
            fun (manipulate: Manipulate<_, _>) ->
                Manipulate(
                    Flow.NamedFlow(
                        flowName,
                        manipulate.Flow
                    )
                )

        static member Rename (name, ?parameters) =
            let flowName = 
                let parameters = defaultArg parameters []
                FlowName.Override(name, parameters)

            fun (reuse: Reuse<_, _>) ->
                Reuse(
                    Flow.NamedFlow(
                        flowName,
                        reuse.Flow
                    )
                )



        static member Batch (?flowName: FlowName) =
            fun (reuses: list<Reuse<_, _>>) ->
                reuses
                |> List.map (fun reuse ->
                    reuse.Flow
                )
                |> Flow<_, _>.Batch(?flowName = flowName)
                |> Reuse


        static member private Factory<'oldUserState, 'newUserState> (factory: Shrimp.Pdf.FlowModel<'oldUserState> -> SplitDocument -> ReuseResponse<'oldUserState, 'newUserState>) =
            Flow.Factory(fun flowModel ->
                let document =
                    match flowModel.Document with 
                    | SplitOrIntegratedDocument.SplitDocument document -> document
                    | SplitOrIntegratedDocument.IntegratedDocument document -> failwith "Invalid token"

                let response =
                    (factory 
                        (flowModel.ToPublicFlowModel())
                        document) 

                let reuse = 
                    match response with 
                    | ReuseResponse.UserState userState ->
                        Reuse.dummy() ||>> (fun _ -> userState)
                    | ReuseResponse.Reuse reuse ->
                        reuse

                reuse.Flow
            )
            |> Reuse

        static member Factory<'oldUserState, 'newUserState> (factory: Shrimp.Pdf.FlowModel<'oldUserState> -> SplitDocument -> Reuse<'oldUserState, 'newUserState>) =
            fun flowModel document ->
                factory flowModel document
                |> ReuseResponse.Reuse

            |> Reuse.Factory

        static member Func<'oldUserState, 'newUserState> (factory: 'oldUserState -> ReuseResponse<'oldUserState, 'newUserState>) =
            Flow.Func(fun userState ->
                let response = 
                    (factory userState) 

                let reuse = 
                    match response with 
                    | ReuseResponse.UserState userState ->
                        Reuse.dummy() ||>> (fun _ -> userState)
                    | ReuseResponse.Reuse reuse ->
                        reuse

                reuse.Flow
            )
            |> Reuse

        static member Func<'oldUserState, 'newUserState> (factory: 'oldUserState -> Reuse<'oldUserState, 'newUserState>) =
            Flow.Func(fun userState ->
                let reuse = 
                    (factory userState) 
                reuse.Flow
            )
            |> Reuse


          
        static member Append (appendix: SplitDocument -> unit) (reuse: Reuse<_, _>) =
            reuse.Append(appendix)



    [<RequireQualifiedAccess>]
    type ManipulateResponse<'originUserState, 'newUserState> =
        | UserState of 'newUserState
        | Manipulate of Manipulate<'originUserState, 'newUserState>

    type Manipulate =
    
        static member Name (flowName) =
            fun (manipulate: Manipulate<_, _>) ->
                Manipulate(
                    Flow.NamedFlow(
                        flowName,
                        manipulate.Flow
                    )
                )

        static member Rename (name, ?parameters) =
            let flowName = 
                let parameters = defaultArg parameters []
                FlowName.Override(name, parameters)

            fun (manipulate: Manipulate<_, _>) ->
                Manipulate(
                    Flow.NamedFlow(
                        flowName,
                        manipulate.Flow
                    )
                )


        static member Batch (?flowName: FlowName) =
           fun (manipulates: list<Manipulate<_, _>>) ->
               manipulates
               |> List.map (fun manipulate ->
                   manipulate.Flow
               )
               |> Flow<_, _>.Batch(?flowName = flowName)
               |> Manipulate


        static member private Factory<'oldUserState, 'newUserState> (factory: Shrimp.Pdf.FlowModel<'oldUserState> -> IntegratedDocument -> ManipulateResponse<'oldUserState, 'newUserState>) =
            Flow.Factory(fun flowModel ->
                let document =
                    match flowModel.Document with 
                    | SplitOrIntegratedDocument.SplitDocument document -> failwith "Invalid token"
                    | SplitOrIntegratedDocument.IntegratedDocument document -> document

                let response =
                    (factory 
                        (flowModel.ToPublicFlowModel())
                        document) 

                let manipulate = 
                    match response with 
                    | ManipulateResponse.UserState userState ->
                        Manipulate.dummy() ||>> (fun _ -> userState)
                    | ManipulateResponse.Manipulate manipulate ->
                        manipulate

                manipulate.Flow
            )
            |> Manipulate

        static member Factory<'oldUserState, 'newUserState> (factory: Shrimp.Pdf.FlowModel<'oldUserState> -> IntegratedDocument -> Manipulate<'oldUserState, 'newUserState>) =
            fun flowModel document ->
                factory flowModel document
                |> ManipulateResponse.Manipulate

            |> Manipulate.Factory

        static member Func<'oldUserState, 'newUserState> (factory: 'oldUserState -> ManipulateResponse<'oldUserState, 'newUserState>) =
            Flow.Func(fun userState ->
                let response = 
                    (factory userState) 

                let manipulate = 
                    match response with 
                    | ManipulateResponse.UserState userState ->
                        Manipulate.dummy() ||>> (fun _ -> userState)
                    | ManipulateResponse.Manipulate manipulate ->
                        manipulate

                manipulate.Flow
            )
            |> Manipulate


        static member Func<'oldUserState, 'newUserState> (factory: 'oldUserState -> Manipulate<'oldUserState, 'newUserState>) =
            Flow.Func(fun userState ->
                let manipulate = 
                    (factory userState) 
            
                manipulate.Flow
            )
            |> Manipulate

