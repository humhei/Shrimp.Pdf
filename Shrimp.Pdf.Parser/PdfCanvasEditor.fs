namespace Atrous.Pdfarser
module Core =
    open iText.Kernel.Pdf.Canvas.Parser
    open iText.Kernel.Pdf.Canvas.Parser.Listener
    open System.Collections.Generic
    open iText.Kernel.Pdf
    open iText.Kernel.Pdf.Canvas
    open iText.Kernel.Pdf.Canvas.Parser.Data
    open Atrous.Pdf.Extensions
    type DynamicGSPdfCanvas(stream,resources,document) =
        inherit PdfCanvas(stream,resources,document)

        member this.SetGS(gs: CanvasGraphicsState) =
            this.gsStack.Clear()
            this.currentGs <- gs

        member this.FixState (operands:seq<PdfObject>) (f: CanvasGraphicsState -> list<PdfCanvas -> PdfCanvas>) =
            PdfCanvas.useCanvas this (fun _ ->
                let gs = this.GetGraphicsState()
                let fs = f gs
                fs |> List.map (fun f -> 
                    f this) |> ignore
                this.WriteRange(operands) |> ignore
            )

        member this.WriteRange(operands: seq<PdfObject>) =
            let l = operands |> Seq.length
            operands 
            |> Seq.iteri (fun i (operand: PdfObject) ->
                if i = l - 1 then
                    this.GetContentStream().GetOutputStream().Write(operand).WriteNewLine() |> ignore
                else
                    this.GetContentStream().GetOutputStream().Write(operand).WriteSpace() |> ignore
                )
            this


    module Listener =
        type DataEventListener<'T when 'T :> AbstractRenderInfo>(ets: EventType list,filter: 'T -> bool) =
            member val CurrentInfo = None with set,get
            member val Datas = List<'T>[]
            interface IEventListener with 
                member this.EventOccurred(data,tp) = 
                    let data = data :?> 'T
                    if filter data then
                        data.PreserveGraphicsState()
                        this.Datas.Add(data)
                        this.CurrentInfo  <- Some data
                member this.GetSupportedEvents() = 
                    List ets :> ICollection<_>

        type internal DummyListener() =
            interface IEventListener with
                member this.EventOccurred(data,tp) = ()
                member this.GetSupportedEvents() =
                     List [EventType.RENDER_PATH] :> ICollection<_>
    open Listener

    module Editor = 

        type FixArgument =
            {
                OperatorName: string
                GS: ParserGraphicsState
                Info: AbstractRenderInfo option
                Operator: PdfLiteral
                Operands: seq<PdfObject>
                Canvas: DynamicGSPdfCanvas
            }

        type internal StackElement =
            {
                TempCanvas: DynamicGSPdfCanvas
                CurrentWrite: FixArgument -> unit
            }


        module internal StreamTree =
            [<RequireQualifiedAccess>]
            type StreamStatus =
                | Keep
                | Prepare
                | Modified
                | LoopDown
                | LoopOver


            [<RequireQualifiedAccess>]
            type ResourceRole =
                | Page
                | Inherited
                | XObject

            and StreamNode =
                {
                    Origin: PdfStream
                    Fixed: PdfStream
                    Status: StreamStatus
                    Resources: PdfResources
                    ResourceRole: ResourceRole
                    Childs: StreamNode list
                    Name: PdfName
                }

            [<RequireQualifiedAccess>]
            module StreamStatus =
                open Atrous.Pdf



            [<RequireQualifiedAccess>]
            module StreamNode =
                open Atrous.Pdf

                let rec mapBack f (streamNode: StreamNode) =
                    { streamNode with 
                        Childs = streamNode.Childs |> List.map (mapBack f) 
                    } |> f


                let equalByBytes (streamNode: StreamNode) =
                    streamNode.Origin.GetBytes() = streamNode.Fixed.GetBytes()

                let transformRock prepare loopDown node = 
                    match node.Status with
                    | StreamStatus.Prepare -> { prepare node with Status = StreamStatus.Modified } 
                    | StreamStatus.LoopDown -> { loopDown node with Status = StreamStatus.LoopOver }
                    | StreamStatus.Keep 
                    | StreamStatus.Modified
                    | StreamStatus.LoopOver -> node

                let transformBatchRock f node =
                    transformRock f f node

                [<RequireQualifiedAccess>]
                module Dump =
                    let private pushStatus =
                        mapBack (fun node ->
                            { node with 
                                Status = 
                                    if (equalByBytes node |> not) then StreamStatus.Prepare
                                    elif node.Childs |> List.exists (fun node -> node.Status = StreamStatus.Prepare) then
                                        match node.ResourceRole with 
                                        | ResourceRole.Inherited | ResourceRole.Page -> StreamStatus.LoopDown
                                        | ResourceRole.XObject -> StreamStatus.Prepare

                                    else StreamStatus.Keep
                            }
                        ) >> fun node ->
                            { node with 
                                Status = 
                                    if (equalByBytes node |> not) 
                                    then StreamStatus.Prepare 
                                    else StreamStatus.Keep 
                            }

                    let private updateResources childs (resources: PdfResources) = 
                        childs |> List.iter (fun child ->
                            let container = resources.GetResource(PdfName.XObject)
                            container.Put(child.Name,child.Fixed) |> ignore
                        )

                    let rec private dumpChild (node: StreamNode) =

                        let loopDown node =
                            let childs = node.Childs |> List.map dumpChild
                            let resources = node.Resources

                            updateResources childs resources
                            { node with 
                                Status = StreamStatus.LoopOver
                                Resources = resources
                                Childs = childs
                            }

                        let prepare = 
                            loopDown << fun node -> 
                                { node with 
                                    Fixed = 
                                        let copiedStream = node.Origin.Clone() :?> PdfStream
                                        copiedStream.SetData(node.Fixed.GetBytes())
                                        copiedStream
                                }

                        transformRock prepare loopDown node

                    let private cataDumpContent (page:PdfPage) (node: StreamNode) =
                        let fixStreamAndStatus node = 
                            transformBatchRock (fun node -> 
                                let copiedStream = node.Origin.Clone() :?> PdfStream
                                copiedStream.SetData(node.Fixed.GetBytes())
                                page.Put(PdfName.Contents,copiedStream) |> ignore
                                { node with 
                                    Status = StreamStatus.Modified 
                                    Fixed = copiedStream }
                            ) node
                    
                        let childs = node.Childs |> List.map dumpChild
                        updateResources childs node.Resources

                        let fixChilds node = 
                            { node with 
                                Childs = childs
                            }

                        node |> fixStreamAndStatus |> fixChilds

                    let private dump (page: PdfPage) (node: StreamNode) =
                        node |> cataDumpContent page

                    let run (page: PdfPage) (node: StreamNode) =
                        node |> pushStatus |> dump page |> ignore



        type internal ContentOperatorWrapper (originalOperator) =
            member this.OriginalOperator: IContentOperator = originalOperator

            interface IContentOperator with 
                member this.Invoke(processor,operator,operands) =
                    let processor = processor :?> PdfCanvasEditor
                    let operatorName = operator.ToString()
                    if operatorName <> "Do" then 
                        try 
                            this.OriginalOperator.Invoke(processor, operator, operands)
                        with ex ->
                            if ex.Message = "Dictionary doesn't have supported font data." 
                            then
                                printfn "Skip checking MM font %A" operator
                                let size = (operands.[1]) :?> PdfNumber
                                let size = size.FloatValue()
                                processor.GetGraphicsState().SetFontSize(size)

                            else failwithf "%A" ex
                            ()

                    processor.Write(operator, operands)

        and PdfCanvasEditor () =
            inherit PdfCanvasProcessor(DataEventListener<AbstractRenderInfo>([EventType.RENDER_PATH; EventType.RENDER_TEXT],fun _ -> true))
            let stack = System.Collections.Generic.Stack()

            member this.Info = 
                let l = this.eventListener :?> DataEventListener<AbstractRenderInfo>
                l.CurrentInfo

            member this.Datas : seq<AbstractRenderInfo> = 
                let l = this.eventListener :?> DataEventListener<AbstractRenderInfo>
                l.Datas :> seq<AbstractRenderInfo>

            override this.RegisterContentOperator(operatorString: string , operator: IContentOperator) : IContentOperator =
                let wrapper = new ContentOperatorWrapper(operator)
                let formOperator = base.RegisterContentOperator(operatorString,wrapper)
                match formOperator with 
                | :? ContentOperatorWrapper as wrapper -> wrapper.OriginalOperator
                | _ -> formOperator

            member internal this.ProcessContentEx(canvas,bytes,resources,write) =
                stack.Push {TempCanvas = canvas; CurrentWrite = write}
                this.ProcessContent(bytes,resources)
                stack.Pop() |> ignore
                canvas.GetContentStream()

            member internal this.Write(operator: PdfLiteral, operands: seq<PdfObject>) : unit =
                let top: StackElement = stack.Peek()
                {
                    OperatorName = operator.ToString()
                    GS = this.GetGraphicsState()
                    Info = this.Info
                    Operator = operator
                    Operands = operands
                    Canvas = top.TempCanvas
                }
                |> top.CurrentWrite 

open Core.Editor.StreamTree



module Operators =
    open Core.Editor
    open iText.Kernel.Pdf
    open iText.Kernel.Pdf.Canvas.Parser.Data
    open Core
    open Atrous.Pdf.Extensions
    open Atrous.Pdf
    open Atrous
    
    let private keep arg = 
        let operands = arg.Operands
        arg.Canvas.WriteRange(operands) |> ignore 

    [<RequireQualifiedAccess>]
    module PdfStream = 
        open Atrous.Pdf

        let getResources parentResources (stream:PdfStream) =
            let r = stream.GetAsDictionary(PdfName.Resources)
            match r with 
            | null -> parentResources
            | _ -> 
                PdfResources r

    let private useNode (page:PdfPage) fix f =
        let editor = new PdfCanvasEditor()
        let rec loop name (resourceRole: ResourceRole) (resources: PdfResources) (obj:PdfObject)  = 
            match obj with 
            | :? PdfStream as stream -> 
                let mutable childs = []
                let write (arg: FixArgument) =
                    let operatorName = arg.OperatorName
                    match operatorName with 
                    | "Do" ->
                        let name = Seq.head arg.Operands :?> PdfName
                        let container = resources.GetResource(PdfName.XObject)
                        let xobjectStream = container.Get(name) :?> PdfStream
                        let subType = xobjectStream.GetAsName(PdfName.Subtype)
                        if subType = PdfName.Form 
                        then
                            let xobjectResources,resourceRole = 
                                let r = xobjectStream.GetAsDictionary(PdfName.Resources)
                                match r with 
                                | null -> resources,ResourceRole.Inherited
                                | _ -> 
                                    PdfResources r,ResourceRole.XObject
                            childs <- 
                                loop 
                                    name
                                    resourceRole
                                    xobjectResources 
                                    xobjectStream 
                                :: childs

                        elif subType = PdfName.Image then ()
                        else failwith "Not Implemented"
                    | _ -> ()

                    arg.Canvas.SetGS(arg.GS)
                    fix arg

                let canvas = new DynamicGSPdfCanvas(new PdfStream(),resources,page.GetDocument())
                let bytes = stream.GetBytes()
                let fixedStream = editor.ProcessContentEx(canvas, bytes, resources, write)
                {
                    Origin = stream
                    Fixed = fixedStream
                    ResourceRole = resourceRole
                    Status = StreamStatus.Keep
                    Resources = resources
                    Childs = childs
                    Name = name
                }

            | :? PdfArray as array ->
                if array |> Seq.forall (fun o -> o :? PdfStream) && Seq.length array > 1 then 
                    let s = new PdfStream()
                    array |> Seq.cast<PdfStream> |> Seq.iter (fun s1 ->
                        s.GetOutputStream().WriteBytes(s1.GetBytes()) |> ignore
                    )
                    loop name resourceRole resources s
                else
                    Logger.notImplemented()

            | :? PdfDictionary as map ->
                Logger.notImplemented()
            | _ -> Logger.notImplemented()

        let contents =  
            let top = page.GetPdfObject()
            top.Get(PdfName.Contents)

        match contents with 
        | null -> Seq.empty
        | _ ->
            let resources = page.GetResources()
            let node = loop PdfName.Contents (ResourceRole.Page) resources contents
            f node
            editor.Datas

    let private writePage (page:PdfPage,fix) =
        useNode page fix (StreamNode.Dump.run page) 

    let fix (page:PdfPage) (select: AbstractRenderInfo -> bool) f =
        let mutable infos = []
        let fixPage =  
            fun (arg:FixArgument) ->
                match arg.OperatorName with 
                | "f" | "F" | "f*" | "S" | "s" | "B" | "n" | "B*" | "b" | "b*" | "Tj" | "TJ"  ->
                    let info = arg.Info.Value
                    if select info then 
                        infos <- 
                            info :: infos
                        f arg
                    else 
                        keep arg
                | _ -> keep arg

        writePage (page,fixPage) |> ignore
        infos

    //let fix (page:PdfPage) (select: AbstractRenderInfo -> bool) f =
    //    let fixPage =  
    //        fun (arg:FixArgument) ->
    //            match arg.OperatorName with 
    //            | "f" | "F" | "f*" | "S" | "s" | "B" | "n" | "B*" | "b" | "b*" | "Tj" | "TJ"  ->
    //                let info = arg.Info
    //                if select info.Value then f arg
    //                else 
    //                    keep arg
    //            | _ -> keep arg

    //    writePage (page,fixPage) |> ignore
    
    let private text arg f =
        match arg.OperatorName with 
        | "Tj" | "TJ" ->
            let trInfo = arg.Info.Value :?> TextRenderInfo
            f trInfo
        | _ -> false

    let private (|TextFill|_|) arg =
        if text arg TextRenderInfo.hasSolidFill then Some (arg.Info.Value.GetFillColor())
        else None

    let private (|TextStroke|_|) arg =
        if text arg TextRenderInfo.hasSolidStroke then Some (arg.Info.Value.GetFillColor())
        else None

    let private (|Fill|_|) arg =
        match arg.OperatorName with 
        | "f" | "F" | "f*" | "B" | "B*" | "b" | "b*" -> Some (arg.Info.Value.GetFillColor())
        | _ ->
            match arg with 
            | TextFill color -> Some color 
            | _ -> None

    let private (|Stroke|_|) arg =
        match arg.OperatorName with 
        | "S" | "s" | "B" | "B*" | "b" | "b*"  -> Some (arg.Info.Value.GetStrokeColor())
        | _ ->
            match arg with 
            | TextStroke color -> Some color 
            | _ -> None


    let private scnColor (arg:FixArgument) =
        match arg with 
        | Stroke color | Fill color ->
            failwith "Not implemented"
            ()
        | _ -> ()


    let read (page:PdfPage) (select: AbstractRenderInfo -> bool) =
        failwith "Not implemented"
        let fixPage = 
            fun (arg:FixArgument) ->
                scnColor arg

        writePage (page,fixPage) |> Seq.filter select


