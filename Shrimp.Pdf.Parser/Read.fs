namespace Shrimp.Pdf.Parser

open iText.Kernel.Geom

#nowarn "0104"
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Listener
open System.Collections.Generic
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Extensions
open Shrimp.FSharp.Plus
open Shrimp.Pdf
open iText.Kernel.Pdf


[<RequireQualifiedAccess>]
type RenderInfoSelector = 
    | PathOrText of (IIntegratedRenderInfo -> bool)
    | Path of (IntegratedPathRenderInfo -> bool)
    | Text of (IntegratedTextRenderInfo -> bool)
    | Dummy
    | AND of RenderInfoSelector list
    | OR of RenderInfoSelector list

[<RequireQualifiedAccess>]
module RenderInfoSelector =
    


    let toEventTypes selector =
        let rec loop selector =
            match selector with 
            | RenderInfoSelector.PathOrText _ -> [EventType.RENDER_TEXT; EventType.RENDER_PATH; EventType.CLIP_PATH_CHANGED]
            | RenderInfoSelector.Path _ -> [EventType.RENDER_PATH; EventType.CLIP_PATH_CHANGED]
            | RenderInfoSelector.Text _ -> [EventType.RENDER_TEXT; EventType.CLIP_PATH_CHANGED]
            | RenderInfoSelector.Dummy -> []
            | RenderInfoSelector.AND selectors ->
                selectors
                |> List.collect(loop)

            | RenderInfoSelector.OR selectors ->
                selectors
                |> List.collect(loop)

        loop selector
        |> List.distinct

    let toRenderInfoPredication (selector) =
        let rec loop selector =
     
            match selector with 

            | RenderInfoSelector.Path predicate ->
                fun (renderInfo: IIntegratedRenderInfo) -> 
                    match renderInfo with 
                    | IIntegratedRenderInfo.Text _ -> false
                    | IIntegratedRenderInfo.Path renderInfo -> 
                        match Seq.length(IPathRenderInfo.toActualPoints renderInfo) with 
                        | 0 -> false
                        | _ -> predicate renderInfo


            | RenderInfoSelector.Text predicate ->
                fun (renderInfo: IIntegratedRenderInfo) ->
                    match renderInfo with 
                    | IIntegratedRenderInfo.Text renderInfo -> predicate renderInfo
                    | IIntegratedRenderInfo.Path _ -> false

            | RenderInfoSelector.PathOrText predicate -> 
                fun (renderInfo: IIntegratedRenderInfo) ->
                    match renderInfo with 
                    | IIntegratedRenderInfo.Path renderInfo -> 
                        //loop (RenderInfoSelector.Text (fun info -> predicate(info :> IIntegratedRenderInfo))) (renderInfo :> IIntegratedRenderInfo)
                        match renderInfo with 
                        | IIntegratedRenderInfo.Text _ -> false
                        | IIntegratedRenderInfo.Path renderInfo -> 
                            match Seq.length(IPathRenderInfo.toActualPoints renderInfo) with 
                            | 0 -> false
                            | _ -> predicate renderInfo

                    | IIntegratedRenderInfo.Text renderInfo -> 
                        predicate renderInfo


            | RenderInfoSelector.Dummy _ -> fun _ -> false


            | RenderInfoSelector.AND selectors ->
                fun (renderInfo: IIntegratedRenderInfo) ->
                    selectors |> List.forall (fun selector -> loop selector renderInfo)

            | RenderInfoSelector.OR selectors ->
                fun (renderInfo: IIntegratedRenderInfo) ->
                    selectors |> List.exists (fun selector -> loop selector renderInfo)




        loop selector


[<Struct>]
type SelectorModiferToken = 
    { Name: string }


module internal Listeners =

    type CurrentRenderInfoStatus =
        | Skiped = 0
        | Selected = 1


    [<AllowNullLiteral>]
    /// a type named FilteredEventListener is already defined in itext7
    /// OR Between selectors of renderInfoSelectorMapping
    type FilteredEventListenerEx(renderInfoSelectorMapping: Map<SelectorModiferToken, RenderInfoSelector>) =
        let prediateMapping = 
            renderInfoSelectorMapping
            |> Map.map (fun token renderInfoSelector -> 
                RenderInfoSelector.toRenderInfoPredication renderInfoSelector
            )

        let mutable currentXObjectClippingBox = XObjectClippingBoxState.Init

        let mutable currentRenderInfo: IIntegratedRenderInfo option = None
        let mutable currentRenderInfoToken = None
        let mutable currentRenderInfoStatus = CurrentRenderInfoStatus.Selected
        let mutable currentClippingPathInfo = ClippingPathInfoState.Init

        let parsedRenderInfos = List<IIntegratedRenderInfo>()

        let supportedEventTypes =
            let supportedEvents = 
                renderInfoSelectorMapping
                |> Map.toList
                |> List.map snd
                |> RenderInfoSelector.OR
                |> RenderInfoSelector.toEventTypes
              
            List supportedEvents :> ICollection<_>

        member this.CurrentRenderInfo = currentRenderInfo.Value

        member this.CurrentRenderInfoStatus = currentRenderInfoStatus

        member this.CurrentRenderInfoToken = currentRenderInfoToken

        member this.ParsedRenderInfos = parsedRenderInfos :> seq<IIntegratedRenderInfo>

        member internal this.GetXObjectClippingBox() = currentXObjectClippingBox

        member internal this.SetXObjectClippingBox(xObjectBBox: Rectangle) =
            currentXObjectClippingBox <-
                match currentXObjectClippingBox with 
                | XObjectClippingBoxState.IntersectedNone -> XObjectClippingBoxState.IntersectedNone
                | XObjectClippingBoxState.Init -> XObjectClippingBoxState.IntersectedSome xObjectBBox
                | XObjectClippingBoxState.IntersectedSome xObjectBBox' ->
                
                    match Rectangle.tryGetIntersection xObjectBBox' xObjectBBox with 
                    | Some rect -> XObjectClippingBoxState.IntersectedSome rect
                    | None -> XObjectClippingBoxState.IntersectedNone 

        member internal this.SetXObjectClippingBoxState(xObjectBBoxState) =
            currentXObjectClippingBox <- xObjectBBoxState

        member internal this.UpdateXObjectClippingBoxCtm(ctm: Matrix) =
            let newXObjectClippingBox =
                match currentXObjectClippingBox with 
                | XObjectClippingBoxState.Init -> XObjectClippingBoxState.Init
                | XObjectClippingBoxState.IntersectedNone -> XObjectClippingBoxState.IntersectedNone
                | XObjectClippingBoxState.IntersectedSome v ->
                    ((AffineTransform.ofMatrix ctm).Transform(v))
                    |> XObjectClippingBoxState.IntersectedSome

            currentXObjectClippingBox <- newXObjectClippingBox


        interface IEventListener with 
            member this.EventOccurred(data, tp) = 

                match tp with 
                | EventType.CLIP_PATH_CHANGED -> 
                    let clippingPathInfo' = data :?> ClippingPathInfo
                    currentClippingPathInfo <- ClippingPathInfoState.Intersected clippingPathInfo'
                    clippingPathInfo'.PreserveGraphicsState()
                | _ ->
                    let renderInfo = 
                        match data with 
                        | :? PathRenderInfo as pathRenderInfo ->
                            { ClippingPathInfos  = 
                                { XObjectClippingBoxState = currentXObjectClippingBox
                                  ClippingPathInfoState = currentClippingPathInfo }
                              PathRenderInfo = pathRenderInfo }
                            :> IIntegratedRenderInfo

                        | :? TextRenderInfo as textRenderInfo ->
                            { ClippingPathInfos = 
                                { XObjectClippingBoxState = currentXObjectClippingBox
                                  ClippingPathInfoState = currentClippingPathInfo }
                              TextRenderInfo = textRenderInfo }
                            :> IIntegratedRenderInfo

                        |_ -> failwith "Not implemented"


                    let predicate _ filter =
                        filter renderInfo

                    match Map.tryFindKey predicate prediateMapping with 
                    | Some token ->

                        renderInfo.Value.PreserveGraphicsState()

                        parsedRenderInfos.Add(renderInfo)
                        currentRenderInfoToken <- Some token
                        currentRenderInfo <- Some renderInfo
                        currentRenderInfoStatus <- CurrentRenderInfoStatus.Selected

                    | None -> 
                        currentRenderInfoStatus <- CurrentRenderInfoStatus.Skiped
                        currentRenderInfo <- None

            member this.GetSupportedEvents() = supportedEventTypes

    type DummyListener() =
        let supportedEvents = List () :> ICollection<_>
            
        interface IEventListener with
            member this.EventOccurred(data,tp) = ()
            member this.GetSupportedEvents() = supportedEvents

open Listeners


//type private ClippingPath_UpdateCtm_Bug_FixmentPdfCanvasProcessor(listener: IEventListener, additionalContentOperators) =
//    inherit PdfCanvasProcessor(listener, additionalContentOperators)

//    let mutable clippingPath_BeforeUpdateCtm: Path option = None

//    override this.ProcessContent(contentBytes, resources) =
//        match clippingPath_BeforeUpdateCtm with 
//        | None -> ()
//        | Some v -> this.GetGraphicsState().SetClippingPath(v)

//        base.ProcessContent(contentBytes, resources)


//    override this.InvokeOperator(operator, operands) =
//        match operator.ToString() with 
//        | "cm" ->
//            let gs = this.GetGraphicsState()
//            clippingPath_BeforeUpdateCtm <- Some (gs.GetClippingPath())
//            base.InvokeOperator(operator, operands)

//            let ctm = 
//                gs.GetCtm()
//                |> AffineTransform.ofMatrix

//            let newClippingPath = ctm.Transform(clippingPath_BeforeUpdateCtm.Value)
//            gs.SetClippingPath(newClippingPath)
//            let info = new ClippingPathInfo(gs, gs.GetClippingPath(), gs.GetCtm())
//            this.GetEventListener().EventOccurred(info, EventType.CLIP_PATH_CHANGED)
            
//        | _ ->
//            base.InvokeOperator(operator, operands)


        
open Constants

type private NonInitialCallbackablePdfCanvasProcessor(listener: IEventListener , additionalContentOperators) =
    inherit PdfCanvasProcessor(listener, additionalContentOperators)

    member internal this.InitClippingPath(page: PdfPage) =
        let clippingPath = new Path()
        let initBox = page.GetCropBox() |> Rectangle.applyMargin (Margin.Create(Constants.MAXIMUM_MM_WIDTH / 2.)) 
        clippingPath.Rectangle(initBox);
        this.GetGraphicsState().SetClippingPath(clippingPath)

        do 
            match this.GetEventListener() with 
            | :? FilteredEventListenerEx as listener -> 
                listener.SetXObjectClippingBox(initBox)
            | _ -> ()


        let gs = this.GetGraphicsState()
        this.EventOccurred(new ClippingPathInfo(gs, gs.GetClippingPath(), gs.GetCtm()), EventType.CLIP_PATH_CHANGED);

    override this.ProcessContent(contentBytes, resources) =
        base.ProcessContent(contentBytes, resources)

    override this.InvokeOperator(operator, operands) =
        //printfn "%s %A" (operator.ToString())(List.ofSeq operands)
        match operator.ToString() with
        | Operators.Do -> 
            match this.GetEventListener() with 
            | :? FilteredEventListenerEx as listener ->
                let name = operands.[0] :?> PdfName
                let resource = this.GetResources()
                let formObject = resource.GetForm(name)

                match formObject with 
                | null -> base.InvokeOperator(operator, operands)
                | formObject ->
                    let bbox = 
                        //PdfFormXObject.getBBox formObject
                        let bbox =  PdfFormXObject.getBBox formObject
                        let ctm = this.GetGraphicsState().GetCtm() |> AffineTransform.ofMatrix
                        ctm.Transform(bbox)

                    let originState = listener.GetXObjectClippingBox()
                    listener.SetXObjectClippingBox(bbox)
                    base.InvokeOperator(operator, operands)
                    listener.SetXObjectClippingBoxState(originState)

            | _ -> base.InvokeOperator(operator, operands)
        | _ ->  
            base.InvokeOperator(operator, operands)
            //match operator.ToString() with 
            //| "cm" ->
            //    match this.GetEventListener() with 
            //    | :? FilteredEventListenerEx as listener ->
            //        let ctm = 
            //            new Matrix(
            //                (operands.[0] :?> PdfNumber).FloatValue(),
            //                (operands.[1] :?> PdfNumber).FloatValue(),
            //                (operands.[2] :?> PdfNumber).FloatValue(),
            //                (operands.[3] :?> PdfNumber).FloatValue(),
            //                (operands.[4] :?> PdfNumber).FloatValue(),
            //                (operands.[5] :?> PdfNumber).FloatValue()
            //            )
            //        base.InvokeOperator(operator, operands)
            //        listener.UpdateXObjectClippingBoxCtm(ctm)

            //    | _ -> base.InvokeOperator(operator, operands)
            //| _ -> base.InvokeOperator(operator, operands)
                



    override this.ProcessPageContent(page) =
        this.InitClippingPath(page);

        this.ProcessContent(page.GetContentBytes(), page.GetResources());



    new (listener: IEventListener) = NonInitialCallbackablePdfCanvasProcessor(listener, dict [])

type NonInitialClippingPathPdfDocumentContentParser(pdfDocument) =
    inherit PdfDocumentContentParser(pdfDocument)
    override this.ProcessContent(pageNumber, renderListener, additionalContentOperators) =
            let processor = new NonInitialCallbackablePdfCanvasProcessor(renderListener, additionalContentOperators)
            processor.ProcessPageContent(pdfDocument.GetPage(pageNumber));
            renderListener


[<RequireQualifiedAccess>]
module NonInitialClippingPathPdfDocumentContentParser =
    open Listeners
    let parse (pageNum: int) (renderInfoSelector: RenderInfoSelector) (parser: NonInitialClippingPathPdfDocumentContentParser) =

        let et = RenderInfoSelector.toEventTypes renderInfoSelector

        match et with 
        | [] -> [] :> seq<IIntegratedRenderInfo>
        | _ ->
            let renderInfoSelectorMapping = Map.ofList [{ Name= "Untitled"}, renderInfoSelector]
            let listener = new FilteredEventListenerEx(renderInfoSelectorMapping)
            parser.ProcessContent(pageNum, listener).ParsedRenderInfos
        
