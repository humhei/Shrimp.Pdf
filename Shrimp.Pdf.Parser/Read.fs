namespace Shrimp.Pdf.Parser

open iText.Kernel.Geom

#nowarn "0104"
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Listener
open System.Collections.Generic
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Extensions
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

            member this.GetSupportedEvents() = 
                let supportedEvents = 
                    renderInfoSelectorMapping
                    |> Map.toList
                    |> List.map snd
                    |> RenderInfoSelector.OR
                    |> RenderInfoSelector.toEventTypes
                  
                List supportedEvents :> ICollection<_>

    type DummyListener() =
        interface IEventListener with
            member this.EventOccurred(data,tp) = ()
            member this.GetSupportedEvents() = List () :> ICollection<_>

open Listeners


type private NonInitialCallbackablePdfCanvasProcessor(listener: IEventListener , additionalContentOperators) =
    inherit PdfCanvasProcessor(listener, additionalContentOperators)
    member private this.InitClippingPath(page: PdfPage) =
        let clippingPath = new Path();
        clippingPath.Rectangle(page.GetCropBox() |> Rectangle.applyMargin (Margin.Create (mm Shrimp.Pdf.Constants.MAXIMUM_MM_WIDTH)));
        this.GetGraphicsState().SetClippingPath(clippingPath)

    override this.InvokeOperator(operator, operands) =
        
        match operator.ToString() with
        | "Do" -> 
            match this.GetEventListener() with 
            | :? FilteredEventListenerEx as listener ->
                let name = operands.[0] :?> PdfName
                let resource = this.GetResources()
                let formObject = resource.GetForm(name)
                let bbox = PdfFormXObject.getBBox formObject
                let originState = listener.GetXObjectClippingBox()
                listener.SetXObjectClippingBox(bbox)
                base.InvokeOperator(operator, operands)
                listener.SetXObjectClippingBoxState(originState)

            | _ -> base.InvokeOperator(operator, operands)
        | _ -> 
            base.InvokeOperator(operator, operands)




    override this.ProcessPageContent(page) =
        this.InitClippingPath(page);
        let gs = this.GetGraphicsState()
        this.EventOccurred(new ClippingPathInfo(gs, gs.GetClippingPath(), gs.GetCtm()), EventType.CLIP_PATH_CHANGED);
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
        
