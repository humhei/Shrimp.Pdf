
namespace Shrimp.Pdf.Parser

open iText.Kernel.Geom

#nowarn "0104"
open iText.Kernel.Colors
open iText.Kernel.Exceptions
open iText.Kernel.Pdf.Colorspace
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Listener
open System.Collections.Generic
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Extensions
open Shrimp.FSharp.Plus
open Shrimp.Pdf
open iText.IO.Image
open iText.Kernel.Pdf
open Shrimp.Pdf.Constants.Operators
open System.Collections.Concurrent



type RenderInfoStoppedException(info: IIntegratedRenderInfoIM) =
    inherit System.Exception()

    member x.StoppedInfo = info

[<RequireQualifiedAccess>]
type RenderInfoSelector = 
    | Image of (IntegratedImageRenderInfo -> bool)
    | PathOrText of (IIntegratedRenderInfo -> bool)
    | Path of (IntegratedPathRenderInfo -> bool)
    | Text of (IntegratedTextRenderInfo -> bool)
    | Dummy
    | AND of RenderInfoSelector list
    | OR of RenderInfoSelector list
    | Not of RenderInfoSelector

[<RequireQualifiedAccess>]
module RenderInfoSelector =
    let toEventTypes selector =
        let rec loop selector =
            match selector with 
            | RenderInfoSelector.Image _ -> [EventType.RENDER_IMAGE; EventType.CLIP_PATH_CHANGED]
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
            | RenderInfoSelector.Not selector ->
                loop selector

        loop selector
        |> List.distinct


    let checkNonImageSelectorExists(selectors: RenderInfoSelector list) =
        let ets =    
            selectors
            |> List.collect toEventTypes
            |> List.distinct

        let containsImage =
            ets
            |> List.tryFind(fun m -> m = EventType.RENDER_IMAGE)
            |> Option.isSome

        match containsImage with 
        | true -> failwithf "RenderInfoSelector %A contain imageSelector, please using NonInitialClippingPathPdfDocumentContentParser.parseIM instead" selectors
        | false -> ()


    let toRenderInfoPredication (selector) =
        let rec loop selector =
            match selector with 
            | RenderInfoSelector.Image _ -> fun _ -> false
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
                        match Seq.length(IPathRenderInfo.toActualPoints renderInfo) with 
                        | 0 -> false    
                        | _ -> predicate renderInfo

                    | IIntegratedRenderInfo.Text renderInfo -> predicate renderInfo


            | RenderInfoSelector.Dummy _ -> fun _ -> false


            | RenderInfoSelector.AND selectors ->
                fun (renderInfo: IIntegratedRenderInfo) ->
                    selectors |> List.forall (fun selector -> loop selector renderInfo)

            | RenderInfoSelector.OR selectors ->
                fun (renderInfo: IIntegratedRenderInfo) ->
                    selectors |> List.exists (fun selector -> loop selector renderInfo)

            | RenderInfoSelector.Not selector ->
                fun (renderInfo: IIntegratedRenderInfo) ->
                    loop selector renderInfo
                    |> not


        loop selector


    let toRenderInfoIMPredication (selector) =
        let rec loop selector =
            match selector with 
            | RenderInfoSelector.Image predicate -> 
                (fun (renderInfo: IIntegratedRenderInfoIM) ->
                    match renderInfo with 
                    | IIntegratedRenderInfoIM.Image renderInfo -> predicate renderInfo
                    | _ -> false
                )
            | RenderInfoSelector.AND selectors ->
                fun (renderInfo: IIntegratedRenderInfoIM) ->
                    selectors |> List.forall (fun selector -> loop selector renderInfo)

            | RenderInfoSelector.OR selectors ->
                fun (renderInfo: IIntegratedRenderInfoIM) ->
                    selectors |> List.exists (fun selector -> loop selector renderInfo)

            | RenderInfoSelector.Not (selector) ->
                fun (renderInfo: IIntegratedRenderInfoIM) ->
                    loop selector renderInfo
                    |> not

            | RenderInfoSelector.Dummy _ -> fun _ -> false
            | RenderInfoSelector.Path predicate ->
                fun (renderInfo: IIntegratedRenderInfoIM) ->
                    match renderInfo with 
                    | IIntegratedRenderInfoIM.Text _ -> false
                    | IIntegratedRenderInfoIM.Path renderInfo -> 
                        match Seq.length(IPathRenderInfo.toActualPoints renderInfo) with 
                        | 0 -> false
                        | _ -> predicate renderInfo
                    | IIntegratedRenderInfoIM.Image _ -> false

            | RenderInfoSelector.PathOrText predicate -> 
                fun (renderInfo: IIntegratedRenderInfoIM) ->
                    match renderInfo with 
                    | IIntegratedRenderInfoIM.Path renderInfo -> 
                        //loop (RenderInfoSelector.Text (fun info -> predicate(info :> IIntegratedRenderInfo))) (renderInfo :> IIntegratedRenderInfo)
                        match Seq.length(IPathRenderInfo.toActualPoints renderInfo) with 
                        | 0 -> false    
                        | _ -> predicate renderInfo

                    | IIntegratedRenderInfoIM.Text renderInfo -> predicate renderInfo
                    | IIntegratedRenderInfoIM.Image _ -> false
                        
            | RenderInfoSelector.Text predicate ->
                fun (renderInfo: IIntegratedRenderInfoIM) ->
                    match renderInfo with 
                    | IIntegratedRenderInfoIM.Text renderInfo -> predicate renderInfo
                    | IIntegratedRenderInfoIM.Path _ -> false
                    | IIntegratedRenderInfoIM.Image _ -> false



        loop selector


[<Struct>]
type SelectorModiferToken = 
    { Name: string }


module internal Listeners =

    type CurrentRenderInfoStatus =
        | Skiped = 0
        | Selected = 1

    let private imageColorSpaceCache = new ConcurrentDictionary<_, _>()

    type FsParserGraphicsState(gs: ParserGraphicsState, previous: FsParserGraphicsState option) =
        let innerGSStack = 
            new Stack<_>([|new CanvasGraphicsState(gs)|])
            
        let mutable fillOpacity = previous |> Option.bind(fun m -> m.FillOpacity)
        let mutable strokeOpacity = previous |> Option.bind(fun m -> m.StrokeOpacity)

        member private x.ApplyFillOpacity(v) =
            match v with 
            | 1.0f -> ()
            | _ -> 
                match fillOpacity with 
                | None -> fillOpacity <- Some v
                | Some fillOpacity2 -> fillOpacity <- Some (v * fillOpacity2)

        member internal x.FillOpacity = fillOpacity
        member internal x.StrokeOpacity = strokeOpacity

        member private x.ApplyStrokeOpacity(v) =
            match v with 
            | 1.0f -> ()
            | _ -> 
                match strokeOpacity with 
                | None -> strokeOpacity <- Some v
                | Some strokeOpacity2 -> strokeOpacity <- Some (v * strokeOpacity2)

        member x.Value = innerGSStack.Peek()

        member x.ProcessGraphicsStateResource(gs: ParserGraphicsState) = 
            x.ApplyFillOpacity(gs.GetFillOpacity())
            x.ApplyStrokeOpacity(gs.GetStrokeOpacity())
            innerGSStack.Push(CanvasGraphicsState gs)
            |> ignore

    [<AllowNullLiteral>]
    type GSStateStackableEventListener() =
        let gsStack = new Stack<_>()


        member internal x.SaveGS(gs: ParserGraphicsState) =
            let gs = 
                match gsStack.Count with 
                | 0 -> FsParserGraphicsState (gs, None)
                | _ -> FsParserGraphicsState (gs, Some (gsStack.Peek()))

            gsStack.Push(gs)

        member internal x.ProcessGraphicsStateResource(gs) =
            match gsStack.Count with 
            | 0 -> x.SaveGS(gs)
            | _-> ()

            gsStack.Peek().ProcessGraphicsStateResource(gs)

        member internal x.FillOpacity =
            match gsStack.Count with 
            | 0 -> None
            | _ -> gsStack.Peek().FillOpacity

        member internal x.StrokeOpacity =
            match gsStack.Count with 
            | 0 -> None
            | _ -> gsStack.Peek().StrokeOpacity

        member internal x.RestoreGS() = 
            gsStack.Pop()
            |> ignore

    [<AllowNullLiteral>]
    /// a type named FilteredEventListener is already defined in itext7
    /// renderInfoSelectorMapping bitwise relation: OR 
    type FilteredEventListenerEx(renderInfoSelectorMapping: Map<SelectorModiferToken, RenderInfoSelector>) =
        inherit GSStateStackableEventListener()
        let et = 
            renderInfoSelectorMapping
            |> Map.toList
            |> List.map snd
            |> List.collect RenderInfoSelector.toEventTypes


        let prediateMapping = 
            renderInfoSelectorMapping
            |> Map.map (fun token renderInfoSelector -> 
                RenderInfoSelector.toRenderInfoIMPredication renderInfoSelector
            )
        let mutable isShowingText = false
        let mutable concatedTextInfos: ResizeArray<IntegratedTextRenderInfo> = ResizeArray() 
        let mutable accumulatedPathOperatorRanges = ResizeArray()


        let mutable currentRenderInfo: IIntegratedRenderInfoIM option = None
        let mutable currentRenderInfoToken = None
        let mutable currentRenderInfoStatus = CurrentRenderInfoStatus.Skiped


        let mutable currentXObjectClippingBox = XObjectClippingBoxState.Init
        let mutable currentClippingPathInfo = ClippingPathInfoState.Init
        let currentClippingPathInfoElementsStack = Stack<ResizeArray<IntersectedClippingPathInfoElement>>()
        do currentClippingPathInfoElementsStack.Push(ResizeArray())
        let mutable currentRenderingClippingPathInfo_Integrated: IntegratedPathRenderInfo option = None
        let parsedRenderInfos = List<IIntegratedRenderInfoIM>()

        let supportedEventTypes =
            let supportedEvents = 
                renderInfoSelectorMapping
                |> Map.toList
                |> List.map snd
                |> RenderInfoSelector.OR
                |> RenderInfoSelector.toEventTypes
              
            List supportedEvents :> ICollection<_>

        member internal x.EventTypes = et

        member internal x.CurrentRenderingClippingPathInfo_Integrated = currentRenderingClippingPathInfo_Integrated

        member internal x.AddPathOperatorRange(operatorRange) = accumulatedPathOperatorRanges.Add(operatorRange)

        member internal x.BeginShowText() = isShowingText <- true
        member internal x.EndShoeText(operatorRange: OperatorRange) = 
            let textInfo = 
                match concatedTextInfos.Count with 
                | 0 -> None
                | 1 -> Some concatedTextInfos.[0]
                | _ ->
                    let lastInfo = 
                        let previous =
                            concatedTextInfos
                            |> List.ofSeq
                            |> List.map(fun m -> 
                                { m with EndTextState = EndTextState.No }
                            )


                        { concatedTextInfos.[concatedTextInfos.Count-1] with 
                            ConcatedTextInfos = previous
                            EndTextState = EndTextState.Yes
                        }
                    Some lastInfo

            let releaseGraphicsState() =
                for info in concatedTextInfos do 
                    info.TextRenderInfo.ReleaseGraphicsState()

            match textInfo with 
            | Some textInfo -> 

                let textInfo = { textInfo with OperatorRange = Some operatorRange }
                let textInfo = (textInfo :> IIntegratedRenderInfoIM)
                let predicate _ filter =
                    filter (textInfo)

                let filtered = Map.filter predicate prediateMapping
                match filtered.IsEmpty with 
                | false ->
                    let tokens =
                        filtered
                        |> Map.toList
                        |> List.map fst

                    parsedRenderInfos.Add(textInfo)

                    currentRenderInfoToken <- Some tokens
                    currentRenderInfo <- Some textInfo
                    currentRenderInfoStatus <- CurrentRenderInfoStatus.Selected

                | true -> 
                    releaseGraphicsState()
                    currentRenderInfoStatus <- CurrentRenderInfoStatus.Skiped
                    currentRenderInfo <- None

            | None -> 
                releaseGraphicsState()
                currentRenderInfoStatus <- CurrentRenderInfoStatus.Skiped
                currentRenderInfo <- None

            concatedTextInfos <- new ResizeArray<_>()
            isShowingText <- false

        member internal this.ConcatedTextInfos = concatedTextInfos :> seq<IntegratedTextRenderInfo>

        member this.CurrentRenderInfo = currentRenderInfo.Value

        member this.CurrentRenderInfoStatus = currentRenderInfoStatus

        member this.CurrentRenderInfoToken = currentRenderInfoToken

        member this.ParsedRenderInfos = parsedRenderInfos :> seq<IIntegratedRenderInfoIM>
        member internal this.SaveGS(gs) = 
            base.SaveGS(gs)
            currentClippingPathInfoElementsStack.Push(ResizeArray()) |> ignore
        
        member internal this.RestoreGS() = 
            base.RestoreGS()
            currentClippingPathInfoElementsStack.Pop() |> ignore

        member internal this.SaveGS_XObject(gs) = 
            base.SaveGS(gs)
        
        member internal this.RestoreGS_XObject() = 
            base.RestoreGS()


        member private this.GetCurrentClippingPathInfoElements() = 
            currentClippingPathInfoElementsStack.Peek()

        member internal this.InitClippingPathInfo() = currentClippingPathInfo <- ClippingPathInfoState.Init

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

        member internal this.GetCurrentClippingPathInfo() = currentClippingPathInfo

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
                    currentClippingPathInfo <- 
                        { ClippingPathInfo = clippingPathInfo'
                          Elements = 
                            currentClippingPathInfoElementsStack.ToArray()
                          }
                        |> ClippingPathInfoState.Intersected 

                        
                    clippingPathInfo'.PreserveGraphicsState()

                | _ ->
                    let renderInfo = 
                        match data with 
                        | :? PathRenderInfo as pathRenderInfo ->
                            let info = 
                                { ClippingPathInfos  = 
                                    { XObjectClippingBoxState = currentXObjectClippingBox
                                      ClippingPathInfoState = currentClippingPathInfo }
                                  PathRenderInfo = pathRenderInfo
                                  AccumulatedPathOperatorRanges = accumulatedPathOperatorRanges
                                  FillOpacity = this.FillOpacity
                                  StrokeOpacity = this.StrokeOpacity }


                            match pathRenderInfo.IsPathModifiesClippingPath() with 
                            | true -> 
                                let operatorRanges = this.GetCurrentClippingPathInfoElements()

                                operatorRanges.Add(
                                    { OperatorRanges = accumulatedPathOperatorRanges
                                      Ctm = pathRenderInfo.GetGraphicsState().GetCtm() }
                                )

                                info.PathRenderInfo.PreserveGraphicsState()
                                currentRenderingClippingPathInfo_Integrated <- Some info

                            | false -> 
                                match pathRenderInfo with 
                                | :? PdfShadingPathRenderInfo -> ()
                                | _ ->
                                    match currentRenderingClippingPathInfo_Integrated with 
                                    | Some info -> info.PathRenderInfo.ReleaseGraphicsState()
                                    | None -> ()

                                currentRenderingClippingPathInfo_Integrated <- None
                                //let bound = 
                                    //IPathRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth info
                                    //|> FsRectangle.OfRectangle
                                ()
                            accumulatedPathOperatorRanges <- ResizeArray<_>()
                            info :> IIntegratedRenderInfoIM



                        | :? TextRenderInfo as textRenderInfo ->
                            { ClippingPathInfos = 
                                { XObjectClippingBoxState = currentXObjectClippingBox
                                  ClippingPathInfoState = currentClippingPathInfo }
                              TextRenderInfo = textRenderInfo
                              EndTextState = EndTextState.Undified
                              ConcatedTextInfos = []
                              OperatorRange = None
                              FillOpacity = this.FillOpacity
                              StrokeOpacity = this.StrokeOpacity
                              }
                            :> IIntegratedRenderInfoIM

                        | :? ImageRenderInfo as imageRenderInfo ->
                            { ClippingPathInfos = 
                                { XObjectClippingBoxState = currentXObjectClippingBox
                                  ClippingPathInfoState = currentClippingPathInfo }
                              ImageRenderInfo = imageRenderInfo
                              FillOpacity = this.FillOpacity
                              StrokeOpacity = this.StrokeOpacity
                              LazyImageData = 
                                lazy 
                                    let rgbIndexedColorSpace =
                                        let image = 
                                            imageRenderInfo
                                                .GetImage()

                                        let bitsPerComponent = 
                                            image
                                                .GetPdfObject()
                                                .GetAsNumber(PdfName.BitsPerComponent)
                                                .IntValue()

                                        let imageType =
                                            image.IdentifyImageType()

                                        let colorSpace = image.GetPdfObject().GetAsArray(PdfName.ColorSpace)

                                        match bitsPerComponent, imageType with 
                                        | 2, _ -> 
                                            match colorSpace.Contains(PdfName.Indexed) && colorSpace.Contains(PdfName.DeviceRGB) with 
                                            | true -> Some { ImageXObject = image; ImageType = imageType }
                                                
                                            | false -> None
                                        | _ -> None
                                    
                                    match rgbIndexedColorSpace with 
                                    | Some rgbIndexedColorSpace -> FsImageData.IndexedRgb rgbIndexedColorSpace
                                    | None -> 
                                        (ImageDataFactory.Create(imageRenderInfo.GetImage().GetImageBytes()))
                                        |> FsImageData.ImageData

                              LazyColorSpace = 
                                lazy 
                                    let fromPdfName name =
                                        match name with 
                                        | EqualTo PdfName.DeviceRGB -> ColorSpace.Rgb
                                        | EqualTo PdfName.DeviceCMYK -> ColorSpace.Cmyk
                                        | EqualTo PdfName.DeviceGray -> ColorSpace.Gray
                                        | EqualTo PdfName.Lab -> ColorSpace.Lab
                                        | _ -> failwithf "Cannot get colorspace from pdfName %O" (name.ToString())
                                    
                                    let pdfObject = imageRenderInfo.GetImage().GetPdfObject()
                                    let colorSpace = pdfObject.Get(PdfName.ColorSpace)
                                    match colorSpace with 
                                    | null -> 
                                        match  pdfObject.Get(PdfName.ImageMask) with 
                                        | null -> failwithf "Not implemented, Cannot get colorSpace from %A" pdfObject
                                        | _ -> Some ImageColorSpaceData.ImageMask
                                    | colorSpace ->
                                        let decode = pdfObject.Get(PdfName.Decode) |> Option.ofObj |> Option.map (fun m -> m :?> PdfArray)
                                        imageColorSpaceCache.GetOrAdd(colorSpace, valueFactory = (fun colorSpace ->
                                            match colorSpace with 
                                            | :? PdfArray as array ->
                                                match array.Get(0) with 
                                                | :? PdfName as name ->
                                                    match name with 
                                                    | EqualTo PdfName.Indexed ->
                                                        let colorSpace = 
                                                            match array.Get(1) with 
                                                            | :? PdfName as name -> fromPdfName name
                                                            | _ -> failwithf "Invalid token, current colorspace pdfArray are %A" array
                                                        let indexedTable = 
                                                            match array.Get(3) with 
                                                            | :? PdfStream as pdfStream -> pdfStream.GetBytes()
                                                            | :? PdfString as pdfString -> pdfString.GetValueBytes()

                                                        { ColorSpace = colorSpace; IndexTable = Some indexedTable; Decode = decode }
                                                        |> ImageColorSpaceData.Indexable
                                                        |> Some
                                                    | EqualTo PdfName.ICCBased -> None
                                                    
                                                    | _ -> failwithf "Invalid token, current colorspace pdfArray are %A" array
                                                | _ -> failwithf "Invalid token, current colorspace pdfArray are %A" array

                                            | :? PdfName as name -> 
                                                { ColorSpace = fromPdfName name; IndexTable = None; Decode = decode  }
                                                |> ImageColorSpaceData.Indexable
                                                |> Some
                                            | _ -> failwithf "Invalid token, current color space is %A" colorSpace 
                                        ))

                                            
                                
                              }
                            :> IIntegratedRenderInfoIM

                        |_ -> failwith "Not implemented"


                    match isShowingText, renderInfo.TagIM with 
                    | true, IntegratedRenderInfoTagIM.Text ->
                        let renderInfo = renderInfo :?> IntegratedTextRenderInfo

                        match renderInfo.TextRenderInfo.GetText().Trim() with 
                        | "" -> ()
                        | _ -> 
                            renderInfo.TextRenderInfo.PreserveGraphicsState()
                            concatedTextInfos.Add(renderInfo)

                    | false, IntegratedRenderInfoTagIM.Text -> ()
                    | true, _ -> failwith "Invalid token"
                    | false, _ -> 
                        let predicate _ filter =
                            filter renderInfo
                        let filtered = Map.filter predicate prediateMapping
                        match filtered.IsEmpty with 
                        | false ->
                            let tokens =
                                filtered
                                |> Map.toList
                                |> List.map fst

                            renderInfo.Value.PreserveGraphicsState()

                            parsedRenderInfos.Add(renderInfo)

                            currentRenderInfoToken <- Some tokens
                            currentRenderInfo <- Some renderInfo
                            currentRenderInfoStatus <- CurrentRenderInfoStatus.Selected

                        | true -> 
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



type internal NonInitialCallbackablePdfCanvasProcessor (listener: FilteredEventListenerEx, additionalContentOperators) =
    inherit PdfCanvasProcessor(listener, additionalContentOperators)

    let mutable initClippingBox = None

    member internal x.GetCurrentResource() = x.GetResources()

    member internal x.PaintShading_InClippingArea(pdfName: PdfName) =
        match listener.EventTypes with 
        | List.Contains EventType.RENDER_PATH & List.Contains EventType.CLIP_PATH_CHANGED ->
            let shading = x.GetCurrentResource().GetShading(pdfName)
            let currentRenderInfo = listener.CurrentRenderingClippingPathInfo_Integrated
            

            match currentRenderInfo with 
            | Some _ -> 
                match currentRenderInfo.Value with 
                | IIntegratedRenderInfoIM.Path renderInfo -> 
                    let canvasTag = 
                        match renderInfo.IsClippingPath with 
                        | true -> renderInfo.PathRenderInfo.GetCanvasTagHierarchy()
                        | false -> failwith "currentRenderInfo should be clipping path here"

                    let color = new PdfShadingColor(shading, x.GetGraphicsState().GetCtm())
                    let gsState = renderInfo.PathRenderInfo.GetGraphicsState()
                    gsState.SetFillColor(color)
                    let newPathRenderInfo =
                        PdfShadingPathRenderInfo(color, Stack canvasTag, gsState, renderInfo.PathRenderInfo.GetPath())
                    
                    for operatorRange in renderInfo.AccumulatedPathOperatorRanges do 
                        listener.AddPathOperatorRange operatorRange
                
                    x.EventOccurred(newPathRenderInfo, EventType.RENDER_PATH)


                | IIntegratedRenderInfoIM.Text renderInfo -> 
                    //renderInfo.TextRenderInfo.GetCanvasTagHierarchy()
                    failwith "Not implemented"
                | IIntegratedRenderInfoIM.Image _ -> failwith "currentRenderInfo should not be Image here"

                
            | None -> 
                match initClippingBox with 
                | Some clippingBox ->
                    match listener.GetXObjectClippingBox() with 
                    | XObjectClippingBoxState.IntersectedSome rect2 ->  
                        let a = Rectangle.equalableValue rect2
                        let b = Rectangle.equalableValue clippingBox
                        match a = b with 
                        | true ->
                            match listener.GetCurrentClippingPathInfo() with 
                            | ClippingPathInfoState.Init ->
                                let gs = x.GetGraphicsState()
                                let clippingPath = gs.GetClippingPath()

                                let canvasTag = [||] :> IList<_>

                                let color = new PdfShadingColor(shading, gs.GetCtm())
                                let gsState = gs
                                gsState.SetFillColor(color)

                                let newPathRenderInfo =
                                    PdfShadingPathRenderInfo(color, Stack canvasTag, gsState, clippingPath)
                                
                                //let operatorRanges =
                                //    [
                                //        { Operator = PdfLiteral("re")
                                //          Operands = [|
                                //            PdfLiteral("re") :> PdfObject
                                //            PdfNumber(rect2.GetXF())
                                //            PdfNumber(rect2.GetYF())
                                //            PdfNumber(rect2.GetWidthF())
                                //            PdfNumber(rect2.GetHeightF())
                                //          |]
                                //          }
                                //    ]

                                //for operatorRange in operatorRanges do 
                                //    listener.AddPathOperatorRange operatorRange
                
                                x.EventOccurred(newPathRenderInfo, EventType.RENDER_PATH)

                            | _ ->
                                failwith "current RenderingClippingPathInfo should be exists before Paint Shading"
                         
                        | false -> failwith "current RenderingClippingPathInfo should be exists before Paint Shading"

                    | _ -> failwith "current RenderingClippingPathInfo should be exists before Paint Shading"

                | None -> failwith "current RenderingClippingPathInfo should be exists before Paint Shading"



        | _ -> ()

    member this.Listener = listener

    member internal this.InitClippingPath(page: PdfPage) =
        let clippingPath = new Path()
        let initBox = page.GetCropBox() |> Rectangle.applyMargin (Margin.Create(Constants.MAXIMUM_MM_WIDTH / 2.)) 
        initClippingBox <- Some initBox
        clippingPath.Rectangle(initBox);
        this.GetGraphicsState().SetClippingPath(clippingPath)

        do 
            match this.GetEventListener() with 
            | :? FilteredEventListenerEx as listener -> 
                listener.SetXObjectClippingBox(initBox)
            | _ -> ()


        let gs = this.GetGraphicsState()
        this.EventOccurred(new ClippingPathInfo(gs, gs.GetClippingPath(), gs.GetCtm()), EventType.CLIP_PATH_CHANGED);
        listener.InitClippingPathInfo()

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

        this.ProcessContent(page.GetContentBytes(), page.GetResources())


    new (listener) = NonInitialCallbackablePdfCanvasProcessor(listener, dict [])


type internal RenderInfoAccumulatableContentOperator (originalOperator, invokeXObjectOperator, fCurrentResource) =
    member this.OriginalOperator: IContentOperator = originalOperator

    member this.Invoke(processor: PdfCanvasProcessor, operator: PdfLiteral, operands, invokeOperatorRange) =
        let processor = processor :?> NonInitialCallbackablePdfCanvasProcessor
        let operatorName = operator.Text()
        match operatorName with 
        | EQ q -> processor.Listener.SaveGS(processor.GetGraphicsState())
        | EQ Q -> processor.Listener.RestoreGS()
        | ContainsBy [Tj; TJ; "'"; "''"] -> 
            processor.Listener.BeginShowText()
        | _ -> ()

        if operatorName <> Do then 
            try 
                this.OriginalOperator.Invoke(processor, operator, operands)
            with ex ->
                if ex.Message = "Dictionary doesn't have supported font data." 
                then
                    Logger.warning (sprintf "Skip checking MM font %A" operator)
                    let size = (operands.[1]) :?> PdfNumber
                    let size = size.FloatValue()
                    processor.GetGraphicsState().SetFontSize(size)

                else reraise()

        else 
            let resource: PdfResources = fCurrentResource processor
            let image = resource.GetImage(operands.[0] :?> PdfName)
            match image with 
            | null -> 
                match invokeXObjectOperator with 
                | true ->
                    processor.Listener.SaveGS_XObject(processor.GetGraphicsState())
                    this.OriginalOperator.Invoke(processor, operator, operands)
                    processor.Listener.RestoreGS_XObject()

                | false -> ()

            | _ -> 
                this.OriginalOperator.Invoke(processor, operator, operands)

        match operatorName with 
        | EQ gs -> processor.Listener.ProcessGraphicsStateResource(processor.GetGraphicsState())
        | EQ sh -> processor.PaintShading_InClippingArea(operands.[0] :?> PdfName)
        | ContainsBy [Tj; TJ; "'"; "''"] -> 
            processor.Listener.EndShoeText({Operator = operator; Operands = ResizeArray operands})

        | ContainsBy [m; v; c; y; l; h; re] -> 
            processor.Listener.AddPathOperatorRange({ Operator = operator; Operands = ResizeArray(operands)})
        | _ -> ()

        invokeOperatorRange()

    interface IContentOperator with 
        member this.Invoke(processor,operator,operands) =
            this.Invoke(processor, operator, operands, ignore)
            

type internal ReaderPdfCanvasProcessor(listener: FilteredEventListenerEx, additionalContentOperators) =
    inherit NonInitialCallbackablePdfCanvasProcessor(listener, additionalContentOperators)

    override this.RegisterContentOperator(operatorString: string, operator: IContentOperator) : IContentOperator =
        let wrapper = new RenderInfoAccumulatableContentOperator(operator, (true), fun processor ->
            processor.GetCurrentResource()
        )
        let formOperator = base.RegisterContentOperator(operatorString, wrapper)
        
        match formOperator with 
        | :? RenderInfoAccumulatableContentOperator as wrapper -> wrapper.OriginalOperator
        | _ -> formOperator




type NonInitialClippingPathPdfDocumentContentParser(pdfDocument) =
    inherit PdfDocumentContentParser(pdfDocument)

    override this.ProcessContent(pageNumber, renderListener, additionalContentOperators) =  
        
        let listener = (renderListener :> IEventListener) :?> FilteredEventListenerEx
        let processor = new ReaderPdfCanvasProcessor(listener, additionalContentOperators)
        processor.ProcessPageContent(pdfDocument.GetPage(pageNumber))
        renderListener

type StoppedParsedRenderInfoIMs =
    { PageNumber: PageNumber
      RenderInfoStoppedException: RenderInfoStoppedException
      ParsedInfos: seq<IIntegratedRenderInfoIM> }


[<RequireQualifiedAccess>]
type StoppableParsedRenderInfoIMs =
    | Stopped of StoppedParsedRenderInfoIMs
    | NonStopped of seq<IIntegratedRenderInfoIM>

[<RequireQualifiedAccess>]
module NonInitialClippingPathPdfDocumentContentParser =
    open Listeners

    let parse (pageNum: int) (renderInfoSelector: RenderInfoSelector) (parser: NonInitialClippingPathPdfDocumentContentParser) =
        let et = RenderInfoSelector.toEventTypes renderInfoSelector

        match et with 
        | [] -> [] :> seq<IIntegratedRenderInfo>
        | _ ->
            let infos = 
                RenderInfoSelector.checkNonImageSelectorExists [renderInfoSelector]
                let renderInfoSelectorMapping = Map.ofList [{ Name= "Untitled"}, renderInfoSelector]
                let listener = new FilteredEventListenerEx(renderInfoSelectorMapping)
                parser.ProcessContent(pageNum, listener).ParsedRenderInfos
                |> Seq.map(fun m -> m :?> IIntegratedRenderInfo)


            infos

    let parseIM (pageNum: int) (renderInfoSelector: RenderInfoSelector) (parser: NonInitialClippingPathPdfDocumentContentParser) =
        let et = RenderInfoSelector.toEventTypes renderInfoSelector

        match et with 
        | [] -> [] :> seq<IIntegratedRenderInfoIM>
        | _ ->
            let renderInfoSelectorMapping = Map.ofList [{ Name= "Untitled"}, renderInfoSelector]
            let listener = new FilteredEventListenerEx(renderInfoSelectorMapping)
            parser.ProcessContent(pageNum, listener).ParsedRenderInfos
        
    let parseIMStoppable (pageNum: int) (renderInfoSelector: RenderInfoSelector) (parser: NonInitialClippingPathPdfDocumentContentParser) =
        let et = RenderInfoSelector.toEventTypes renderInfoSelector

        match et with 
        | [] -> [] :> seq<IIntegratedRenderInfoIM> |>StoppableParsedRenderInfoIMs.NonStopped
        | _ ->
            let renderInfoSelectorMapping = Map.ofList [{ Name= "Untitled"}, renderInfoSelector]
            let listener = new FilteredEventListenerEx(renderInfoSelectorMapping)
            try
                parser.ProcessContent(pageNum, listener).ParsedRenderInfos
                |> StoppableParsedRenderInfoIMs.NonStopped
            with :? RenderInfoStoppedException as ex ->
                { RenderInfoStoppedException = ex 
                  PageNumber = PageNumber pageNum
                  ParsedInfos = listener.ParsedRenderInfos }
                |> StoppableParsedRenderInfoIMs.Stopped
                
