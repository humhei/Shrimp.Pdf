﻿namespace Shrimp.Pdf.Extract

open iText.Kernel.Pdf

#nowarn "0104"
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open Shrimp.Pdf.Constants
open Shrimp.Pdf.PdfNames
open Shrimp.Pdf.Parser
open Shrimp.FSharp.Plus
open Akkling
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Xobject
open Shrimp.Pdf.SlimFlow




[<AutoOpen>]
module _Extract =
    let internal reuse name parameters f = Reuse(f = f, flowName = FlowName.Override(name, parameters))
    



    type internal TargetRenewableNewPageInfoElement =
        { RenewableInfos: RenewableInfo list
          TagColor: FsColor option
          BoundPredicate: (RenewablePathInfo -> bool) option
          RectangleTransform: RectangleTransform option
          BorderKeepingPageNumbers: int list
          TagInfosTransform: AffineTransformRecord option }
    with    
        static member Create(infos: RenewableInfo list, borderKeppingPageNumbers, ?tagColor, ?boundPredicate, ?rectangleTransform) =
            { RenewableInfos = infos
              TagColor = tagColor
              BorderKeepingPageNumbers = borderKeppingPageNumbers
              BoundPredicate = boundPredicate
              RectangleTransform = rectangleTransform 
              TagInfosTransform = None }

    type internal TargetNewInfosInOriginPageElement =
        { RenewableInfos: RenewableInfo list
          NewBound: IndexedBound
          Bound: IndexedBound
          BoundPredicate: RenewablePathInfo -> bool
          BorderKeepingPageNumbers: int list
          TextPickerTagColor: FsColor option }
    with 
        member x.RectangleTransform =
            { OldRect = x.Bound.Bound
              NewRect = x.NewBound.Bound }

    [<RequireQualifiedAccess>]
    type internal TargetNewInfosElementUnion =
        | NewPageInfo of TargetRenewableNewPageInfoElement
        | OriginPage of TargetNewInfosInOriginPageElement
    with 
        member x.Infos =
            match x with 
            | TargetNewInfosElementUnion.NewPageInfo v -> v.RenewableInfos
            | TargetNewInfosElementUnion.OriginPage v -> v.RenewableInfos

        member x.BorderKeepingPageNumbers =
            match x with 
            | TargetNewInfosElementUnion.NewPageInfo v -> v.BorderKeepingPageNumbers
            | TargetNewInfosElementUnion.OriginPage v -> v.BorderKeepingPageNumbers


        member x.TagColor =
            match x with 
            | TargetNewInfosElementUnion.NewPageInfo v -> v.TagColor
            | TargetNewInfosElementUnion.OriginPage v -> v.TextPickerTagColor

    [<RequireQualifiedAccess>]
    type ExtractToTwoPages_PageBoxSetter =
        | ToSelection of pageBoxKind: PageBoxKind * Margin
        | KeepOrigin


    [<RequireQualifiedAccess>]
    type internal TargetRenewablePageInfo<'userState> =
        | EmptyPage of TargetPageBox
        | NewPageCase of TargetPageBox * TargetRenewableNewPageInfoElement * writerPageSetter: (SlimWriterPageSetter) * background: SlimBackground list
        | Non
        | NewInfosInOriginPage of TargetNewInfosInOriginPageElement list
        | SplitToTwoPages of 
             RenewableInfo list * Selector<'userState> * pageboxSetter: ExtractToTwoPages_PageBoxSetter * secondarySelector: (Rectangle -> Selector<'userState>) option

    type internal TargetRenewablePageInfo =
        static member NewPage(targetPageBox, targetElement, ?writerPageSetter, ?background) =
            TargetRenewablePageInfo.NewPageCase(
                targetPageBox,
                targetElement,
                defaultArg writerPageSetter SlimWriterPageSetter.Ignore,
                defaultArg background []
            )


    let internal writeInfos maxLevel_plus writer (writerPageBox: Rectangle) writerCanvas (infos: RenewableInfo list) =
        match infos with 
        | [] -> ()
        | _ ->
            let maxLevel = 
                infos
                |> List.map(fun m -> m.ContainerID.Length)
                |> List.max
                |> fun m -> m - 1 

            let maxLevel = maxLevel

            let rec loop (writerCanvas: OffsetablePdfCanvas) (xobjectGsState: al1List<FsParserGraphicsStateValue> option) (level: int) (infos: RenewableInfo list) =
                match level = maxLevel, xobjectGsState with 
                | true, None -> 
                    infos 
                    |> List.splitIfChangedByKey(fun m -> 
                        List.tryItem level m.GsStates.AsList
                        |> Option.map(fun m -> m.CustomHashCode)
                    )
                    |> List.iter(fun (customHashCode, infos) ->
                        let gs = 
                            match customHashCode with 
                            | None -> None
                            | Some _ -> 
                                let gs = infos.Head.GsStates.AsList.[level]
                                let gs = 
                                    gs.AsList
                                    |> AtLeastOneList.TryCreate

                                gs
                                |> Option.map(fun gs ->
                                    gs
                                    |> AtLeastOneList.map(fun m -> m.FsExtState)
                                    |> FsExtGState.Concat
                                )

                        match gs with 
                        | None -> ()
                        | Some gsState ->
                            writerCanvas.SaveState() |> ignore
                            gsState.Renewable().ApplyCtm_WriteToCanvas(writerCanvas)
                            

                        let infos = infos.AsList
                        infos
                        |> List.splitIfChangedByKey(fun m -> m.ClippingPathInfos.XObjectClippingBoxState.Serializable)
                        |> List.iter(fun (xobjectRect, infos) ->
                            let infos = infos.AsList
                            let writeInfos() =
                                infos
                                |> List.splitIfChangedByKey(fun m -> m.ClippingPathInfos.ClippingPathInfoState)
                                |> List.iter(fun (clippingPathInfo, infos) ->   
                                    let infos = infos.AsList
                                    let writeInfos() =
                                        for info in infos do
                                            info.CopyToDocument(writer, writerCanvas.GetResources()).ApplyCtm_WriteToCanvas(writerCanvas)
    
                                    match clippingPathInfo with 
                                    | ClippingPathInfoState.Init -> writeInfos()
                                    | ClippingPathInfoState.Intersected (clippingPathInfo) -> 
                                        clippingPathInfo.Renewable().ApplyCtm_WriteToCanvas(writerCanvas, (fun writerCanvas ->
                                            writeInfos()
                                            writerCanvas
                                        ))
                                )

                            match xobjectRect with 
                            | SerializableXObjectClippingBoxState.IntersectedNone -> failwith "Invalid token: visble render info XObjectClippingBoxState cannot be IntersectedNone"
                            | SerializableXObjectClippingBoxState.Init -> writeInfos() 
                            | SerializableXObjectClippingBoxState.IntersectedSome rect ->
                                match rect.Width, rect.Height with 
                                | BiggerThan MAXIMUM_MM_WIDTH, BiggerThan MAXIMUM_MM_WIDTH -> writeInfos()
                                | _ ->
                                    PdfCanvas.useCanvas writerCanvas (fun writerCanvas ->
                                        writerCanvas
                                            .Rectangle(rect.AsRectangle)
                                            .Clip()
                                            .EndPath()
                                            |> ignore

                                        writeInfos()

                                        writerCanvas

                                    )
                        )

                        match gs with 
                        | None -> ()
                        | Some _ ->
                            writerCanvas.RestoreState() |> ignore
                            

                    )


                | _ ->

                    match xobjectGsState with 
                    | Some gsStates ->
                        let gsState =
                            gsStates
                            |> AtLeastOneList.map(fun m -> m.FsExtState)
                            |> FsExtGState.Concat

                        writerCanvas.SaveState() |> ignore

                        gsState.Renewable().ApplyCtm_WriteToCanvas(writerCanvas)
                        //gsState.AsList
                        //|> List.iter(fun gsState ->
                        //    gsState.Renewable().ApplyCtm_WriteToCanvas(writerCanvas)
                        //)
                        let rect = 
                            infos 
                            |> List.map(fun m -> m.VisibleBound1)
                            |> AtLeastOneList.Create
                            |> Rectangle.ofRectangles

                        let writerCanvas2 =  
                            let offset_X = writerCanvas.XOffset + rect.GetX() - writerPageBox.GetX()
                            let offset_Y = writerCanvas.YOffset + rect.GetY() - writerPageBox.GetY()
           
                            let xobject = PdfFormXObject(rect)
                            writerCanvas.AddXObject(xobject) |> ignore
                            let group = PdfTransparencyGroup()
                            group.SetIsolated(false)
                            group.SetKnockout(false)
                            xobject.SetGroup(group) |> ignore
                            OffsetablePdfCanvas(xobject, writer, offset_X, offset_Y)
                
                        loop writerCanvas2 None level infos

                        writerCanvas.RestoreState()
                        |> ignore


                    | None ->
                        infos
                        |> List.splitIfChangedByKey(fun m ->
                             m.ContainerID.[0..level+1]
                        )
                        |> List.iter(fun ((containerID), infos) ->
                            let isXObject = containerID.Length = level + 2 
                            match isXObject with 
                            | true ->
                                infos.AsList
                                |> List.splitIfChangedByKey(fun m -> 
                                    m.GsStates.AsList.[level].CustomHashCode
                                )

                                |> List.iter(fun (customHashCode, infos1) -> 
                                    let goToNext() =
                                        let gsStates = 
                                            infos1.Head.GsStates.AsList.[level].AsList

                                        match gsStates with 
                                        | [] -> loop writerCanvas (None) (level+1) infos1.AsList
                                        | _ ->
                                            loop writerCanvas (AtLeastOneList.TryCreate gsStates) (level+1) infos1.AsList

                                            //failwithf ""
                                            //let gsStatesCtnIds = gsStates.[level].ContainerID
                                            //match gsStatesCtnIds = containerID.[0..level+1] with 
                                            //| true -> 
                                            //    let gsStates = 
                                            //        gsStates
                                            //        |> List.tryItem level
                                            //        |> Option.toList

                                            //    loop writerCanvas (AtLeastOneList.TryCreate gsStates) (level+1) infos1.AsList

                                            //| false -> 
                                            //    match gsStatesCtnIds = containerID.[0..level] with
                                            //    | true ->
                                            //        let gsStates = 
                                            //            gsStates
                                            //            |> List.tryItem level
                                            //            |> Option.toList

                                            //        loop writerCanvas (AtLeastOneList.TryCreate gsStates) (level+1) infos1.AsList
                                            //    | false -> failwithf "Not implemented"
                                    

                                    match infos1.Length = infos.Length with 
                                    | true -> goToNext()
                                    | false -> goToNext()
                                        //let r = 
                                        //    infos.AsList
                                        //    |> List.map(fun m -> m.ContainerID, m.GsStates.[0..level])
                                        //    |> List.distinctBy snd
                                        //failwithf "Not implemented %A" r
                                )
                            | false -> loop writerCanvas None (level+1) infos.AsList

                        )


            loop writerCanvas None 0 infos



    let internal extractVisibleRenewableInfosToWriter 
        (configuration: Configuration)
        (args: PageModifingArguments<_>) 
        selector
        (infosSplitter: RenewableInfo list -> list<TargetRenewablePageInfo<_>>)
        keepOriginPage
        suffixOperation
        (borderKeepingPageNumbers: int list)
        (reader: PdfDocument)
        (writer: PdfDocumentWithCachedResources) =
        let parser = new NonInitialClippingPathPdfDocumentContentParser(reader)
        let pageNumber = args.PageNum
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let keepBorder = List.contains pageNumber borderKeepingPageNumbers
        
        let loggingPageCountInterval = loggingPageCountInterval.Value

        let logInfo (text) = 
            let logger: PageLogger =
                { LoggerLevel = configuration.LoggerLevel 
                  LoggingPageCountInterval = loggingPageCountInterval }

            logger.Log(text, alwaysPrintingConsole_If_Info = true)
       


        let infos =
            let selector = 
                Selector.toRenderInfoSelector args selector

            NonInitialClippingPathPdfDocumentContentParser.parseIM pageNumber selector parser


        let infos = 
            let infos = 
                infos
                |> List.ofSeq
                |> List.map(fun m -> m.AsUnion.SetVisibleBound0())
                |> List.filter(fun m -> m.LazyVisibleBound0.IsSome)

            infos
            |> List.map(fun info ->
                info.Renewable()
            )
            |> List.map(fun m -> m.UpdateVisibleBound1())

        stopWatch.Stop()
        logInfo(fun () ->
            sprintf "    extracting page %d/%d, found infos %d in %O" pageNumber args.TotalNumberOfPages infos.Length stopWatch.Elapsed
        ) pageNumber


        stopWatch.Restart()

        let splittedInfos = 
            infos
            |> infosSplitter


        splittedInfos
        |> List.iter(fun (targetPageInfo) ->
            let readerPage = args.Page

            match keepOriginPage with 
            | true -> 
                let writerPage = readerPage.CopyTo(writer)
                writer.AddPage(writerPage)
                |> ignore

            | false -> ()

            let writeAreaInfos (element: TargetNewInfosElementUnion) (writerCanvas: OffsetablePdfCanvas) (writerPage: PdfPage) =
                let pageBox = writerPage.GetActualBox()
                let infos = element.Infos

                let writeInfos2 writerCanvas infos = writeInfos 0 writer pageBox writerCanvas infos
                let writeInfos1 infos = writeInfos 1 writer pageBox writerCanvas infos
                let writeInfos infos = writeInfos 0 writer pageBox writerCanvas infos

                let infoChoices(boundPredicate) =   
                    let infoChoices =
                        infos
                        |> List.map (fun info ->
                            match info.OriginInfo with 
                            | IIntegratedRenderInfoIM.Vector vector ->
                                if 
                                    match info with 
                                    | RenewableInfo.Path info ->
                                        boundPredicate info

                                    | _ -> false

                                then Choice1Of3 info
                                else
                                    match element.TagColor with 
                                    | None -> Choice3Of3 info
                                    | Some tagColor ->
                                        let isTagColor =
                                            IAbstractRenderInfo.ColorIs(FillOrStrokeOptions.FillOrStroke, fun color ->
                                                FsColor.OfItextColor color
                                                |> FsColor.equal tagColor 
                                            ) vector

                                        match isTagColor with 
                                        | true -> Choice2Of3 info
                                        | false -> Choice3Of3 info

                            | IIntegratedRenderInfoIM.Pixel _ -> Choice3Of3 info
                        )

                    let bounds =
                        infoChoices
                        |> List.choose(function
                            | Choice1Of3 v -> Some v
                            | _ -> None
                        )

                    let infos = 
                        infoChoices
                        |> List.choose(function
                            | Choice3Of3 v -> Some v
                            | _ -> None
                        )

                    let tagInfos = 
                        infoChoices
                        |> List.choose(function
                            | Choice2Of3 v -> Some v
                            | _ -> None
                        )


                    {| Bounds = bounds 
                       TagInfos = tagInfos
                       Infos = infos |}

                let writeInfosWithTransform (boundPredicate) (rect: RectangleTransform) =
                    let infoChoices = infoChoices boundPredicate
                    let newArea = rect.NewRect
                    let oldArea = rect.OldRect


                    let transform_scale = 
                        let scaleX = newArea.GetWidthF() / oldArea.GetWidthF()
                        let scaleY = newArea.GetHeightF() / oldArea.GetHeightF()

                        //AffineTransformRecord.DefaultValue
                        { 
                            AffineTransformRecord.DefaultValue with 
                                ScaleX = scaleX
                                ScaleY = scaleY
                                //TranslateX = 
                        }
                        
                    let scaledArea = 
                        AffineTransform.ofRecord(transform_scale).Transform(
                            oldArea
                        )

                    let transform_translate =
                        let translateX = newArea.GetXF() - scaledArea.GetXF()
                        let translateY = (newArea.GetYF() - scaledArea.GetYF())
                        { AffineTransformRecord.DefaultValue with 
                            TranslateX = translateX
                            TranslateY = translateY 
                        }



                    PdfCanvas.useCanvas writerCanvas (fun writerCanvas ->
                        writerCanvas.SaveState() |> ignore

                        writerCanvas
                            .ConcatMatrix(AffineTransform.ofRecord transform_translate)
                            .ConcatMatrix(AffineTransform.ofRecord transform_scale)
                            |> ignore

                        writeInfos(infoChoices.Infos)

                        if keepBorder
                        then writeInfos(infoChoices.TagInfos)

                        writerCanvas.RestoreState() |> ignore
                        if keepBorder
                        then writeInfos(infoChoices.Bounds)

                        writerCanvas
                    )

                match element with 
                | TargetNewInfosElementUnion.NewPageInfo element ->     
                    match element.BoundPredicate with 
                    | None -> writeInfos(infos)
                    | Some boundPredicate -> 
                        match element.RectangleTransform with 
                        | Some rect ->  
                            writeInfosWithTransform boundPredicate rect
                        | None ->

                            let infoChoices = infoChoices boundPredicate
                            PdfCanvas.useCanvas writerCanvas (fun writerCanvas ->
                                match element.TagInfosTransform with 
                                | None -> writeInfos(infoChoices.Infos)
                                | Some _ -> writeInfos1 (infoChoices.Infos)

                                let existsKeepBorderInfos =
                                    keepBorder &&
                                    (
                                        infoChoices.Bounds.Length >= 1
                                        || infoChoices.TagInfos.Length >= 1
                                    )


                                if existsKeepBorderInfos
                                then 
                                    let stream = writerPage.NewContentStreamAfter()
                                    let cuttingDieLayerEnum =
                                        match infoChoices.Bounds.Length >= 1, infoChoices.TagInfos.Length >= 1 with 
                                        | true, false -> CuttingDieShpLayerInfosEnum.CuttingDie
                                        | false, true -> CuttingDieShpLayerInfosEnum.TagInfos
                                        | true, true -> CuttingDieShpLayerInfosEnum.CuttingAndTagInfos
                                        | false, false -> failwithf "Invalid token"

                                    stream.Put(PdfName.ShpLayerCuttingDie, PdfString(cuttingDieLayerEnum.Text()))
                                    |> ignore

                                    let boundingCanvas = 
                                        OffsetablePdfCanvas(stream, writerPage.GetResources(), writer)
                                        
                                    match element.TagInfosTransform with 
                                    | None -> ()
                                    | Some tagInfosTransform ->
                                        boundingCanvas.SaveState() |> ignore
                                        boundingCanvas
                                        |> PdfCanvas.concatMatrixByTransformRecord tagInfosTransform
                                        |> ignore

                                    let bounds = infoChoices.Bounds |> List.map(fun m -> m.SetContainerToPage())
                                    let tagInfos = infoChoices.TagInfos |> List.map(fun m -> m.SetContainerToPage())

                                    writeInfos2 boundingCanvas (bounds)
                                    writeInfos2 boundingCanvas (tagInfos)

                                    match element.TagInfosTransform with 
                                    | None -> ()
                                    | Some _ ->
                                        boundingCanvas.RestoreState() |> ignore



                                writerCanvas
                            )


                | TargetNewInfosElementUnion.OriginPage (element) ->
                    writeInfosWithTransform element.BoundPredicate element.RectangleTransform

            let rec loop targetPageInfo = 
                match targetPageInfo with 
                | TargetRenewablePageInfo.SplitToTwoPages (infos, selector, pageBoxSetter, secondarySelector) ->

                    let selector = Selector.toRenderInfoSelector args selector
                    let predication = RenderInfoSelector.toRenderInfoIMPredication selector
                    let infos1, infos2 =
                        infos
                        |> List.partition (fun m -> predication m.OriginInfo)

                    let rect = 
                        let rects = 
                            infos1
                            |> List.choose(fun m -> IIntegratedRenderInfoIM.getBound BoundGettingStrokeOptions.WithStrokeWidth m.OriginInfo)
                            |> AtLeastOneList.TryCreate
                            
                        match rects with 
                        | None -> failwithf "Cannot select any object by selector %A" selector
                        | Some rects ->
                            let rect = 
                                rects
                                |> Rectangle.ofRectangles

                            rect

                    let infos1, infos2 =
                        match secondarySelector with 
                        | None -> infos1, infos2
                        | Some secondarySelector ->
                            let secondarySelector = 
                                secondarySelector rect
                                |> Selector.toRenderInfoSelector args 

                            let predication = RenderInfoSelector.toRenderInfoIMPredication secondarySelector
                            let infos2_1, infos2_2 =
                                infos2
                                |> List.partition (fun m -> predication m.OriginInfo)
                                
                            infos1 @ infos2_1, infos2_2

                            

                    let targetPageBox = 
                        match pageBoxSetter with 
                        | ExtractToTwoPages_PageBoxSetter.KeepOrigin ->  TargetPageBox None
                        | ExtractToTwoPages_PageBoxSetter.ToSelection (pageBoxKind, margin) ->
                            let rect = 
                                rect
                                |> Rectangle.applyMargin margin

                            TargetPageBox (Some (pageBoxKind, rect))

                    let asTargetPageInfo infos  =
                        let element = 
                            { RenewableInfos = infos
                              TagColor = None 
                              BoundPredicate = None 
                              RectangleTransform = None 
                              BorderKeepingPageNumbers = borderKeepingPageNumbers
                              TagInfosTransform = None }


                        TargetRenewablePageInfo.NewPage(targetPageBox, element, SlimWriterPageSetter.Ignore)

                    loop (asTargetPageInfo infos1)
                    loop (asTargetPageInfo infos2)



                | TargetRenewablePageInfo.EmptyPage targetPageBox ->
                    let writerPage = writer.AddNewPage(PageSize(readerPage.GetActualBox()))
                    match targetPageBox.Value with 
                    | None -> 
                        writerPage.SetPageBoxToPage(readerPage) |> ignore
                    | Some (pageBoxKind, pageBox) -> 
                        writerPage.SetPageBoxToPage(readerPage) |> ignore

                        writerPage.SetPageBox(pageBoxKind, pageBox)
                        |> ignore

                | TargetRenewablePageInfo.Non -> ()
                | TargetRenewablePageInfo.NewPageCase (targetPageBox, infos, writerPageSetter, background) ->
                    let writerPage, writerCanvas, tagInfosTransforms = writerPageSetter.GenerateWriterPageAndCanvas(readerPage, writer, background, writeInfos)
                            
                    let layerOptions =
                        background
                        |> List.choose(fun m -> m.LayerName)
                        |> List.tryLast

                    PdfCanvas.useCanvas writerCanvas (fun writerCanvas ->
                        let writeInfos() = 
                            writerCanvas
                                .WriteLiteral("0 G\n")
                                .WriteLiteral("0 g\n")
                                |> ignore

                            let infos = { infos with TagInfosTransform = tagInfosTransforms }

                            writeAreaInfos (TargetNewInfosElementUnion.NewPageInfo infos) writerCanvas writerPage

                        match layerOptions with 
                        | None -> writeInfos()
                        | Some layerName -> 
                            let currentLayer = layerName.CurrentLayer.CreateLayer(writer)
                            writerCanvas.BeginLayerUnion(currentLayer)
                            |> ignore

                            writeInfos()

                            writerCanvas.EndLayerUnion(currentLayer)
                            |> ignore

                        
                        writerCanvas

                    ) |> ignore
                    suffixOperation (writerPage, writerCanvas)

                    //writerPage.SetPageBoxToPage(readerPage) |> ignore



                    match writerPageSetter, infos.RenewableInfos with 
                    | SlimWriterPageSetter.Ignore, _ 
                    | _, [] -> 
                        match targetPageBox.Value with 
                        | Some (pageBoxKind, pageBox) -> 
                            writerPage.SetPageBoxToPage(readerPage)
                            |> ignore<PdfPage>

                            writerPage.SetPageBox(pageBoxKind, pageBox) 
                            |> ignore<PdfPage>

                        | None -> 
                            writerPage.SetPageBoxToPage(readerPage)
                            |> ignore<PdfPage>

                    | SlimWriterPageSetter.Some setter, _ ->
                        setter.InvokePage (readerPage, writerPage, writer)

                | TargetRenewablePageInfo.NewInfosInOriginPage (infos) ->
                    let writerPage = writer.AddNewPage(PageSize(readerPage.GetActualBox()))
                    writerPage.SetPageBoxToPage(readerPage) |> ignore
                    let writerCanvas = new OffsetablePdfCanvas(writerPage.GetContentStream(0), writerPage.GetResources(), writer)
                    PdfCanvas.useCanvas writerCanvas (fun writerCanvas ->
                        writerCanvas
                            .WriteLiteral("0 G\n")
                            .WriteLiteral("0 g\n")
                            |> ignore

                        infos
                        |> List.iter(fun infos ->    
                            writeAreaInfos (TargetNewInfosElementUnion.OriginPage infos) writerCanvas writerPage
                        )

                        writerCanvas
                    )
                    suffixOperation (writerPage, writerCanvas)


            loop targetPageInfo
        )


        stopWatch.Stop()
        logInfo(fun () ->
            sprintf "    writedAreaInfos in %O" stopWatch.Elapsed
        ) pageNumber

        splittedInfos


    type Reuses with
        static member ExtractIM(pageSelector: PageSelector, selector, ?slimFlow: SlimFlowUnion<_, _>, ?keepOriginPage) =    
            let slimFlow =
                match slimFlow with 
                | None -> None
                | Some slimFlow ->
                    slimFlow
                    <+>
                    SlimModifyPage.MapInfos(fun args infos ->
                        infos.Background
                        |> List.iter(fun bk -> bk.Internal_RecoverVisibleBound_ForWrite())

                        infos
                    )
                    |> Some
            let keepOriginPage = defaultArg keepOriginPage false
            fun (flowModel: FlowModel<_>) (splitDocument: SplitDocument) ->
                let flowModel =
                    { flowModel with 
                        Configuration = 
                            { flowModel.Configuration with SlimableFlowLoggingOptions = SlimableFlowLoggingOptions.Slim }
                    } 
                let reader = splitDocument.Reader
                let totalNumberOfPages = reader.GetNumberOfPages()
                let pageNumbers = reader.GetPageNumbers(pageSelector)
                let borderKeepingPageNumbers = [1..totalNumberOfPages]

                let bks = ResizeArray()

                pageNumbers
                |> List.iter(fun pageNumber ->
                    let readerPage = reader.GetPage(pageNumber)
                    let args: PageModifingArguments<_> = 
                        { UserState = flowModel.UserState
                          Page = readerPage
                          TotalNumberOfPages = totalNumberOfPages
                          PageNum = pageNumber
                        }

                    extractVisibleRenewableInfosToWriter
                        flowModel.Configuration
                        args
                        selector
                        (fun infos -> 
                            match slimFlow with 
                            | None -> [TargetRenewablePageInfo.NewPage(TargetPageBox None, TargetRenewableNewPageInfoElement.Create (infos, borderKeepingPageNumbers), SlimWriterPageSetter.Ignore)]
                            | Some slimFlow ->
                                let r = slimFlow.Invoke flowModel args (RenewableInfos.Create infos) SlimWriterPageSetter.Ignore
                                let infos = r.Infos.AsList
                                let writerPageSetter = r.WriterPageSetter
                                let background = r.Infos.Background
                                bks.Add(background)
                                let boundPredicate (info: RenewablePathInfo) =
                                    info.Tag = RenewablePathInfoTag.CuttingDie

                                [TargetRenewablePageInfo.NewPage(TargetPageBox None, TargetRenewableNewPageInfoElement.Create (infos, borderKeepingPageNumbers, boundPredicate = boundPredicate), writerPageSetter, background = background)]
                        )
                        keepOriginPage
                        ignore
                        borderKeepingPageNumbers
                        reader
                        splitDocument.Writer
                    |> ignore
                )

                bks
                |> List.ofSeq
                |> List.concat
                |> List.distinctBy(fun m -> m.BackgroundFile.ClearedPdfFile.PdfPath)
                |> List.iter(fun m -> (m :> System.IDisposable).Dispose())
                

            |> reuse 
                "Extract objects"
                ["pageSelector" => pageSelector.Text
                 "selector" => selector.ToString()
                 "keepOriginPage" => keepOriginPage.ToString() ]


        static member SlimFlows(pageSelector: PageSelector, slimFlow) =
            Reuses.ExtractIM(
                pageSelector,
                Selector.All(fun _ _ -> true),
                slimFlow = slimFlow,
                keepOriginPage = false )
            |> Reuse.rename 
                "Slim Flows"
                []

        static member ExtractPaths(pageSelector, pathSelector, ?keepOriginPage) =
            Reuses.ExtractIM(pageSelector, Selector.Path pathSelector, ?keepOriginPage = keepOriginPage)


        static member ExtractToTwoPages (selector: Selector<'userState>, ?pageBoxSetter, ?secondarySelector, ?infosFilter) =
            let pageBoxSetter = defaultArg pageBoxSetter ExtractToTwoPages_PageBoxSetter.KeepOrigin
            fun (flowModel: FlowModel<_>) (splitDocument: SplitDocument) ->
                let reader = splitDocument.Reader
                let totalNumberOfPages = reader.GetNumberOfPages()

                [1..totalNumberOfPages]
                |> List.iter(fun pageNumber ->
                    let readerPage = reader.GetPage(pageNumber)
                    let args: PageModifingArguments<_> = 
                        { UserState = flowModel.UserState
                          Page = readerPage
                          TotalNumberOfPages = totalNumberOfPages
                          PageNum = pageNumber
                        }

                    extractVisibleRenewableInfosToWriter
                        flowModel.Configuration
                        args
                        (Selector.All(fun _ _ -> true))
                        (fun infos ->   
                            let infos =
                                match infosFilter with 
                                | None -> infos
                                | Some infosFilter -> infosFilter infos
                            [TargetRenewablePageInfo.SplitToTwoPages(infos, selector, pageBoxSetter, secondarySelector)]
                        )
                        false
                        ignore
                        []
                        reader
                        splitDocument.Writer
                    |> ignore
                )

            |> reuse 
                "ExtractToTwoPages"
                ["selector" => selector.ToString()
                 "pageBoxSetter" => pageBoxSetter.ToString()]
