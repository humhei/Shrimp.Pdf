namespace Shrimp.Pdf.Extract

open iText.Kernel.Pdf

#nowarn "0104"
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open Shrimp.Pdf.Constants
open Shrimp.Pdf.Parser
open Shrimp.FSharp.Plus
open Akkling
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Xobject




[<AutoOpen>]
module _Extract =
    let internal reuse name parameters f = Reuse(f = f, flowName = FlowName.Override(name, parameters))
    



    type internal TargetRenewableNewPageInfoElement =
        { RenewableInfos: RenewableInfo list
          TagColor: FsColor option
          BoundPredicate: (IIntegratedRenderInfo -> bool) option
          RectangleTransform: RectangleTransform option
          BorderKeepingPageNumbers: int list }
    with    
        static member Create(infos: RenewableInfo list, borderKeppingPageNumbers, ?tagColor, ?boundPredicate, ?rectangleTransform) =
            { RenewableInfos = infos
              TagColor = tagColor
              BorderKeepingPageNumbers = borderKeppingPageNumbers
              BoundPredicate = boundPredicate
              RectangleTransform = rectangleTransform }

    type internal TargetNewInfosInOriginPageElement =
        { RenewableInfos: RenewableInfo list
          NewBound: IndexedBound
          Bound: IndexedBound
          BoundPredicate: IIntegratedRenderInfo -> bool
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
        | NewPage of TargetPageBox * TargetRenewableNewPageInfoElement
        | Non
        | NewInfosInOriginPage of TargetNewInfosInOriginPageElement list
        | SplitToTwoPages of 
             RenewableInfo list * Selector<'userState> * pageboxSetter: ExtractToTwoPages_PageBoxSetter * secondarySelector: (Rectangle -> Selector<'userState>) option

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
                |> List.filter(IIntegratedRenderInfoIM.isVisible)
            infos
            |> List.map(fun info ->
                match info with 
                | IIntegratedRenderInfoIM.Vector info ->
                    match info with 
                    | IIntegratedRenderInfo.Path pathInfo -> 
                        let pathInfo = pathInfo.Renewable()
                        pathInfo
                        |> RenewableInfo.Path

                    | IIntegratedRenderInfo.Text textInfo -> 
                        textInfo.Renewable()
                        |> RenewableInfo.Text

                | IIntegratedRenderInfoIM.Pixel image ->
                    image.Renewable()
                    |> RenewableInfo.Image
            )

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

            let writeAreaInfos (element: TargetNewInfosElementUnion) (writerCanvas: PdfCanvas) (writerPage: PdfPage) =
                let infos = element.Infos
                let writeInfos(infos: RenewableInfo list) =
                

                    infos
                    |> List.splitIfChangedByKey(fun m -> m.SoftMasks)
                    |> List.iter(fun (solfMasks, infos) ->
                        match solfMasks with 
                        | None -> ()
                        | Some solfMasks ->
                            writerCanvas.SaveState() |> ignore

                            solfMasks.AsList
                            |> List.iter(fun solfMask ->
                                solfMask.Renewable().ApplyCtm_WriteToCanvas(writerCanvas)
                            )

                        let writerCanvas2 = 
                            match solfMasks with 
                            | None -> writerCanvas
                            | Some solfMasks ->
                                let rect = 
                                    writerPage.GetActualBox()
                                    //solfMasks
                                    //|> AtLeastOneList.map(fun m -> m.ActualBBox.AsRectangle)
                                    //|> Rectangle.ofRectangles

                                let xobject = PdfFormXObject(rect)
                                writerCanvas.AddXObject(xobject) |> ignore
                                let group = PdfTransparencyGroup()
                                group.SetIsolated(false)
                                group.SetKnockout(false)
                                xobject.SetGroup(group) |> ignore
                                PdfCanvas(xobject, writer)

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
                                            info.CopyToDocument(writer, writerCanvas2.GetResources(), readerPage).ApplyCtm_WriteToCanvas(writerCanvas2)
                
                                    match clippingPathInfo with 
                                    | ClippingPathInfoState.Init -> writeInfos()
                                    | ClippingPathInfoState.Intersected (clippingPathInfo) -> 
                                        clippingPathInfo.Renewable().ApplyCtm_WriteToCanvas(writerCanvas2, (fun writerCanvas ->
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
                                    PdfCanvas.useCanvas writerCanvas2 (fun writerCanvas ->
                                        writerCanvas
                                            .Rectangle(rect.AsRectangle)
                                            .Clip()
                                            .EndPath()
                                            |> ignore

                                        writeInfos()

                                        writerCanvas

                                    )
                        )

                        match solfMasks with 
                        | None -> ()
                        | Some solfMasks ->
                            writerCanvas.RestoreState() |> ignore

                    )


                let infoChoices(boundPredicate) =   
                    let infoChoices =
                        infos
                        |> List.map (fun info ->
                            match info.OriginInfo with 
                            | IIntegratedRenderInfoIM.Vector vector ->
                                if boundPredicate vector 
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
                                writeInfos(infoChoices.Infos)

                                if keepBorder
                                then writeInfos(infoChoices.Bounds)

                                if keepBorder
                                then writeInfos(infoChoices.TagInfos)

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
                            |> List.choose(fun m -> IIntegratedRenderInfoIM.getBound BoundGettingStrokeOptions.WithoutStrokeWidth m.OriginInfo)
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
                              BorderKeepingPageNumbers = borderKeepingPageNumbers }


                        TargetRenewablePageInfo.NewPage(targetPageBox, element)

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
                | TargetRenewablePageInfo.NewPage (targetPageBox, infos) ->

                    let writerPage = writer.AddNewPage(PageSize(readerPage.GetActualBox()))

                    let writerCanvas = new PdfCanvas(writerPage.GetContentStream(0), writerPage.GetResources(), writer)
                    PdfCanvas.useCanvas writerCanvas (fun writerCanvas ->
                        writerCanvas
                            .WriteLiteral("0 G\n")
                            .WriteLiteral("0 g\n")
                            |> ignore

                        writeAreaInfos (TargetNewInfosElementUnion.NewPageInfo infos) writerCanvas writerPage
                        writerCanvas

                    ) |> ignore
                    suffixOperation (writerPage, writerCanvas)

                    //writerPage.SetPageBoxToPage(readerPage) |> ignore

                    match targetPageBox.Value with 
                    | Some (pageBoxKind, pageBox) -> 
                        writerPage.SetPageBoxToPage(readerPage)
                        |> ignore<PdfPage>

                        writerPage.SetPageBox(pageBoxKind, pageBox) 
                        |> ignore<PdfPage>

                    | None -> 
                        writerPage.SetPageBoxToPage(readerPage)
                        |> ignore<PdfPage>

                | TargetRenewablePageInfo.NewInfosInOriginPage (infos) ->
                    let writerPage = writer.AddNewPage(PageSize(readerPage.GetActualBox()))
                    writerPage.SetPageBoxToPage(readerPage) |> ignore
                    let writerCanvas = new PdfCanvas(writerPage.GetContentStream(0), writerPage.GetResources(), writer)
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
        static member ExtractIM(pageSelector: PageSelector, selector, ?keepOriginPage) =    
            let keepOriginPage = defaultArg keepOriginPage false
            fun (flowModel: FlowModel<_>) (splitDocument: SplitDocument) ->
                let reader = splitDocument.Reader
                let totalNumberOfPages = reader.GetNumberOfPages()
                let pageNumbers = reader.GetPageNumbers(pageSelector)
                let borderKeepingPageNumbers = [1..totalNumberOfPages]

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
                        (fun infos -> [TargetRenewablePageInfo.NewPage(TargetPageBox None, TargetRenewableNewPageInfoElement.Create (infos, borderKeepingPageNumbers))])
                        keepOriginPage
                        ignore
                        borderKeepingPageNumbers
                        reader
                        splitDocument.Writer
                    |> ignore
                )

            |> reuse 
                "Extract objects"
                ["pageSelector" => pageSelector.Text
                 "selector" => selector.ToString()
                 "keepOriginPage" => keepOriginPage.ToString() ]



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
