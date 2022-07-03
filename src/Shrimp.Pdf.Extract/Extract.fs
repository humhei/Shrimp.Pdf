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
    type internal TargetRenewablePageInfo =
        | EmptyPage of TargetPageBox
        | NewPage of TargetPageBox * TargetRenewableNewPageInfoElement
        | Non
        | NewInfosInOriginPage of TargetNewInfosInOriginPageElement list

    let internal extractVisibleRenewableInfosToWriter 
        (args: PageModifingArguments<_>) 
        selector 
        (infosSplitter: RenewableInfo list -> list<TargetRenewablePageInfo>)
        keepOriginPage
        suffixOperation
        (borderKeepingPageNumbers: int list)
        (reader: PdfDocument)
        (writer: PdfDocumentWithCachedResources) =
        let parser = new NonInitialClippingPathPdfDocumentContentParser(reader)
        let pageNumber = args.PageNum
        let keepBorder = List.contains pageNumber borderKeepingPageNumbers

        let infos =
            let selector = 
                Selector.toRenderInfoSelector args selector

            NonInitialClippingPathPdfDocumentContentParser.parseIM pageNumber selector parser

        let infos = 
            infos
            |> List.ofSeq
            |> List.filter(IIntegratedRenderInfoIM.isVisible)
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
                    |> List.groupBy(fun m -> m.ClippingPathInfos.XObjectClippingBoxState.Serializable)
                    |> List.iter(fun (xobjectRect, infos) ->
                        let writeInfos() =
                            infos
                            |> List.groupBy(fun m -> m.ClippingPathInfos.ClippingPathInfoState)
                            |> List.iter(fun (clippingPathInfo, infos) ->   
                                let writeInfos() =
                                    for info in infos do
                                        info.CopyToDocument(writer, writerPage.GetResources(), readerPage).ApplyCtm_WriteToCanvas(writerCanvas)
                
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

                        if keepBorder
                        then writeInfos(infoChoices.Bounds)

                        writerCanvas
                            .ConcatMatrix(AffineTransform.ofRecord transform_translate)
                            .ConcatMatrix(AffineTransform.ofRecord transform_scale)
                            |> ignore

                        if keepBorder
                        then writeInfos(infoChoices.TagInfos)

                        writeInfos(infoChoices.Infos)
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
                                if keepBorder
                                then writeInfos(infoChoices.Bounds)

                                if keepBorder
                                then writeInfos(infoChoices.TagInfos)

                                writeInfos(infoChoices.Infos)
                                writerCanvas
                            )


                | TargetNewInfosElementUnion.OriginPage (element) ->
                    writeInfosWithTransform element.BoundPredicate element.RectangleTransform


            match targetPageInfo with 
            | TargetRenewablePageInfo.EmptyPage targetPageBox ->
                let writerPage = writer.AddNewPage(PageSize(readerPage.GetActualBox()))
                match targetPageBox.Value with 
                | None -> writerPage.SetPageBoxToPage(readerPage) |> ignore
                | Some pageBox -> writerPage.SetAllBox(pageBox) |> ignore

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
                | Some pageBox -> writerPage.SetAllBox(pageBox) |> ignore
                | None -> writerPage.SetPageBoxToPage(readerPage) |> ignore

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


        )

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
