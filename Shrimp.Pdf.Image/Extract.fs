namespace Shrimp.Pdf

open iText.Kernel.Pdf

#nowarn "0104"
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open Shrimp.Pdf
open Shrimp.Pdf.Constants
open Shrimp.Pdf.Parser
open Shrimp.FSharp.Plus
open Akkling
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas


type IndexedBound =
    { Index: int 
      Bound: Rectangle }
with 
    member x.ApplyMargin margin = 
        { x with Bound = Rectangle.applyMargin margin x.Bound }



[<AutoOpen>]
module _Extract =
    let internal reuse name parameters f = Reuse(f = f, flowName = FlowName.Override(name, parameters))
    
    type internal TargetPageBox = TargetPageBox of Rectangle option
    with 
        member x.Value =
            let (TargetPageBox v) = x
            v


    type internal TargetRenewableNewPageInfoElement =
        { RenewableInfos: RenewableInfo list }
    with    
        static member Create(infos: RenewableInfo list) =
            { RenewableInfos = infos }

    type internal TargetNewInfosInOriginPageElement =
        { RenewableInfos: RenewableInfo list
          NewBound: IndexedBound
          Bound: IndexedBound
          BoundPredicate: IIntegratedRenderInfo -> bool }

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
        (reader: PdfDocument)
        (writer: PdfDocumentWithCachedResources) =
        let parser = new NonInitialClippingPathPdfDocumentContentParser(reader)
        let pageNumber = args.PageNum


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

            let writeAreaInfos(infos: RenewableInfo list) (areaTransform: TargetNewInfosInOriginPageElement option) (writerCanvas: PdfCanvas) (writerPage: PdfPage) =
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

                match areaTransform with 
                | None -> writeInfos(infos)
                | Some (element) ->
                    let bounds, infos =
                        infos
                        |> List.partition (fun info ->
                            match info.OriginInfo with 
                            | IIntegratedRenderInfoIM.Vector vector ->
                                element.BoundPredicate vector
                            | IIntegratedRenderInfoIM.Pixel _ -> false
                        )
                    let newArea = element.NewBound
                    let oldArea = element.Bound


                    let transform_scale = 
                        let scaleX = newArea.Bound.GetWidthF() / oldArea.Bound.GetWidthF()
                        let scaleY = newArea.Bound.GetHeightF() / oldArea.Bound.GetHeightF()

                        //AffineTransformRecord.DefaultValue
                        { 
                            AffineTransformRecord.DefaultValue with 
                                ScaleX = scaleX
                                ScaleY = scaleY
                                //TranslateX = 
                        }
                        
                    let scaledArea = 
                        AffineTransform.ofRecord(transform_scale).Transform(
                            oldArea.Bound
                        )

                    let transform_translate =
                        
                        let translateX = newArea.Bound.GetXF() - scaledArea.GetXF()
                        let translateY = (newArea.Bound.GetYF() - scaledArea.GetYF())
                        { AffineTransformRecord.DefaultValue with 
                            TranslateX = translateX
                            TranslateY = translateY 
                        }



                    PdfCanvas.useCanvas writerCanvas (fun writerCanvas ->
                        writeInfos(bounds)

                        writerCanvas
                            .ConcatMatrix(AffineTransform.ofRecord transform_translate)
                            .ConcatMatrix(AffineTransform.ofRecord transform_scale)
                            |> ignore
                        writeInfos(infos)
                        writerCanvas
                    )


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

                    let infos = infos.RenewableInfos
                    writeAreaInfos infos None writerCanvas writerPage

                    writerCanvas
                ) |> ignore

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
                        writeAreaInfos infos.RenewableInfos (Some infos) writerCanvas writerPage
                    )

                    writerCanvas
                )
        )

        splittedInfos


    type Reuses with
        static member ExtractIM(pageSelector: PageSelector, selector, ?keepOriginPage) =    
            let keepOriginPage = defaultArg keepOriginPage false
            fun (flowModel: FlowModel<_>) (splitDocument: SplitDocument) ->
                let reader = splitDocument.Reader
                let totalNumberOfPages = reader.GetNumberOfPages()
                let pageNumbers = reader.GetPageNumbers(pageSelector)

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
                        (fun infos -> [TargetRenewablePageInfo.NewPage(TargetPageBox None, TargetRenewableNewPageInfoElement.Create infos)])
                        keepOriginPage
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
