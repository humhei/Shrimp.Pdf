namespace Shrimp.Pdf.DSL
open FParsec
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Parser
open iText.Kernel.Pdf.Canvas.Parser
open Shrimp.Pdf
open iText.Kernel.Geom
open iText.Layout
open iText.Layout.Properties
open iText.Kernel.Pdf.Canvas
open Fake.IO
open System.IO
open Shrimp.Pdf.Colors
open Shrimp.FSharp.Plus
open Shrimp.Pdf

type DataTableParsingFormat =
    { ColNum: int }



type ColoredText =
    { Text: string 
      Color: PdfCanvasColor }

type PageModifier<'userState, 'newUserState> = PageModifingArguments<'userState> -> seq<IIntegratedRenderInfo> -> 'newUserState

type RectangleTransform = 
    { OldRect: Rectangle
      NewRect: Rectangle}
with
    member x.ApplyToCanvas(canvas: PdfCanvas) =
        let oldArea = x.OldRect
        let newArea = x.NewRect

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

        canvas
            .ConcatMatrix(AffineTransform.ofRecord transform_translate)
            .ConcatMatrix(AffineTransform.ofRecord transform_scale)


type PageModifier =

    static member private AddNew (canvasAreaOptions, canvasActionsBuilder) : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            let page = args.Page
            let rootArea = page.GetArea(canvasAreaOptions)
            let canvas = 
                new Canvas(page, rootArea)


            let canvasActions = canvasActionsBuilder args

            (canvas, canvasActions)
            ||> List.fold(fun canvas canvasAction ->
                canvasAction canvas
            ) 
            |> ignore

    static member private AddNew (canvasActionsBuilder) : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            let page = args.Page
            let canvas = new PdfCanvas(page)

            let canvasActions = canvasActionsBuilder args

            (canvas, canvasActions)
            ||> List.fold(fun canvas canvasAction ->
                canvasAction canvas
            ) 
            |> ignore


    static member GetBoundOfSelector() : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->   
            let trimedBox = 
                infos
                |> Seq.map (IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth)
                |> AtLeastOneList.Create
                |> Rectangle.ofRectangles
            trimedBox

    static member GetColors() : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            infos
            |> List.ofSeq
            |> List.collect (fun info ->
                let hasFill = IAbstractRenderInfo.hasFill info
                let hasStroke = IAbstractRenderInfo.hasStroke info
                if hasFill && hasStroke 
                then [ info.Value.GetFillColor(); info.Value.GetStrokeColor() ]
                elif hasFill 
                then [info.Value.GetFillColor()]
                elif hasStroke 
                then [info.Value.GetStrokeColor()]
                else []
            )
            |> Colors.distinct
            |> List.ofSeq

    static member PickTextInfo(picker: IntegratedTextRenderInfo -> _ option) : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            infos
            |> List.ofSeq
            |> List.choose IIntegratedRenderInfo.asITextRenderInfo
            |> List.choose (fun renderInfo ->
                renderInfo
                |> picker 
            )

    //static member PickTexts(picker: TextInfoRecord -> _ option) : PageModifier<_, _> =
    //    PageModifier.PickTexts(fun (info: IntegratedTextRenderInfo) ->
    //        picker info.RecordValue
    //    )

    static member PickWord(picker: PdfConcatedWord -> _ option) : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            infos
            |> List.ofSeq
            |> List.choose IIntegratedRenderInfo.asITextRenderInfo
            |> List.choose (fun renderInfo ->
                let text = ITextRenderInfo.getPdfConcatedText renderInfo
                picker text
            )


    static member PickText(picker: Parser<_, _>, ?wordSep) : PageModifier<_, _> =
        PageModifier.PickWord(
            picker = (
                fun (text: PdfConcatedWord) ->
                    match FParsec.CharParsers.run picker (text.ConcatedText(?wordSep = wordSep)) with 
                    | Success (result, _ ,_ )-> Some result
                    | Failure (msg, _ , _) -> None
            )
        )


    static member PickLine(picker: PdfConcatedLine -> _ option, ?selectionGrouper: SelectionGrouper) : PageModifier<_, _> =
        let selectionGrouper = defaultArg selectionGrouper SelectionGrouper.DefaultValue

        fun (args: PageModifingArguments<_>) infos ->
            let infos = 
                infos
                |> List.ofSeq
                |> List.choose IIntegratedRenderInfo.asITextRenderInfo
                |> List.map(fun m -> m, IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth m)
                |> List.distinctBy_explictly<_, _>(fun (m, bound) ->
                    Rectangle.equalableValue bound
                )

            let infos = 
                infos
                |> selectionGrouper.GroupBy_Bottom(snd) 

            infos
            |> Seq.choose(fun (_, infos) ->
                let text = 
                    infos
                    |> List.ofSeq
                    |> List.map fst
                    |> List.map (ITextRenderInfo.getPdfConcatedText)
                    |> PdfConcatedLine
                picker text
            )
            |> List.ofSeq

    static member SetPageBox(pageBoxKind: PageBoxKind, rect: Rectangle) : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            PdfPage.setPageBox pageBoxKind rect args.Page
            |> ignore

    static member SetPageBoxTo(pageBoxKind: PageBoxKind, size: FsSize, ?xEffect, ?yEffect) : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            let pageBox = 
                match pageBoxKind with 
                | PageBoxKind.AllBox -> args.Page.GetActualBox()
                | _ -> PdfPage.getPageBox pageBoxKind args.Page

            let newRect =
                pageBox
                    .setWidth(defaultArg xEffect XEffort.Middle, (fun _ -> size.Width))
                    .setHeight(defaultArg yEffect YEffort.Middle, (fun _ -> size.Height))

            PdfPage.setPageBox pageBoxKind newRect args.Page
            |> ignore

    static member SetPageBoxes(pageBoxKindes: PageBoxKind list, rect: Rectangle) : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            for pageBoxKind in pageBoxKindes do
                PdfPage.setPageBox pageBoxKind rect args.Page
                |> ignore

    static member GetPageEdge (pageBoxKind: PageBoxKind, innerBox: Rectangle) : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            args.Page.GetPageEdge(innerBox, pageBoxKind) 

    static member GetPageEdge (pageBoxKind: PageBoxKind, innerBox: FsSize) : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            args.Page.GetPageEdge(innerBox, pageBoxKind) 


    static member AddText(canvasAreaOptions, text, mapping) : PageModifier<_, _> =
        PageModifier.AddNew (canvasAreaOptions, (fun args ->
            [ Canvas.addText text (mapping) ]
        ))


    static member AddLine(line, mapping) : PageModifier<_, _> =
        PageModifier.AddNew (fun args ->
            [ PdfCanvas.addLine line mapping ]
        )

    static member AddLine(canvasAreaOptions: AreaGettingOptions, startPosition: Position, endPosition: Position, mapping) : PageModifier<_, _> =
        PageModifier.AddNew (fun args ->
            let area = args.Page.GetArea(canvasAreaOptions)
            let line =
                let startPoint = area.GetPoint(startPosition)
                let endPoint = area.GetPoint(endPosition)
                { Start = startPoint 
                  End = endPoint }

            [ PdfCanvas.addLine line mapping ]
        )



    static member AddText(pageBoxKind, text, mapping) : PageModifier<_, _> =
        PageModifier.AddText(AreaGettingOptions.PageBox pageBoxKind, text, mapping)

    static member AddText(canvasRootArea: Rectangle, text, mapping) : PageModifier<_, _> =
        PageModifier.AddText(AreaGettingOptions.Specfic canvasRootArea, text, mapping)

    static member AddColoredTexts(canvasAreaOptions: AreaGettingOptions, coloredTexts: ColoredText list , mapping) : PageModifier<_, _> =
        fun pageModifingArgs infos ->
            match coloredTexts with 
            | [] -> ()
            | _ ->
                let canvas = 
                    let page = pageModifingArgs.Page
                    new Canvas(page, page.GetArea(canvasAreaOptions))

                let args = mapping CanvasAddTextArguments.DefaultValue

                let offsets =
                    let totalTextWidth =
                        let totalText = 
                            coloredTexts 
                            |> List.map (fun m -> m.Text) 
                            |> String.concat ""

                        canvas.CalcTextWidth(totalText, args)

                    let textWidths = 
                        coloredTexts
                        |> List.map (fun coloredText ->
                            canvas.CalcTextWidth(coloredText.Text, args)
                        )

                    let scanedTextWidths = 
                        (0., textWidths)
                        ||> List.scan(fun offset textWidth ->
                            textWidth + offset
                        )

                    match args.GetCalculatedHorizontalTextAlignment() with 
                    | TextAlignment.LEFT -> scanedTextWidths
                    | TextAlignment.CENTER -> 
                        textWidths
                        |> List.mapi (fun i textWidth ->
                            -totalTextWidth / 2. + scanedTextWidths.[i] + textWidth / 2.
                        )

                    | TextAlignment.RIGHT -> 
                        let scanedTextWidths = List.take textWidths.Length scanedTextWidths
                        scanedTextWidths |> List.mapi (fun i scanedTextWidth -> -totalTextWidth + scanedTextWidth + textWidths.[i]) 

                    | _ -> failwith "Not implemented"

                let pageModifier = 
                    coloredTexts
                    |> List.mapi(fun i coloredText ->
                        let args = 
                            { args with 
                                Position = 
                                    Position.mapValue (fun (x, y) -> 
                                        (offsets.[i] + x, y)
                                    ) args.Position 

                                FontColor = coloredText.Color
                            }
                        PageModifier.AddText(canvasAreaOptions, coloredText.Text, fun _ -> args) 
                    )
                    |> PageModifier.Batch

                pageModifier pageModifingArgs infos
                |> ignore
            


    static member AddRectangleToCanvasRootArea(canvasAreaOptions, mapping: PdfCanvasAddRectangleArguments -> PdfCanvasAddRectangleArguments) : PageModifier<_, _> =
        PageModifier.AddNew (canvasAreaOptions, (fun args ->
            [ Canvas.addRectangleToRootArea mapping ]
        ))

    static member AddVarnish(canvasAreaOptions, varnish) =
        PageModifier.AddRectangleToCanvasRootArea(canvasAreaOptions, fun args ->
            { args with 
                FillColor = NullablePdfCanvasColor.Separation varnish
                StrokeColor = NullablePdfCanvasColor.N
                IsFillOverprint = true
            }
        )

    static member ClippingContentsToPageBox(pageBoxKind: PageBoxKind, ?margin: Margin): PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos -> 
            let pageBox = args.Page.GetPageBox(pageBoxKind)
            let contentBox = 
                pageBox |> Rectangle.applyMargin (defaultArg margin Margin.Zero)
            let writerCanvas = 
                new PdfCanvas(args.Page.NewContentStreamBefore(), args.Page.GetResources(), args.Page.GetDocument())

            writerCanvas
                .SaveState()
                .Rectangle(contentBox)
                .EoClip()
                .EndPath()
                |> ignore

            writerCanvas.AttachContentStream(args.Page.NewContentStreamAfter())
                    
            writerCanvas.RestoreState() |> ignore



    static member ScaleContentsTo(fRectangle: Rectangle -> Rectangle): PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos -> 
            let pageBox = args.Page.GetActualBox()
            let contentBox = fRectangle pageBox
            let writerCanvas = 
                new PdfCanvas(args.Page.NewContentStreamBefore(), args.Page.GetResources(), args.Page.GetDocument())

            let transform =
                { OldRect = pageBox 
                  NewRect = contentBox }



            writerCanvas
                .SaveState()
            |> transform.ApplyToCanvas
            |> ignore

            writerCanvas.AttachContentStream(args.Page.NewContentStreamAfter())
                
            writerCanvas.RestoreState() |> ignore



    static member Batch(pageModifiers: PageModifier<_, _> list) : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            pageModifiers
            |> List.map(fun pageModifier ->
                pageModifier args infos
            )
            |> ignore



[<AutoOpen>]
module ModifyPageOperators =
    type ModifyPage =
        static member private CreateCommon(parse, name, pageSelector: PageSelector, selector, pageModifier, ?parameters: list<string * string>) =
            let flowName = 
                let parameters =
                    match parameters with 
                    | None ->
                        [
                            "pageSelector" => pageSelector.ToString()
                            "selector" => selector.ToString()
                        ]
                    | Some parameters ->
                        (
                            [
                                "pageSelector" => pageSelector.ToString()
                                "selector" => selector.ToString()
                            ]
                            @ parameters
                        )
                        |> List.distinctBy(fun (key, _) -> key.Trim().ToLower())
                   

                FlowName.Override(name, parameters)

            let f =
                fun (flowModel: FlowModel<_>) (integratedDocument: IntegratedDocument) ->
                    
                    let document = integratedDocument.Value
                    let parser = new NonInitialClippingPathPdfDocumentContentParser(document)
                    let selectedPageNumbers = document.GetPageNumbers(pageSelector) 

                    let newUserState = 
                        document
                        |> PdfDocument.getPages
                        |> List.indexed
                        |> List.choose(fun (i, page) ->
                            let pageNum = (i + 1)
                            if List.contains pageNum selectedPageNumbers then
                                let args = 
                                    { UserState = flowModel.UserState;
                                      Page = page
                                      TotalNumberOfPages = document.GetNumberOfPages()
                                      PageNum = pageNum }

                                let renderInfoSelector = Selector.toRenderInfoSelector args selector
                                let infos = parse pageNum renderInfoSelector parser
                                Some (pageModifier args infos) 
                            else None
                        )
                    newUserState

            Manipulate(
                flowName = flowName,
                f = f
            )
        static member Create(name, pageSelector: PageSelector, selector, pageModifier, ?parameters: list<string * string>) =
            ModifyPage.CreateCommon(
                NonInitialClippingPathPdfDocumentContentParser.parse,
                name,
                pageSelector,
                selector,
                pageModifier,
                ?parameters = parameters)

        static member CreateIM(name, pageSelector: PageSelector, selector, pageModifier, ?parameters: list<string * string>) =
            ModifyPage.CreateCommon(
                NonInitialClippingPathPdfDocumentContentParser.parseIM,
                name,
                pageSelector,
                selector,
                pageModifier,
                ?parameters = parameters
            )

        static member RemoveLayer(layerName: string) =
            Manipulate(fun flowModel document ->
                for page in document.Value.GetPages() do
                    PdfPage.removeLayer layerName page
                    |> ignore
            )



    type PdfRunner with 
    
        static member private ReadInfosCommon(parse, pdfFile: PdfFile, selector, fInfos, ?pageSelector, ?inShadowMode) =
            let pdfFile = 
                match defaultArg inShadowMode true with 
                | true ->
                    let ext = Path.GetFileName pdfFile.Path
                    let tmpPath = System.IO.Path.GetTempFileNameEx() |> Path.changeExtension ext
                    System.IO.File.Copy(pdfFile.Path, tmpPath, true)
                    PdfFile tmpPath

                | false -> pdfFile

            use document = new ReaderDocument(pdfFile.Path)
            let document = document.Reader
            let pageSelector = defaultArg pageSelector PageSelector.All
            let parser = 
                NonInitialClippingPathPdfDocumentContentParser(document)

            let pageNumbers = document.GetPageNumbers(pageSelector)
            let totalNumberOfPages = document.GetNumberOfPages()
            pageNumbers
            |> List.map(fun pageNumber ->
                let page = document.GetPage(pageNumber)
                let args =
                    { PageModifingArguments.UserState = () 
                      Page = page 
                      PageNum = pageNumber
                      TotalNumberOfPages = totalNumberOfPages }
                let selector = 

                    Selector.toRenderInfoSelector args selector
                let infos = parse pageNumber selector parser
                fInfos args infos
            )

        static member ReadTotalPageNumber(pdfFile: PdfFile, ?inShadowMode) =
            let pdfFile = 
                match defaultArg inShadowMode true with 
                | true ->
                    let ext = Path.GetFileName pdfFile.Path
                    let tmpPath = System.IO.Path.GetTempFileNameEx() |> Path.changeExtension ext
                    System.IO.File.Copy(pdfFile.Path, tmpPath, true)
                    PdfFile tmpPath

                | false -> pdfFile


            use document = new ReaderDocument(pdfFile.Path)
            let document = document.Reader
            document.GetNumberOfPages()

        static member ReadInfos(pdfFile: PdfFile, selector, fInfos, ?pageSelector, ?inShadowMode) =
            PdfRunner.ReadInfosCommon(
                NonInitialClippingPathPdfDocumentContentParser.parse,
                pdfFile,
                selector,
                fInfos,
                ?pageSelector = pageSelector,
                ?inShadowMode = inShadowMode
            )

        static member ReadInfosIM(pdfFile: PdfFile, selector, fInfos, ?pageSelector, ?inShadowMode) =
            PdfRunner.ReadInfosCommon(
                NonInitialClippingPathPdfDocumentContentParser.parseIM,
                pdfFile,
                selector,
                fInfos,
                ?pageSelector = pageSelector,
                ?inShadowMode = inShadowMode
            )

        static member ReadInfosIMStoppable(pdfFile: PdfFile, selector, fInfos, ?pageSelector, ?inShadowMode) =
            PdfRunner.ReadInfosCommon(
                NonInitialClippingPathPdfDocumentContentParser.parseIMStoppable,
                pdfFile,
                selector,
                fInfos,
                ?pageSelector = pageSelector,
                ?inShadowMode = inShadowMode
            )
   


        static member ReadTextInfos(pdfFile: PdfFile, ?selector, ?pageSelector, ?inShadowMode) =
            PdfRunner.ReadInfos(
                pdfFile, 
                Selector.Text (defaultArg selector (fun _ _ -> true)),
                fInfos = (fun args infos ->
                    infos
                    |> List.ofSeq
                    |> List.choose (IIntegratedRenderInfo.asITextRenderInfo)
                ),
                ?pageSelector = pageSelector,
                ?inShadowMode = inShadowMode
            )

        static member ReadTextInfos_Record(pdfFile, ?selector, ?pageSelector, ?inShadowMode) =
            PdfRunner.ReadTextInfos(pdfFile, ?selector = selector, ?pageSelector = pageSelector, ?inShadowMode = inShadowMode)
            |> List.map(List.map(fun m -> m.RecordValue))


        static member ReadPathInfos(pdfFile: PdfFile, ?selector, ?pageSelector, ?inShadowMode) =
            PdfRunner.ReadInfos(
                pdfFile, 
                Selector.Path (defaultArg selector (fun _ _ -> true)),
                fInfos = (fun args infos ->
                    infos
                    |> List.ofSeq
                    |> List.choose (IIntegratedRenderInfo.asIPathRenderInfo)
                ),
                ?pageSelector = pageSelector,
                ?inShadowMode = inShadowMode
            )

        static member ReadPathInfos_Record(pdfFile, ?selector, ?pageSelector, ?inShadowMode) =
            PdfRunner.ReadPathInfos(pdfFile, ?selector = selector, ?pageSelector = pageSelector, ?inShadowMode = inShadowMode)
            |> List.map(List.map(fun m -> m.RecordValue))



        static member ReadColors(pdfFile: PdfFile, ?selector, ?pageSelector, ?inShadowMode) =
            PdfRunner.ReadInfos(
                pdfFile, 
                defaultArg selector (Selector.PathOrText (fun _ _ -> true)),
                fInfos = (fun args infos ->
                    infos
                    |> List.ofSeq
                    |> List.collect (fun m -> 
                        [
                            match IAbstractRenderInfo.hasFill m with 
                            | true -> yield FsColor.OfItextColor <| m.Value.GetFillColor()
                            | false -> ()

                            match IAbstractRenderInfo.hasStroke m with 
                            | true -> yield FsColor.OfItextColor <| m.Value.GetStrokeColor()
                            | false -> ()
                        ]
                    )
                ),
                ?pageSelector = pageSelector,
                ?inShadowMode = inShadowMode
            )
            |> List.concat
            |> FsColors.distinct
            |> List.ofSeq

        /// default pageSelector should be PageSelector.First
        static member CheckInfos(pageSelector, pdfFile: PdfFile, selector, ?inShadowMode) =
            PdfRunner.ReadInfos(pdfFile, selector, (fun _ infos -> infos), pageSelector, ?inShadowMode = inShadowMode)
            |> List.forall(fun infos ->
                Seq.length infos > 0
            )
       

