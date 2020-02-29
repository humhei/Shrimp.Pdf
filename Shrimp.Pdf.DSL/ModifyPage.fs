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
open Shrimp.Pdf.Colors


type ColoredText =
    { Text: string 
      Color: PdfCanvasColor }

type PageModifier<'userState, 'newUserState> = PageModifingArguments<'userState> -> seq<IIntegratedRenderInfo> -> 'newUserState

type PageModifier =

    static member private AddNew (canvasAreaOptions, canvasActionsBuilder) : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            let page = args.Page
            let canvas = 
                let rootArea = page.GetArea(canvasAreaOptions)
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

    static member PickTexts(picker: Parser<_, _>) : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            infos
            |> List.ofSeq
            |> List.choose IIntegratedRenderInfo.asITextRenderInfo
            |> List.choose (fun renderInfo ->
                let text = ITextRenderInfo.getText renderInfo
                match FParsec.CharParsers.run picker text with 
                | Success (result, _ ,_ )-> Some result
                | Failure (msg, _ , _) -> None
            )

    static member SetPageBox(pageBoxKind: PageBoxKind, rect: Rectangle) : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            PdfPage.setPageBox pageBoxKind rect args.Page
            |> ignore

    static member SetPageBoxes(pageBoxKindes: PageBoxKind list, rect: Rectangle) : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            for pageBoxKind in pageBoxKindes do
                PdfPage.setPageBox pageBoxKind rect args.Page
                |> ignore

    static member GetPageEdge (pageBoxKind: PageBoxKind, innerBox: Rectangle) : PageModifier<_, _> =
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

                    | TextAlignment.RIGHT -> scanedTextWidths |> List.map (fun scanedTextWidth -> -scanedTextWidth) 

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
        static member Create(name, pageSelector: PageSelector, selector, pageModifier, ?parameters: list<string * string>) =
            let flowName = 
                let parameters =
                    match parameters with 
                    | None ->
                        [
                            "pageSelctor" => pageSelector.ToString()
                            "selector" => selector.ToString()
                        ]
                    | Some parameters ->
                        (
                            [
                                "pageSelctor" => pageSelector.ToString()
                                "selector" => selector.ToString()
                            ]
                            @ parameters
                        )
                        |> List.distinctBy(fun (key, _) -> key.Trim().ToLower())
                   

                FlowName.Override(name, 
                    parameters
                )

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
                                let infos = NonInitialClippingPathPdfDocumentContentParser.parse pageNum renderInfoSelector parser
                                Some (pageModifier args infos) 
                            else None
                        )
                    newUserState

            Manipulate(
                flowName = flowName,
                f = f
            )

