namespace Shrimp.Pdf.DSL
open FParsec
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Parser
open iText.Kernel.Pdf.Canvas.Parser
open Shrimp.Pdf
open iText.Kernel.Geom
open iText.Layout
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf.Colors

[<RequireQualifiedAccess>]
type CanvasAreaOptions =
    | PageBox of PageBoxKind
    | Specfic of Rectangle

type PageModifier<'userState, 'newUserState> = PageModifingArguments<'userState> -> seq<IIntegratedRenderInfo> -> 'newUserState

type PageModifier =

    static member private AddNew (canvasAreaOptions, canvasActionsBuilder) : PageModifier<_, _> =
        fun (args: PageModifingArguments<_>) infos ->
            let page = args.Page
            let canvas = 
                let rootArea =
                    match canvasAreaOptions with 
                    | CanvasAreaOptions.PageBox pageBoxKind -> page.GetPageBox(pageBoxKind)
                    | CanvasAreaOptions.Specfic rect -> rect

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
                |> Seq.map (IAbstractRenderInfo.getBound BoundGettingOptions.WithoutStrokeWidth)
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

    static member AddLine(canvasAreaOptions: CanvasAreaOptions, startPosition: Position, endPosition: Position, mapping) : PageModifier<_, _> =
        PageModifier.AddNew (fun args ->
            let area = 
                match canvasAreaOptions with 
                | CanvasAreaOptions.PageBox pageBoxKind -> 
                    args.Page.GetPageBox(pageBoxKind)
                | CanvasAreaOptions.Specfic area -> area

            let line =
                let startPoint = area.GetPoint(startPosition)
                let endPoint = area.GetPoint(endPosition)
                { Start = startPoint 
                  End = endPoint }

            [ PdfCanvas.addLine line mapping ]
        )

    static member AddText(pageBoxKind, text, mapping) : PageModifier<_, _> =
        PageModifier.AddText(CanvasAreaOptions.PageBox pageBoxKind, text, mapping)

    static member AddText(canvasRootArea: Rectangle, text, mapping) : PageModifier<_, _> =
        PageModifier.AddText(CanvasAreaOptions.Specfic canvasRootArea, text, mapping)

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


[<AutoOpen>]
module ModifyPageOperators =

    let modifyPageCurried name (pageSelector: PageSelector) (selector: Selector<_>) (pageModifier: PageModifier<_, _>)  = 
        fun (flowModel: FlowModel<_>) (integratedDocument: IntegratedDocument) ->
            let document = integratedDocument.Value
            let parser = new NonInitialClippingPathPdfDocumentContentParser(document)
            let selectedPageNumbers = document.GetPageNumbers(pageSelector) 
            Logger.infoWithStopWatch (sprintf "MODIFYPAGE: %s" name) (fun _ ->
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
            )
        |> Manipulate


    let modifyPage (name: string, pageSelector, (selector: Selector<'userState>), (pageModifier: PageModifier<_, _>)) =
        modifyPageCurried (name) pageSelector selector pageModifier

    //let flush() =
    //    fun (flowModel: FlowModel<_>) (document: IntegratedDocument) ->
    //        document.Value.FlushCopiedObjects(document.Value)
    //        flowModel.UserState
    //    |> Manipulate