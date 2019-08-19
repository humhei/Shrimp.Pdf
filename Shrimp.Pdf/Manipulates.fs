namespace Shrimp.Pdf
open Shrimp.Pdf.Parser
open iText.Layout
open Shrimp.Pdf.Extensions
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf


[<RequireQualifiedAccess>]
module IntegralDocument =
    let addNew (pageSelector: PageSelector) (pageBoxKind: PageBoxKind) (canvasActions: list<Canvas -> Canvas>) (document: IntegralDocument) =

        let selectedPageNumbers = document.Value.GetPageNumbers(pageSelector) 
        for i = 1 to document.Value.GetNumberOfPages() do
            if List.contains i selectedPageNumbers then
                let page = document.Value.GetPage(i)

                let canvas = new Canvas(page, page.GetPageBox(pageBoxKind))
                (canvas, canvasActions)
                ||> List.fold(fun pdfCanvas canvasAction ->
                    canvasAction pdfCanvas
                ) 
                |> ignore





module Manipulates =

    let modify (pageSelector: PageSelector) (operators: list<(PdfPage -> RenderInfoSelector) * SelectionModifier>)   =
        fun flowModel (document: IntegralDocument) ->
            operators
            |> List.iter (fun (renderInfoSelectorFactory, modifier) ->
                IntegralDocument.modify (pageSelector) renderInfoSelectorFactory modifier document
            ) 
            flowModel.UserState
        |> Manipulate

    let addNew (pageSelector: PageSelector) (pageBoxKind: PageBoxKind) canvasActionsFactory =
        fun flowModel (document: IntegralDocument) ->
            let canvasActions = canvasActionsFactory flowModel.UserState
            IntegralDocument.addNew pageSelector pageBoxKind canvasActions document
            flowModel.UserState
        |> Manipulate

    type PageModifingArguments<'userState> =
        { UserState: 'userState
          Page: PdfPage
          PageNum: int }

    let modifyPage (pageSelector: PageSelector) (renderInfoSelectorFactory: PageModifingArguments<_> -> RenderInfoSelector) operator  = 
        fun (flowModel: FlowModel<_>) (document: IntegralDocument) ->
            let document = document.Value
            let parser = new PdfDocumentContentParser(document)
            let selectedPageNumbers = document.GetPageNumbers(pageSelector) 

            document
            |> PdfDocument.getPages
            |> List.indexed
            |> List.choose(fun (i, page) ->
                let pageNum = (i + 1)
                if List.contains pageNum selectedPageNumbers then
                    let renderInfoSelector = 
                        renderInfoSelectorFactory 
                            { UserState = flowModel.UserState;
                              Page = page
                              PageNum = pageNum }
                    let infos = PdfDocumentContentParser.parse pageNum renderInfoSelector parser
                    operator 
                        { UserState = flowModel.UserState;
                          Page = page
                          PageNum = pageNum }
                        infos 
                else None
            )
        |> Manipulate

    let trimToVisible pageSelector =
        modifyPage 
            pageSelector
            (fun _ -> 
                RenderInfoSelector.OR 
                    [ RenderInfoSelector.Path PathRenderInfo.isVisible
                      RenderInfoSelector.Text TextRenderInfo.isVisible ]
            )
            (fun args renderInfos ->
                let bound = 
                    renderInfos
                    |> Seq.map (AbstractRenderInfo.getBound BoundGettingOptions.WithStrokeWidth)
                    |> Rectangle.ofRectangles
                args.Page.SetActualBox(bound)
                |> ignore
                None
            ) ||>> ignore