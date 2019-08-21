namespace Shrimp.Pdf
open Shrimp.Pdf.Parser
open iText.Layout
open Shrimp.Pdf.Extensions
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf


[<RequireQualifiedAccess>]
module IntegratedDocument =
    let addNew (pageSelector: PageSelector) (pageBoxKind: PageBoxKind) (canvasActionsFactory: (int * PdfPage) -> list<Canvas -> Canvas>) (document: IntegratedDocument) =

        let selectedPageNumbers = document.Value.GetPageNumbers(pageSelector) 
        for i = 1 to document.Value.GetNumberOfPages() do
            if List.contains i selectedPageNumbers then
                let page = document.Value.GetPage(i)
                let canvasActions = canvasActionsFactory(i, page)
                let canvas = new Canvas(page, page.GetPageBox(pageBoxKind))
                (canvas, canvasActions)
                ||> List.fold(fun pdfCanvas canvasAction ->
                    canvasAction pdfCanvas
                ) 
                |> ignore


module Manipulates =

    type PageModifingArguments<'userState> =
        { UserState: 'userState
          Page: PdfPage
          PageNum: int }

    let modify (pageSelector: PageSelector) (operators: list<(PageModifingArguments<_> -> RenderInfoSelector) * SelectionModifier>)   =
        fun (flowModel: FlowModel<_>) (document: IntegratedDocument) ->
            operators
            |> List.iter (fun (renderInfoSelectorFactory, modifier) ->
                IntegratedDocument.modify 
                    pageSelector
                    (fun (pageNum, page) ->
                        { UserState = flowModel.UserState 
                          PageNum = pageNum 
                          Page = page } 
                        |> renderInfoSelectorFactory 
                    ) 
                    modifier
                    document
            ) 
            flowModel.UserState
        |> Manipulate

    let addNew (pageSelector: PageSelector) (pageBoxKind: PageBoxKind) canvasActionsFactory =
        fun (flowModel: FlowModel<_>) (document: IntegratedDocument) ->
            
            IntegratedDocument.addNew 
                pageSelector 
                pageBoxKind 
                (fun (pageNum, pdfPage) -> 
                    { PageNum = pageNum 
                      Page = pdfPage 
                      UserState = flowModel.UserState }
                    |> canvasActionsFactory
                ) 
                document

            flowModel.UserState
        |> Manipulate



    let modifyPage (pageSelector: PageSelector) (renderInfoSelectorFactory: PageModifingArguments<_> -> RenderInfoSelector) operator  = 
        fun (flowModel: FlowModel<_>) (document: IntegratedDocument) ->
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


    let trimToVisible (margin: Margin) pageSelector =
        modifyPage 
            pageSelector
            (fun _ -> 
                RenderInfoSelector.OR 
                    [ RenderInfoSelector.PathIntegrated IntegratedRenderInfo.isVisible
                      RenderInfoSelector.TextIntegrated IntegratedRenderInfo.isVisible ]
            )
            (fun args renderInfos ->
                let bound = 
                    renderInfos
                    |> Seq.choose (IntegratedRenderInfo.tryGetVisibleBound BoundGettingOptions.WithStrokeWidth)
                    |> Rectangle.ofRectangles
                args.Page.SetActualBox(bound |> Rectangle.applyMargin margin)
                |> ignore
                None
            ) ||>> ignore