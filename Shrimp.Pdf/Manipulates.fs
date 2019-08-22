namespace Shrimp.Pdf
open Shrimp.Pdf.Parser
open iText.Layout
open Shrimp.Pdf.Extensions
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf
open System.Runtime.CompilerServices


[<RequireQualifiedAccess>]
module IntegratedDocument =
    let addNew (pageSelector: PageSelector) (pageBoxKind: PageBoxKind) (canvasActionsFactory: (PageNumber * PdfPage) -> list<Canvas -> Canvas>) (document: IntegratedDocument) =

        let selectedPageNumbers = document.Value.GetPageNumbers(pageSelector) 
        let totalNumberOfPages = document.Value.GetNumberOfPages()
        for i = 1 to totalNumberOfPages do
            if List.contains i selectedPageNumbers then
                let page = document.Value.GetPage(i)
                let canvasActions = canvasActionsFactory(PageNumber i, page)
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
          TotalNumberOfPages: int
          PageNum: int }

    [<Extension>]
    type PageModifingArgumentsExtensions() =
        [<Extension>]
        static member PageUserState(xs: PageModifingArguments<'T list>) = 
            if xs.UserState.Length = xs.TotalNumberOfPages then
                xs.UserState.[xs.PageNum - 1]
            else failwithf "Cannot get current userState because states length %d is not equal to totalNumberOfPages %d" xs.UserState.Length xs.TotalNumberOfPages 

    let modify (pageSelector: PageSelector) (operators: list<(PageModifingArguments<_> -> RenderInfoSelector) * SelectionModifier>)   =
        fun (flowModel: FlowModel<_>) (document: IntegratedDocument) ->
            operators
            |> List.iter (fun (renderInfoSelectorFactory, modifier) ->
                IntegratedDocument.modify 
                    pageSelector
                    (fun (PageNumber pageNum, page) ->
                        { UserState = flowModel.UserState 
                          PageNum = pageNum 
                          TotalNumberOfPages = document.Value.GetNumberOfPages()
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
                (fun (PageNumber pageNum, pdfPage) -> 
                    { PageNum = pageNum 
                      Page = pdfPage 
                      TotalNumberOfPages = document.Value.GetNumberOfPages()
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
                    let args = 
                        { UserState = flowModel.UserState;
                          Page = page
                          TotalNumberOfPages = document.GetNumberOfPages()
                          PageNum = pageNum }

                    let renderInfoSelector = renderInfoSelectorFactory args
                          
                    let infos = PdfDocumentContentParser.parse pageNum renderInfoSelector parser

                    operator args infos 
                else None
            )
        |> Manipulate


    let trimToVisible (margin: Margin) pageSelector =
        modifyPage 
            pageSelector
            (fun _ -> 
                RenderInfoSelector.OR 
                    [ RenderInfoSelector.Path IIntegratedRenderInfo.isVisible
                      RenderInfoSelector.Text IIntegratedRenderInfo.isVisible ]
            )
            (fun args renderInfos ->
                let bound = 
                    renderInfos
                    |> Seq.choose (IIntegratedRenderInfo.tryGetVisibleBound BoundGettingOptions.WithStrokeWidth)
                    |> Rectangle.ofRectangles
                args.Page.SetActualBox(bound |> Rectangle.applyMargin margin)
                |> ignore
                None
            ) ||>> ignore