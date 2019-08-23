namespace Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Parser
open iText.Kernel.Pdf.Canvas.Parser
open Shrimp.Pdf
open FParsec
open iText.Kernel.Geom


type Operator =
    static member GetBound() =
        fun (args: PageModifingArguments<_>) infos ->
            let trimedBox = 
                infos
                |> Seq.map (IAbstractRenderInfo.getBound BoundGettingOptions.WithoutStrokeWidth)
                |> Rectangle.ofRectangles
            Some trimedBox

    static member PickTexts(picker: Parser<_, _>) =
        fun (args: PageModifingArguments<_>) infos ->
            infos 
            |> Seq.choose IIntegratedRenderInfo.asITextRenderInfo
            |> Seq.tryPick (fun renderInfo ->
                let text = ITextRenderInfo.getText renderInfo
                match run picker text with 
                | Success (result, _ ,_ )-> Some result
                | Failure (_, _ , _) -> None
            )

    static member SetPageBox(rect: Rectangle, pageBoxKind: PageBoxKind) =
        fun (args: PageModifingArguments<_>) infos ->
            PdfPage.setPageBox pageBoxKind rect args.Page
            |> ignore
            None

    static member GetPageEdge (innerBox: Rectangle, pageBoxKind: PageBoxKind) =
        fun (args: PageModifingArguments<_>) infos ->
            PdfPage.getEdge innerBox pageBoxKind args.Page
            |> Some

    static member AddText(text, mapping)  =
        fun (args: PageModifingArguments<_>) ->
            [ Canvas.addText text (mapping) ]



[<AutoOpen>]
module ModifyPageOperators =
    let modifyPageCurried name (pageSelector: PageSelector) (selector: Selector<_>) operator  = 
        fun (flowModel: FlowModel<_>) (document: IntegratedDocument) ->
            let document = document.Value
            let parser = new PdfDocumentContentParser(document)
            let selectedPageNumbers = document.GetPageNumbers(pageSelector) 
            Logger.infoWithStopWatch (sprintf "MODIFYPAGE: %s" name) (fun _ ->
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
                        let infos = PdfDocumentContentParser.parse pageNum renderInfoSelector parser
                        operator args infos 
                    else None
                )
            )
        |> Manipulate


    let modifyPage (name: string, pageSelector, (selector: Selector<'userState>), (operator)) =
        modifyPageCurried (name) pageSelector selector operator

    let addNewCurried name (pageSelector: PageSelector) (pageBoxKind: PageBoxKind) canvasActionsFactory =
        fun (flowModel: FlowModel<_>) (document: IntegratedDocument) ->
            Logger.info (sprintf "ADDNEW: %s" name)
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

    let addNew (name: string, pageSelector, pageboxKind, canvasActionsFactory) =
        addNewCurried name pageSelector pageboxKind canvasActionsFactory