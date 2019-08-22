namespace Shrimp.Pdf

open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Manipulates
open Parser.ReadMutual
open Shrimp.Pdf.Parser
open FParsec
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf

[<AutoOpen>]
module DSLOperators =
    let (<&>) (a: PageModifingArguments<_> -> #IAbstractRenderInfo -> bool) (b:PageModifingArguments<_> -> #IAbstractRenderInfo -> bool) =
        fun args renderInfo ->
            a args renderInfo
            && b args renderInfo

    let (!!) (a: PageModifingArguments<_> -> #IAbstractRenderInfo -> bool) =
        fun args renderInfo ->
            not (a args renderInfo)
            

module DSL =

    type Selector<'userState> =
        | Path of (PageModifingArguments<'userState> -> IntegratedPathRenderInfo -> bool)
        | Text of (PageModifingArguments<'userState> -> IntegratedTextRenderInfo -> bool)
        | Factory of (PageModifingArguments<'userState> -> Selector<'userState>)
        | PathOrText of (PageModifingArguments<'userState> -> IIntegratedRenderInfo -> bool)
        | Dummy
        | AND of Selector<'userState> list
        | OR of Selector<'userState> list


    type _SelectionModifierAddNewArguments<'userState> =
        { CurrentRenderInfo: IIntegratedRenderInfo  
          PageModifingArguments: PageModifingArguments<'userState> }

    with 
        member x.PageNum = x.PageModifingArguments.PageNum

        member x.UserState = x.PageModifingArguments.UserState

        member x.Page = x.PageModifingArguments.Page

    type _SelectionModifierFixmentArguments<'userState> =
        { Close: OperatorRange
          PageModifingArguments: PageModifingArguments<'userState> }

    with 
        member x.PageNum = x.PageModifingArguments.PageNum

        member x.UserState = x.PageModifingArguments.UserState

        member x.Page = x.PageModifingArguments.Page


    [<RequireQualifiedAccess>]
    module private Selector =
        let rec toRenderInfoSelector (args: PageModifingArguments<_>) selector =
            match selector with 
            | Selector.Path factory -> factory args |> RenderInfoSelector.Path
            | Selector.Text factory -> factory args |> RenderInfoSelector.Text 
            | Selector.PathOrText factory -> factory args |> RenderInfoSelector.PathOrText
            | Selector.Factory factory -> toRenderInfoSelector args (factory args)
            | Selector.Dummy -> RenderInfoSelector.Dummy
            | Selector.AND selectors -> 
                selectors
                |> List.map (toRenderInfoSelector args)
                |> RenderInfoSelector.AND
            | Selector.OR selectors ->
                selectors
                |> List.map (toRenderInfoSelector args)
                |> RenderInfoSelector.OR


    let modifyPage (name: string, pageSelector, (selector: Selector<'userState>), (operator)) =
        modifyPage (name) pageSelector (fun args -> Selector.toRenderInfoSelector args (selector)) operator

    let addNew (name: string, pageSelector, pageboxKind, canvasActionsFactory) =
        addNew name pageSelector pageboxKind canvasActionsFactory

    type Modifier<'userState> =
        | DropColor 
        | AddNew of list<_SelectionModifierAddNewArguments<'userState> -> list<PdfCanvas -> PdfCanvas>>
        | Fix of list<_SelectionModifierFixmentArguments<'userState> -> list<PdfCanvas -> PdfCanvas>>

 

    [<RequireQualifiedAccess>]
    module private Modifier =
        let toSelectionModifier (pageModifingArguments: PageModifingArguments<_>) (modifier: Modifier<_>) =
            match modifier with
            | Modifier.AddNew factorys ->
                fun (args: _SelectionModifierAddNewArguments) ->
                    let actions = 
                        let args = 
                            { CurrentRenderInfo = args.CurrentRenderInfo
                              PageModifingArguments = pageModifingArguments }

                        factorys
                        |> List.collect (fun factory -> factory args)

                    actions
                |> SelectionModifier.AddNew

            | Modifier.Fix factorys ->
                fun (args: _SelectionModifierFixmentArguments) ->
                    let actions = 
                        let args  = 
                            { Close = args.Close
                              PageModifingArguments = pageModifingArguments }
                        factorys
                        |> List.collect (fun factory -> factory args)
                    actions
                |> SelectionModifier.Fix

            | Modifier.DropColor -> SelectionModifier.DropColor

    type Modify =
        static member SetStrokeColor(color: Color) =
            fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
                [
                    PdfCanvas.setStrokeColor (color)
                    PdfCanvas.writeOperatorRange args.Close
                ]

        static member AddRectangleToBound(mapping) =
            fun (args: _SelectionModifierAddNewArguments<'userState>)  ->
                let border = IAbstractRenderInfo.getBound BoundGettingOptions.WithStrokeWidth args.CurrentRenderInfo
                [
                    PdfCanvas.addRectangle border mapping
                ]

    type _SelectAndModify<'userState> =
        { Selector: Selector<'userState> 
          Modifier: Modifier<'userState> }

    let modify (name: string, pageSelector, (operators: list<_SelectAndModify<'userState>>)) =
        modify 
            name
            pageSelector 
            (
                operators 
                |> List.map (fun (selectAndModify) ->
                    (fun args -> Selector.toRenderInfoSelector args selectAndModify.Selector), (fun args -> Modifier.toSelectionModifier args selectAndModify.Modifier)
                )
            )

 



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


    type Info =
        static member StrokeColoris (color: Color) =
            fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
                IAbstractRenderInfo.hasStroke info
                && info.Value.GetStrokeColor() = color

        static member StrokeColorisOneOf (colors: Color seq)  =
            fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
                colors
                |> Seq.exists (fun color -> 
                    (Info.StrokeColoris color) args info
                )

        static member FillColoris (color: Color) =
            fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
                IAbstractRenderInfo.hasFill info
                && info.Value.GetFillColor() = color

        static member FillColorisOneOf (colors: Color seq) =
            fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
                colors
                |> Seq.exists (fun color -> Info.FillColoris color args info)


        static member BoundIsInsideOf (rect, ?boundGettingOptions) =
            fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
                let boundGettingOptions = defaultArg boundGettingOptions BoundGettingOptions.WithoutStrokeWidth
                (IAbstractRenderInfo.getBound boundGettingOptions info).IsInsideOf(rect)

        static member BoundIsOutsideOf (rect, ?boundGettingOptions) =
            fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
                let boundGettingOptions = defaultArg boundGettingOptions BoundGettingOptions.WithoutStrokeWidth
                (IAbstractRenderInfo.getBound boundGettingOptions info).IsOutsideOf(rect)

        static member BoundIsInsideOfPageBox(?pageBoxKind: PageBoxKind, ?boundGettingOptions) =
            fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) ->
                let pageBoxKind = defaultArg pageBoxKind PageBoxKind.ActualBox
                let pageBox = args.Page.GetPageBox(pageBoxKind)
                match boundGettingOptions with 
                | Some boundGettingOptions -> Info.BoundIsInsideOf (pageBox, boundGettingOptions) args info
                | None -> Info.BoundIsInsideOf (pageBox) args info

    [<AutoOpen>]
    module Reuse =
        let tilePagesByRenderInfoSelectorFactory (selector: Selector<'userState>) =
            fun (flowModel: FlowModel<'userState>) (splitDocument: SplitDocument) ->
                Logger.info (sprintf "TILEPAGESBYSELECTORFACTORY")

                let reader = splitDocument.Reader
                let parser = new PdfDocumentContentParser(reader)
                for i = 1 to reader.GetNumberOfPages() do
                    let readerPage = reader.GetPage(i)
                    let args =
                        { Page = readerPage
                          UserState = flowModel.UserState 
                          TotalNumberOfPages = splitDocument.Reader.GetNumberOfPages() 
                          PageNum = i }
                    
                    let selector = (Selector.toRenderInfoSelector args selector)
                    let infos = PdfDocumentContentParser.parse i selector parser
                    for info in infos do
                        let bound = IAbstractRenderInfo.getBound BoundGettingOptions.WithoutStrokeWidth info
                        let writer = splitDocument.Writer
                        let writerPageResource = readerPage.CopyTo(writer)
                        PdfPage.setPageBox PageBoxKind.AllBox bound writerPageResource |> ignore
                        writer.AddPage(writerPageResource)
                        |> ignore

                flowModel.UserState
            |> Reuse
