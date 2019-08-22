namespace Shrimp.Pdf

open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Manipulates
open Parser.ReadMutual
open Shrimp.Pdf.Parser
open FParsec
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Layout
open iText.StyledXmlParser.Jsoup.Select

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

    type _SelectAndModify<'userState> =
        { Selector: Selector<'userState> 
          Modifier: SelectionModifier }

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

    //type Modifier<'userState> =
    //    | DropColor 
    //    | AddNew of (_SelectionModifierAddNewArguments<'userState> -> list<PdfCanvas -> PdfCanvas>)
    //    | Fix of (_SelectionModifierFixmentArguments<'userState> -> list<PdfCanvas -> PdfCanvas>)

    //[<RequireQualifiedAccess>]
    //module private Modifier =
    //    let toSelectionModifier (pageModifingArguments: PageModifingArguments<_>) (modifier: Modifier<_>) =
    //        match modifier with
    //        | Modifier.AddNew factory ->
    //            fun (args: _SelectionModifierAddNewArguments) ->
    //                let actions = 
    //                    let args = 
    //                        { CurrentRenderInfo = args.CurrentRenderInfo
    //                          PageModifingArguments = pageModifingArguments }
    //                    factory args

    //                actions
    //            |> SelectionModifier.AddNew

    //        | Modifier.Fix factory ->
    //            fun (args: _SelectionModifierFixmentArguments) ->
    //                let actions = 
    //                    let args  = 
    //                        { Close = args.Close
    //                          PageModifingArguments = pageModifingArguments }
    //                    factory args

    //                actions
    //            |> SelectionModifier.Fix

    //        | Modifier.DropColor -> SelectionModifier.DropColor
            

    [<RequireQualifiedAccess>]
    module private Selector =
        let rec toRenderInfoSelectorFactory (args: PageModifingArguments<_>) selector =

            match selector with 
            | Selector.Path factory -> factory args |> RenderInfoSelector.Path
            | Selector.Text factory -> factory args |> RenderInfoSelector.Text 
            | Selector.PathOrText factory -> factory args |> RenderInfoSelector.PathOrText
            | Selector.Factory factory -> toRenderInfoSelectorFactory args (factory args)
            | Selector.Dummy -> RenderInfoSelector.Dummy
            | Selector.AND selectors -> 
                selectors
                |> List.map (toRenderInfoSelectorFactory args)
                |> RenderInfoSelector.AND
            | Selector.OR selectors ->
                selectors
                |> List.map (toRenderInfoSelectorFactory args)
                |> RenderInfoSelector.OR


    let modifyPage (pageSelector, (selector: Selector<'userState>), (operator)) =
        modifyPage pageSelector (fun args -> Selector.toRenderInfoSelectorFactory args (selector)) operator

    let modify (pageSelector, (operators: list<_SelectAndModify<'userState>>)) =
        modify 
            pageSelector 
            (
                operators 
                |> List.map (fun (selectAndModify) ->
                    (fun args -> Selector.toRenderInfoSelectorFactory args selectAndModify.Selector), selectAndModify.Modifier
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


