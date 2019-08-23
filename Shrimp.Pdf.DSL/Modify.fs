namespace Shrimp.Pdf.DSL
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Parser
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf

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

type _SelectorAndModifier<'userState> =
    { Selector: Selector<'userState> 
      Modifier: Modifier<'userState>
      Name: string }

[<AutoOpen>]
module ModifyOperators =
    let modify (pageSelector, (operators: list<_SelectorAndModifier<'userState>>)) =
        let names = 
            operators
            |> List.map (fun selectorAndModifier -> selectorAndModifier.Name)

        if names.Length <> (List.distinct names).Length then failwithf "Duplicated keys in SelectorAndModifiers %A" operators

        fun (flowModel: FlowModel<_>) (document: IntegratedDocument) ->
            let totalNumberOfPages = document.Value.GetNumberOfPages()
            IntegratedDocument.modify
                (String.concat "\n" names)
                pageSelector 
                (
                    fun (PageNumber pageNum, pdfPage) ->
                        operators 
                        |> List.map (fun (selectAndModify) ->
                            let pageModifingArguments =
                                { PageNum = pageNum 
                                  UserState = flowModel.UserState
                                  Page = pdfPage
                                  TotalNumberOfPages = totalNumberOfPages }
                            ( SelectorModiferToken selectAndModify.Name, 
                                ( Selector.toRenderInfoSelector pageModifingArguments selectAndModify.Selector,
                                    Modifier.toSelectionModifier pageModifingArguments selectAndModify.Modifier)
                            )
                        )
                        |> Map.ofList
                ) document

            flowModel.UserState

        |> Manipulate