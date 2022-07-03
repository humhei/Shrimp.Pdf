namespace Shrimp.Pdf.Extract

open iText.Kernel.Pdf

#nowarn "0104"
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open Shrimp.Pdf
open Shrimp.Pdf.Constants
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Colors
open Shrimp.Pdf.Imposing
open Shrimp.FSharp.Plus
open System
open Akkling
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas

open iText.Kernel.Pdf

#nowarn "0104"
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open Shrimp.Pdf
open Shrimp.Pdf.Parser
open Shrimp.FSharp.Plus
open iText.Kernel.Geom
open System
open Shrimp.Pdf.Image
open Akkling

[<AutoOpen>]
module _TransformForEach =

    [<RequireQualifiedAccess>]
    type TransformForEachSuffixAction =
        | AddCropmarkAndCropToBorder of Cropmark * Margin
        | CropToBorder of Margin
    with 
        static member AddCropmarkAndCropToBorder_Default =
            TransformForEachSuffixAction.AddCropmarkAndCropToBorder(Cropmark.defaultValue, Margin.MM6)

        

    type Reuses with 
        static member TransformForEach (selector: Selector<'userState>, transform: IndexedBound -> Rectangle, ?renewInfosSplitter: PageTilingRenewInfosSplitter, ?suffixAction: TransformForEachSuffixAction, ?borderSorter, ?borderKeepingPageSelector, ?textPicker) =
            let renewInfosSplitter = defaultArg renewInfosSplitter PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside_MM0
            let borderSorter = defaultArg borderSorter SelectionSorter.DefaultValue
            let borderKeepingPageSelector = defaultArg borderKeepingPageSelector (PageSelector.First)
            fun (flowModel: FlowModel<'userState>) (splitDocument: SplitDocument) ->

                let reader = splitDocument.Reader
                let borderKeepingPageNumbers = 
                    reader.GetPageNumbers(borderKeepingPageSelector)

                let parser = new NonInitialClippingPathPdfDocumentContentParser(reader)
                let boundGroups = 
                    [
                        for i = 1 to reader.GetNumberOfPages() do
                            let readerPage = reader.GetPage(i)
                            let args =
                                { Page = readerPage
                                  UserState = flowModel.UserState 
                                  TotalNumberOfPages = splitDocument.Reader.GetNumberOfPages() 
                                  PageNum = i }
                
                                

                            let selector = (Selector.toRenderInfoSelector args selector)
                            let predicate = RenderInfoSelector.toRenderInfoPredication selector
                            let args = args.DisposeUserState()

                            let bounds = 
                                let rects =
                                    NonInitialClippingPathPdfDocumentContentParser.parse i selector parser
                                    |> List.ofSeq
                                    |> List.map (fun info -> 
                                        IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth info
                                    )
                                    |> Rectangle.removeInboxes

                                rects
                                |> List.mapi(fun i bound ->
                                    { Index = i 
                                      Bound = bound }
                                )

                            let boundGroups =
                                bounds
                                |> List.map(fun bound ->
                                    {  IndexedBound = bound 
                                       Distincter = 0 :> IComparable
                                       Args = args
                                       BoundPredicate = predicate }
                                )

                            yield boundGroups

                    ]
                    |> List.concat
            
                

                boundGroups
                |> List.groupBy(fun m -> m.PdfPageNumber)
                |> List.map(fun (pdfPageNumber, boundGroups) ->
                    match boundGroups with 
                    | [] -> []
                    | _ ->
                        let splitter = renewInfosSplitter
                        let boundPredicate = boundGroups.[0].BoundPredicate
                        let args: PageModifingArguments<_> =
                            boundGroups.[0].Args

                        let selector =
                            Selector.All(InfoIM.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                        
                        let infos = 
                            extractVisibleRenewableInfosToWriter 
                                args
                                selector
                                (fun infos ->
                                    let bounds = 
                                        boundGroups
                                        |> List.map(fun m -> m.Bound)

                                    let targetPageInfos = 
                                        splitter.Infos__GroupOrFilter_IntoOp(
                                            bounds,
                                            infos,
                                            (fun m ->   
                                                let targetPageBox = splitter.GetBound() m.OriginInfo
                                                TargetPageBox targetPageBox
                                            )
                                            //predicateEx = (fun info bound infoBound ->
                                            //    match info with 
                                            //    | RenewableInfo.Path info ->
                                            //        match FsColor.OfItextColor info.StrokeColor with 
                                            //        | FsColor.EqualTo FsColor.RGB_BLUE _ -> true
                                            //        | _ -> true

                                            //    | _ -> true
                                            //)
                                        )
                                        |> List.choose id

                                    targetPageInfos
                                    |> List.map(fun (indexedBound, infos) ->
                                        { TargetNewInfosInOriginPageElement.RenewableInfos = infos 
                                          Bound = indexedBound 
                                          NewBound = 
                                            { indexedBound with Bound = transform indexedBound }
                                          BoundPredicate = boundPredicate
                                          BorderKeepingPageNumbers = borderKeepingPageNumbers
                                          TextPickerTagColor =
                                            match textPicker with 
                                            | Some textPicker -> textPicker.TagColor
                                            | None -> None
                                        }
                                    )
                                    |> TargetRenewablePageInfo.NewInfosInOriginPage
                                    |> List.singleton

                                )
                                false
                                (fun (pdfPage, canvas) ->
                                    match suffixAction with 
                                    | Some suffixAction ->
                                        let areas = 
                                            boundGroups
                                            |> List.map(fun m -> m.Bound)
                                            |> borderSorter.SortToLists
                                            |> List.map AtLeastOneList.Create
                                            |> AtLeastOneList.Create
                                            |> CropmarkAreas
                                    
                                        match suffixAction with 
                                        | TransformForEachSuffixAction.AddCropmarkAndCropToBorder (cropmark, _) ->
                                            areas.AddCropmarkToPdfCanvas(cropmark, canvas)
                                            |> ignore

                                        | TransformForEachSuffixAction.CropToBorder _ -> ()

                                        let margin =
                                            match suffixAction with
                                            | TransformForEachSuffixAction.AddCropmarkAndCropToBorder (_, margin) ->
                                                margin
                                            | TransformForEachSuffixAction.CropToBorder margin -> margin

                                        let bound = 
                                            areas.AsAl1List
                                            |> Rectangle.ofRectangles

                                        pdfPage.SetAllBox(
                                            Rectangle.applyMargin margin bound
                                        )
                                        |> ignore

                                    | None -> ()

                                )
                                borderKeepingPageNumbers
                                reader
                                splitDocument.Writer

                        let infos =
                            let info = List.exactlyOne infos
                            match info with 
                            | TargetRenewablePageInfo.NewInfosInOriginPage infos -> infos
                            |_ -> failwith "Invalid token"

                        let r = 
                            match textPicker with 
                            | Some textPicker ->
                                let textPicker = textPicker.TransformTextPickers
                                infos
                                |> List.map(fun infos ->
                                    let textInfos = 
                                        infos.RenewableInfos
                                        |> List.choose(fun m -> IIntegratedRenderInfoIM.asVector m.OriginInfo)

                                    textPicker args infos.Bound textInfos
                                )

                            | None -> []
                        r


                )
                |> AtLeastOneList.Create


            |> reuse 
                "TransformForEach"
                [ "selector" => selector.ToString()
                  "transform" => transform.ToString()
                  "pageTilingRenewOptions" => renewInfosSplitter.ToString()
                  "suffixAction" => sprintf "%A" suffixAction
                  "borderSorter" => borderSorter.ToString()
                  "borderKeepingPageSelector" => borderKeepingPageSelector.ToString()
                  "textPicker" => sprintf "%A" textPicker ]

