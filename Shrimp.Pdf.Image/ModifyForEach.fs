namespace Shrimp.Pdf

open iText.Kernel.Pdf

#nowarn "0104"
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Extensions
open Shrimp.Pdf
open Shrimp.Pdf.Constants
open Shrimp.Pdf.Parser
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

//[<AutoOpen>]
//module _ModifyForEach =
//    type Reuses with 
//        static member ModifyForEach (selector: Selector<'userState>, ?renewInfosSplitter: PageTilingRenewInfosSplitter) =
//            let renewInfosSplitter = defaultArg renewInfosSplitter PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside_MM0

//            fun (flowModel: FlowModel<'userState>) (splitDocument: SplitDocument) ->

//                let reader = splitDocument.Reader
//                let parser = new NonInitialClippingPathPdfDocumentContentParser(reader)
//                let boundGroups = 
//                    [
//                        for i = 1 to reader.GetNumberOfPages() do
//                            let readerPage = reader.GetPage(i)
//                            let args =
//                                { Page = readerPage
//                                  UserState = flowModel.UserState 
//                                  TotalNumberOfPages = splitDocument.Reader.GetNumberOfPages() 
//                                  PageNum = i }
                
                                

//                            let selector = (Selector.toRenderInfoSelector args selector)

//                            let args = args.DisposeUserState()

//                            let bounds = 
//                                let rects =
//                                    NonInitialClippingPathPdfDocumentContentParser.parse i selector parser
//                                    |> List.ofSeq
//                                    |> List.map (fun info -> 
//                                        IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth info
//                                    )
//                                    |> Rectangle.removeInboxes

//                                rects

//                            let boundGroups =
//                                bounds
//                                |> List.map(fun bound ->
//                                    {  Bound = bound 
//                                       Distincter = 0 :> IComparable
//                                       Args = args }
//                                )

//                            yield boundGroups

//                    ]
//                    |> List.concat
            

//                boundGroups
//                |> List.groupBy(fun m -> m.PdfPageNumber)
//                |> List.map(fun (m, boundGroups) ->
//                    match boundGroups with 
//                    | [] -> ()
//                    | _ ->
//                        let splitter = renewInfosSplitter
//                        let args: PageModifingArguments<_> =
//                            boundGroups.[0].Args

//                        let selector =
//                            Selector.All(InfoIM.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))

//                        extractVisibleRenewableInfosToWriter 
//                            args
//                            selector
//                            (fun infos ->
//                                let bounds = 
//                                    boundGroups
//                                    |> List.map(fun m -> m.Bound)

//                                let targetPageInfos = 
//                                    splitter.Infos__GroupOrFilter_IntoOp(
//                                        bounds,
//                                        infos,
//                                        (fun m ->   
//                                            let targetPageBox = splitter.GetBound() BoundGettingStrokeOptions.WithStrokeWidth m.OriginInfo
//                                            TargetPageBox targetPageBox
//                                        )
//                                    )
//                                    |> List.choose id




//                                |> List.mapi(fun i targetPageInfoOp ->
//                                    match targetPageInfoOp with 
//                                    | Some (targetPageBox, infos) ->
//                                        TargetRenewablePageInfo.NewPage(TargetPageBox (Some targetPageBox.Bound), infos)

//                                    | None ->
//                                        TargetRenewablePageInfo.EmptyPage(TargetPageBox (Some bounds.[i]))
//                                )
//                            )
//                            false
//                            reader
//                            splitDocument.Writer
//                        |> ignore


//                    PageTilingResultCount boundGroups.Length 


//                )
//                |> AtLeastOneList.Create


//            |> reuse 
//                "TilePages"
//                [ "selector" => selector.ToString()
//                  "pageTilingRenewOptions" => renewInfosSplitter.ToString() ]

