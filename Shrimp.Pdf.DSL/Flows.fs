namespace Shrimp.Pdf
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open Shrimp.FSharp.Plus
open iText.Kernel.Pdf
open System.IO
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas

[<AutoOpen>]
module _Flows =
    open iText.Kernel.Pdf.Canvas.Parser

    type PageFilter<'T>(pageFilter_Info: Selector<'T>, pageFilter_Infos: InfosSelector<'T>) =
        

        member x.PageFilter_Info = pageFilter_Info

        member x.PageFilter_Infos = pageFilter_Infos

        member x.SetInfo(fInfo) = PageFilter(fInfo pageFilter_Info, pageFilter_Infos) 

        member x.AddFilter_BoundIsInsideActualBox() =
            x.SetInfo(fun selector ->
                Selector.AND[
                    selector
                    PathOrText(
                        Info.BoundIsInsideOf(AreaGettingOptions.PageBox PageBoxKind.ActualBox)
                    )
                ]
            )

        member x.SetInfos(fSelectors) = PageFilter(pageFilter_Info, fSelectors pageFilter_Infos) 


        new (pageFilter: Selector<_>) = 
            PageFilter(pageFilter, InfosSelector.All)

        new (pageFilter: InfosSelector<_>) = 
            
            let toSelector infosSelector =
                Selector.Factory(fun args ->
                    let selector = 
                        let rec loop infosSelector = 
                            match infosSelector with 
                            | InfosSelector.Path _ -> Selector.Path(fun _ _ -> true)
                            | InfosSelector.Text _ -> Selector.Text(fun _ _ -> true)
                            | InfosSelector.Factory f -> loop (f args)
                            | InfosSelector.PathOrText _ -> Selector.PathOrText(fun _ _ -> true)
                            | InfosSelector.AND infosSelectors -> 
                                infosSelectors
                                |> List.map loop
                                |> Selector.AND

                            | InfosSelector.OR infosSelectors -> 
                                infosSelectors
                                |> List.map loop
                                |> Selector.OR

                            | InfosSelector.All _ -> Selector.PathOrText(fun _ _ -> true)

                        loop infosSelector

                    let eventTypes =
                        selector
                        |> Selector.toRenderInfoSelector args
                        |> RenderInfoSelector.toEventTypes

                    match List.sort eventTypes with 
                    | [EventType.RENDER_TEXT; EventType.RENDER_PATH; EventType.CLIP_PATH_CHANGED] -> Selector.PathOrText(fun _ _ -> true)
                    | [EventType.RENDER_PATH; EventType.CLIP_PATH_CHANGED] -> Selector.Path(fun _ _ -> true)
                    | [EventType.RENDER_TEXT; EventType.CLIP_PATH_CHANGED] -> Selector.Text(fun _ _ -> true)
                    | _ -> failwithf "Not implement, eventTypes: %A" eventTypes

                )
                
            PageFilter(toSelector pageFilter, pageFilter)



    type Flows =

        static member FilterPages(pageFilter: PageFilter<_>) =    
            let flow = 
                Flow.Manipulate(
                    ModifyPage.Create(
                        "FilterPages",
                        PageSelector.All,
                        pageFilter.PageFilter_Info,
                        (fun args infos ->
                            match Seq.length infos > 0 && (InfosSelector.predicate pageFilter.PageFilter_Infos) args infos with 
                            | true -> Some args.PageNum
                            | false -> None
                        )
                    )
                    ||>> (List.choose id >> PageNumSequence.Create)
                )
                <+>Flow.Func(fun (sequence: PageNumSequence) ->
                    Flow.Reuse(Reuses.SequencePages sequence)
                )

            flow


        static member Overly_ManipulateClippingArea(clippingPathSelector, manipulate: Manipulate<_, _>) =
            let flow =
                Flow.Manipulate (
                    Manipulate.Factory(fun flowModel doc ->
                        let clippingPathFile = 
                            let clippingPdfPath = Path.ChangeExtension(flowModel.File, ".clipping.pdf")
                            File.Copy(flowModel.File, clippingPdfPath, true)
                            let cllippingPdfFileFlow = 
                                Modify.CreateClippingPath(fun args info -> clippingPathSelector (args.MapUserState(fun _ -> flowModel.UserState)) info)
                                <+>
                                ((fun _ -> flowModel.UserState) <<|| manipulate)

                            PdfRunner.Manipulate(PdfFile clippingPdfPath) cllippingPdfFileFlow
                        
                        let cancelCompoundPath() =
                            Modify.Create_Record
                                ( PageSelector.All,
                                  [
                                    { 
                                        SelectorAndModifiersRecord.Name = "cancel compound paths"
                                        //Selector = Selector.Path(selector)
                                        Selector = Selector.Path(clippingPathSelector)
                                        Modifiers =[
                                            Modifier.CancelFillAndStroke() 
                                        ]
                                    }
                                  ]
                                )

                        cancelCompoundPath()
                        ||>> (fun userState ->
                            clippingPathFile
                        )
                    )
                )
                <+>
                Flow.Reuse(
                    Reuse.Func(fun clippingPathFile ->
                        Reuses.AddForeground(clippingPathFile)
                    )
                )
                //Flow.Reuse(
                //    Reuse(fun flowModel doc ->
                //        doc.Reader.GetPages()
                //        |> List.iteri(fun i page ->
                //            let xobject = page.CopyAsFormXObject(doc.Writer)
                //            let writerPage = 
                //                let pageSize = PageSize(page.GetActualBox())
                //                doc.Writer
                //                    .AddNewPage(pageSize)
                //                    .SetPageBoxToPage(page)

                //            let pdfCanvas = PdfCanvas(writerPage)

                //            let renewablePathInfos: RenewablePathInfo list = List.item i flowModel.UserState 

                //            //pdfCanvas.AddXObject(xobject) |> ignore

                //            let __addClippedXObject =
                                
                //                let accumulatedPathOperatorRanges =
                //                    renewablePathInfos
                //                    |> List.collect(fun m -> m.ApplyCtm_To_AccumulatedPathOperatorRanges())

                //                for operatorRange in accumulatedPathOperatorRanges do
                //                    PdfCanvas.writeOperatorRange operatorRange pdfCanvas
                //                    |> ignore
                //                pdfCanvas.Clip().EndPath() |> ignore
                //                pdfCanvas.AddXObject(xobject) |> ignore
                //            ()
                //        )

                //    )
                //)

            flow


