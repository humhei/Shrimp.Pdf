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

    [<RequireQualifiedAccess>]
    type Overly_Clip_ManipulateArea =
        | ClippingArea
        | Background

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


        static member Overly_Clip_Manipulate(clippingPathSelector, manipulate: Manipulate<_, _>, area: Overly_Clip_ManipulateArea, ?excludingSelector, ?keepCompoundPath) =
            let flow =
                Flow.Func(fun originUserState ->
                    let manipulate = (fun _ -> originUserState) <<|| manipulate ||>> ignore
                    let clippingPathSelector (args: PageModifingArguments<_>) info =
                        clippingPathSelector (args.MapUserState(fun _ -> originUserState)) info
                    let readClippingPathSelector() = 
                        ModifyPage.Create(
                            "Read clippingPathSelector",
                            PageSelector.All,
                            Selector.Path clippingPathSelector,
                            (fun args info -> List.ofSeq info)
                        )

                    Flow.Manipulate(
                        readClippingPathSelector()
                    )
                    <+>
                    Flow.Func(fun (infos: _ list list) ->
                        let isAllPage_ExistsClippingPathInfo =
                            infos
                            |> List.forall(fun m -> 
                                m.Length > 0
                            )

                        match isAllPage_ExistsClippingPathInfo with 
                        | true -> 
                            Flow.Manipulate (
                                Manipulate.Factory(fun flowModel doc ->


                                    let clippingPathFile = 
                                        let clippingPdfPath = Path.ChangeExtension(flowModel.File, ".clipping.pdf")
                                        File.Copy(flowModel.File, clippingPdfPath, true)
                                        let cllippingPdfFileFlow = 
                                            let apply_excludingSelector() =
                                                (
                                                    match excludingSelector with 
                                                    | None -> Manipulate.dummy() ||>> ignore
                                                    | Some excludingSelector ->
                                                        let excludingSelector = Selector.redirectSelectorUserState originUserState excludingSelector
                                                        Modify.Create(
                                                            PageSelector.All,
                                                            [
                                                                { SelectorAndModifiersRecord.Selector = excludingSelector
                                                                  Name = "Remove excluding items"
                                                                  Modifiers = [
                                                                    Modifier.CancelFillAndStroke()
                                                                  ]
                                                                }
                                                            ]
                                                        )
                                                )

                                            apply_excludingSelector()
                                            <+>
                                            match area with 
                                            | Overly_Clip_ManipulateArea.ClippingArea ->
                                                Modify.CreateClippingPath(clippingPathSelector)
                                                <+>
                                                (manipulate)
                                            | Overly_Clip_ManipulateArea.Background ->
                                                Modify.CreateClippingPath(clippingPathSelector)

                                        PdfRunner.Manipulate(PdfFile clippingPdfPath) cllippingPdfFileFlow
                                    
                                    let cancelCompoundPath() =
                                        match defaultArg keepCompoundPath false with 
                                        | false ->
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

                                        | true -> Manipulate.dummy() ||>> ignore

                                    cancelCompoundPath()
                                    <+>(match area with 
                                        | Overly_Clip_ManipulateArea.ClippingArea -> Manipulate.dummy() ||>> ignore
                                        | Overly_Clip_ManipulateArea.Background -> 
                                            (manipulate) 
                                    )
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
                        | false -> 
                            Flow.dummy() ||>> ignore
                    )

                )



            flow


