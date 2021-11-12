namespace Shrimp.Pdf
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open Shrimp.FSharp.Plus
open iText.Kernel.Pdf
open System.IO

[<AutoOpen>]
module _Flows =
    open iText.Kernel.Pdf.Canvas.Parser

    type PageFilter<'T>(pageFilter_Info: Selector<'T>, pageFilter_Infos: InfosSelector<'T>) =
        

        member x.PageFilter_Info = pageFilter_Info

        member x.PageFilter_Infos = pageFilter_Infos

        member x.SetInfo(fInfo) = PageFilter(fInfo pageFilter_Info, pageFilter_Infos) 

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

