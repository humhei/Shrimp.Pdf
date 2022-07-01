namespace Shrimp.Pdf.Extract

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
module _Tile =
    
    type PageTilingDistincterInfo =
        {  IndexedBound: IndexedBound
           Distincter: IComparable
           BoundPredicate: IIntegratedRenderInfo -> bool
           Args: PageModifingArguments<unit>
        }
    with 
        member x.Bound = x.IndexedBound.Bound

        member x.PdfPage = x.Args.Page
        member x.PdfPageNumber = x.Args.PageNum
    

    [<RequireQualifiedAccess>]
    type PageTilingDistincter = 
        | TextCase of dinstincter: (PageModifingArguments<unit> -> IndexedBound -> IntegratedTextRenderInfo list -> IComparable) * suffixFilter: (PageTilingDistincterInfo list -> PageTilingDistincterInfo list)
    with 
        static member Text(f, ?filter) =
            PageTilingDistincter.TextCase(f, defaultArg filter id)
    
    [<RequireQualifiedAccess>]
    type PageTilingRenewInfosSplitter =
        | Filter__BoundIs_InsideOrCross of Margin
        | Groupby_CenterPointIsInside
        | Groupby_DenseBoundIsInside of Margin
    with    
        

        member internal x.GetBound() =
            match x with 
            | PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside _ ->
                IIntegratedRenderInfoIM.getDenseBound BoundGettingStrokeOptions.WithoutStrokeWidth
    
            | PageTilingRenewInfosSplitter.Groupby_CenterPointIsInside _ ->
                IIntegratedRenderInfoIM.getBound BoundGettingStrokeOptions.WithoutStrokeWidth
    
            | PageTilingRenewInfosSplitter.Filter__BoundIs_InsideOrCross _ ->
                IIntegratedRenderInfoIM.getBound BoundGettingStrokeOptions.WithStrokeWidth
    
        static member ``Groupby_DenseBoundIsInside_MM1.5`` =
            PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside Margin.``MM1.5``
    
        static member Groupby_DenseBoundIsInside_MM0 =
            PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside Margin.Zero
    
        static member Groupby_DenseBoundIsInside_MM3 =
            PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside Margin.MM3
    

        static member ``Filter__BoundIs_InsideOrCross_MM1.5`` =
            PageTilingRenewInfosSplitter.Filter__BoundIs_InsideOrCross Margin.``MM1.5``
            
    
        member internal x.Infos__GroupOrFilter_IntoOp(bounds: Rectangle list, infos: 'info list, fBound, ?predicateEx) =
            let bounds =
                bounds
                |> List.mapi(fun i bound ->
                    { Index = i
                      Bound = bound }
                )

            let rec loop_groupBy predicate accum bounds infos =
                match bounds with 
                | [] -> List.rev accum
                | bound :: t -> 
                    let currentInfos, leftInfos =
                        infos
                        |> List.partition (fun info ->
                            let infoBound: TargetPageBox = fBound info
                            match infoBound.Value with 
                            | None -> false
                            | Some infoBound ->
                                (match predicateEx with 
                                    | Some predicateEx -> 
                                        predicateEx info bound infoBound 
                                        && predicate bound infoBound
                                    | None -> predicate bound infoBound
                                )

                                   
                        )

                    let r = 
                        match currentInfos with 
                        | [] -> None
                        | _ -> Some (bound, currentInfos)

                    loop_groupBy predicate (r :: accum) t leftInfos


            match x with 
            | PageTilingRenewInfosSplitter.Filter__BoundIs_InsideOrCross margin ->
                let bounds =
                    bounds
                    |> List.map (fun m -> m.ApplyMargin margin)
                    
                bounds
                |> List.mapi(fun i bound ->
                    let infos = 
                        infos
                        |> List.filter(fun info -> 
                            let infoBound: TargetPageBox = fBound info
                            match infoBound.Value with 
                            | Some infoBound -> infoBound.Is_InsideOrCross_Of(bound.Bound)
                            | None -> false
                        )
                    match infos with 
                    | [] -> None
                    | _ ->
                        Some (bound.ApplyMargin -margin, infos)
                )
    
            | PageTilingRenewInfosSplitter.Groupby_CenterPointIsInside ->
                loop_groupBy 
                    (fun indexedBound infoBound ->
                        infoBound.IsCenterPointInsideOf (indexedBound.Bound)
                    )
                    []
                    bounds
                    infos
     
            | PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside margin ->
                let bounds =
                    bounds
                    |> List.map (fun m -> m.ApplyMargin margin)
    
                loop_groupBy 
                    (fun (indexedBound: IndexedBound) infoBound ->
                        infoBound.IsInsideOf(indexedBound.Bound)
                    )
                    []
                    bounds
                    infos
                |> List.map(Option.map(fun (m, v) ->
                    m.ApplyMargin(-margin), v
                ))
            

        member internal x.Infos__GroupOrFilter_Into(bounds: Rectangle list, infos: 'info list, fBound) =
            x.Infos__GroupOrFilter_IntoOp(bounds, infos, fBound)
            |> List.choose id

    [<RequireQualifiedAccess>]
    type PageTilingRenewOptions = 
        | UsingOriginPdfPage
        | VisibleInfosInActualBox of PageTilingRenewInfosSplitter
    with 
        static member DefaultValue = PageTilingRenewOptions.UsingOriginPdfPage
    
    
    type PageTilingResultCount = PageTilingResultCount of int
    with 
        member x.Value = 
            let (PageTilingResultCount count) = x
            count

    type Reuses with 
        static member TilePages (tileTable: TileTableIndexer, ?direction: Direction, ?pageTilingRenewOptions: PageTilingRenewOptions) =
            let direction = defaultArg direction Direction.Horizontal
            let pageTilingRenewOptions = defaultArg pageTilingRenewOptions PageTilingRenewOptions.UsingOriginPdfPage
            let colNum = tileTable.ColNum
            let rowNum = tileTable.RowNum
            let cellsCount = colNum * rowNum

            Reuse(fun flowModel splitDocument ->
                let reader = splitDocument.Reader
                let writer = splitDocument.Writer

                let totalNumberOfPages = reader.GetNumberOfPages()

                for i = 1 to totalNumberOfPages do
                    let pageNumber = i
                    let readerPage = reader.GetPage(i)
                    let actualBox = readerPage.GetActualBox()

                    let tileIndexeres =
                        [ 1 .. cellsCount ]
                        |> List.map(fun j ->
                            let index = (i-1) * cellsCount + j
                            TileCellIndexer.Create((index-1), direction)
                        )

                    match pageTilingRenewOptions with 
                    | PageTilingRenewOptions.UsingOriginPdfPage ->
                        for tileIndexer in tileIndexeres do
                            let tileBox = Rectangle.getTile tileIndexer tileTable actualBox

                            let writerPage = readerPage.CopyTo(writer)
                            writer.AddPage(writerPage) |> ignore

                            PdfPage.setPageBox PageBoxKind.AllBox tileBox writerPage
                            |> ignore

                    | PageTilingRenewOptions.VisibleInfosInActualBox splitter ->
                        let args: PageModifingArguments<_> = 
                            { UserState = flowModel.UserState
                              Page = readerPage
                              TotalNumberOfPages = totalNumberOfPages
                              PageNum = pageNumber
                            }

                        let selector =
                            Selector.All(InfoIM.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))

                        extractVisibleRenewableInfosToWriter 
                            args
                            selector
                            (fun infos ->
                                let bounds = 
                                    tileIndexeres
                                    |> List.map(fun tileIndexer ->
                                        Rectangle.getTile tileIndexer tileTable actualBox
                                    )

                                splitter.Infos__GroupOrFilter_IntoOp(
                                    bounds,
                                    infos,
                                    (fun m ->   
                                        let targetPageBox = splitter.GetBound() m.OriginInfo
                                        TargetPageBox targetPageBox
                                    )
                                )
                                |> List.mapi(fun i targetPageInfoOp ->
                                    match targetPageInfoOp with 
                                    | Some (targetPageBox, infos) ->
                                        TargetRenewablePageInfo.NewPage(TargetPageBox (Some targetPageBox.Bound), TargetRenewableNewPageInfoElement.Create infos)

                                    | None ->
                                        TargetRenewablePageInfo.EmptyPage(TargetPageBox (Some bounds.[i]))
                                )

                            )
                            false
                            ignore
                            reader
                            writer
                        |> ignore
                            


                PageTilingResultCount (cellsCount)
                |> List.replicate (reader.GetNumberOfPages())
                |> AtLeastOneList.Create

            ) 
            |> Reuse.rename 
                "TilePages"
                ["tileTable" => tileTable.ToString()
                 "pageTilingRenewOptions" => pageTilingRenewOptions.ToString() ]
        

        static member TilePages (selector: Selector<'userState>, ?distincter: PageTilingDistincter, ?sorter: SelectionSorter, ?pageTilingRenewOptions: PageTilingRenewOptions) =
            let sorter = defaultArg sorter (SelectionSorter.DefaultValue)
            let pageTilingRenewOptions = defaultArg pageTilingRenewOptions PageTilingRenewOptions.UsingOriginPdfPage

            fun (flowModel: FlowModel<'userState>) (splitDocument: SplitDocument) ->

                let reader = splitDocument.Reader
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

                                sorter.Sort (rects)
                                |> List.mapi (fun i bound ->
                                    { Index = i
                                      Bound = bound }
                                )

                            let boundGroups =
                                match distincter with 
                                | None -> 
                                    bounds
                                    |> List.map(fun bound ->
                                        {  IndexedBound = bound 
                                           Distincter = 0 :> IComparable
                                           Args = args
                                           BoundPredicate = predicate }
                                    )

                                | Some (distincter) ->
                                    match distincter with 
                                    | PageTilingDistincter.TextCase (distincter, _) ->
                                        let infos = 
                                            let selector = 
                                                Selector.Text(fun _ _ -> true)
                                                |> Selector.toRenderInfoSelector args

                                            NonInitialClippingPathPdfDocumentContentParser.parse i selector parser
                                            |> List.ofSeq

                                        let infos = 
                                            infos
                                            |> List.choose IIntegratedRenderInfo.asITextRenderInfo

                                        let groupedInfos =
                                            infos
                                            |> List.groupBy(fun m ->
                                                let textBound = ITextRenderInfo.getDenseBound BoundGettingStrokeOptions.WithoutStrokeWidth m
                                                bounds
                                                |> List.tryFind(fun bound ->
                                                    textBound.IsInsideOf bound.Bound 
                                                )
                                            )

                                        groupedInfos
                                        |> List.choose(fun (bound, infos) ->
                                            match bound with 
                                            | None -> None
                                            | Some bound ->
                                                {  IndexedBound = bound 
                                                   Distincter = 
                                                    infos
                                                    |> distincter (args.DisposeUserState()) (bound)
                                                   Args = args
                                                   BoundPredicate = predicate
                                                }
                                                |> Some 
                                        )
                                 

                            yield boundGroups

                    ]
                    |> List.concat
            
                let boundGroups =
                    match distincter with 
                    | None -> boundGroups
                    | Some v ->
                        match v with 
                        | PageTilingDistincter.TextCase (_, filter) ->
                            boundGroups
                            |> List.distinctBy_explictly<_, IComparable>(fun m -> m.Distincter)
                            |> filter

                boundGroups
                |> List.groupBy(fun m -> m.PdfPageNumber)
                |> List.map(fun (pdfPageNumber, boundGroups) ->
                    match pageTilingRenewOptions with 
                    | PageTilingRenewOptions.UsingOriginPdfPage ->
                        boundGroups
                        |> List.iter(fun m -> 
                            let readerPage = m.PdfPage
                            let bound = m.Bound
                            let writer = splitDocument.Writer
                            let writerPageResource = readerPage.CopyTo(writer)
                            PdfPage.setPageBox PageBoxKind.AllBox bound writerPageResource |> ignore
                            writer.AddPage(writerPageResource)
                            |> ignore
                        )

                    | PageTilingRenewOptions.VisibleInfosInActualBox splitter ->
                        match boundGroups with 
                        | [] -> ()
                        | _ ->
                            let args: PageModifingArguments<_> =
                                boundGroups.[0].Args

                            let selector =
                                Selector.All(InfoIM.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))

                            extractVisibleRenewableInfosToWriter 
                                args
                                selector
                                (fun infos ->
                                    let bounds = 
                                        boundGroups
                                        |> List.map(fun m -> m.Bound)

                                    splitter.Infos__GroupOrFilter_IntoOp(
                                        bounds,
                                        infos,
                                        (fun m ->   
                                            let targetPageBox = splitter.GetBound() m.OriginInfo
                                            TargetPageBox targetPageBox
                                        )
                                    )
                                    |> List.mapi(fun i targetPageInfoOp ->
                                        match targetPageInfoOp with 
                                        | Some (targetPageBox, infos) ->
                                            TargetRenewablePageInfo.NewPage(TargetPageBox (Some targetPageBox.Bound), TargetRenewableNewPageInfoElement.Create infos)

                                        | None ->
                                            TargetRenewablePageInfo.EmptyPage(TargetPageBox (Some bounds.[i]))
                                    )
                                )
                                false
                                ignore
                                reader
                                splitDocument.Writer
                            |> ignore


                    PageTilingResultCount boundGroups.Length 


                )
                |> AtLeastOneList.Create


            |> reuse 
                "TilePages"
                [ "selector" => selector.ToString()
                  "distincter" => sprintf "%A" distincter
                  "pageTilingRenewOptions" => pageTilingRenewOptions.ToString() ]


        static member PickFromPageTilingResult(pageTilingResults: AtLeastOneList<PageTilingResultCount>, picker: PageNumSequence) =
            let pageNumSequence = 
                ([], pageTilingResults.AsList)
                ||> List.scan(fun accum m -> 
                    let maxValue = 
                        match List.tryLast accum with 
                        | Some last -> last 
                        | None -> 0

                    [(maxValue + 1) .. (m.Value + maxValue)]
                ) 
                |> List.tail
                |> List.collect (fun m ->
                    picker.Value
                    |> List.choose (fun n -> List.tryItem (n.PageNumValue - 1) m)
                )
                |> EmptablePageNumSequence.Create

            Reuses.SequencePages(pageNumSequence)