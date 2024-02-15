namespace Shrimp.Pdf.Extract

open iText.Kernel.Pdf
open Shrimp.Pdf.Colors
open Shrimp.Pdf.Imposing
open System.IO
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
    
    
    type PageTilingLayoutResult =
        { ColNums: int list 
          RowNum: int }
    with 
        member x.CellsCount =
            x.ColNums
            |> List.sum
    
    type PageTilingDistincterInfos =
        { Value: PageTilingDistincterInfo list 
          Layout: PageTilingLayoutResult }

    type PageTilingTextPickers = 
        { 
            TransformTextPickers: (PageModifingArguments<unit> -> IndexedBound -> IIntegratedRenderInfo list -> IComparable)
            TagColor: FsColor option
        }


    [<RequireQualifiedAccess>]
    type PageTilingDistincter = 
        | TextCase of dinstincter: (PageModifingArguments<unit> -> IndexedBound -> IntegratedTextRenderInfo list -> IComparable) * tagColor: FsColor option * suffixFilter: (PageTilingDistincterInfo list -> PageTilingDistincterInfo list)
    with 
        static member Text(f, ?tagColor, ?filter) =
            PageTilingDistincter.TextCase(f, tagColor, defaultArg filter id)
    
        member x.TagColor =
            match x with 
            | PageTilingDistincter.TextCase(_, color, _) -> color

    [<RequireQualifiedAccess>]
    type PageTilingDistincterOrTextPicker =
        | Non
        | Distincter of PageTilingDistincter
        | TextPicker of PageTilingTextPickers
    with 
        member x.TagColor =
            match x with 
            | PageTilingDistincterOrTextPicker.Non -> None
            | PageTilingDistincterOrTextPicker.Distincter (v) -> v.TagColor
            | PageTilingDistincterOrTextPicker.TextPicker v -> v.TagColor

    type internal ExpandedRenewableInfo(info: RenewableInfo) =
        member x.Info = info

    type internal ExpandedRenewableInfos(info: RenewableInfo, splitTextToWords: bool) =
        let infos =
            match info with 
            | RenewableInfo.Image _
            | RenewableInfo.Path _ -> [info]
                    
            | RenewableInfo.Text textInfo ->
                match splitTextToWords with 
                | false -> [info]
                | true ->
                    match textInfo.OriginInfo.SplitToWords() with 
                    | []
                    | [_] -> [info]
                    | infos ->
                        infos
                        |> List.mapi(fun i m -> 
                            m.Renewable(
                                isWord = true
                            )
                        )
                        |> List.map RenewableInfo.Text

            |> List.map ExpandedRenewableInfo

        member x.Infos = infos

    type internal BoundCachableInfo(info: ExpandedRenewableInfo, fBound) =
        let bound: TargetPageBox = fBound info

        member x.Info = info

        member x.Bound = bound


    [<RequireQualifiedAccess>]
    type SamplePageExtractingOptions =
        | Non
        | FirstPageFirstSelector of PdfPath
        | FirstPageMultipleSelectors of pageNumbers: PageNumber al2List * targetPdfPath: PdfPath

    [<RequireQualifiedAccess>]
    type PageTilingRenewInfosSplitter =
        | Filter__BoundIs_InsideOrCross of Margin
        | Groupby_CenterPointIsInside of splitTextToWords: bool
        | Groupby_DenseBoundIsInside of splitTextToWords:bool * Margin
    with    
        member internal x.SplitTextToWords =
            match x with 
            | Filter__BoundIs_InsideOrCross _ -> false
            | Groupby_CenterPointIsInside b -> b
            | Groupby_DenseBoundIsInside (b, _) -> b

        member x.SetSplitTextToWords(b) =
            match x with 
            | Filter__BoundIs_InsideOrCross _ -> x
            | Groupby_CenterPointIsInside _ -> PageTilingRenewInfosSplitter.Groupby_CenterPointIsInside(b)
            | Groupby_DenseBoundIsInside (_, margin) -> PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside(b, margin)

        member internal x.BoundGettingStrokeOptions =
            match x with 
            | PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside _ ->
                BoundGettingStrokeOptions.WithoutStrokeWidth
    
            | PageTilingRenewInfosSplitter.Groupby_CenterPointIsInside _ ->
                BoundGettingStrokeOptions.WithoutStrokeWidth
    
            | PageTilingRenewInfosSplitter.Filter__BoundIs_InsideOrCross _ ->
                BoundGettingStrokeOptions.WithStrokeWidth

        member internal x.GetInfoBound() =
            match x with 
            | PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside _ ->
                IIntegratedRenderInfoIM.getDenseBound x.BoundGettingStrokeOptions
    
            | PageTilingRenewInfosSplitter.Groupby_CenterPointIsInside _ ->
                IIntegratedRenderInfoIM.getBound x.BoundGettingStrokeOptions
    
            | PageTilingRenewInfosSplitter.Filter__BoundIs_InsideOrCross _ ->
                IIntegratedRenderInfoIM.getBound x.BoundGettingStrokeOptions
    
        static member ``Groupby_DenseBoundIsInside_MM1.5`` =
            PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside (true, Margin.``MM1.5``)
    
        static member Groupby_DenseBoundIsInside_MM0 =
            PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside (true, Margin.Zero)
    
        static member Groupby_DenseBoundIsInside_MM3 =
            PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside (true, Margin.MM3)
    

        static member ``Filter__BoundIs_InsideOrCross_MM1.5`` =
            PageTilingRenewInfosSplitter.Filter__BoundIs_InsideOrCross Margin.``MM1.5``
            
        
        //static member Filter__BoundIs_InsideOrCross_MM0 =
        //    PageTilingRenewInfosSplitter.Filter__BoundIs_InsideOrCross Margin.Zero
            

        static member Filter__BoundIs_InsideOrCross_MM3 =
            PageTilingRenewInfosSplitter.Filter__BoundIs_InsideOrCross Margin.MM3
            
    
        member internal x.Infos__GroupOrFilter_IntoOp(bounds: Rectangle list, infos: RenewableInfo list, ?predicateEx) =
            /// NOTE: BoxWithText.Pick already sort infos
            /// SO we shouldn't use sorter here

            //let selectionSorter = 
            //    defaultArg selectionSorter (SelectionSorter.CreatePlane(mm 0.5))

            let bounds =
                bounds
                |> List.mapi(fun i bound ->
                    { Index = i
                      Bound = bound }
                )

            let infos =
                infos
                |> List.map (fun m -> ExpandedRenewableInfos(m, x.SplitTextToWords))
                |> List.collect(fun m -> m.Infos)
                |> List.map(fun m -> BoundCachableInfo(m, fun info -> 
                    (x.GetInfoBound() info.Info.OriginInfo)
                    |> TargetPageBox.Create
                ))
                |> List.filter(fun m -> m.Bound.Value.IsSome)

            let loop_groupBy predicate predicateCross bounds infos =
                let rec loop accum (bounds: IndexedBound list) (infos: BoundCachableInfo list) =
                    
                    match bounds with 
                    | [] -> List.rev accum
                    | bound :: t -> 
                        let currentInfos, leftInfos =
                            let rec loop2 currentInfos leftInfos infos =
                                match infos with 
                                | [] -> List.rev currentInfos, List.rev leftInfos
                                | (info: BoundCachableInfo) :: t ->

                                    let info =
                                        let infoBound: TargetPageBox = info.Bound

                                        match infoBound.Value with 
                                        | None -> None
                                        | Some (_, infoBound) ->
                                            let b = 
                                                (match predicateEx with 
                                                    | Some predicateEx -> 
                                                        predicateEx info bound infoBound 
                                                        && predicate bound infoBound
                                                    | None -> predicate bound infoBound
                                                )

                                            match b with 
                                            | true -> Some (Choice1Of2 info) 
                                            | false -> Some (Choice2Of2 info) 


                                    match info with 
                                    | None -> loop2 currentInfos leftInfos t
                                    | Some (Choice1Of2 currentInfo) ->
                                        loop2 (currentInfo :: currentInfos) leftInfos t

                                    | Some (Choice2Of2 leftInfo) ->
                                        match leftInfo.Bound.Value with 
                                        | None -> loop2 (currentInfos) (leftInfo :: leftInfos) t
                                        | Some (_, leftInfoBound) ->
                                            match predicateCross bound leftInfoBound with 
                                            | true -> loop2 (leftInfo :: currentInfos) (leftInfo :: leftInfos) t
                                            | false -> loop2 (currentInfos) (leftInfo :: leftInfos) t

                            loop2 [] [] infos

                        let r = 
                            match currentInfos with 
                            | [] -> None
                            | _ -> 
                                //let currentInfos =
                                //    currentInfos
                                //    |> List.map(fun info ->
                                //        { UserState = info 
                                //          Rectangle = 
                                //            match info.Bound.Value with 
                                //            | Some rect -> rect 
                                //            | None -> Rectangle(0.f, 0.f)
                                //        }
                                //    )

                                //let currentInfos =
                                //    currentInfos
                                //    |> selectionSorter.SortToLists_WithUserState
                                //    |> List.concat
                                //    |> List.map(fun m -> m.UserState)


                                Some (bound, currentInfos |> List.map(fun m -> m.Info.Info))

                        loop ((r) :: accum) t leftInfos


                loop [] bounds infos

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
                            let infoBound: TargetPageBox = info.Bound
                            match infoBound.Value with 
                            | Some (_, infoBound) -> infoBound.Is_InsideOrCross_Of(bound.Bound)
                            | None -> false
                        )
                    match infos with 
                    | [] -> None
                    | _ ->
                        Some (bound.ApplyMargin -margin, infos |> List.map (fun m -> m.Info.Info))
                )
    
            | PageTilingRenewInfosSplitter.Groupby_CenterPointIsInside _  ->
                loop_groupBy 
                    (fun indexedBound infoBound ->
                        infoBound.IsCenterPointInsideOf (indexedBound.Bound)
                    )
                    (fun indexedBound infoBound ->
                        infoBound.IsCrossOf(indexedBound.Bound)
                    )
                    bounds
                    infos

     
            | PageTilingRenewInfosSplitter.Groupby_DenseBoundIsInside (_, margin) ->
                let bounds =
                    bounds
                    |> List.map (fun m -> m.ApplyMargin margin)
    
                loop_groupBy 
                    (fun (indexedBound: IndexedBound) infoBound ->
                        infoBound.IsInsideOf(indexedBound.Bound)
                    )
                    (fun indexedBound infoBound ->
                        infoBound.IsCrossOf(indexedBound.Bound)
                    )
                    bounds
                    infos
                |> List.map(Option.map(fun (m, v) ->
                    m.ApplyMargin(-margin), v
                ))


        member internal x.Infos__GroupOrFilter_Into(bounds: Rectangle list, infos: RenewableInfo list, fBound) =
            x.Infos__GroupOrFilter_IntoOp(bounds, infos, fBound)
            |> List.choose id

    [<RequireQualifiedAccess>]
    type PageTilingRenewOptions = 
        | UsingOriginPdfPage
        | VisibleInfosInActualBox of PageTilingRenewInfosSplitter
    with 
        static member DefaultValue = PageTilingRenewOptions.UsingOriginPdfPage
    
        member internal x.SplitTextToWords = 
            match x with 
            | UsingOriginPdfPage _ -> false
            | VisibleInfosInActualBox v -> v.SplitTextToWords

    type PageTilingResultCount = PageTilingResultCount of int
    with 
        member x.Value = 
            let (PageTilingResultCount count) = x
            count

    [<RequireQualifiedAccess>]
    type PageTilingResultValue =
        | UsingOriginPdfPage 
        | Renew_EmptyPage
        | Renew_Distincter of IComparable
        | Renew_TextPicker of IComparable
        | Renew_TextPicker_Distincter__Non

    [<RequireQualifiedAccess>]
    type PageTilingResultValue_TextPicker =
        | Renew_TextPicker of IComparable
        | Renew_TextPicker_Distincter__Non

    type PageTilingResult_TextPicker =
        { PageNumber: PageNumber 
          Bound: SerializableIndexedBound
          Value_TextPicker: PageTilingResultValue_TextPicker }


    type PageTilingResult =
        { PageNumber: PageNumber 
          Bound: SerializableIndexedBound
          Value: PageTilingResultValue }
    with 
        member internal x.ToTextPicker_OR_Fail() =
            { PageNumber = x.PageNumber 
              Bound = x.Bound
              Value_TextPicker =
                match x.Value with 
                | PageTilingResultValue.Renew_TextPicker v -> 
                    PageTilingResultValue_TextPicker.Renew_TextPicker v

                | PageTilingResultValue.Renew_TextPicker_Distincter__Non ->
                    PageTilingResultValue_TextPicker.Renew_TextPicker_Distincter__Non 
                | _ -> failwithf "Cannot convert %A to PageTilingResultValue_TextPicker" x.Value
              }

    [<RequireQualifiedAccess>]
    type PageTilingLayoutResults =
        | DistinctedOne of PageTilingLayoutResult
        | Many of PageTilingLayoutResult al1List
    with 
        member x.Head =
            match x with 
            | PageTilingLayoutResults.DistinctedOne v -> v
            | PageTilingLayoutResults.Many v -> v.Head

        static member Create(layouts: al1List<PageTilingLayoutResult>) =
            match List.distinct layouts.AsList with 
            | [layout] -> PageTilingLayoutResults.DistinctedOne layout
            | [] -> failwith "Invalid token"
            | layouts -> PageTilingLayoutResults.Many (AtLeastOneList.Create layouts)


        static member Concat(layoutLists: al1List<PageTilingLayoutResults>) =
            layoutLists.AsList
            |> List.collect(fun m ->
                match m with 
                | PageTilingLayoutResults.DistinctedOne v -> [v]
                | PageTilingLayoutResults.Many v -> v.AsList
            )
            |> AtLeastOneList.Create
            |> PageTilingLayoutResults.Create



    
    type SamplePdfFile =
        { PageSize: FsSize 
          PdfFile: PdfFile }

    type PageTilingResults_TextPicker = 
        { Value_TextPicker:  al1List<PageTilingResult_TextPicker list> 
          PageTilingResultCounts: PageTilingResultCount al1List
          Layouts: PageTilingLayoutResults
          SamplePdfFile: SamplePdfFile option }


    type TiledAndNUpResult =
        {
            PageTilingResults: PageTilingResults_TextPicker
            ImposingDocument: ImposingDocument
            TiledPdfFile: PdfFile
        }


    type PageTilingResults = 
        { Value:  al1List<PageTilingResult list> 
          PageTilingResultCounts: PageTilingResultCount al1List
          Layouts: PageTilingLayoutResults
          SamplePdfFile: SamplePdfFile option
          OriginBorderKeepingNumbers: int list }
    with 
        member x.ToTextPicker_OR_Fail() =
            { Value_TextPicker =
                x.Value
                |> AtLeastOneList.map(fun m ->
                    m
                    |> List.map(fun m -> m.ToTextPicker_OR_Fail())
                )

              PageTilingResultCounts = x.PageTilingResultCounts
              Layouts = x.Layouts
              SamplePdfFile = x.SamplePdfFile
            }

    [<RequireQualifiedAccess>]
    module private Array2D =

        let private toSeqs (input: 'a[,]) =
            let l1 = input.GetLowerBound(0)
            let u1 = input.GetUpperBound(0)
            seq {
                for i = l1 to u1 do
                    yield input.[i,*] :> seq<'a>
            }

        let toLists (input: 'a[,]) =
            let l1 = input.GetLowerBound(0)
            let u1 = input.GetUpperBound(0)
            [
                for i = l1 to u1 do
                    yield List.ofArray input.[i,*] 
            ]

        let transpose (input: 'a[,]) =
            let l1 = input.GetLowerBound(1)
            let u1 = input.GetUpperBound(1)
            seq {
                for i = l1 to u1 do
                    yield input.[*,i]
            }
            |> array2D
    
    //[<RequireQualifiedAccess>]
    //type private TileTableIndexerOrAreaTable =
    //    | TileTableIndexer of TileTableIndexer
    //    | AreaTables of AreaTable list
    //with 
    //    member x.GetIndexedAreas(pageNumber: PageNumber, actualBox: Rectangle, direction) =
    //        match x with 
    //        | TileTableIndexerOrAreaTable.AreaTables tables ->
    //            let rows = tables.[pageNumber.Value].Rows
    //            let areas = 
    //                rows
    //                |> List.map(fun (AreaRow row) ->
    //                    row
    //                )

    //            match direction with 
    //            | Direction.Horizontal -> 
    //                areas
    //                |> List.concat

    //            | Direction.Vertical ->
    //                areas
    //                |> array2D
    //                |> Array2D.transpose
    //                |> Array2D.toLists
    //                |> List.concat

    //        | TileTableIndexerOrAreaTable.TileTableIndexer tileTable ->
    //            let colNum = tileTable.ColNum
    //            let rowNum = tileTable.RowNum
    //            let cellsCount = colNum * rowNum
    //            let tileIndexeres =
    //                [ 1 .. cellsCount ]
    //                |> List.map(fun j ->
    //                    let index = (pageNumber.Value-1) * cellsCount + j
    //                    TileCellIndexer.Create((index-1), direction)
    //                )

    //            tileIndexeres
    //            |> List.map(fun tileIndexer ->
    //                Rectangle.getTile tileIndexer tileTable actualBox
    //                |> FsRectangle.OfRectangle
    //            )
    let internal logExtractingFile pageNumber (flowModel: FlowModel<_>) =
        match pageNumber with 
        | 1 -> 
            let logInfo (text) = 
                let configuration = flowModel.Configuration
                let logger: PageLogger =
                    { LoggerLevel = configuration.LoggerLevel 
                      LoggingPageCountInterval = loggingPageCountInterval.Value }

                logger.Log(text, alwaysPrintingConsole_If_Info = true)

            logInfo 
                (fun () -> sprintf "Extracting %s" (Path.GetFileNameWithoutExtension flowModel.File)) 1
        | _ -> ()

    type Flows with 
        static member TilePages (areaTables: AreaTable list, ?sorter: SelectionSorter, ?pageTilingRenewOptions: PageTilingRenewOptions) =
            let pageTilingRenewOptions = defaultArg pageTilingRenewOptions PageTilingRenewOptions.UsingOriginPdfPage
            let sorter = defaultArg sorter (SelectionSorter.DefaultValue)

            Reuse(fun flowModel splitDocument ->
                let reader = splitDocument.Reader
                let writer = splitDocument.Writer

                let totalNumberOfPages = reader.GetNumberOfPages()
                let borderKeepingPageNumbers = [1..totalNumberOfPages]
                let __ensureAreaTableLengthEqualToTotalPageCount =
                    match areaTables.Length = totalNumberOfPages with 
                    | true -> ()
                    | false -> failwithf "totalNumberOfPages %d <> areaTables's Length %d" totalNumberOfPages areaTables.Length



                for pageNumber = 1 to totalNumberOfPages do
                    let readerPage = reader.GetPage(pageNumber)
                    let areaTable = areaTables.[pageNumber-1]

                    let cells = 
                        areaTable.Cells
                        |> List.map(fun m -> m.AsRectangle)

                    match cells with 
                    | [] -> ()
                    | _ ->
                        let cells = 
                            cells
                            |> sorter.Sort

                        match pageTilingRenewOptions with 
                        | PageTilingRenewOptions.UsingOriginPdfPage ->


                            for cell in cells do
                                let tileBox = cell

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

                            logExtractingFile pageNumber flowModel

                            extractVisibleRenewableInfosToWriter 
                                flowModel.Configuration
                                args
                                selector
                                (fun infos ->
                                    let bounds = cells

                                    splitter.Infos__GroupOrFilter_IntoOp(
                                        bounds,
                                        infos
                                    )
                                    |> List.mapi(fun i targetPageInfoOp ->
                                        match targetPageInfoOp with 
                                        | Some (targetPageBox, infos) ->
                                            TargetRenewablePageInfo.NewPage(TargetPageBox.Create (Some targetPageBox.Bound), TargetRenewableNewPageInfoElement.Create (infos, borderKeepingPageNumbers))

                                        | None ->
                                            TargetRenewablePageInfo.EmptyPage(TargetPageBox.Create (Some bounds.[i]))
                                    )

                                )
                                false
                                ignore
                                borderKeepingPageNumbers
                                reader
                                writer
                            |> ignore
                            
                areaTables
                |> List.map(fun areaTable -> PageTilingResultCount areaTable.CellsCount)

            ) 
            |> Reuse.rename 
                "TilePages"
                ["tileTable" => areaTables.ToString()
                 "pageTilingRenewOptions" => pageTilingRenewOptions.ToString() ]
            |> Flow.Reuse
        
        static member TilePages (tileTable: TileTableIndexer, ?direction: Direction, ?pageTilingRenewOptions: PageTilingRenewOptions, ?pageBoxKind, ?margin: Margin) =
            let pageBoxKind = defaultArg pageBoxKind PageBoxKind.ActualBox
            let margin = defaultArg margin Margin.Zero
            let direction = defaultArg direction Direction.Horizontal
            
            let pageTilingRenewOptions = defaultArg pageTilingRenewOptions PageTilingRenewOptions.UsingOriginPdfPage
            let colNum = tileTable.ColNum
            let rowNum = tileTable.RowNum
            let cellsCount = colNum * rowNum

            Reuse(fun flowModel splitDocument ->
                let reader = splitDocument.Reader
                let writer = splitDocument.Writer

                let totalNumberOfPages = reader.GetNumberOfPages()
                let borderKeepingPageNumbers = [1..totalNumberOfPages]

                for pageNumber = 1 to totalNumberOfPages do
                    let readerPage = reader.GetPage(pageNumber)
                    let tilingPageBox = readerPage.GetPageBox(pageBoxKind) |> Rectangle.applyMargin margin

                    let tileIndexeres =
                        [ 1 .. cellsCount ]
                        |> List.map(fun i ->
                            let index = (pageNumber-1) * cellsCount + i
                            TileCellIndexer.Create((index-1), direction)
                        )

                    match pageTilingRenewOptions with 
                    | PageTilingRenewOptions.UsingOriginPdfPage ->
                        for tileIndexer in tileIndexeres do
                            let tileBox = Rectangle.getTile tileIndexer tileTable tilingPageBox

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

                        logExtractingFile pageNumber flowModel

                        extractVisibleRenewableInfosToWriter 
                            flowModel.Configuration
                            args
                            selector
                            (fun infos ->
                                let bounds = 
                                    tileIndexeres
                                    |> List.map(fun tileIndexer ->
                                        Rectangle.getTile tileIndexer tileTable tilingPageBox
                                    )

                                splitter.Infos__GroupOrFilter_IntoOp(
                                    bounds,
                                    infos
                                )
                                |> List.mapi(fun i targetPageInfoOp ->
                                    match targetPageInfoOp with 
                                    | Some (targetPageBox, infos) ->
                                        TargetRenewablePageInfo.NewPage(TargetPageBox.Create (Some targetPageBox.Bound), TargetRenewableNewPageInfoElement.Create (infos, borderKeepingPageNumbers))

                                    | None ->
                                        TargetRenewablePageInfo.EmptyPage(TargetPageBox.Create (Some bounds.[i]))
                                )

                            )
                            false
                            ignore
                            borderKeepingPageNumbers
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
                 "pageBoxKind" => pageBoxKind.ToString()
                 "margin" => margin.ToString()
                 "pageTilingRenewOptions" => pageTilingRenewOptions.ToString() ]
            |> Flow.Reuse

        static member TilePages (selector: Selector<'userState>, ?distincterOrTextPicker: PageTilingDistincterOrTextPicker, ?sorter: SelectionSorter, ?pageTilingRenewOptions: PageTilingRenewOptions, ?borderKeepingPageSelector, ?transform: IndexedBound -> Rectangle, ?samplePageExtractingOptions) =
            let sorter = defaultArg sorter (SelectionSorter.DefaultValue)
            let borderKeepingPageSelector = defaultArg borderKeepingPageSelector (NullablePageSelector.All)

            let samplePageExtractingOptions = defaultArg samplePageExtractingOptions SamplePageExtractingOptions.Non

            let distincterOrTextPicker =
                match distincterOrTextPicker with 
                | None -> PageTilingDistincterOrTextPicker.Non
                | Some v -> v 

            let distincter = 
                match distincterOrTextPicker with 
                | PageTilingDistincterOrTextPicker.Distincter (v) -> Some v
                | _ -> None

            let pageTilingRenewOptions = defaultArg pageTilingRenewOptions PageTilingRenewOptions.UsingOriginPdfPage

            fun (flowModel: FlowModel<'userState>) (splitDocument: SplitDocument) ->

                let reader = splitDocument.Reader

                let originBorderKeepingPageNumbers = reader.GetPageNumbers(borderKeepingPageSelector)

                let borderKeepingPageNumbers =
                    match samplePageExtractingOptions with 
                    | SamplePageExtractingOptions.FirstPageMultipleSelectors _ 
                    | SamplePageExtractingOptions.FirstPageFirstSelector _ ->
                        if List.contains 1 originBorderKeepingPageNumbers 
                        then originBorderKeepingPageNumbers
                        else 1 :: originBorderKeepingPageNumbers

                    | SamplePageExtractingOptions.Non -> originBorderKeepingPageNumbers 


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

                            let boundLists = 
                                let rects =
                                    NonInitialClippingPathPdfDocumentContentParser.parse i selector parser
                                    |> List.ofSeq
                                    |> List.map (fun info -> 
                                        IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth info
                                    )
                                    |> Rectangle.removeInboxes

                                sorter.SortToLists (rects)

                            let bounds = 
                                List.concat boundLists
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
                                    | PageTilingDistincter.TextCase (distincter, _, _) ->
                                        let infos = 
                                            let selector = 
                                                Selector.Text(fun _ _ -> true)
                                                |> Selector.toRenderInfoSelector args

                                            NonInitialClippingPathPdfDocumentContentParser.parse i selector parser
                                            |> List.ofSeq

                                        let infos = 
                                            let infos = 
                                                infos
                                                |> List.choose IIntegratedRenderInfo.asITextRenderInfo
                                            
                                            match pageTilingRenewOptions.SplitTextToWords with 
                                            | false -> infos
                                            | true -> infos |> List.collect(fun m -> m.SplitToWords())

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
                                 
                            yield
                                { Layout = 
                                        { ColNums =
                                            boundLists
                                            |> List.map(fun m -> m.Length)
                                        
                                          RowNum = boundLists.Length
                                        }

                                  Value = boundGroups }

                    ]
            
                match boundGroups with 
                | [] -> failwithf "Cannot tiling to any results by selector %A" selector
                | _ -> ()


                let layouts =
                    boundGroups
                    |> List.map(fun m ->
                        m.Layout
                    )


                let boundGroups = 
                    boundGroups
                    |> List.collect(fun m -> m.Value)

                match boundGroups with 
                | [] -> failwithf "Cannot tiling to any results by selector %A" selector
                | _ -> ()

                let boundGroups =
                    match distincter with 
                    | None -> boundGroups
                    | Some v ->
                        match v with 
                        | PageTilingDistincter.TextCase (_, _, filter) ->
                            boundGroups
                            |> List.distinctBy_explictly<_, IComparable>(fun m -> m.Distincter)
                            |> filter

                
                
                let r = 
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

                            boundGroups
                            |> List.map(fun boundGroup ->
                                { PageNumber = PageNumber pdfPageNumber 
                                  Bound = boundGroup.IndexedBound.Serializable 
                                  Value = PageTilingResultValue.UsingOriginPdfPage }
                            )


                        | PageTilingRenewOptions.VisibleInfosInActualBox splitter ->
                            match boundGroups with 
                            | [] -> []
                            | _ ->
                                let args: PageModifingArguments<_> =
                                    boundGroups.[0].Args

                                let selector =
                                    Selector.All(InfoIM.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
                            
                                let infos =
                                    logExtractingFile pdfPageNumber flowModel

                                    extractVisibleRenewableInfosToWriter 
                                        flowModel.Configuration
                                        args
                                        selector
                                        (fun infos ->
                                            let bounds = 
                                                boundGroups
                                                |> List.map(fun m -> m.Bound)

                                            splitter.Infos__GroupOrFilter_IntoOp(
                                                bounds,
                                                infos
                                            )
                                            |> List.mapi(fun i targetPageInfoOp ->
                                                match targetPageInfoOp with 
                                                | Some (targetPageBox, infos) ->
                                                    TargetRenewablePageInfo.NewPage(
                                                        TargetPageBox.Create (Some targetPageBox.Bound),
                                                        TargetRenewableNewPageInfoElement.Create(
                                                            infos,
                                                            borderKeepingPageNumbers,
                                                            ?tagColor = distincterOrTextPicker.TagColor,
                                                            boundPredicate = boundGroups.[0].BoundPredicate,
                                                            ?rectangleTransform = 
                                                                match transform with 
                                                                | None -> None
                                                                | Some transform ->
                                                                    { 
                                                                        OldRect = targetPageBox.Bound
                                                                        NewRect = transform targetPageBox
                                                                    }
                                                                    |> Some

                                                        )
                                                    )

                                                | None ->
                                                    TargetRenewablePageInfo.EmptyPage(TargetPageBox.Create (Some bounds.[i]))
                                            )
                                        )
                                        false
                                        ignore
                                        borderKeepingPageNumbers
                                        reader
                                        splitDocument.Writer

                                let infos =
                                    infos
                                    |> List.mapi(fun i info ->
                                        let createResult (value) =
                                            { PageNumber = PageNumber args.PageNum
                                              Bound = boundGroups.[i].IndexedBound.Serializable
                                              Value = value }
                                        match info with 
                                        | TargetRenewablePageInfo.NewPageCase (bound, element, _, _) ->
                                            match distincterOrTextPicker with 
                                            | PageTilingDistincterOrTextPicker.Distincter _ ->
                                                PageTilingResultValue.Renew_Distincter boundGroups.[i].Distincter
                                            
                                            | PageTilingDistincterOrTextPicker.Non _ ->
                                                PageTilingResultValue.Renew_TextPicker_Distincter__Non

                                            | PageTilingDistincterOrTextPicker.TextPicker textPicker ->
                                                let textPicker = textPicker.TransformTextPickers

                                                let textInfos = 
                                                    element.RenewableInfos
                                                    |> List.choose(fun m -> IIntegratedRenderInfoIM.asVector m.OriginInfo)

                                                textPicker args boundGroups.[i].IndexedBound textInfos
                                                |> PageTilingResultValue.Renew_TextPicker

                                        | TargetRenewablePageInfo.EmptyPage _ -> 
                                            PageTilingResultValue.Renew_EmptyPage
                                        | _ -> failwith "Invalid token"
                                        |> createResult
                                    )

                                infos

                    )
                    |> AtLeastOneList.Create

                { Value = r  

                  PageTilingResultCounts =
                    r
                    |> AtLeastOneList.map(fun m -> PageTilingResultCount m.Length)

                  Layouts =
                    
                    match AtLeastOneList.TryCreate layouts with 
                    | Some layouts -> PageTilingLayoutResults.Create layouts
                    | None -> failwith "Invalid token, PageTilingLayoutResult list cannot be empty here"

                  SamplePdfFile = None
                  OriginBorderKeepingNumbers = originBorderKeepingPageNumbers
                }


            |> reuse 
                "TilePages"
                [ "selector" => selector.ToString()
                  "distincterOrTextPicker" => sprintf "%A" distincterOrTextPicker
                  "pageTilingRenewOptions" => pageTilingRenewOptions.ToString()
                  "samplePageExtractingOptions" => samplePageExtractingOptions.ToString() ]
            |> fun flow ->
                match samplePageExtractingOptions with 
                | SamplePageExtractingOptions.Non -> Flow.Reuse flow
                | SamplePageExtractingOptions.FirstPageFirstSelector targetPdfPath 
                | SamplePageExtractingOptions.FirstPageMultipleSelectors (_, targetPdfPath) ->
                    let pageNumbers = 
                        match samplePageExtractingOptions with 
                        | SamplePageExtractingOptions.FirstPageMultipleSelectors (pageNumbers, targetPdfPath) ->
                            pageNumbers.AsList

                        | SamplePageExtractingOptions.FirstPageFirstSelector (_) -> [PageNumber 1]
                        | _ -> failwith "Invaldi token"


                    Flow.Func(fun userState ->
                        Flow.Reuse(
                            flow
                            <+>
                            Reuse.Factory(fun flowModel splitDocument ->
                                let sampleFile = 
                                    let r = flowModel.UserState.Value
                                    match r.Head with 
                                    | [] -> failwith "Invalid token, tiling should be found in first page when trying extracting samplePdfFile"
                                    | r :: _ ->
                                        let sampleDocument = new PdfDocument(new PdfWriter(targetPdfPath.Path))
                                        for pageNumber in pageNumbers do 
                                            let page = splitDocument.Reader.GetPage(pageNumber.Value)
                                            let samplePage = page.CopyTo(sampleDocument)
                                            let newPage = sampleDocument.AddPage(samplePage) 
                                            newPage.ClippingContentsToPageBox(PageBoxKind.ActualBox, Margin.Create(mm 0.3))

                                        sampleDocument.Close()
                                        { PdfFile = PdfFile targetPdfPath
                                          PageSize = FsSize.ofFsRectangle r.Bound.Bound }

                                Reuse.dummy()
                                ||>> (fun _ ->
                                    { flowModel.UserState with 
                                        SamplePdfFile = Some sampleFile
                                    }
                                )
                            )
                        )
                        <.+>
                        Flow.Func(fun (pageTilingResults: PageTilingResults) ->
                            ((fun _ -> userState) <<||
                                if List.contains 1 pageTilingResults.OriginBorderKeepingNumbers
                                then 
                                    Flow.dummy() ||>> ignore
                                else 
                                    Flow.Manipulate(
                                        Modify.CancelFillAndStroke(
                                            selector,
                                            PageSelector.Expr(
                                                PageSelectorExpr.Between(
                                                    SinglePageSelectorExpr.Begin 1,
                                                    SinglePageSelectorExpr.Begin pageTilingResults.Layouts.Head.CellsCount
                                                ))
                                        )
                                    )
                            )
                        )


                    )

                    
                    



        static member TilePagesAndNUp(selector, textPicker: PageTilingTextPickers, ?transform, ?borderKeepingPageSelector, ?pageTilingRenewInfosSplitter, ?fArgs, ?samplePageExtractingOptions) =
            (
                Flows.TilePages
                    (selector,
                    distincterOrTextPicker = PageTilingDistincterOrTextPicker.TextPicker(textPicker),
                    ?pageTilingRenewOptions = (pageTilingRenewInfosSplitter |> Option.map PageTilingRenewOptions.VisibleInfosInActualBox),
                    borderKeepingPageSelector = defaultArg borderKeepingPageSelector NullablePageSelector.First,
                    ?transform = transform,
                    ?samplePageExtractingOptions = samplePageExtractingOptions
                )
                ||>> (fun m -> m.ToTextPicker_OR_Fail())
            )
            <++>
            Flow.Factory(fun flowModel ->
                Flow.Reuse(
                    Reuse.Func(fun (r: PageTilingResults_TextPicker) ->
                        match r.Layouts with 
                        | PageTilingLayoutResults.DistinctedOne r -> 
                            Reuses.Impose(fun args ->
                                { args with 
                                    ColNums = r.ColNums
                                    RowNum = r.RowNum
                                    Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (Margin.MM6)
                                    Cropmark = Some Cropmark.defaultValue
                                    Background = Background.Size FsSize.MAXIMUN
                                }
                                |> (defaultArg fArgs id)
                            )

                        | _ -> failwith "Not implemented"
                    )
                )
                ||>> (fun imposingDocument -> imposingDocument, flowModel.PdfFile
                )
            )
            ||>> (fun (pageTilingResults, (imposingDocument, tiledPdfFile)) ->
                {
                    PageTilingResults = pageTilingResults
                    ImposingDocument = imposingDocument
                    TiledPdfFile = tiledPdfFile
                }
            )

        /// pick subPage for each page
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
                |> PageNumSequence.Create

            Reuses.SequencePages(pageNumSequence)
            |> Flow.Reuse