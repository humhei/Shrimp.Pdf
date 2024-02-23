namespace Shrimp.Pdf.SlimFlow

open Shrimp.Pdf.icms2.client

#nowarn "0104"
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open Shrimp.FSharp.Plus
open Shrimp.Pdf.Extensions
open iText.Kernel.Geom
open iText.Kernel.Pdf
open Shrimp.Pdf.DSL




type RenewableInfos = 
    { Infos: RenewableInfo list 
      LazyCuttingDieBound0: Rectangle option
      Background: SolidableSlimBackground list
      InternalFlowModel: option<InternalFlowModel<int>>
      PageNumber: PageNumber
      TotalNumberOfPages: int }
with 
    member x.CuttingDieBound0 =
        match x.LazyCuttingDieBound0 with 
        | None -> failwithf "CuttingDie bound is None, please set cuttingDie bound first"
        | Some bound -> bound

    member x.IsCuttingDieSetted = x.LazyCuttingDieBound0.IsSome

    static member Create(infos, pageNumber, totalNumberOfPages) =
        { Infos = infos 
          Background = []
          InternalFlowModel = None
          LazyCuttingDieBound0 = None
          PageNumber = pageNumber
          TotalNumberOfPages = totalNumberOfPages }

    member x.AsList = x.Infos

    member x.MapInfos(name, parameters, f) =
        let mapInfos() = 
            x.Background
            |> List.iter(fun bk -> bk.ModifyInfos(name, parameters, f))

            {x with 
                Infos = f x.Infos
            }

        match x.InternalFlowModel with 
        | None -> mapInfos()
        | Some flowModel1 ->
            let flowModel1 =
                { flowModel1 with 
                    FlowName =
                        match flowModel1.FlowName with 
                        | None -> FlowName.New(name)
                        | Some flowName ->
                            FlowName.Override(name).SetParentFlowName(flowName)
                        |> Some   
                }

            PdfLogger.TryInfoWithFlowModel(flowModel1.UserState, flowModel1, fun () -> mapInfos())

    member x.MapInfo(name, parameters, f) =
        x.MapInfos(name, parameters, fun infos ->
            infos
            |> List.map(fun m ->
                f m
            )
        )

    member x.MapVector(name, parameters, f) =
        x.MapInfos(name, parameters, fun infos ->
            infos
            |> List.map(fun m ->
                match m with 
                | RenewableInfo.Vector info -> RenewableInfo.ofVector(f info)
                | RenewableInfo.Pixel _ -> m
            )
        )



    member x.MapPath(name, parameters, f) =
        x.MapInfos(name, parameters, fun infos ->
            infos
            |> List.map(fun m ->
                m.MapPath f
            )
        )

    member x.MapText(name, parameters, f) =
        x.MapInfos(name, parameters, fun infos ->
            infos
            |> List.map(fun m ->
                m.MapText f
            )
        )

    member x.MapWord(name, parameters, f) =
        x.MapInfos(name, parameters, fun infos ->
            infos
            |> List.collect(fun m ->
                m.MapWord f
            )
        )


    member x.MapImage(name, parameters, f) =
        x.MapInfos(name, parameters, fun infos ->
            infos
            |> List.map(fun m ->
                m.MapImage f
            )
        )

    member x.FilterInfos(name, f) =
        let bkInfos = 
            x.Background
            |> List.collect(fun bk -> bk.FilterInfos(name, f))

        let infos = List.filter f x.Infos
        infos @ bkInfos

    member x.ChooseInfos(name, f) =
        let bkInfos = 
            x.Background
            |> List.collect(fun bk -> bk.ChooseInfos(name, f))
          

        let infos = List.choose f x.Infos
        infos @ bkInfos

    member x.CollectInfos(name, f) =
        let bkInfos = 
            x.Background
            |> List.collect(fun bk -> bk.CollectInfos(name, f))

        let infos = f x.Infos
        infos @ bkInfos

    member x.AllPaths() =
        x.FilterInfos("AllPaths", fun m -> 
            match m with 
            | RenewableInfo.Image _ -> false
            | RenewableInfo.Path info -> true
            | RenewableInfo.Text info -> false
        )





    member x.CuttingDieInfos() =
        x.FilterInfos("CuttingDieInfos", fun m ->
            match m with 
            | RenewableInfo.Path info -> info.Tag = RenewableInfoTag.CuttingDie
            | RenewableInfo.Image _
            | RenewableInfo.Text _ -> false
        )

    member x.RefreshCuttingDieInfosBound() =
        let bound = 
            x.CuttingDieInfos()
            |> List.map(fun m -> m.VisibleBound0)
            |> AtLeastOneList.TryCreate
            |> Option.map Rectangle.ofRectangles
        
        { x with LazyCuttingDieBound0 = bound }

    member x.SetCuttingDie(cuttingDieColors: FsColor list) =
        match x.IsCuttingDieSetted with 
        | true -> x
        | false ->
            let r = 
                x.MapInfos("SetCuttingDie", [], List.map(fun m ->
                    match m with 
                    | RenewableInfo.Path info ->
                        match info.LazyStrokeColor with 
                        | None -> m
                        | Some strokeColor ->

                            match FsColors.contains strokeColor cuttingDieColors with 
                            | true -> 
                                { info with Tag = RenewableInfoTag.CuttingDie }
                                |> RenewableInfo.Path
                            | false -> m
                    | RenewableInfo.Image _
                    | RenewableInfo.Text _ -> m
                ))

            r.RefreshCuttingDieInfosBound()


    member x.AllColors() =
        x.CollectInfos("AllColors", fun infos ->
            infos
            |> List.collect(fun info -> 
                match info with 
                | RenewableInfo.Image _ -> []
                | RenewableInfo.Path info -> [info.LazyFillColor; info.LazyStrokeColor]
                | RenewableInfo.Text info -> [info.LazyFillColor; info.LazyStrokeColor]
            )
            |> List.choose id
            |> FsColors.distinct
            |> List.ofSeq
        )
        |> FsColors.distinct
        |> List.ofSeq

    member x.SetColor() =
        x.MapInfos("SetColor", [], List.map(fun m -> m.SetColor()))


    member x.VisibleBound1() =
        x.ChooseInfos("GetVisibleBound", fun info ->
            Some info.VisibleBound1
        )
        |> AtLeastOneList.TryCreate
        |> Option.map Rectangle.ofRectangles
      
    member x.ReplaceColors(name, parameters, picker: FsColor -> NullableFsColor option, ?ops: Modify_ReplaceColors_Options) =
        let ops = defaultArg ops Modify_ReplaceColors_Options.DefaultValue
        let info_boundis_args = ops.Info_BoundIs_Args |> Option.map SABInfo_BoundIs_Args.Create
        let modifiedInfos = ResizeArray()
        let fillOrStrokeModifingOptions = ops.FillOrStrokeOptions.ToModifingOptions()

        let replaceColors() = 
            let mutable existsCancel = false
            let r = 
                x.MapInfos(name, parameters, List.map(fun info ->
                    match ops.RenewableInfoTagPredicate info.RenewableInfoTag with 
                    | false -> info
                    | true ->
                        let picker color =
                            match picker color with 
                            | Some color ->  
                                match color with 
                                | NullableFsColor.N -> existsCancel <- true
                                | _ -> ()

                                modifiedInfos.Add info
                                color
                                |> Some

                            | None -> None

                        let b = 
                            match info_boundis_args with 
                            | Some info_boundis_args ->
                                info_boundis_args.PredicateBound(info.GetDenseVisibleBound(info_boundis_args.BoundGettingStrokeOptions))
                            | None -> true
                    
                            &&
                            match ops.SelectorTag, info with 
                            | SelectorTag.Path, RenewableInfo.Path _ 
                            | SelectorTag.Text, RenewableInfo.Text _ -> true
                            | SelectorTag.PathOrText, RenewableInfo.Path _ 
                            | SelectorTag.PathOrText, RenewableInfo.Text _ -> true
                            | _ -> false

                        match b with 
                        | true -> info.MapColor(picker, fillOrStrokeOptions = fillOrStrokeModifingOptions)
                        | false -> info
            ))

            match ops.PageInfosValidation with 
            | PageInfosValidation.Ignore -> ()
            | PageInfosValidation.ValidateLength validate -> 
                validate x.PageNumber modifiedInfos.Count

            match existsCancel with 
            | false ->  r
            | true ->
                r.MapInfos(
                    "Remove CanceledInfos",
                    [yield "name" => name
                     yield! parameters],
                     fun infos ->
                        infos
                        |> List.filter(fun info -> not info.IsClosed)
                )

        match ops.PageSelector with 
        | PageSelector.All -> replaceColors()
        | _ ->
            let pageNumbers = PdfDocument.GetPageNumbers_Static ops.PageSelector x.TotalNumberOfPages
            match List.contains x.PageNumber.Value pageNumbers with 
            | true -> replaceColors()
            | false -> x


    member x.ReplaceColors(name, parameters, picker: FsColor -> FsColor option, ?ops: Modify_ReplaceColors_Options) =
        let picker color =
            match picker color with 
            | Some color -> Some (NullableFsColor.FsColor color)
            | None -> None

        x.ReplaceColors(name, parameters, picker = picker, ?ops = ops)

    member x.ReplaceColors(colorMappings: ColorMappings, ?ops) =
        let text = colorMappings.LoggingText
        let picker = 
            colorMappings.AsPicker()
            >> Option.map (fun m -> m.ToNullableFsColor())

        x.ReplaceColors(
            "ReplaceColors_ColorMapping",
            ["ColorMapping" => text],
            picker = picker,
            ?ops = ops
        )

    member x.ReplaceColors(originColors: FsColor list, newColor: FsColor, ?ops) =
        let originColorText = 
            originColors
            |> List.map(fun m -> m.LoggingText_Raw)
            |> String.concat "\n"

        let name = "ReplaceColors_tuple" 
        let parameters =
            [
                "OriginColor" => originColorText
                "NewColor" => newColor.LoggingText_Raw
            ] 

        let picker color =
            match FsColors.contains color originColors with 
            | true -> Some newColor
            | false -> None

        x.ReplaceColors(name, parameters, picker = picker, ?ops = ops)

    member x.ReplaceAlternativeColors(name, parameters, picker: AlternativeFsColor -> FsColor option, ?ops) =
        let picker (fsColor: FsColor) =
            match fsColor.AsAlternativeFsColor with 
            | Some color -> picker color
            | None -> None

        x.ReplaceColors(
            name, 
            parameters,
            picker = picker,
            ?ops = ops
        )



 

[<RequireQualifiedAccess>]
type PageBoxOrigin =
    | LeftBottom 

type SlimWriterPageBoxSetter_SuffixActions =
    { SetTrimBoxByStrokeColors: Rectangle option }

