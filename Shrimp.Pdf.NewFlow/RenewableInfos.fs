namespace Shrimp.Pdf.SlimFlow

#nowarn "0104"
open Shrimp.Pdf
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Colors
open Shrimp.FSharp.Plus
open Shrimp.Pdf.Extensions
open iText.Kernel.Geom



type RenewableInfos = 
    { Infos: RenewableInfo list 
      LazyCuttingDieBound0: Rectangle option
      Background: SolidableSlimBackground list
      InternalFlowModel: option<InternalFlowModel<int>> }
with 
    member x.CuttingDieBound0 =
        match x.LazyCuttingDieBound0 with 
        | None -> failwithf "CuttingDie bound is None, please set cuttingDie bound first"
        | Some bound -> bound

    member x.IsCuttingDieSetted = x.LazyCuttingDieBound0.IsSome

    static member Create(infos) =
        { Infos = infos 
          Background = []
          InternalFlowModel = None
          LazyCuttingDieBound0 = None }

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
            | RenewableInfo.Path info -> info.Tag = RenewablePathInfoTag.CuttingDie
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
                                { info with Tag = RenewablePathInfoTag.CuttingDie }
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
      

    member x.ReplaceColors(colorMappings: ColorMappings) =
        let text = colorMappings.LoggingText
        let picker = colorMappings.AsPicker()
        x.MapInfos("ReplaceColors_ColorMapping", ["ColorMapping" => text], List.map(fun info ->
            info.MapColor(fun color ->
                match picker color with 
                | Some newColor -> (newColor.ToFsColor())
                | None -> None
            )
        ))

    member x.ReplaceColors(originColors: FsColor list, newColor: FsColor) =
        let originColorText = 
            originColors
            |> List.map(fun m -> m.LoggingText_Raw)
            |> String.concat "\n"

        let name = "ReplaceColors_tuple" 
        let paramters =
            [
                "OriginColor" => originColorText
                "NewColor" => newColor.LoggingText_Raw
            ] 
        x.MapInfos(name, paramters, List.map(fun info ->
            info.MapColor(fun color ->  
                match FsColors.contains color originColors with 
                | true -> Some newColor
                | false -> None
            )
        ))

    member x.ReplaceColors(name, paramters, picker: FsColor -> FsColor option) =
        x.MapInfos(name, paramters, List.map(fun info ->
            info.MapColor(picker)
        ))
 

[<RequireQualifiedAccess>]
type PageBoxOrigin =
    | LeftBottom 

type SlimWriterPageBoxSetter_SuffixActions =
    { SetTrimBoxByStrokeColors: Rectangle option }

