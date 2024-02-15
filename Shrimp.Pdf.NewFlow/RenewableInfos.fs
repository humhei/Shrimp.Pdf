﻿namespace Shrimp.Pdf.SlimFlow

#nowarn "0104"
open Shrimp.Pdf
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Colors
open Shrimp.FSharp.Plus
open Shrimp.Pdf.Extensions
open iText.Kernel.Geom



type RenewableInfos = 
    { Infos: RenewableInfo list 
      IsCuttingDieSetted: bool
      Background: SlimBackground option }
with 
    static member Create(infos) =
        { Infos = infos 
          IsCuttingDieSetted = false
          Background = None }

    member x.AsList = x.Infos

    member x.MapInfos(name, f) =
        match x.Background with 
        | None -> ()
        | Some bk -> 
            bk.ModifyInfos(name, f)

        {x with 
            Infos = f x.Infos
        }

    member x.FilterInfos(name, f) =
        let bkInfos = 
            match x.Background with 
            | None -> []
            | Some bk -> 
                bk.FilterInfos(name, f)

        let infos = List.filter f x.Infos
        infos @ bkInfos

    member x.ChooseInfos(name, f) =
        let bkInfos = 
            match x.Background with 
            | None -> []
            | Some bk -> 
                bk.ChooseInfos(name, f)

        let infos = List.choose f x.Infos
        infos @ bkInfos

    member x.CollectInfos(name, f) =
        let bkInfos = 
            match x.Background with 
            | None -> []
            | Some bk -> 
                bk.CollectInfos(name, f)

        let infos = f x.Infos
        infos @ bkInfos

    member x.AllPaths() =
        x.FilterInfos("AllPaths", fun m -> 
            match m with 
            | RenewableInfo.Image _ -> false
            | RenewableInfo.Path info -> true
            | RenewableInfo.Text info -> false
        )


    member x.SetCuttingDie(cuttingDieColors: FsColor list) =
        match x.IsCuttingDieSetted with 
        | true -> x
        | false ->
            let r = 
                x.MapInfos("SetCuttingDie", List.map(fun m ->
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

            { r with IsCuttingDieSetted = true }


    member x.CuttingDieInfos() =
        x.FilterInfos("CuttingDieInfos", fun m ->
            match m with 
            | RenewableInfo.Path info -> info.Tag = RenewablePathInfoTag.CuttingDie
            | RenewableInfo.Image _
            | RenewableInfo.Text _ -> false
        )

    member x.CuttingDieInfosBound() =
        x.CuttingDieInfos()
        |> List.map(fun m -> m.VisibleBound)
        |> AtLeastOneList.TryCreate
        |> Option.map Rectangle.ofRectangles
        

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
        x.MapInfos("SetColor", List.map(fun m -> m.SetColor()))


    member x.VisibleBound() =
        x.ChooseInfos("GetVisibleBound", fun info ->
            Some info.VisibleBound
        )
        |> AtLeastOneList.TryCreate
        |> Option.map Rectangle.ofRectangles
      

    member x.ReplaceColors(colorMappings: ColorMappings) =
        let text = colorMappings.LoggingText
        let picker = colorMappings.AsPicker()
        x.MapInfos("ReplaceColors_ColorMapping:" + text, List.map(fun info ->
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

        let name = sprintf "ReplaceColors_tuple: %A" (originColorText, newColor.LoggingText_Raw)
        x.MapInfos(name, List.map(fun info ->
            info.MapColor(fun color ->  
                match FsColors.contains color originColors with 
                | true -> Some newColor
                | false -> None
            )
        ))

[<RequireQualifiedAccess>]
type PageBoxOrigin =
    | LeftBottom 

type SlimWriterPageBoxSetter_SuffixActions =
    { SetTrimBoxByStrokeColors: Rectangle option }

