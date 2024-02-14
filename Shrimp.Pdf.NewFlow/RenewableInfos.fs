namespace Shrimp.Pdf.SlimFlow
#nowarn "0104"
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Xobject
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open System
open Shrimp.FSharp.Plus

type RenewableInfos = 
    { Infos: RenewableInfo list 
      IsCuttingDieSetted: bool }
with 
    static member Create(infos) =
        { Infos = infos 
          IsCuttingDieSetted = false }

    member x.AsList = x.Infos

    member x.MapInfos(f) =
        {x with Infos = f x.Infos }

    member x.AllPaths() =
        x.MapInfos(fun infos ->
            infos
            |> List.filter(fun m -> 
                match m with 
                | RenewableInfo.Image _ -> false
                | RenewableInfo.Path info -> true
                | RenewableInfo.Text info -> false
            )
        )


    member x.SetCuttingDie(cuttingDieColors: FsColor list) =
        match x.IsCuttingDieSetted with 
        | true -> x
        | false ->
            let r = 
                x.MapInfos(List.map(fun m ->
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
        x.MapInfos(List.filter(fun m ->
            match m with 
            | RenewableInfo.Path info -> info.Tag = RenewablePathInfoTag.CuttingDie
            | RenewableInfo.Image _
            | RenewableInfo.Text _ -> false
        ))
        

    member x.AllColors() =
        x.AsList
        |> List.collect(fun m -> 
            match m with 
            | RenewableInfo.Image _ -> []
            | RenewableInfo.Path info -> [info.LazyFillColor; info.LazyStrokeColor]
            | RenewableInfo.Text info -> [info.LazyFillColor; info.LazyStrokeColor]
        )
        |> List.choose id
        |> FsColors.distinct
        |> List.ofSeq

    member x.SetColor() =
        x.MapInfos(List.map(fun m -> m.SetColor()))

    member x.Select(f) =
        x.MapInfos(List.filter(fun m -> f m))

    member x.SelectPath(f) =
        x.MapInfos(List.filter(fun m -> 
            match m with 
            | RenewableInfo.Path info -> f info
            | RenewableInfo.Text _ -> false
            | RenewableInfo.Image _ -> false

        ))

    member x.SelectText(f) =
        x.MapInfos(List.filter(fun m -> 
            match m with 
            | RenewableInfo.Text info -> f info
            | RenewableInfo.Path _ -> false
            | RenewableInfo.Image _ -> false
        ))



    member x.VisibleBound() =
        match AtLeastOneList.TryCreate x.AsList with 
        | None -> None
        | Some infos ->
            infos
            |> AtLeastOneList.map(fun m -> m.VisibleBound)
            |> Rectangle.ofRectangles
            |> Some

[<RequireQualifiedAccess>]
type PageBoxOrigin =
    | LeftBottom 

type SlimWriterPageBoxSetter_SuffixActions =
    { SetTrimBoxByStrokeColors: Rectangle option }

