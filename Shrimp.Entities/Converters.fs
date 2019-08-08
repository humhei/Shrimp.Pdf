namespace Atrous.Entities
module Converters =
    open System.Collections.Generic
    open System
    open Atrous.Pdf.ColorBook
    open System.Collections.Immutable
    open Atrous.Pdf.Colors
    open Atrous.Pdf
    open Atrous.Pdf.Colors.ColorConverter
    
    let pantoneConverter enum = 
        let hex = unbox enum
        let color = LabColor.fromHex hex |> DeviceRgb.fromLab
        let values = color.GetColorValue() |> Array.map (fun value ->
            value * 255.f |> int
        )
        sprintf "BtColor.FromRgb(%d,%d,%d)" values.[0] values.[1] values.[2] |> box

    let UciDetailConverters = ImmutableHashSet.CreateRange [typeof<ColorCard>]
    let customValueConverters : IDictionary<Type,Type * (obj -> obj)> = 
        dict 
            [
                typeof<PantoneColorEnum>,(typeof<string>,pantoneConverter)
            ]
