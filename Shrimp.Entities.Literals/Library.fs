namespace Shrimp.Entities.Literal
open Deedle
open Shrimp.Extensions
[<RequireQualifiedAccess>]
module ClearLiteral =
    let [<Literal>] art = "ART"
    let [<Literal>] order = "ORDER"
    let [<Literal>] size = "SIZE"


    let [<Literal>] cm = "CM"
    let [<Literal>] range = "Range"
    let [<Literal>] index = "Index"
    let [<Literal>] cartonKey = "CartonKey"
    let [<Literal>] shoeKey = "ShoeKey"
    let [<Literal>] mixedNumber = "MixedNumber"
    let [<Literal>] cartonNumber = "CartonNumber"
    let [<Literal>] number = "Number"
    let [<Literal>] cartonTotalNumber = "CartonTotalNumber"
    let [<Literal>] orderCartonTotalNumber = "orderCartonTotalNumber"
    let [<Literal>] sizeRange = "SizeRange"
    let [<Literal>] fraction = "Fraction"
    let [<Literal>] fractionRange = "FractionRange"
    let [<Literal>] ``单码装`` = "单码装"
    let [<Literal>] ``混码装`` = "混码装"
    let [<Literal>] ``装法`` = "装法"
    let [<Literal>] ``双/件`` = "双/件"
    let [<Literal>] ``件数`` = "件数"

    let wrapRange s = s + "Range"

    let sizes  = ["SIZE";"EUR";"US";"UK";]
    let shoeLinears = sizes @ [cm;fraction]

    //    

    let private pair key literals = if List.contains key literals then Some key else None
    let (|Sizes|_|) key = pair key sizes

    let (|ShoeLinerars|_|) key = pair key shoeLinears

[<RequireQualifiedAccess>]
module UnsureLiteral =
    let ``件数`` = ["件数";"总件数"]
    let textGroups = [``件数``]

[<RequireQualifiedAccess>]
module Frame =

    let findSizes (frame: Frame<_,string>) =
        frame.ColumnKeys
        |> Seq.filter (fun key -> Seq.contains key ClearLiteral.sizes)

    let findSize (frame: Frame<_,string>) =
        findSizes frame |> Seq.head

    let private pair literals (frame: Frame<_,string>) =
        frame.ColumnKeys 
        |> Seq.filter(fun key -> List.contains key literals)
        |> Option.ofSeq
        |> Option.map (fun keys ->
            keys |> Seq.map (fun key ->
                key,frame.GetColumn(key)
            )
        )

    let (|ShoeLinears|_|) frame =
        pair ClearLiteral.shoeLinears frame

[<RequireQualifiedAccess>]
module Series =

    let inline private pair (literals: string list) (key,frame: Frame<_,string>) =
        if List.contains key literals
        then frame.GetColumn key |> Some
        else None

    let (|Sizes|_|) (column,frame) =
        pair ClearLiteral.sizes (column,frame)

    let (|ShoeLinears|_|) (column,frame) =
        pair ClearLiteral.shoeLinears (column,frame)

    let (|CartonKey|_|) (column,frame) =
        pair [ClearLiteral.cartonKey] (column,frame)

    let (|CartonNumber|_|) (column,frame) =
        pair [ClearLiteral.cartonNumber] (column,frame)

    let (|CartonTotalNumber|_|) (column,frame) =
        pair [ClearLiteral.cartonTotalNumber] (column,frame)

    let (|UnTypedShoeLinears|_|) (column,frame) =
        match (column,frame) with 
        | ShoeLinears series -> Some series 
        | (key,frame) ->
            let series = frame.GetColumn(key)
            if Series.isUnique series then None 
            else Some series

    let (|Number|_|) (column,frame) =
        pair [ClearLiteral.number] (column,frame)
