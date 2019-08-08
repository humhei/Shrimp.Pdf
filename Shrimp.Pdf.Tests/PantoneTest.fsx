open System.Drawing
open System

let encodeToString (values: int list) =
    match values with 
    | [l;a;b] -> 
        sprintf "0x%02x%02x%02x" l (a + 128) (b + 128)
    | _ -> invalidArg "value" (values.ToString())
let values = [82;-31;-6]
/// 71 177 163
let text = encodeToString values
let k = 0x52617a