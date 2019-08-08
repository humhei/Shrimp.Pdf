namespace Atrous.Pdf

module Barcode =
    open FParsec
    let runWithResultBack parser (s:string) =
        CharParsers.run parser s 
        |> function 
            | ParserResult.Success (x,_,_) -> x
            | ParserResult.Failure _ -> failwithf "failed parse %A" s   

    let isValidEan (code:string) length =
        if code.Length = length then 
            let digits = code |> Seq.map (string >> runWithResultBack pint32) |> List.ofSeq
            let s = if length % 2 = 0 then 3 else 1
            let s2 = if s = 3 then 1 else 3
            let left = digits |> List.last
            let right =
                let accum = 
                    digits |> List.take (length - 1) |> List.mapi (fun i digit -> 
                        let s = if i % 2 = 0 then s else s2
                        digit * s 
                    ) |> List.sum 
                (10 - accum % 10) % 10
            left = right
        else false

    let isValidEan13 code =
        isValidEan code 13
