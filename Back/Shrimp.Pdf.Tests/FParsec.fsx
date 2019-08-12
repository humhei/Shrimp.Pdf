#I @"bin\Debug\net462\"
#r @"FParsec.dll"
#r @"FParsecCS.dll"
open FParsec
open System


let test p (str:string) =
    match run p str with
    | Success(result, _, _)   -> [result]
    | Failure(errorMsg, _, _) -> 
        printf "%A" errorMsg
        []

let range = "=fsi(piv,$N$2)"
test (pstring "=fsi" >>. skipCharsTillString "," true 100 >>. charsTillString ")" false 100) range