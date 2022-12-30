namespace Shrimp.Pdf
#nowarn "0104"
open Fake.IO
open Shrimp.Pdf.Extensions
open iText.Kernel.Pdf
open System.IO
open Fake.IO.FileSystemOperators
open Shrimp.FSharp.Plus
open System
open System.Text


type VerticalText =
    { OriginText: StringIC 
      ConvertedText: string} 
with 
    static member Create(text: string) =
        let toVerticalTexts(text: string) =
            let chars = text.ToCharArray()
        
        
            let getBytes (char: Char) =
                char
                |> string
                |> Encoding.UTF8.GetBytes
        
            let rec loop stringAccum chars =
                match chars with 
                | [] -> List.rev stringAccum
                | char :: t  -> 
                    match char with 
                    | '{' ->
                        let rightIndex =
                            t
                            |> List.findIndex(fun m -> m = '}')
                        let texts  =
                            '︷' :: t.[0..rightIndex-1] @ ['︸'] 
                            |> List.map string
                            |> List.rev
        
                        loop (texts @ stringAccum) (t.[rightIndex+1..])
        
                    | _ -> 
                        
                        let nextChar = 
                            List.tryHead t 
        
                        match nextChar with 
                        | None ->
                            loop (string char :: stringAccum) []
        
                        | Some nextChar ->
                            
                            let bytes = getBytes char
                            match bytes.Length with 
                            | BiggerOrEqual 2 -> 
                                match nextChar with 
                                | ',' 
                                | '，' ->
                                    loop (System.String [|char; nextChar|] :: stringAccum) t.[1..]
        
                                | _ -> loop (string char :: stringAccum) t
        
                            | _ ->
                                let nextBytes = getBytes nextChar
                                match nextBytes.Length with 
                                | BiggerOrEqual 2 -> 
                                    loop (string char :: stringAccum) t
                                    
                                | _ -> 
                                    let nextChar2 = List.tryItem 1 t
                                    match nextChar2 with 
                                    | Some ',' 
                                    | Some '，' ->
        
                                        loop (System.String [|char; nextChar; nextChar2.Value|] :: stringAccum) t.[2..]
                                    
                                    | _ ->
                                        loop (System.String [|char; nextChar|] :: stringAccum) t.[1..]
        
                
        
            let chars =
                chars
                |> List.ofArray
                |> List.filter(fun m -> m <> '\n')
        
            loop [] (chars)
        
        { OriginText    = StringIC text 
          ConvertedText = toVerticalTexts text |> String.concat "\n" }
        