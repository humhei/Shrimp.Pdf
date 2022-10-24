// Learn more about F# at http://fsharp.org
namespace Shrimp.Pdf.Extract
open Akka.Configuration
open Shrimp.Akkling.Cluster.Intergraction.Configuration
open Shrimp.FSharp.Plus
open FParsec
open FParsec.CharParsers
open Shrimp.Pdf

[<AutoOpen>]
module internal Config =

    type private AssemblyFinder = AssemblyFinder

    let internal config = 
        lazy
            ConfigurationFactory
                .FromResource<AssemblyFinder>("Shrimp.Pdf.Extract.reference.conf")
            |> Configuration.fallBackByApplicationConf


    let internal loggingPageCountInterval = 
        lazy config.Value.GetInt("Shrimp.Pdf.Extract.LoggingPageCountInterval")

//[<AutoOpen>]
//module _Types =
//    type TextTransform = 
//        { Texts: PdfConcatedText list 
//          Delimiter: string 
//          WordSep: string }
//    with 
    
//        static member Create(texts: PdfConcatedText list, ?delimiter, ?wordSep) =
//            let delimiter = defaultArg delimiter ""
//            let wordSep = defaultArg wordSep delimiter
//            { 
//                Texts = texts
//                Delimiter = delimiter
//                WordSep = wordSep
//            }
            
//        static member Empty = TextTransform.Create []

//        static member Singlton(text) =
//            TextTransform.Create [text]

//        member private x.MapText(fText) =
//            { x with 
//                Texts = x.Texts |> List.map fText
//            }
         
    
//        //member private x.ReplaceGreaterThanSignToBar() =
//        //    x.MapText(fun (m: string) -> m.Replace(">", "-"))

    
//        member x.OriginText = 
//            x.Texts
//            |> List.map(fun m -> m.ConcatedText(x.WordSep))
//            |> String.concat x.Delimiter

//        //member x.Text = x.FileName.FileName

//        member x.FileName =
//            x.OriginText
//            |> ValidFileName.Create
    

    
//        member private x.MapTexts(fTexts) =
//            { x with Texts = fTexts x.Texts }


//        member x.Parity() =
//            x.MapTexts(fun texts ->
//                let rec loop accum texts =
//                    match texts with 
//                    | [] -> List.rev accum
//                    | [singleton] -> List.rev (singleton :: accum)
//                    | odd :: even :: t ->
//                        loop ((odd + even) :: accum) t
//                loop [] texts
//            )
    


//        member private x.MapDelimiter(fDelimiter) =
//            let newDelimiter = fDelimiter x.Delimiter
//            { x with Delimiter = newDelimiter }
    
    
//        member x.Regex(pattern, ?name) =
//            let v = 
//                match name with 
//                | None ->
//                    match x.OriginText with 
//                    | ParseRegex.HeadF pattern v -> v
//                | Some name ->
//                    match x.OriginText with 
//                    | ParseRegex.NamedHeadF (pattern, name) v -> v
    
//            TextTransform.Create ([PdfConcatedText.Create v], x.Delimiter)



//        member x.Item(index) =
//            x.MapTexts (List.item(index) >> List.singleton)
    
//        member x.Items(indexes: int list) =
//            x.MapTexts (fun texts ->
//                indexes
//                |> List.map(fun i -> texts.[i])
//            )

//        member x.TryItems(indexes: int list) =
//            x.MapTexts (fun texts ->
//                indexes
//                |> List.choose(fun i -> List.tryItem i texts)
//            )

//        member x.SplitBySpace() =
//            x.MapTexts (fun texts ->
//                texts
//                |> List.collect(fun text -> 
//                    text.ConcatedText(x.WordSep).Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) 
//                    |> List.ofArray
//                    |> List.map PdfConcatedText.Create

//                )
//            )

    
//        member x.ConcatBy(delimiter: string) =
//            x.MapDelimiter(fun _ -> delimiter)
    
//        member x.HeadAndLast_ConcatWithBar() =
//            x
//                .MapTexts(fun texts -> [texts.[0]; List.last texts])
//                .MapDelimiter(fun _ -> "-")
    
//        member x.ConcatWithUnderLine() = x.ConcatBy("_")
    
//        member x.GetSlice(start: int option, finish: int option) =
//            let start = defaultArg start 0
//            let finish = defaultArg finish x.Texts.Length
//            x.Items([start .. finish])
        