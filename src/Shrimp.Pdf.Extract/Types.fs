// Learn more about F# at http://fsharp.org
namespace Shrimp.Pdf.Extract
open Akka.Configuration
open Shrimp.Akkling.Cluster.Intergraction.Configuration
open Shrimp.FSharp.Plus

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

[<AutoOpen>]
module _Types =
    type TextTransform = internal TextTransform of string list * delimiter: string
    with 
        static member Singlton(text: string) =
            TextTransform([text], "")
    
        static member Empty =
            TextTransform([], "")
    
        static member Create(texts: string list, ?delimiter) =
            TextTransform(texts, defaultArg delimiter "")
            
        member private x.Texts =
            let (TextTransform (texts, delimiter)) = x
            texts

        member private x.MapText(fText) =
            let newTexts = 
                x.Texts
                |> List.map fText
            TextTransform(newTexts, x.Delimiter)
    
        //member private x.ReplaceGreaterThanSignToBar() =
        //    x.MapText(fun (m: string) -> m.Replace(">", "-"))

    
        member private x.FileName =
            let (TextTransform (texts, delimiter)) = x
            texts
            |> String.concat delimiter
            |> ValidFileName.Create
    
    
        member x.Text = x.FileName.FileName

        member x.OriginText = x.FileName.OriginFileName

    
        member x.Delimiter =
            let (TextTransform (texts, delimiter)) = x
            delimiter
    
        member private x.MapTexts(fTexts) =
            let newTexts = fTexts x.Texts
            TextTransform(newTexts, x.Delimiter)
    


        member x.Parity() =
            x.MapTexts(fun texts ->
                let rec loop accum texts =
                    match texts with 
                    | [] -> List.rev accum
                    | [singleton] -> List.rev (singleton :: accum)
                    | odd :: even :: t ->
                        loop ((odd + even) :: accum) t
                loop [] texts
            )
    


        member private x.MapDelimiter(fDelimiter) =
            let newDelimiter = fDelimiter x.Delimiter
            TextTransform(x.Texts, newDelimiter)
    
    
        member x.Regex(pattern, ?name) =
            let v = 
                match name with 
                | None ->
                    match x.OriginText with 
                    | ParseRegex.HeadF pattern v -> v
                | Some name ->
                    match x.OriginText with 
                    | ParseRegex.NamedHeadF (pattern, name) v -> v
    
            TextTransform ([v], x.Delimiter)



        member x.Item(index) =
            x.MapTexts (List.item(index) >> List.singleton)
    
        member x.Items(indexes: int list) =
            x.MapTexts (fun texts ->
                indexes
                |> List.map(fun i -> texts.[i])
            )

        member x.SplitBySpace() =
            x.MapTexts (fun texts ->
                texts
                |> List.collect(fun text -> 
                    text.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray)
            )

    
        member x.ConcatBy(delimiter: string) =
            x.MapDelimiter(fun _ -> delimiter)
    
        member x.HeadAndLast_ConcatWithBar() =
            x
                .MapTexts(fun texts -> [texts.[0]; List.last texts])
                .MapDelimiter(fun _ -> "-")
    
        member x.ConcatWithUnderLine() = x.ConcatBy("_")
    
        member x.GetSlice(start: int option, finish: int option) =
            let start = defaultArg start 0
            let finish = defaultArg finish x.Texts.Length
            x.Items([start .. finish])
        