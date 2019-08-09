namespace Shrimp.EntitiesCsv

open System.IO
open Microsoft.FSharp.Reflection
open System
open Shrimp.Entities.Converters

type ImposerDataCell =
    {
        Header: string
        Value: obj
        Type: Type
    }

type ImposerDataRow = ImposerDataCell list

[<RequireQualifiedAccess>]
module ImposerDataRow =
    let getCellFromKey key (row:ImposerDataRow) = 
        row |> List.find (fun data -> data.Header = key)

    let getValueFromKey key (row:ImposerDataRow) = 
        let data = row |> getCellFromKey key 
        data.Value

type ImposerDataTable = ImposerDataRow list
[<RequireQualifiedAccess>]
module Reflection =
    open System.Reflection
    open System.Collections.Concurrent
    open System.Collections.Generic

    let private headerLengthConverter = new ConcurrentDictionary<Type,int>()
    let defaultTypeOfOption tp = 
        match tp with 
        | tp when FSharpType.IsRecord tp -> tp
        | tp ->
            tp.GetProperty("Value") 
            |> Option.ofObj
            |> Option.map (fun p -> p.PropertyType)
            |> Option.defaultValue tp

    let private emptyConverters = dict []

    let rec handleNestedRecordHeader (customValueConverters: IDictionary<Type,Type * (obj -> obj)>) (tp: Type, name: string) =
        let reuse = handleNestedRecordHeader customValueConverters
        let reuseEmpty = handleNestedRecordHeader (emptyConverters)
        
        let header = 
            let tp = defaultTypeOfOption tp
            match tp with 
            | tp when FSharpType.IsRecord tp ->
                tp.GetProperties() 
                |> Array.collect (fun info -> reuse (info.PropertyType,name + info.Name))
            | tp when FSharpType.IsUnion tp && UciDetailConverters.Contains(tp) ->
                let ucis = FSharpType.GetUnionCases tp
                let fields = ucis |> Array.collect (fun uci -> uci.GetFields() |> Array.collect(fun info ->
                    reuse (info.PropertyType,name + info.DeclaringType.Name)
                ))
                (Array.append [|name|] fields)
            | tp when customValueConverters.ContainsKey(tp) ->
                let convertedTp,_ = customValueConverters.[tp]
                Array.append 
                    (reuseEmpty (tp,name))
                    (reuse (convertedTp,name + "_Converted"))
            | _ -> 
                [| name |]
        headerLengthConverter.TryAdd(tp,header.Length) |> ignore
        header


    let rec handleNestedRecord (customValueConverters: IDictionary<Type,Type * (obj -> obj)>) (x:obj) =
        let reuse = handleNestedRecord customValueConverters
        let reuseEmpty = handleNestedRecord (emptyConverters)
        let tp = x.GetType()
        match tp,tp.GetProperty("Value") |> Option.ofObj with 
        | _, Some vp ->
            reuse (vp.GetValue(x))      
        | tp, None when FSharpType.IsRecord tp ->
            tp.GetProperties() 
            |> Array.collect (fun info -> 
                reuse (info.GetValue(x)))
        | tp, None when FSharpType.IsUnion tp && UciDetailConverters.Contains(tp.DeclaringType) ->
            let ucis = FSharpType.GetUnionCases tp
            let fields = ucis |> Array.collect (fun uci -> uci.GetFields() |> Array.collect (fun info ->
                if info.DeclaringType = tp then
                    reuse (info.GetValue(x))
                else
                    let propType = info.PropertyType
                    Array.replicate headerLengthConverter.[propType] ""
            ))
            Array.append [|x.ToString()|] fields
        | tp, None when customValueConverters.ContainsKey(tp) ->
            let _,converter = customValueConverters.[tp]
            Array.append
                (reuseEmpty x)
                (reuse (converter x))
        | _, None -> 
            [| x.ToString() |]

[<RequireQualifiedAccess>]
module Array =
    let join delimiter xs = 
        xs 
        |> Array.map (fun x -> x.ToString())
        |> String.concat delimiter

[<RequireQualifiedAccess>]
module Seq =
    open System.Text
    open OfficeOpenXml


    let write (path:string) (data:seq<'a>) = 
        use writer = new StreamWriter(path,false,Encoding.UTF8)
        data
        |> Seq.iter writer.WriteLine 

    let writeXlsx (path:string) (data:seq<seq<'a>>) = 
        use p = new ExcelPackage()
        let ws = p.Workbook.Worksheets.Add("Sheet1")
        data
        |> Seq.map (fun r -> r |> Seq.map box)
        |> Seq.map Array.ofSeq
        |> ws.Cells.["A1"].LoadFromArrays
        |> ignore
        p.SaveAs(new FileInfo(path))





    let private handleNestedRecord = Reflection.handleNestedRecord customValueConverters
    let private handleNestedRecordHeader = Reflection.handleNestedRecordHeader customValueConverters


    let xlsxWithProp (useEnclosure:bool) (headerMapping:string -> string) (datas:seq<ImposerDataRow>) =
        
        let enclose s =
            match useEnclosure with
            | true -> "\"" + s + "\""
            | false -> s



        let header =
            datas
            |> Seq.head
            |> Seq.collect (fun gd -> handleNestedRecordHeader (gd.Type,gd.Header))
            |> Seq.map headerMapping 

        let rows = 
            datas 
            |> Seq.map (fun r ->
                r |> Seq.collect (fun gd -> handleNestedRecord gd.Value) |> Seq.map enclose
            )

        seq {
            yield header
            yield! rows
        }
    let csvWithProp (separator:string) (useEnclosure:bool) (headerMapping:string -> string) (datas:seq<ImposerDataRow>) =
        
        let inline enclose s =
            match useEnclosure with
            | true -> "\"" + (string s) + "\""
            | false -> string s


        let header =
            datas
            |> Seq.head
            |> Seq.collect (fun gd -> handleNestedRecordHeader (gd.Type,gd.Header))
            |> Seq.map headerMapping 
            |> String.concat separator

        let rows = 
            datas 
            |> Seq.map (fun r ->
                r |> Seq.collect (fun gd -> handleNestedRecord gd.Value) |> Seq.map enclose |> String.concat separator
            )

        seq {
            yield header
            yield! rows
        }
        
    let csv (separator:string) (useEnclosure:bool) (headerMapping:string -> string) (data:seq<'a>) =
        seq {
            let dataType = typeof<'a>
            let stringSeqDataType = typeof<System.Collections.Generic.IEnumerable<string>>
            let inline enclose s =
                match useEnclosure with
                | true -> "\"" + (string s) + "\""
                | false -> string s

            let header = 
                match dataType with
                | ty when FSharpType.IsRecord ty ->
                    FSharpType.GetRecordFields dataType
                    |> Array.collect (fun info -> 
                        (handleNestedRecordHeader >> Array.map headerMapping) (info.PropertyType,info.Name))                    
                | ty when FSharpType.IsTuple ty -> 
                    FSharpType.GetTupleElements dataType
                    |> Array.mapi (fun idx info -> headerMapping(string idx) )
                | ty when ty.IsAssignableFrom stringSeqDataType ->
                    data :?> seq<seq<string>> |> Seq.head
                    |> Seq.toArray
                | _ -> dataType.GetProperties()
                    |> Array.map (fun info -> headerMapping info.Name)

            yield header |> Array.map enclose |> Array.join separator

            let lines =
                match dataType with 
                | ty when FSharpType.IsRecord ty -> 
                    data |> Seq.map (FSharpValue.GetRecordFields >> Array.collect handleNestedRecord)
                | ty when FSharpType.IsTuple ty ->
                    data |> Seq.map (FSharpValue.GetTupleFields >> Array.collect handleNestedRecord)
                | ty when ty.IsAssignableFrom stringSeqDataType ->
                    data :?> seq<seq<string>> |> Seq.tail
                    |> Seq.map (fun ss -> Seq.toArray ss)
                | _ -> 
                    let props = dataType.GetProperties()
                    data |> Seq.map ( fun line -> 
                                props |> Array.map ( fun prop ->
                                prop.GetValue(line).ToString()))                                     
                |> Seq.map (Array.map enclose)
            yield! lines |> Seq.map (Array.join separator)
        }

