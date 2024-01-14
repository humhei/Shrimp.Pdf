namespace Shrimp.Pdf.PostScript
/// https://personal.math.ubc.ca/~cass/courses/ps.html
open System
open Shrimp.FSharp.Plus
open Shrimp.FSharp.Plus.Math
open System.Collections.Generic

[<AutoOpen>]
module _PostScriptInterpreter =
    [<RequireQualifiedAccess>]
    type PostScriptValue =
        | Double of float
        | Boolean of bool
        | String of string
    with 
        member x.AsDouble =
            match x with 
            | Double v -> v
            | String v -> Double.parse_detailError v
            | _ -> failwithf "%A is not an valid double" x

        member x.AsBoolean =
            match x with 
            | Boolean v -> v
            | String v -> Boolean.Parse v
            | _ -> failwithf "%A is not an valid boolean" x

        member x.AsInt =
            match x with 
            | Double v -> Double.toInt_Safe v
            | String v -> System.Int32.Parse v
            | _ -> failwithf "%A is not an valid int" x

    [<RequireQualifiedAccess>]
    module PostScriptValue =
        let asInt(v: PostScriptValue) = v.AsInt

        let asDouble(v: PostScriptValue) = v.AsDouble

        let asBoolean(v: PostScriptValue) = v.AsBoolean

    type IOperator = 
        abstract member InvokeStack: stack: Stack<PostScriptValue> -> Stack<PostScriptValue>
    
    type PostScriptValueOperator(value: PostScriptValue) =
        interface IOperator with 
            member x.InvokeStack(stack: Stack<PostScriptValue>) =
                stack.Push(value)
                stack

    let private popParams1(stack: Stack<PostScriptValue>) =
        stack.Pop()

    let private popParams1_int(stack: Stack<PostScriptValue>) =
        stack.Pop().AsInt
        

    let private popParams2(stack: Stack<PostScriptValue>) =
        let param2 = stack.Pop()
        let param1 = stack.Pop()
        param1, param2

    let private popParams2_int(stack: Stack<PostScriptValue>) =
        let a, b = popParams2 stack
        a.AsInt, b.AsInt

    let private popParams2_double(stack: Stack<PostScriptValue>) =
        let a, b = popParams2 stack
        a.AsDouble, b.AsDouble

    let private popParams3(stack: Stack<PostScriptValue>) =
        let param3 = stack.Pop()
        let param2 = stack.Pop()
        let param1 = stack.Pop()
        param1, param2, param3

    module StackOperators = 
        type IStackOperator =
            abstract member Invoke: stack: Stack<PostScriptValue> -> Stack<PostScriptValue>
            inherit IOperator

        type StackOperator(f) =
            member x.Invoke(stack) = 
                f stack |> ignore
                stack

            interface IStackOperator with 
                member x.Invoke(stack) = x.Invoke(stack)
                member x.InvokeStack(stack) = x.Invoke(stack)

        type RollOperator() =
            inherit StackOperator(fun stack ->
                let length, shift = popParams2_int stack

                let poped =
                    [ 1..length ]
                    |> List.map(fun _ -> stack.Pop())
                    |> List.rev

                let shifted = List.shift shift poped
                for item in shifted do
                    stack.Push(item)
            )


        type CopyOperator() =
            inherit StackOperator(fun stack ->
                let length = popParams1_int stack
                let poped =
                    [ 1..length ]
                    |> List.map(fun _ -> stack.Pop())
                    |> List.rev

                for item in poped do
                    stack.Push(item)

                for item in poped do
                    stack.Push(item)

            )

        type DupOperator() =
            inherit StackOperator(fun stack ->
                let value = stack.Peek()
                stack.Push(value)
            )

        type PopOperator() =
            inherit StackOperator(fun stack ->
                stack.Pop()
                |> ignore
            )

        type ExchOperator() =
            inherit StackOperator(fun stack ->
                let v1 = stack.Pop()
                let v2 = stack.Pop()
                stack.Push(v1)
                stack.Push(v2)
            )
        
        type IndexOperator() =
            inherit StackOperator(fun stack ->
                let index = popParams1_int stack
                let array = stack.ToArray()
                stack.Push(array.[index])
            )


        let directOperators =
            [
                nameof(PopOperator) => (PopOperator >> fun r -> r :> IOperator)
                nameof(ExchOperator) => (ExchOperator >> fun r -> r :> IOperator)
                nameof(DupOperator) => (DupOperator >> fun r -> r :> IOperator)
                nameof(CopyOperator) => (CopyOperator >> fun r -> r :> IOperator)
                nameof(IndexOperator) => (IndexOperator >> fun r -> r :> IOperator)
                nameof(RollOperator) => (RollOperator >> fun r -> r :> IOperator)
            ]

        let oneParamOperators = 
            [
                //nameof(CopyOperator) => (PostScriptValue.asInt >> CopyOperator >> fun r -> r :> IOperator)
                //nameof(IndexOperator) => (PostScriptValue.asInt >> IndexOperator >> fun r -> r :> IOperator)
            ]

        let twoParamsOperators = 
            [
                //nameof(RollOperator) => (fun (input1, input2) -> 
                //    (PostScriptValue.asInt input1, PostScriptValue.asInt input2)
                //    |> RollOperator
                //    |> fun m -> m :> IOperator
                //)
            ]

    module BitwiseOperators =

        type IBitwiseOperator =
            inherit IOperator
            abstract member Invoke: bool -> bool

        type IBitShiftOperator =
            inherit IOperator
            abstract member Invoke: int -> int

        type IComparisonOperator =
            inherit IOperator
            abstract member Compare: PostScriptValue -> bool

        type IBooleanOperator =
            inherit IOperator
            abstract member Value: bool
            
        type ParamBitwiseOperator(paramB: bool, predicate: bool -> bool) =
            member x.ParamB = paramB

            member x.Invoke(b) =
                predicate b

            interface IBitwiseOperator with 
                member x.Invoke(b) = x.Invoke(b)

                member x.InvokeStack(stack) =
                    let value = stack.Pop().AsBoolean
                    let paramB = stack.Pop().AsBoolean
                    let b = x.Invoke(value)
                    stack.Push(PostScriptValue.Boolean b)
                    stack

        type BooleanOperator(b) =
            interface IBooleanOperator with 
                member x.Value = b
                member x.InvokeStack(stack) =
                    stack.Push(PostScriptValue.Boolean b)
                    stack


        type ComparisonOperator(param, compare) =
            member x.Param = param
                member x.Compare(value) = compare value

            interface IComparisonOperator with
                member x.Compare(value) = compare value

                member x.InvokeStack(stack) =
                    let value = stack.Peek()
                    let b = x.Compare(value)
                    stack.Push(PostScriptValue.Boolean b)
                    stack

        type AndOperator(paramB) =
            inherit ParamBitwiseOperator(paramB, fun value ->
                value && paramB
            )

        type FalseOperator() =
            inherit BooleanOperator(false)

        type LEOperator(param) =
            inherit ComparisonOperator(param, fun value ->
                value <= param
            )

        type NotOperator() =
            member x.Invoke(b) = not b
            interface IBitwiseOperator with 
                member x.Invoke(b) = not b

                member x.InvokeStack(stack) =
                    let value = stack.Pop().AsBoolean
                    let b = x.Invoke(value)
                    stack.Push(PostScriptValue.Boolean b)
                    stack

        type TrueOperator() =
            inherit BooleanOperator(true)

        type BitShiftOperator(shift: int) =

            member x.Invoke(value: int) =
                let value = value
                match shift with 
                | BiggerOrEqual 0 -> 
                    value <<< shift

                | _ -> 
                    value >>> shift

            interface IBitShiftOperator with 
                member x.Invoke(value: int) = x.Invoke(value)

                member x.InvokeStack(stack) =
                    let value = stack.Pop().AsInt
                    let b = x.Invoke(value)
                    stack.Push(PostScriptValue.Double (b))
                    stack
                

        type GEOperator(param) =
            inherit ComparisonOperator(param, fun value ->
                value >= param
            )

        type LTOperator(param) =
            inherit ComparisonOperator(param, fun value ->
                value < param
            )

        type OrOperator(paramB) =
            inherit ParamBitwiseOperator(paramB, fun value ->
                value || paramB
            )

        type XOROperator(paramB) =
            inherit ParamBitwiseOperator(paramB, fun value ->
                 (value || paramB) && (value <> paramB)
            )

        type EQOperator(paramB) =
            inherit ComparisonOperator(paramB, fun value ->
                value = paramB
            )

        type GTOperator(param) =
            inherit ComparisonOperator(param, fun value ->
                value > param
            )

        type NEOperator(paramB) =
            inherit ComparisonOperator(paramB, fun value ->
                value <> paramB
            )

        let directOperators =
            [
                nameof(TrueOperator) => (TrueOperator >> fun r -> r :> IOperator)
                nameof(FalseOperator) => (FalseOperator >> fun r -> r :> IOperator)
                nameof(NotOperator) => (NotOperator >> fun r -> r :> IOperator)
            ]

        let oneParamOperators = 
            [
                nameof(AndOperator) => (PostScriptValue.asBoolean >> AndOperator >> fun r -> r :> IOperator)
                nameof(OrOperator) => (PostScriptValue.asBoolean >> OrOperator >> fun r -> r :> IOperator)
                nameof(XOROperator) => (PostScriptValue.asBoolean >> XOROperator >> fun r -> r :> IOperator)
                nameof(LEOperator) => (id >> LEOperator >> fun r -> r :> IOperator)
                nameof(LTOperator) => (id >> LTOperator >> fun r -> r :> IOperator)
                nameof(GEOperator) => (id >> GEOperator >> fun r -> r :> IOperator)
                nameof(GTOperator) => (id >> GTOperator >> fun r -> r :> IOperator)
                nameof(NEOperator) => (id >> NEOperator >> fun r -> r :> IOperator)
                nameof(EQOperator) => (id >> EQOperator >> fun r -> r :> IOperator)
                nameof(BitShiftOperator) => (PostScriptValue.asInt >> BitShiftOperator >> fun r -> r :> IOperator)
            ]

        let twoParamsOperators = 
            [
                
            ]



    module ArithmeticOperators =
        type IArithmeticOperator =
            inherit IOperator
            abstract member Invoke: float -> float

        type NotImplementedArithmeticOperator() =
            interface IArithmeticOperator with 
                member x.Invoke(input) = failwithf "Not implemented postscript operator %A" (x.GetType())
                member x.InvokeStack(stack) = failwithf "Not implemented postscript operator %A" (x.GetType())


        type DirectArithmeticOperator(mapping) =
            member x.Mapping = mapping

            member x.Invoke(input) = mapping input

            interface IArithmeticOperator with 
                member x.Invoke(input) = x.Invoke(input)
                member x.InvokeStack(stack) =
                    let value = stack.Pop().AsDouble
                    let b = x.Invoke(value)
                    stack.Push(PostScriptValue.Double (b))
                    stack

        type ParamArithmeticOperator(param, mapping) =
            member x.Mapping = mapping

            member x.Param = param
            member x.Invoke(input) = mapping input 

            interface IArithmeticOperator with 
                member x.Invoke(input) = x.Invoke(input)
                member x.InvokeStack(stack) =
                    let value = stack.Pop().AsDouble
                    let b = x.Invoke(value)
                    stack.Push(PostScriptValue.Double (b))
                    stack


        type AbsOperator() =
            inherit DirectArithmeticOperator(Math.Abs)

        type CviOperator() =
            inherit NotImplementedArithmeticOperator()

        type FloorOperator() =
            inherit DirectArithmeticOperator(Math.Floor)

        type ModOperator(moder: float) =
            inherit ParamArithmeticOperator(moder, fun value ->
                Double.toInt_Safe value % Double.toInt_Safe moder
                |> float
            )

        type SinOperator() =
            inherit DirectArithmeticOperator(Math.Sin)

        type AddOperator(addingValue: float) =
             inherit ParamArithmeticOperator(addingValue, fun value ->
                 addingValue + value
             )


        type CvrOperator() =
            inherit NotImplementedArithmeticOperator()

        type IdivOperator(divider: float) =
            inherit ParamArithmeticOperator(divider, fun value ->
                value / divider
                |> floor
            )

        type MulOperator(multiple: float) =
             inherit ParamArithmeticOperator(multiple, fun value ->
                 multiple * value
             )

        type SqrtOperator() =
            inherit DirectArithmeticOperator(Math.Sqrt)


        type AtanOperator() =
             inherit DirectArithmeticOperator(Math.Atan)


        type DivOperator(divider: float) =
             inherit ParamArithmeticOperator(divider, fun value ->
                 value / divider
             )

        type LnOperator() =
             inherit DirectArithmeticOperator(Math.Log)

        type NegOperator() =
             inherit DirectArithmeticOperator(fun m -> -m)

        type SubOperator(suber) =
             inherit ParamArithmeticOperator(suber, fun value ->
                 value - suber
             )

        type CeilingOperator() =
             inherit DirectArithmeticOperator(Math.Ceiling)

        
        type ExpOperator() =
             inherit DirectArithmeticOperator(Math.Exp)

        type LogOperator() =
             inherit DirectArithmeticOperator(Math.Log10)

        type RoundOperator() =
             inherit DirectArithmeticOperator(Math.Round)

        type TruncateOperator() =
             inherit DirectArithmeticOperator(Math.Truncate)

        type CosOperator() =
             inherit DirectArithmeticOperator(Math.Cos)

        let directOperators =
            [
                nameof(AbsOperator) => (AbsOperator >> fun r -> r :> IOperator)              
                nameof(CviOperator) => (CviOperator >> fun r -> r :> IOperator)
                nameof(FloorOperator) => (FloorOperator >> fun r -> r :> IOperator)
                nameof(SinOperator) => (SinOperator >> fun r -> r :> IOperator)
                nameof(CvrOperator) => (CvrOperator >> fun r -> r :> IOperator)
                nameof(SqrtOperator) => (SqrtOperator >> fun r -> r :> IOperator)
                nameof(AtanOperator) => (AtanOperator >> fun r -> r :> IOperator)
                nameof(LnOperator) => (LnOperator >> fun r -> r :> IOperator)
                nameof(NegOperator) => (NegOperator >> fun r -> r :> IOperator)
                nameof(CeilingOperator) => (CeilingOperator >> fun r -> r :> IOperator)
                nameof(ExpOperator) => (ExpOperator >> fun r -> r :> IOperator)
                nameof(LogOperator) => (LogOperator >> fun r -> r :> IOperator)
                nameof(RoundOperator) => (RoundOperator >> fun r -> r :> IOperator)
                nameof(TruncateOperator) => (TruncateOperator >> fun r -> r :> IOperator)
                nameof(CosOperator) => (CosOperator >> fun r -> r :> IOperator)
            ]

        let oneParamOperators = 
            [
                nameof(ModOperator) => (PostScriptValue.asDouble >> ModOperator >> fun r -> r :> IOperator)    
                nameof(AddOperator) => (PostScriptValue.asDouble >> AddOperator >> fun r -> r :> IOperator)
                nameof(MulOperator) => (PostScriptValue.asDouble >> MulOperator >> fun r -> r :> IOperator)
                nameof(DivOperator) => (PostScriptValue.asDouble >> DivOperator >> fun r -> r :> IOperator)
                nameof(IdivOperator) => (PostScriptValue.asDouble >> IdivOperator >> fun r -> r :> IOperator)
                nameof(SubOperator) => (PostScriptValue.asDouble >> SubOperator >> fun r -> r :> IOperator)
            ]

        let twoParamsOperators = 
            [
                
            ]

    
    [<RequireQualifiedAccess>]
    type OperatorUnion =
        | Direct   of (unit -> IOperator)
        | OneParam of (PostScriptValue -> IOperator)
        | TwoParams of (PostScriptValue * PostScriptValue -> IOperator)

    let private directOperators = 
        StackOperators.directOperators 
        @ ArithmeticOperators.directOperators 
        @ BitwiseOperators.directOperators
        |> List.mapFst(fun m -> m.TrimEnding("Operator", ignoreCase = true))
        |> List.mapSnd OperatorUnion.Direct

    let private oneParamOperators = 
        StackOperators.oneParamOperators
        @ ArithmeticOperators.oneParamOperators
        @ BitwiseOperators.oneParamOperators
        |> List.mapFst(fun m -> m.TrimEnding("Operator", ignoreCase = true))
        |> List.mapSnd OperatorUnion.OneParam

    let private twoParamsOperators = 
        StackOperators.twoParamsOperators
        @ ArithmeticOperators.twoParamsOperators
        @ BitwiseOperators.twoParamsOperators
        |> List.mapFst(fun m -> m.TrimEnding("Operator", ignoreCase = true))
        |> List.mapSnd OperatorUnion.TwoParams

    let private allOperators = 
        let operators = 
            directOperators @ oneParamOperators @ twoParamsOperators
            |> List.mapFst StringIC

        let map = Map.ofList operators
        match map.Count = operators.Length with 
        | true -> map
        | false -> 
            operators
            |> List.ensureNotDuplicatedWith fst
            |> ignore
            
            failwithf ""

    type IFOperator =
        { Current: IOperator list }

    type PostScriptInterpreter private (inputValues: PostScriptValue list, operators: IOperator list) =
        let stack = Stack<PostScriptValue>(inputValues)
        let newStack =
            (stack, operators)
            ||> List.fold(fun stack operator ->
                operator.InvokeStack(stack)
            )
            

        member x.StackResult = newStack
            
        static member Create(inputValues: PostScriptValue list, inputString: string) =
            let inputString = 
                inputString.Trim().TrimStart('{').TrimEnd('}')
                |> Text.trimAllToOne

            let operators = 
                let rec loop accumOperands accumOperators (parts: StringIC list) =
                    match parts with 
                    | [] -> 
                        match accumOperands with  
                        | _ :: _ -> failwithf "exists left operands %A" accumOperands
                        | [] -> List.rev accumOperators

                    | part :: t ->
                        match part.Value with 
                        | "{" -> failwithf ""

                        | _ ->
                            match allOperators.TryFind part with 
                            | Some operator -> 
                                match operator with 
                                | OperatorUnion.Direct operator -> 
                                    match accumOperands with 
                                    | [] -> loop [] (operator() :: accumOperators) t
                                    | _ -> failwithf "Invalid accum operands %A for direct operator %s" accumOperands part.Value

                                | OperatorUnion.OneParam operator -> 
                                    match accumOperands with 
                                    | [operand] -> 
                                        loop [] (operator operand :: accumOperators) t
                                    | _ -> failwithf "Invalid accum operands %A for OneParam operator %s" accumOperands part.Value

                                | OperatorUnion.TwoParams operator -> 
                                    match accumOperands with 
                                    | [operand1; operand2] -> loop [] (operator (operand1, operand2) :: accumOperators) t
                                    | _ -> failwithf "Invalid accum operands %A for OneParam operator %s" accumOperands part.Value


                            | None -> 
                                loop (PostScriptValue.String part.Value :: accumOperands) accumOperators t

                let parts = 
                    inputString.SplitAsListAndTrim(" ")
                    |> List.map StringIC

                loop [] [] parts

            
            PostScriptInterpreter(inputValues, operators)

