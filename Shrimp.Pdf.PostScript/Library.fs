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
        
    let private popParams1_Boolean(stack: Stack<PostScriptValue>) =
        stack.Pop().AsBoolean

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

        type IBitwiseOperator1 =
            inherit IOperator
            abstract member Invoke: bool -> bool


        type IBitwiseOperator2 =
            inherit IOperator
            abstract member Invoke: bool * bool -> bool

        type IBitShiftOperator =
            inherit IOperator
            abstract member Invoke: value: int * shift: int -> int

        type IComparisonOperator =
            inherit IOperator
            abstract member Compare: PostScriptValue * PostScriptValue -> bool

        type IBooleanOperator =
            inherit IOperator
            abstract member Value: bool
            
        type ParamBitwiseOperator(predicate: bool -> bool -> bool) =
            member x.Invoke(b1, b2) =
                predicate b1 b2

            interface IBitwiseOperator2 with 
                member x.Invoke(b1, b2) = x.Invoke(b1, b2)

                member x.InvokeStack(stack) =
                    let paramB = stack.Pop().AsBoolean
                    let value = stack.Pop().AsBoolean
                    let b = x.Invoke(value, paramB)
                    stack.Push(PostScriptValue.Boolean b)
                    stack

        type BooleanOperator(b) =
            interface IBooleanOperator with 
                member x.Value = b
                member x.InvokeStack(stack) =
                    stack.Push(PostScriptValue.Boolean b)
                    stack


        type ComparisonOperator(compare) =
            member x.Compare(v1, v2) = compare v1 v2

            interface IComparisonOperator with
                member x.Compare(v1, v2) = compare v1 v2

                member x.InvokeStack(stack) =
                    let paramB = stack.Pop()
                    let value = stack.Peek()
                    let b = x.Compare(value, paramB)
                    stack.Push(PostScriptValue.Boolean b)
                    stack

        type AndOperator() =
            inherit ParamBitwiseOperator(fun value paramB ->
                value && paramB
            )

        type FalseOperator() =
            inherit BooleanOperator(false)

        type LEOperator() =
            inherit ComparisonOperator(fun value param ->
                value <= param
            )

        type NotOperator() =
            member x.Invoke(b) = not b
            interface IBitwiseOperator1 with 
                member x.Invoke(b) = not b

                member x.InvokeStack(stack) =
                    let value = stack.Pop().AsBoolean
                    let b = x.Invoke(value)
                    stack.Push(PostScriptValue.Boolean b)
                    stack

        type TrueOperator() =
            inherit BooleanOperator(true)

        type BitShiftOperator() =

            member x.Invoke(value: int, shift) =
                let value = value
                match shift with 
                | BiggerOrEqual 0 -> 
                    value <<< shift

                | _ -> 
                    value >>> shift

            interface IBitShiftOperator with 
                member x.Invoke(value: int, shift) = x.Invoke(value, shift)

                member x.InvokeStack(stack) =
                    let shift = stack.Pop().AsInt
                    let value = stack.Pop().AsInt
                    let b = x.Invoke(value, shift)
                    stack.Push(PostScriptValue.Double (b))
                    stack
                

        type GEOperator() =
            inherit ComparisonOperator(fun value param ->
                value >= param
            )

        type LTOperator() =
            inherit ComparisonOperator(fun value param ->
                value < param
            )

        type OrOperator() =
            inherit ParamBitwiseOperator(fun value paramB ->
                value || paramB
            )

        type XOROperator() =
            inherit ParamBitwiseOperator(fun value paramB ->
                 (value || paramB) && (value <> paramB)
            )

        type EQOperator() =
            inherit ComparisonOperator(fun value paramB ->
                value = paramB
            )

        type GTOperator() =
            inherit ComparisonOperator(fun value param ->
                value > param
            )

        type NEOperator() =
            inherit ComparisonOperator(fun value paramB ->
                value <> paramB
            )

        let directOperators =
            [
                nameof(TrueOperator) => (TrueOperator >> fun r -> r :> IOperator)
                nameof(FalseOperator) => (FalseOperator >> fun r -> r :> IOperator)
                nameof(NotOperator) => (NotOperator >> fun r -> r :> IOperator)
                nameof(AndOperator) => (AndOperator >> fun r -> r :> IOperator)
                nameof(OrOperator) => (OrOperator >> fun r -> r :> IOperator)
                nameof(XOROperator) => (XOROperator >> fun r -> r :> IOperator)
                nameof(LEOperator) => ( LEOperator >> fun r -> r :> IOperator)
                nameof(LTOperator) => ( LTOperator >> fun r -> r :> IOperator)
                nameof(GEOperator) => ( GEOperator >> fun r -> r :> IOperator)
                nameof(GTOperator) => ( GTOperator >> fun r -> r :> IOperator)
                nameof(NEOperator) => ( NEOperator >> fun r -> r :> IOperator)
                nameof(EQOperator) => ( EQOperator >> fun r -> r :> IOperator)
                nameof(BitShiftOperator) => (BitShiftOperator >> fun r -> r :> IOperator)
            ]

        let oneParamOperators = 
            [
                //nameof(AndOperator) => (PostScriptValue.asBoolean >> AndOperator >> fun r -> r :> IOperator)
                //nameof(OrOperator) => (PostScriptValue.asBoolean >> OrOperator >> fun r -> r :> IOperator)
                //nameof(XOROperator) => (PostScriptValue.asBoolean >> XOROperator >> fun r -> r :> IOperator)
                //nameof(LEOperator) => (id >> LEOperator >> fun r -> r :> IOperator)
                //nameof(LTOperator) => (id >> LTOperator >> fun r -> r :> IOperator)
                //nameof(GEOperator) => (id >> GEOperator >> fun r -> r :> IOperator)
                //nameof(GTOperator) => (id >> GTOperator >> fun r -> r :> IOperator)
                //nameof(NEOperator) => (id >> NEOperator >> fun r -> r :> IOperator)
                //nameof(EQOperator) => (id >> EQOperator >> fun r -> r :> IOperator)
                //nameof(BitShiftOperator) => (PostScriptValue.asInt >> BitShiftOperator >> fun r -> r :> IOperator)
            ]

        let twoParamsOperators = 
            [
                
            ]



    module ArithmeticOperators =

        type IArithmeticOperator1 =
            inherit IOperator
            abstract member Invoke: float -> float

        type IArithmeticOperator2 =
            inherit IOperator
            abstract member Invoke: float * float -> float

        type NotImplementedArithmeticOperator() =
            interface IArithmeticOperator1 with 
                member x.Invoke(input) = failwithf "Not implemented postscript operator %A" (x.GetType())
                member x.InvokeStack(stack) = failwithf "Not implemented postscript operator %A" (x.GetType())


        type DirectArithmeticOperator(mapping) =
            member x.Mapping = mapping

            member x.Invoke(input) = mapping input

            interface IArithmeticOperator1 with 
                member x.Invoke(input) = x.Invoke(input)
                member x.InvokeStack(stack) =
                    let value = stack.Pop().AsDouble
                    let b = x.Invoke(value)
                    stack.Push(PostScriptValue.Double (b))
                    stack

        type ParamArithmeticOperator(mapping) =
            member x.Mapping = mapping

            member x.Invoke(input, param) = mapping input param

            interface IArithmeticOperator2 with 
                member x.Invoke(input, param) = x.Invoke(input, param)
                member x.InvokeStack(stack) =
                    let param = stack.Pop().AsDouble
                    let value = stack.Pop().AsDouble
                    let b = x.Invoke(value, param)
                    stack.Push(PostScriptValue.Double (b))
                    stack


        type AbsOperator() =
            inherit DirectArithmeticOperator(Math.Abs)

        type CviOperator() =
            inherit NotImplementedArithmeticOperator()

        type FloorOperator() =
            inherit DirectArithmeticOperator(Math.Floor)

        type ModOperator() =
            inherit ParamArithmeticOperator(fun value moder ->
                Double.toInt_Safe value % Double.toInt_Safe moder
                |> float
            )

        type SinOperator() =
            inherit DirectArithmeticOperator(Math.Sin)

        type AddOperator() =
             inherit ParamArithmeticOperator(fun value addingValue ->
                 addingValue + value
             )


        type CvrOperator() =
            inherit NotImplementedArithmeticOperator()

        type IdivOperator() =
            inherit ParamArithmeticOperator(fun value divider ->
                value / divider
                |> floor
            )

        type MulOperator() =
             inherit ParamArithmeticOperator(fun value multiple ->
                 multiple * value
             )

        type SqrtOperator() =
            inherit DirectArithmeticOperator(Math.Sqrt)


        type AtanOperator() =
             inherit DirectArithmeticOperator(Math.Atan)


        type DivOperator() =
             inherit ParamArithmeticOperator(fun value divider ->
                 value / divider
             )

        type LnOperator() =
             inherit DirectArithmeticOperator(Math.Log)

        type NegOperator() =
             inherit DirectArithmeticOperator(fun m -> -m)

        type SubOperator() =
             inherit ParamArithmeticOperator(fun value suber ->
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
                nameof(ModOperator) => (ModOperator >> fun r -> r :> IOperator)    
                nameof(AddOperator) => (AddOperator >> fun r -> r :> IOperator)
                nameof(MulOperator) => (MulOperator >> fun r -> r :> IOperator)
                nameof(DivOperator) => (DivOperator >> fun r -> r :> IOperator)
                nameof(IdivOperator) => (IdivOperator >> fun r -> r :> IOperator)
                nameof(SubOperator) => (SubOperator >> fun r -> r :> IOperator)
            ]

        let oneParamOperators = 
            [
                //nameof(ModOperator) => (PostScriptValue.asDouble >> ModOperator >> fun r -> r :> IOperator)    
                //nameof(AddOperator) => (PostScriptValue.asDouble >> AddOperator >> fun r -> r :> IOperator)
                //nameof(MulOperator) => (PostScriptValue.asDouble >> MulOperator >> fun r -> r :> IOperator)
                //nameof(DivOperator) => (PostScriptValue.asDouble >> DivOperator >> fun r -> r :> IOperator)
                //nameof(IdivOperator) => (PostScriptValue.asDouble >> IdivOperator >> fun r -> r :> IOperator)
                //nameof(SubOperator) => (PostScriptValue.asDouble >> SubOperator >> fun r -> r :> IOperator)
            ]

        let twoParamsOperators = 
            [
                
            ]

    
    //[<RequireQualifiedAccess>]
    //type OperatorUnion =
    //    | Direct   of (unit -> IOperator)
    //    | OneParam of (PostScriptValue -> IOperator)
    //    | TwoParams of (PostScriptValue * PostScriptValue -> IOperator)

    let private directOperators = 
        StackOperators.directOperators 
        @ ArithmeticOperators.directOperators 
        @ BitwiseOperators.directOperators
        |> List.mapFst(fun m -> m.TrimEnding("Operator", ignoreCase = true))
        //|> List.mapSnd OperatorUnion.Direct

    //let private oneParamOperators = 
    //    StackOperators.oneParamOperators
    //    @ ArithmeticOperators.oneParamOperators
    //    @ BitwiseOperators.oneParamOperators
    //    |> List.mapFst(fun m -> m.TrimEnding("Operator", ignoreCase = true))
    //    |> List.mapSnd OperatorUnion.OneParam

    //let private twoParamsOperators = 
    //    StackOperators.twoParamsOperators
    //    @ ArithmeticOperators.twoParamsOperators
    //    @ BitwiseOperators.twoParamsOperators
    //    |> List.mapFst(fun m -> m.TrimEnding("Operator", ignoreCase = true))
    //    |> List.mapSnd OperatorUnion.TwoParams

    let private allOperators = 
        let operators = 
            directOperators 
            |> List.mapFst StringIC

        let __checkNamesNotDuplicated =
            operators
            |> List.map fst
            |> List.ensureNotDuplicated

        Map.ofList operators
        
    let private invokeOperators (operators: IOperator list) stack = 
        (stack, operators)
        ||> List.fold(fun stack operator ->
            operator.InvokeStack(stack)
        )

    type IFOperator =
        { IfTrue: IOperator list }
    with 
        interface IOperator with 
            member x.InvokeStack(stack) =
                let b = stack.Pop().AsBoolean
                match b with 
                | true -> invokeOperators x.IfTrue stack
                | false -> stack


    type IfElseOperator =
        { IfTrue: IOperator list
          IfFalse: IOperator list }
    with 
        interface IOperator with 
            member x.InvokeStack(stack) =
                let b = stack.Pop().AsBoolean
                match b with 
                | true -> invokeOperators x.IfTrue stack
                | false -> invokeOperators x.IfFalse stack

    type PostScriptInterpreter private (operators: IOperator list) =
        let stack = Stack<PostScriptValue>()
        let newStack = invokeOperators operators stack
            

        member x.StackResult = newStack
            
        static member Create(inputValues: PostScriptValue list, inputString: string) =
            let IF = "if"
            let IF_ELSE = "ifelse"
            let inputString = 
                inputString.Trim().TrimStart('{').TrimEnd('}')
                |> Text.trimAllToOne
                |> fun m ->     
                    m.Replace("{", " { ").Replace("}", " } ")

            let operators = 
                let expressionStack = Stack()
                let stack = Stack()
                let rec loop accumOperands accumOperators (parts: StringIC list) =
                    match parts with 
                    | [] -> 
                        match accumOperands with  
                        | _ :: _ -> failwithf "exists left operands %A" accumOperands
                        | [] -> List.rev accumOperators

                    | part :: t ->
                        match part.Value with 
                        | "{" -> 
                            let accumOperands =
                                accumOperands
                                |> List.map PostScriptValueOperator
                                |> List.map(fun m -> m :> IOperator)

                            stack.Push((accumOperands @ accumOperators))
                            loop [] [] t

                        | "}" ->
                            let expression = 
                                let accumOperands =
                                    accumOperands
                                    |> List.map PostScriptValueOperator
                                    |> List.map(fun m -> m :> IOperator)

                                (accumOperands @ accumOperators)

                            expressionStack.Push(expression)
                            loop [] (stack.Pop()) t

                        | String.EqualIC IF ->
                            match expressionStack.Count with 
                            | 1 ->
                                failwithf ""
                            | count -> failwithf "Expression stack Length should be 1 for if but here is %d" count
                        | String.EqualIC IF_ELSE ->
                            match expressionStack.Count with 
                            | 2 -> ()
                            | count -> failwithf "Expression stack Length should be 2 for ifelse but here is %d" count
                            
                            match accumOperands with 
                            | [] -> ()
                            | _ :: _ -> failwithf "accumOperands %A for ifelse should be empty" accumOperands

                            let if_False = expressionStack.Pop()
                            let if_True = expressionStack.Pop()
                            let operator = 
                                { IfTrue = if_True 
                                  IfFalse = if_False }
                            loop [] (operator :: accumOperators) t

                        | _ ->
                            match allOperators.TryFind part with 
                            | Some operator -> 
                                let accumOperands =
                                    accumOperands
                                    |> List.map PostScriptValueOperator
                                    |> List.map(fun m -> m :> IOperator)
                                loop [] (operator() :: accumOperands @ accumOperators) t

                            | None -> 
                                loop (PostScriptValue.String part.Value :: accumOperands) accumOperators t

                let parts = 
                    inputString.SplitAsListAndTrim(" ")
                    |> List.map StringIC

                loop (List.rev inputValues) [] parts

            
            PostScriptInterpreter(operators)

