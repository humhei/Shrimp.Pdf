namespace Atrous.Entities
module XLParser =
    open ExcelProcess
    open FParsec
    open Atrous.Entities.Types
    open MatrixParsers
    open Atrous.Utils

    module Math =
        open Atrous.Extensions

        let isHalf float =
            ((float * 2.).ToString() |> (String.isInt32))

    open Math

    module Shoes =

        let eur = [19..46]

        let isEur i = i>=19 && i<46

        let isUK i = i>=3. && i<12. && isHalf i

        let isUS i = i>=5. && i<12.5 && isHalf i

        let isFraction i = i < 5

        let isManOfEur eur =
            eur > 38 && eur < 47

        let isWomanOfEur eur =
            eur > 34 && eur < 43

        let isManOfUK size =
            size > 5.5 && size < 12.
        
        let isWomanOfUK size =
            size > 2.5 && size < 9.5

    [<RequireQualifiedAccess>]
    module Sex =
        let ofEurs (eurs: int list) =
            if List.forall Shoes.isManOfEur eurs then Sex.Man  
            elif List.forall Shoes.isWomanOfEur eurs then Sex.Woman
            else failwith "Not implemnted"

        let ofUKS (uks: float list) =
            if List.forall Shoes.isManOfUK uks then Sex.Man  
            elif List.forall Shoes.isWomanOfUK uks then Sex.Woman
            else failwith "Not implemnted"

    [<RequireQualifiedAccess>]
    module FParsec =

        let art:Parser<string,unit> =
            let isIdentifierFirstChar c = isLetter c || isDigit c
            let isIdentifierChar c = isLetter c || isDigit c || c = '-'
            many1Satisfy2L isIdentifierFirstChar isIdentifierChar "art"

        let color : Parser<ColorEnum,unit> =
            ColorEnum.enums |> Seq.map pstringCI |> choice |>> ColorEnum.ofString
        
        let parentheses : Parser<char,unit> =
            ['('
             '（'
             ')'
             '）'
            ] |> anyOf

        let colon : Parser<char,unit> =
            [':'
             '：'
            ] |> anyOf 

    open Shoes

    [<RequireQualifiedAccess>]
    module SeqBySpace =

        let private isSizeGroup numbers =
            let p1=
                numbers |> List.forall isEur
            let p2 = numbers.Length > 1
            p1 && p2      

        let private isUKGroup numbers =
            let p1=
                numbers |> List.forall isUK
            let p2 = numbers.Length > 1
            p1 && p2      

        let private isFractionGroup numbers =
            let p1=
                numbers |> List.forall isFraction
            let p2 = numbers.Length > 1
            p1 && p2     

        let pintSepBySpace : Parser<int32 list,unit> = many1 (pchar ' ') |> sepEndBy1 pint32
        let pfloatSepBySpace : Parser<float list,unit> = many1 (pchar ' ') |> sepEndBy1 pfloat

        let pSize ms = 
            let parser = !^^ pintSepBySpace isSizeGroup
            parser ms 

        let pFraction ms = 
            let parser = !^^ pintSepBySpace isFractionGroup
            parser ms 

        let pUK ms = 
            let parser = !^^ pfloatSepBySpace isUKGroup
            parser ms 

    [<RequireQualifiedAccess>]
    module Single =
        open System
        open System.Globalization

        let pTotal =
            let sumLiteral =
                (pstring "合计" <|> pstring "总计") .>> ((skipString "：") <|> spaces)
            let sumCarton = (pint32 .>> (pstring "双" <|> pstring "") .>> pstring "/") .>>. (pint32 .>> pstring "箱")
            let p1 =
                !^= sumLiteral
                <==> !^ sumCarton
                |||> snd
            let p2 = sumLiteral >>. sumCarton |> (!^)
            u2 p1 p2

        let pColor =
            !^ FParsec.color

        let pEUR ms = 
            let parser = !^^ pint32 Shoes.isEur
            parser ms

        let pUK ms = 
            let parser = !^^ pfloat Shoes.isUK
            parser ms

        let pUS ms = 
            let parser = !^^ pfloat Shoes.isUS
            parser ms

        let pFraction ms = 
            let parser = !^^ pint32 Shoes.isFraction
            parser ms

        let pDate format  =
            mxPTextWith (fun s ->
                match format with 
                | "yyMMdd" -> 
                    DateTime.TryParseExact(s,format,CultureInfo.InvariantCulture,DateTimeStyles.None)
                    |> function 
                        | true,date -> 
                            printf "parsing date format %s successfully" s
                            Some date
                        | false,_ -> 
                            printf "Invalid date format %s" s
                            None 
                | _ -> failwithf "Invalid date format %s" s
            )


        let pColorWith language : MatrixParser<ColorEnum> =
            mxPTextWith (fun t ->
                ColorEnum.map.Values |> Seq.filter(fun s ->
                    t.Contains s
                )
                |> List.ofSeq
                |> function 
                    | [item] -> 
                        match language with 
                        | Language.Chinese ->
                            ColorEnum.fromZH item 
                        | _ ->
                            ColorEnum.ofString item
                        |> Some

                    | _ -> None
            )