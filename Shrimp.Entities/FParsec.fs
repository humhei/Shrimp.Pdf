namespace Shrimp.Entities

module FParsec =
    open FParsec
    open Shrimp.Pdf.Types
    open Shrimp.Utils
    let email : Parser<_,unit> = 
        let prefix = 
            let isIdentifierFirstChar c = isLetter c
            let isIdentifierChar c = isLetter c || isDigit c || c = '_'
            many1Satisfy2 isIdentifierFirstChar isIdentifierChar
        let suffix = many (letter <|> digit) .>>. pstring ".com" 
        prefix .>>. pchar '@' .>>. suffix

