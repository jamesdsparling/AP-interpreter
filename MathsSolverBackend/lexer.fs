// lexer.fs
namespace MathsSolverBackend

module Lexer =

    type Token =
        | INTEGER of int
        | FLOAT of float
        | PLUS
        | MINUS
        | UNARY_MINUS  // Unary negation
        | TIMES
        | DIVIDE
        | REMAINDER
        | POWER
        | LPAREN
        | RPAREN
        | EOF
        | SIN   // Trigonometric functions
        | COS
        | TAN

    let str2lst s = [for c in s -> c]
    let lexError = System.Exception("Lexer error")

    let isDigit c = '0' <= c && c <= '9'
    let isBlank c = System.Char.IsWhiteSpace(c)

    let isDigitOrDot c = isDigit c || c = '.'

    let extractNumber (input: char list) =
        let rec extractAccum input acc =
            match input with
            | head :: tail when isDigitOrDot head -> extractAccum tail (head :: acc)
            | _ -> (List.rev acc, input)
        extractAccum input []

    let lexer (input: string) =
        let rec scan input =
            match input with
            | [] -> [EOF]
            | '+'::tail -> PLUS :: scan tail
            | '-'::tail -> MINUS :: scan tail
            | '*'::tail -> TIMES :: scan tail
            | '/'::tail -> DIVIDE :: scan tail
            | '^'::tail -> POWER :: scan tail
            | '('::tail -> LPAREN :: scan tail
            | ')'::tail -> RPAREN :: scan tail
            | 's'::'i'::'n'::tail -> SIN :: scan tail
            | 'c'::'o'::'s'::tail -> COS :: scan tail
            | 't'::'a'::'n'::tail -> TAN :: scan tail
            | c :: tail when isBlank c -> scan tail
            | c :: tail when isDigit c -> 
                let (numList, rest) = extractNumber (c :: tail)
                let numStr = new string (Array.ofList numList)
                if numStr.Contains(".") then 
                    FLOAT (float numStr) :: scan rest
                else
                    INTEGER (int numStr) :: scan rest
                
            | _ -> raise lexError

        scan (str2lst input)

