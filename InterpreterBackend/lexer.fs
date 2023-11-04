// lexer.fs
namespace InterpreterBackend

module Lexer =

    type Token =
        | INTEGER of int
        | FLOAT of float
        | PLUS
        | MINUS
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
        | EQUATION              // Assignment operator
        | VARIABLE of string    // Variable assignment

    let lexError = System.Exception("Lexer error")

    let str2lst s = [for c in s -> c]
    let isBlank c = System.Char.IsWhiteSpace c
    let isDigit c = System.Char.IsDigit c
    let isChar c = System.Char.IsLetter c           // Checks if character is a letter
    let intVal (c:char) = (int)((int)c - (int)'0')

    let rec scInt (iStr, iVal) = 
        match iStr with
        | c::tail when isDigit c -> scInt(tail, 10*iVal+(intVal c))
        | _ -> (iStr, iVal)

    let rec scFloat fStr fVal factor =
        match fStr with
        | c::tail when isDigit c -> 
            let newFVal = fVal + ((float (intVal c)) / factor)
            scFloat tail newFVal (factor * 10.0)
        | _ -> (fStr, fVal)

    // For parsing variable identidiers
    let rec scChar(iStr, vName:string) =
        match iStr with
        | c::tail when isChar c -> scChar(tail, vName + c.ToString())
        | _ -> (iStr, vName)

    let lexer (input: string) =
        let rec scan input =
            match input with
            | [] -> [EOF]
            | '+' :: tail -> PLUS :: scan tail
            | '-' :: tail -> MINUS :: scan tail
            | '*' :: tail -> TIMES :: scan tail
            | '/' :: tail -> DIVIDE :: scan tail
            | '^' :: tail -> POWER :: scan tail
            | '(' :: tail -> LPAREN :: scan tail
            | ')' :: tail -> RPAREN :: scan tail
            | '=' :: tail -> EQUATION :: scan tail
            | 's' :: 'i'::'n'::tail -> SIN :: scan tail
            | 'c' :: 'o'::'s'::tail -> COS :: scan tail
            | 't' :: 'a'::'n'::tail -> TAN :: scan tail
            | c :: tail when isBlank c -> scan tail
            | c :: tail when isDigit c -> 
                let (iStr, iVal) = scInt(tail, intVal c)
                match iStr with
                | '.' :: dTail -> 
                    let (fStr, fVal) = scFloat dTail 0.0 10.0
                    FLOAT ((float iVal) + fVal) :: scan fStr
                | _ -> INTEGER iVal :: scan iStr            
            | c :: tail when isChar c -> 
                let (iStr, vName) = scChar(tail, c.ToString())
                VARIABLE vName :: scan iStr
            | _ -> raise lexError

        scan (str2lst input)


