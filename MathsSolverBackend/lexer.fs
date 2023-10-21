// lexer.fs
// Breaks down input into a sequence of tokens

namespace MathsSolverBackend

module Lexer =

    type Token =
        | NUMBER of float
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

    // Convert input (string) -> tokens
    let tokenize (s: string) =
        let isDigit c = '0' <= c && c <= '9'

        let rec aux pos acc =
            if pos = s.Length then
                (acc @ [ EOF ]) // End of string
            else
                match s.[pos] with
                | c when System.Char.IsWhiteSpace(c) -> aux (pos + 1) acc // Whitespace is ignored
                | '+' -> aux (pos + 1) (acc @ [ PLUS ])
                | '-' when pos = 0 || [LPAREN; PLUS; MINUS; TIMES; DIVIDE; REMAINDER; POWER] |> List.contains (List.last acc) ->
                    aux (pos + 1) (acc @ [ UNARY_MINUS ])
                | '-' -> aux (pos + 1) (acc @ [ MINUS ])
                | '*' -> aux (pos + 1) (acc @ [ TIMES ])
                | '/' -> aux (pos + 1) (acc @ [ DIVIDE ])
                | '%' -> aux (pos + 1) (acc @ [ REMAINDER ])
                | '^' -> aux (pos + 1) (acc @ [ POWER ])
                | '(' -> aux (pos + 1) (acc @ [ LPAREN ])
                | ')' -> aux (pos + 1) (acc @ [ RPAREN ])
                | c when isDigit c -> // Token is a number
                    let start = pos

                    let rec findEnd pos =
                        if pos < s.Length && (isDigit s.[pos] || s.[pos] = '.') then
                            findEnd (pos + 1)
                        else
                            pos

                    let endPos = findEnd (pos + 1)
                    let number = float (s.Substring(start, endPos - start))

                    let newAcc = acc @ [ NUMBER number ]
                    // Check if the next token should be implied as TIMES
                    let nextPos = pos + (endPos - start)
                    if nextPos < s.Length && (s.[nextPos] = '(' || isDigit s.[nextPos]) then
                        aux nextPos (newAcc  @ [ TIMES ])
                    else
                        aux endPos newAcc 

                | 's' -> aux (pos + 1) (acc @ [ SIN ])
                | 'c' -> aux (pos + 1) (acc @ [ COS ])
                | 't' -> aux (pos + 1) (acc @ [ TAN ])

                | _ -> failwith "Invalid character encountered"

        aux 0 []
