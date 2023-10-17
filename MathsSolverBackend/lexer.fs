// lexer.fs
// Breaks down input into a sequence of tokens

namespace MathsSolverBackend

module Lexer =

    type Token =
        | NUMBER of float
        | PLUS
        | MINUS
        | TIMES
        | DIVIDE
        | REMAINDER
        | POWER
        | LPAREN
        | RPAREN
        | EOF

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
                        if pos < s.Length && isDigit s.[pos] then
                            findEnd (pos + 1)
                        else
                            pos

                    let endPos = findEnd (pos + 1)
                    let number = float (s.Substring(start, endPos - start))
                    aux endPos (acc @ [ NUMBER number ])
                | _ -> failwith "Invalid character encountered"

        aux 0 []
