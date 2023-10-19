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
<<<<<<< HEAD
                | '-' ->       
                    if pos = 0 || s.[pos - 1] = '(' || not(s.[pos - 1] = ')' || isDigit s.[pos - 1]) (*|| s.[pos - 1] = '-' *)(*&& isDigit s.[pos - 1]*) then
                        // Token is a unary minus
                        aux (pos + 1) (acc @ [ UNARY_MINUS ])
                    else
                        // Token is a binary minus
                        aux (pos + 1) (acc @ [ MINUS ])

=======
                | '-' when pos = 0 || [LPAREN; PLUS; MINUS; TIMES; DIVIDE; REMAINDER; POWER] |> List.contains (List.last acc) ->
                    aux (pos + 1) (acc @ [ UNARY_MINUS ])
                | '-' -> aux (pos + 1) (acc @ [ MINUS ])
>>>>>>> d11eeed9dcf584b4428a775c88e463faa7dafedf
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

                    (*aux endPos (acc @ [ NUMBER number ])*)
                | _ -> failwith "Invalid character encountered"

        aux 0 []
