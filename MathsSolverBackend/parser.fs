// parser.fs
// Transforms sequence of tokens into AST (Abstract Syntax Tree)

namespace MathsSolverBackend

    module Parser =
    
        open Lexer

        type Expression =
            | Plus of Expression * Expression
            | Times of Expression * Expression
            | Number of float

        // Convert tokens -> syntax tree
        let parse (tokens: Token list) =
            let mutable pos = 0

            let currentToken () = tokens.[pos]
            let eat token =
                if tokens.[pos] = token then pos <- pos + 1
                else failwith ("Expected " + token.ToString())
    
            // Parse grammar
            let rec factor () =
                match currentToken () with
                | NUMBER n -> 
                    eat (NUMBER n)
                    Number n
                | LPAREN -> 
                    eat LPAREN
                    let value = expr ()
                    eat RPAREN
                    value
                | _ -> failwith "Expected number or ("
    
            and term () =
                let left = factor ()
                term' left

            and term' left =
                match currentToken () with
                | TIMES -> 
                    eat TIMES
                    let right = factor ()
                    term' (Times (left, right))
                | _ -> left
    
            and expr () =
                let left = term ()
                expr' left

            and expr' left =
                match currentToken () with
                | PLUS -> 
                    eat PLUS
                    let right = term ()
                    expr' (Plus (left, right))
                | _ -> left
    
            expr ()

