// shunting_yard.fs
// Uses shunting-yard algorithm to convert infix notation to postfix

namespace MathSolverBackend
    module ShuntingYard = 

        open Lexer

        let precedence token =
            match token with
            | PLUS -> 1
            | TIMES -> 2
            | _ -> 0

        let rec processTokens tokens output ops =
            match tokens with
            | [] | EOF :: _ -> List.rev output @ ops
            | (NUMBER n) :: rest -> 
                processTokens rest (NUMBER n :: output) ops
            | LPAREN :: rest ->
                processTokens rest output (LPAREN :: ops)
            | RPAREN :: rest ->
                let (beforeParen, afterParen) = splitAtParen ops
                processTokens rest (List.rev beforeParen @ output) afterParen
            | token :: rest when [PLUS; TIMES] |> List.contains token ->
                let (lowerPrecedence, sameOrHigherPrecedence) = List.partition (fun op -> precedence op < precedence token) ops
                processTokens rest (List.rev sameOrHigherPrecedence @ output) (token :: lowerPrecedence) 
            | token :: _ -> 
                failwithf "Unexpected token: %A" token

        and splitAtParen ops =
            let rec aux acc = function
                | [] -> failwith "Mismatched parenthesis"
                | LPAREN :: rest -> (List.rev acc, rest)
                | op :: rest -> aux (op :: acc) rest
            aux [] ops

        let infixToPostfix tokens =
            processTokens tokens [] []
