// shunting_yard.fs
// Uses shunting-yard algorithm to convert infix notation to postfix

namespace MathsSolverBackend

module ShuntingYard =

    open Lexer

    let precedence token =
        match token with
        | PLUS | MINUS -> 1
        | TIMES | DIVIDE | REMAINDER -> 2
        | POWER -> 3
        | UNARY_MINUS -> 4
        | _ -> 0

    let rec processTokens tokens output ops =
        match tokens with
        | []
        | EOF :: _ -> List.rev output @ ops
        | (NUMBER n) :: tail -> processTokens tail (NUMBER n :: output) ops
        | LPAREN :: tail -> processTokens tail output (LPAREN :: ops)
        | RPAREN :: tail ->
            let (beforeParen, afterParen) = splitAtParen ops
            processTokens tail (List.rev beforeParen @ output) afterParen
        | token :: tail when [ PLUS; MINUS; TIMES; DIVIDE; REMAINDER; POWER; UNARY_MINUS ] |> List.contains token ->
            let (lowerPrecedence, sameOrHigherPrecedence) =
                List.partition (fun op -> precedence op < precedence token) ops

            processTokens tail (List.rev sameOrHigherPrecedence @ output) (token :: lowerPrecedence)
        | token :: _ -> failwithf "Unexpected token: %A" token

    and splitAtParen ops =
        let rec aux acc =
            function
            | [] -> failwith "Mismatched parenthesis"
            | LPAREN :: tail -> (List.rev acc, tail)
            | op :: tail -> aux (op :: acc) tail

        aux [] ops

    let infixToPostfix tokens = processTokens tokens [] []
