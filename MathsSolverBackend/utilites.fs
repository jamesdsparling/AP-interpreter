namespace MathsSolverBackend

module Utilities =

    open Lexer
    
    // Function to check if a token is an operator
    let isOperator token =
        match token with
        | PLUS | MINUS | TIMES | DIVIDE | REMAINDER | POWER -> true
        | _ -> false