// interpreter.fs
// Evaluates mathmatical expressions

namespace MathSolverBackend

module Interpreter =

    open Lexer
    open ShuntingYard

    // Get the first 2 elements from a stack
    let pop2 stack = 
        match stack with
        | a :: b :: tail -> (a, b, tail)
        | _ -> failwith "Not enough values on stack"

    // Evaluate result of expression
    let evaluatePostfix tokens =
        let mutable stack = []

        for token in tokens do
            match token with
            | NUMBER n -> stack <- n :: stack
            | PLUS ->
                let a, b, tail = pop2 stack
                stack <- (a + b) :: tail
            | MINUS ->
                let a, b, tail = pop2 stack
                stack <- (b - a) :: tail
            | TIMES ->
                let a, b, tail = pop2 stack
                stack <- (a * b) :: tail
            | DIVIDE ->
                let a, b, tail = pop2 stack
                stack <- (b / a) :: tail
            | REMAINDER ->
                let a, b, tail = pop2 stack
                stack <- (b % a) :: tail
            | POWER ->
                let a, b, tail = pop2 stack
                stack <- (b ** a) :: tail
            | _ -> ()

        List.head stack

    // Interpret input string
    let interpret input =
        let tokens = tokenize input
        let postfix = infixToPostfix tokens
        evaluatePostfix postfix

    // Input values
    let test () =
        let expressions = [ "3 + 5"; "3 + 5 * 2"; "(3 + 5) * 2"; "10"; "2 * 3 + 4 * 5" ]
        expressions |> List.iter (fun expr -> printfn "%s = %f" expr (interpret expr))

    test ()
