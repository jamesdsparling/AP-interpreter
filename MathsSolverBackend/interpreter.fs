// interpreter.fs
// Evaluates mathmatical expressions

namespace MathSolverBackend

module Interpreter =

    open Lexer
    open ShuntingYard

    // Evaluate result of expression
    let evaluatePostfix tokens =
        let mutable stack = []

        for token in tokens do
            match token with
            | NUMBER n -> stack <- n :: stack
            | PLUS ->
                let a = List.head stack
                stack <- List.tail stack
                let b = List.head stack
                stack <- List.tail stack
                stack <- (a + b) :: stack
            | TIMES ->
                let a = List.head stack
                stack <- List.tail stack
                let b = List.head stack
                stack <- List.tail stack
                stack <- (a * b) :: stack
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
