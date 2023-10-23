// interpreter.fs
// Evaluates mathmatical expressions

namespace MathsSolverBackend

module Interpreter =

    open System
    open Lexer
    open ShuntingYard

    type AngleMode = Degrees | Radians
    let toRadians = Math.PI / 180.0

    // Get the first element from a stack
    let pop stack =
        match stack with
        | a :: tail -> (a, tail)
        | _ -> failwith "Not enough values on stack (pop1)"

    // Get the first 2 elements from a stack
    let pop2 stack = 
        match stack with
        | a :: b :: tail -> (a, b, tail)
        | _ -> failwith "Not enough values on stack (pop2)"

    // Evaluate result of expression
    let evaluatePostfix tokens mode =
        let mutable stack = []

        let convertAngle mode value =
            match mode with
            | Degrees -> value * toRadians
            | Radians -> value


        for token in tokens do
            match token with
            | NUMBER n -> stack <- n :: stack
            | PLUS ->
                let a, b, tail = pop2 stack
                stack <- (a + b) :: tail
            | MINUS ->
                let a, b, tail = pop2 stack
                stack <- (b - a) :: tail
            | UNARY_MINUS ->
                let a, tail = pop stack
                stack <- (-a) :: tail
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
            | SIN ->
                let a, tail = pop stack
                let converted = convertAngle mode a
                stack <- Math.Round(sin(converted), 10) :: tail
            | COS ->
                let a, tail = pop stack
                let converted = convertAngle mode a
                stack <- Math.Round(cos(converted), 10) :: tail
            | TAN ->
                let a, tail = pop stack
                let converted = convertAngle mode a
                stack <- Math.Round(tan(converted), 10) :: tail
            | _ -> ()

        List.head stack

    // Interpret input string
    let interpret input mode =
        let tokens = tokenize input
        let postfix = infixToPostfix tokens
        evaluatePostfix postfix mode
