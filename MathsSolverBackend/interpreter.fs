// interpreter.fs
namespace MathsSolverBackend

module Interpreter =

    open Lexer

    let parseError = System.Exception("Parser error")

    type AngleMode = Degrees | Radians
    let toRadians = System.Math.PI / 180.0

    let rec evaluateExpr tokens mode =
        let convertAngle mode value =
            match mode with
            | Degrees -> value * toRadians
            | Radians -> value

        let rec E tList acc =
            match tList with
            | (INTEGER n) :: tail -> E tail (acc + float n)
            | (FLOAT n) :: tail -> E tail (acc + n)
            | PLUS :: tail -> 
                let (newTail, value) = T tail
                E newTail (acc + value)
            | MINUS :: tail -> 
                let (newTail, value) = T tail
                E newTail (acc - value)
            | _ -> (tList, acc)

        and T tList =
            match tList with
            | (INTEGER n) :: tail -> (tail, float n)
            | (FLOAT n) :: tail -> (tail, n)
            | SIN :: LPAREN :: tail -> 
                let (newTail, value) = E tail 0.0
                (match newTail with
                 | RPAREN :: tail -> (tail, System.Math.Sin(convertAngle mode value))
                 | _ -> raise parseError)
            | COS :: LPAREN :: tail -> 
                let (newTail, value) = E tail 0.0
                (match newTail with
                 | RPAREN :: tail -> (tail, System.Math.Cos(convertAngle mode value))
                 | _ -> raise parseError)
            | TAN :: LPAREN :: tail -> 
                let (newTail, value) = E tail 0.0
                (match newTail with
                 | RPAREN :: tail -> (tail, System.Math.Tan(convertAngle mode value))
                 | _ -> raise parseError)
            | _ -> raise parseError

        E tokens 0.0

    let interpret input mode =
        let tokens = lexer input
        let _, result = evaluateExpr tokens mode
        result
