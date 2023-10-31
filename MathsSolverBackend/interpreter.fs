// interpreter.fs
namespace MathsSolverBackend

module Interpreter =

    open Lexer

    let parseError = System.Exception("Parser error")

    type AngleMode = Degrees | Radians
    let toRadians = System.Math.PI / 180.0

    let rec evaluateExpr tList mode =
        let convertAngle mode value =
            match mode with
            | Degrees -> value * toRadians
            | Radians -> value

        // Expression - addition & subtraction
        let rec E tList = (T >> Eopt) tList 
        and Eopt (tList, value) = 
            match tList with
            | PLUS :: tail ->
                let (tLst, tval) = T tail
                Eopt (tLst, value + tval)
            | MINUS :: tail ->
                let (tLst, tval) = T tail
                Eopt (tLst, value - tval)
            | _ -> (tList, value)

        // Term - multiplication, division & remainder
        and T tList = (P >> Topt) tList
        and Topt (tList, value) =
            match tList with
            | TIMES :: tail ->
                let (tLst, tval) = P tail
                Topt (tLst, value * tval)
            | DIVIDE :: tail ->
                let (tLst, tval) = P tail
                Topt (tLst, value / tval)
            | REMAINDER :: tail ->
                let (tLst, tval) = P tail
                Topt (tLst, value % tval) 
            | _ -> (tList, value)

        // Power - exponent operations
        and P tList = (NR >> Popt) tList
        and Popt (tList, value) =
            match tList with
            | POWER :: tail ->
                let (tLst, tval) = NR tail
                Popt (tLst, System.Math.Pow(value, tval))
            | _ -> (tList, value)

        // Numeric/Parenthesized - numbers, unary operations & functions
        and NR tList =
            match tList with 
            | INTEGER value :: tail -> (tail, float value)
            | FLOAT value :: tail -> (tail, value)
            | MINUS :: tail ->
                let (tLst, tval) = NR tail
                (tLst, -tval)
            | SIN :: tail ->
                let (tLst, tval) = NR tail
                (tLst, System.Math.Sin(convertAngle mode tval))
            | COS :: tail ->
                let (tLst, tval) = NR tail
                (tLst, System.Math.Cos(convertAngle mode tval))
            | TAN :: tail ->
                let (tLst, tval) = NR tail
                (tLst, System.Math.Tan(convertAngle mode tval))
            | LPAREN :: tail ->
                let (tLst, tval) = E tail
                match tLst with 
                | RPAREN :: tail -> (tail, tval)
                | _ -> raise parseError
            | _ -> raise parseError
        E tList

    let interpret input mode =
        let tokens = lexer input
        let _, result = evaluateExpr tokens mode
        result

