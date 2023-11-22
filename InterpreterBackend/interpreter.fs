// interpreter.fs
namespace InterpreterBackend
open System.Collections.ObjectModel

module Interpreter =

    open Lexer

    let parseError = System.Exception("Parser error")

    type AngleMode = Degrees | Radians
    let toRadians = System.Math.PI / 180.0

    type Number =
    | Int of int
    | Float of float

    let addNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (x + y)
        | (Float x, Float y) -> Float (x + y)
        | (Int x, Float y) -> Float ((float x) + y)
        | (Float x, Int y) -> Float (x + (float y))

    let subtractNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (x - y)
        | (Float x, Float y) -> Float (x - y)
        | (Int x, Float y) -> Float ((float x) - y)
        | (Float x, Int y) -> Float (x - (float y))

    let multiplyNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (x * y)
        | (Float x, Float y) -> Float (x * y)
        | (Int x, Float y) -> Float ((float x) * y)
        | (Float x, Int y) -> Float (x * (float y))

    let divideNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (x / y)
        | (Float x, Float y) -> Float (x / y)
        | (Int x, Float y) -> Float ((float x) / y)
        | (Float x, Int y) -> Float (x / (float y))

    let moduloNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (x % y)
        | (Float x, Float y) -> Float (x % y)
        | (Int x, Float y) -> Float ((float x) % y)
        | (Float x, Int y) -> Float (x % (float y))

    let powerNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (System.Convert.ToInt32(System.Math.Pow(float x, float y)))
        | (Float x, Float y) -> Float (System.Math.Pow(x, y))
        | (Int x, Float y) -> Float (System.Math.Pow((float x), y))
        | (Float x, Int y) -> Float (System.Math.Pow(x, (float y)))

    let negateNumber a =
        match a with
        | Int x -> Int (-x)
        | Float x -> Float (-x)

    let sinNumber a =
        match a with
        | Int x -> Float(System.Math.Sin(x))
        | Float x -> Float(System.Math.Sin(x))

    let cosNumber a =
        match a with
        | Int x -> Float(System.Math.Cos(x))
        | Float x -> Float(System.Math.Cos(x))

    let tanNumber a =
        match a with
        | Int x -> Float(System.Math.Tan(x))
        | Float x -> Float(System.Math.Tan(x))

    let initialSymbolTable =
        Map.ofList [
            "x", 10.0;   // Example variable "x" with an initial value
            "y", 20.0;   // Another example variable "y" with an initial value
        ]

    // Define a symbol table (variableName -> variableValue)
    let mutable symbolTable = Map.empty<string, Number> 

    // Function to look up variable values
    let lookupVariable variableName =
        match Map.tryFind variableName symbolTable with
        | Some value -> value
        | None -> raise parseError

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
                Eopt (tLst, addNumbers value tval)
            | MINUS :: tail ->
                let (tLst, tval) = T tail
                Eopt (tLst, subtractNumbers value tval)
            | _ -> (tList, value)

        // Term - multiplication, division & remainder
        and T tList = (P >> Topt) tList
        and Topt (tList, value) =
            match tList with
            | TIMES :: tail ->
                let (tLst, tval) = P tail
                Topt (tLst, multiplyNumbers value tval)
            | DIVIDE :: tail ->
                let (tLst, tval) = P tail
                Topt (tLst, divideNumbers value tval)
            | REMAINDER :: tail ->
                let (tLst, tval) = P tail
                Topt (tLst, moduloNumbers value tval) 
            | _ -> (tList, value)

        // Power - exponent operations
        and P tList = (NR >> Popt) tList
        and Popt (tList, value) =
            match tList with
            | POWER :: tail ->
                let (tLst, tval) = NR tail
                Popt (tLst, powerNumbers value tval)
            | _ -> (tList, value)

        // Numeric/Parenthesized - numbers, unary operations & functions
        and NR tList =

            match tList with
            | FLOAT value :: LPAREN :: tail ->
                let newTail = FLOAT value :: TIMES :: LPAREN :: tail
                let (tLst, tval) = E newTail
                (tLst, tval)
            | FLOAT value :: VARIABLE vName :: tail ->
                let newTail = FLOAT value :: TIMES :: VARIABLE vName :: tail
                let (tLst, tval) = E newTail
                (tLst, tval)
            | INTEGER value :: LPAREN :: tail ->
                let newTail = INTEGER value :: TIMES :: LPAREN :: tail
                let (tLst, tval) = E newTail
                (tLst, tval)
            | INTEGER value :: VARIABLE vName :: tail ->
                let newTail = INTEGER value :: TIMES :: VARIABLE vName :: tail
                let (tLst, tval) = E newTail
                (tLst, tval)
            | VARIABLE vName :: LPAREN :: tail ->
                let newTail = VARIABLE vName :: TIMES :: LPAREN :: tail
                let (tLst, tval) = E newTail
                (tLst, tval)

            | INTEGER value :: tail -> (tail, Int(value))
            | FLOAT value :: tail -> (tail, Float(value))
            | VARIABLE vName :: tail ->
                let variableValue = lookupVariable vName
                (tail, variableValue)
            | MINUS :: tail ->
                let (tLst, tval) = NR tail
                (tLst, negateNumber tval)
            | SIN :: tail ->
                let (tLst, tval) = NR tail
                (tLst, sinNumber tval)
            | COS :: tail ->
                let (tLst, tval) = NR tail
                (tLst, cosNumber tval)
            | TAN :: tail ->
                let (tLst, tval) = NR tail
                (tLst, tanNumber tval)
            | LPAREN :: tail ->
                let (tLst, tval) = E tail
                match tLst with 
                | RPAREN :: tail -> (tail, tval)
                | _ -> raise parseError
            | _ -> raise parseError

        let VA tList =
            match tList with
            | TYPEINT :: VARIABLE vName :: EQUATION :: tail ->
                let (|AInt|_|) =
                    function
                    | Int x -> Some x
                    | _ -> None
                
                let (tLst, tval) = E tail
                match tval with
                | AInt x -> 
                    // Update the symbol table with the variable assignment
                    symbolTable <- Map.add vName tval symbolTable
                    (tLst, tval)

                
            | TYPEFLOAT :: VARIABLE vName :: EQUATION :: tail ->
                let (|AFloat|_|) =
                    function
                    | Float x -> Some x
                    | _ -> None
                
                let (tLst, tval) = E tail
                match tval with
                | AFloat x -> 
                    // Update the symbol table with the variable assignment
                    symbolTable <- Map.add vName tval symbolTable
                    (tLst, tval)
            | _ -> (E tList)
        VA tList


    let interpret input mode =
        let tokens = lexer input
        let _, result = evaluateExpr tokens mode
        result

