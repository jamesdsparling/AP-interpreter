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

    let numberToFloat (a: Number) =
        match a with
        | Int x -> float x
        | Float x -> x

    // Define a symbol table (variableName -> variableValue)
    let mutable symbolTable = Map.empty<string, Number> 
    type SymbolData = {
        Key: string
        Value: Number
        DisplayValue: string
        Type: string
    }

    let symbolList = 
        Map.toList symbolTable |> List.map (fun (k,v) ->
            let displayValue, valueType = 
                match v with
                | Int i -> i.ToString(), "Int"
                | Float f -> f.ToString(), "Float"
            { Key = k; Value = v; DisplayValue = displayValue; Type = valueType }
        )
    type SymbolViewModel() = 
        member val Symbols = ObservableCollection<SymbolData>(symbolList) with get, set
        member this.UpdateSymbols() = 
            this.Symbols.Clear()
            let symbolList = 
                Map.toList symbolTable |> List.map (fun (k,v) ->
                    let displayValue, valueType = 
                        match v with
                        | Int i -> i.ToString(), "Int"
                        | Float f -> f.ToString(), "Float"
                    { Key = k; Value = v; DisplayValue = displayValue; Type = valueType }
                )
            for symbol in symbolList do
                this.Symbols.Add(symbol)


    // Define a symbol table (variableName -> variableValue)
    type SymbolData = { Key: string; Value: float }
    //let mutable symbolTable = Map.empty<string, float>
    let symbolList = Map.toList initialSymbolTable |> List.map (fun (k,v) -> {Key = k; Value = v})
    type SymbolViewModel() = 
        member val Symbols = ObservableCollection<SymbolData>(symbolList) with get, set
        member this.UpdateSymbols() = 
            this.Symbols.Clear()
            let symbolList = Map.toList initialSymbolTable |> List.map (fun (k,v) -> {Key = k; Value = v})
            for symbol in symbolList do
                this.Symbols.Add(symbol)


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
                let (tLst, tval) = E tail
                match tval with
                | Int x -> 
                    // Update the symbol table with the variable assignment
                    symbolTable <- Map.add vName tval symbolTable
                    (tLst, tval)
                | Float x -> 
                    let itval = Int(int x)
                    // Update the symbol table with the variable assignment
                    symbolTable <- Map.add vName itval symbolTable
                    (tLst, itval)

            | TYPEFLOAT :: VARIABLE vName :: EQUATION :: tail ->
                let (tLst, tval) = E tail
                match tval with
                | Float x -> 
                    // Update the symbol table with the variable assignment
                    symbolTable <- Map.add vName tval symbolTable
                    (tLst, tval)
                | Int x -> 
                    // Update the symbol table with the variable assignment
                    let ftval = Float(float x)
                    symbolTable <- Map.add vName ftval symbolTable
                    (tLst, ftval)

            | FORLOOP :: INTEGER value :: LESSTHAN :: VARIABLE vName :: LESSTHAN :: INTEGER value2 :: tail ->
                let (tLst, tval) = E tail
                (tLst, tval)
            | _ -> (E tList)
        VA tList

    let rec controlFlow tList =
        let rec CF tList =
            match tList with
            | FORLOOP :: INTEGER value :: tail ->
                match tail with
                    | LESSTHAN :: VARIABLE vName :: tail ->
                        match tail with
                            | LESSTHAN :: INTEGER value2 :: tail ->
                                (tail, vName + " is greater than " + value.ToString() + "; " + vName + " is less than " + value2.ToString())
                            | LESSTHANOREQUAL :: INTEGER value2 :: tail ->
                                (tail, vName + " is greater than " + value.ToString() + "; " + vName + " is less than or equal to " + value2.ToString())
                            | _ -> raise parseError
                    | LESSTHANOREQUAL :: VARIABLE vName :: tail ->
                        match tail with
                            | LESSTHAN :: INTEGER value2 :: tail ->
                                (tail, vName + " is greater than or equal to " + value.ToString() + "; " + vName + " is less than " + value2.ToString())
                            | LESSTHANOREQUAL :: INTEGER value2 :: tail ->
                                (tail, vName + " is greater than or equal to  " + value.ToString() + "; " + vName + " is less than or equal to " + value2.ToString())
                            | _ -> raise parseError
                    | _ -> raise parseError
                | _ -> raise parseError
        CF tList


    let interpret input mode =
        let tokens = lexer input
        let _, result = evaluateExpr tokens mode
        result

    let interpretControlFlow input =
        let tokens = lexer input
        let _, result = controlFlow tokens
        result

