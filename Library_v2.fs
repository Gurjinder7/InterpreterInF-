namespace interpreterLibrary

module Interpreter =
    //let hello name =
        //printfn "Hello %s" name


    // Simple Interpreter in F#
    // Author: R.J. Lapeer 
    // Date: 23/10/2022
    // Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

    open System

    exception MyError of int

    type terminal =
        | Add
        | Sub
        | Mul
        | Div
        | Lpar
        | Rpar
        | Num of float
        | Pow
        | Mod

    let str2lst s = [ for c in s -> c ]
    let isblank c = System.Char.IsWhiteSpace c
    let isdigit c = System.Char.IsDigit c
    let lexError = System.Exception("Lexer error")
    let intVal (c: char) = (int) ((int) c - (int) '0')
    let parseError = System.Exception("Parser error")

    let rec scFloat (iStr, iVal, isDecimal, multiplier) =
        match iStr with
        | c :: tail when isdigit c ->
            if isDecimal then
                let decimalVal = iVal + (float (intVal c) * multiplier)
                scFloat (tail, decimalVal, isDecimal, multiplier / 10.0)
            else
                scFloat (tail, 10.0 * iVal + float (intVal c), isDecimal, multiplier)
        | '.' :: tail when not isDecimal -> scFloat (tail, iVal, true, 0.1)
        | _ -> (iStr, iVal)

    let lexer input =
        let rec scan input =
            match input with
            | [] -> []
            | '+' :: tail -> Add :: scan tail
            | '-' :: tail -> Sub :: scan tail
            | '*' :: tail -> Mul :: scan tail
            | '/' :: tail -> Div :: scan tail
            | '%' :: tail -> Mod :: scan tail
            | '^' :: tail -> Pow :: scan tail
            | '(' :: tail -> Lpar :: scan tail
            | ')' :: tail -> Rpar :: scan tail
            | c :: tail when isblank c -> scan tail
            | c :: tail when isdigit c ->
                let (iStr, iVal) = scFloat (tail, float (intVal c), false, 1.0)
                Num iVal :: scan iStr
            | _ -> raise (MyError(9876))

        scan (str2lst input)

    let getInputString () : string =
        // Console.Write("Enter an expression: ")
        Console.ReadLine()

    // Grammar in BNF:
    // <E>        ::= <T> <Eopt>
    // <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
    // <T>        ::= <NR> <Topt> | <NR>
    // <Topt>     ::= "*" <NR> <Topt> | "/" <NR> <Topt> | "%" <NR> <Topt> | <empty>
    // <P>        ::= <NR> <Popt> | <NR>
    // <Popt>     ::= "^" <NR> <Popt> | <empty>
    // <F>        ::= "-" <NR> | "^" <NR> | <NR>
    // <NR>       ::= "Num" <value> | "(" <E> ")"

    let parser tList =
        let rec E tList = (T >> Eopt) tList // >> is forward function composition operator: let inline (>>) f g x = g(f x)

        and Eopt tList =
            match tList with
            | Add :: tail -> (T >> Eopt) tail
            | Sub :: tail -> (T >> Eopt) tail
            | _ -> tList

        and T tList = (P >> Topt) tList

        and Topt tList =
            match tList with
            | Mul :: tail -> (P >> Topt) tail
            | Div :: tail -> (P >> Topt) tail
            | Mod :: tail -> (P >> Topt) tail
            | _ -> tList

        and P tList = (F >> Popt) tList

        and Popt tList =
            match tList with
            | Pow :: tail -> (F >> Popt) tail
            | _ -> tList

        and F tList =
            match tList with
            | Sub :: tail -> NR tail
            | _ -> NR tList

        and NR tList =
            match tList with
            | Num value :: tail -> tail
            | Lpar :: tail ->
                match E tail with
                | Rpar :: tail -> tail
                | _ -> raise (MyError(98765))
            | _ -> raise (MyError(98765))

        E tList

    let parseNeval tList =
        let rec E tList = (T >> Eopt) tList

        and Eopt (tList, value) =
            match tList with
            | Add :: tail ->
                let (tLst, tval) = T tail
                Eopt(tLst, value + tval)
            | Sub :: tail ->
                let (tLst, tval) = T tail
                Eopt(tLst, value - tval)
            | _ -> (tList, value)

        and T tList = (P >> Topt) tList

        and Topt (tList, value) =
            match tList with
            | Mul :: tail ->
                let (tLst, tval) = P tail
                Topt(tLst, value * tval)
            | Div :: tail ->
                let (tLst, tval) = P tail
                Topt(tLst, value / tval)
            | Mod :: tail ->
                let (tLst, tval) = P tail

                let modResult =
                    if value % tval < 0.0 then
                        (value % tval + tval)
                    else
                        (value % tval)

                Topt(tLst, modResult)
            | _ -> (tList, value)

        and P tList = (F >> Popt) tList

        and Popt (tList, value) =
            match tList with
            | Pow :: tail ->
                let (tLst, tval) = F tail
                Popt(tLst, value ** tval)
            | _ -> (tList, value)

        and F tList =
            match tList with
            | Sub :: tail ->
                let (tLst, tval) = NR tail
                (tLst, -tval)
            | _ -> NR tList

        and NR tList =
            match tList with
            | Num value :: tail -> (tail, value)
            | Lpar :: tail ->
                let (tLst, tval) = E tail

                match tLst with
                | Rpar :: tail -> (tail, tval)
                | _ -> raise (MyError(98765))
            | _ -> raise (MyError(98765))

        E tList

    let rec printTList (lst: list<terminal>) : list<string> =
        match lst with
        | head :: tail ->
            Console.Write("{0} ", head.ToString())
            printTList tail

        | [] ->
            Console.Write("EOL\n")
            []


    [<EntryPoint>]
    let main argv  =
        Console.WriteLine("Simple Interpreter")
        let input:string = String.concat " " argv
        try
        // Console.WriteLine("Simple Interpreter")
            //let input: string = getInputString ()
            let oList = lexer input
            // let sList = printTList oList
            // let pList = printTList (parser oList)
            // Console.WriteLine(pList)
            // Console.WriteLine(sList)
            let Out = parseNeval oList
            Console.WriteLine(System.Math.Round(snd Out, 3))
            let intVal = int (System.Math.Round(snd Out, 3))
            intVal
         with
            | :? System.DivideByZeroException -> 87654
            | MyError(i) -> i
            | _ -> reraise ()
            

