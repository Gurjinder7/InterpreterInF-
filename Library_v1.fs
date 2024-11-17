namespace interpreterLibrary

module Interpreter =
    //let hello name =
        //printfn "Hello %s" name


    // Simple Interpreter in F#
    // Author: R.J. Lapeer 
    // Date: 23/10/2022
    // Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

    open System

    type intAndFloat = 
        | Num of int
        | Num1 of float

    exception MyError of int

    type terminal = 
        Add | Sub | Mul | Div | Pow| Mod | Lpar | Rpar | Num of int | Num1 of float

    let str2lst s = [for c in s -> c]
    let isblank c = System.Char.IsWhiteSpace c
    let isdigit c = System.Char.IsDigit c
    let lexError = System.Exception("Lexer error")
    let intVal (c:char) = (int)((int)c - (int)'0')
    let parseError = System.Exception("Parser error")

    let rec scInt(iStr, iVal) = 
        match iStr with
        c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
        | _ -> (iStr, iVal)

    let lexer input = 
        let rec scan input =
            match input with
            | [] -> []
            | '+'::tail -> Add :: scan tail
            | '-'::tail -> Sub :: scan tail
            | '*'::tail -> Mul :: scan tail 
            | '/'::tail -> Div :: scan tail
            | '('::tail -> Lpar:: scan tail
            | ')'::tail -> Rpar:: scan tail
            | '^'::tail -> Pow:: scan tail
            | '%'::tail -> Mod:: scan tail
            | c :: tail when isblank c -> scan tail
            | c :: tail when isdigit c -> let (iStr, iVal) = scInt(tail, intVal c) 
                                          Num iVal :: scan iStr
            | _ -> raise (MyError(44455666))
        scan (str2lst input)

    let getInputString() : string = 
        Console.Write("Enter an expression: ")
        Console.ReadLine()

    // Grammar in BNF:
    // <E>        ::= <T> <Eopt>
    // <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
    // <T>        ::= <NR> <Topt>
    // <Topt>     ::= "*" <NR> <Topt> | "/" <NR> <Topt> | <empty>
    // <NR>       ::= "Num" <value> | "(" <E> ")"

    let parser tList = 
        let rec E tList = (T >> Eopt) tList         // >> is forward function composition operator: let inline (>>) f g x = g(f x)
        and Eopt tList = 
            match tList with
            | Add :: tail -> (T >> Eopt) tail
            | Sub :: tail -> (T >> Eopt) tail
            | _ -> tList
        and T tList = (NR >> Topt) tList
        and Topt tList =
            match tList with
            | Mul :: tail -> (NR >> Topt) tail
            | Div :: tail -> (NR >> Topt) tail
            | Pow :: tail -> (NR >> Topt) tail
            | Mod :: tail -> (NR >> Topt) tail
            | _ -> tList
        and NR tList =
            match tList with 
            | Num value :: tail -> tail
            | Lpar :: tail -> match E tail with 
                              | Rpar :: tail -> tail
                              | _ -> raise (MyError(4455566))
            | _ -> raise (MyError(4455566))
        E tList

    let parseNeval tList = 
        let rec E tList = (T >> Eopt) tList
        and Eopt (tList, value) = 
            match tList with
            | Add :: tail -> let (tLst, tval) = T tail
                             Eopt (tLst, value + tval)
            | Sub :: tail -> let (tLst, tval) = T tail
                             Eopt (tLst, value - tval)
            | _ -> (tList, value)
        and T tList = (NR >> Topt) tList
        and Topt (tList, value) =
            match tList with
            | Mul :: tail -> let (tLst, tval) = NR tail
                             Topt (tLst, value * tval)
            | Div :: tail -> let (tLst, tval) = NR tail
                             Topt (tLst, value / tval)
            | Pow :: tail -> let (tLst, tval) = NR tail
                             Topt (tLst, int(float value ** float tval))
            | Mod :: tail -> let (tLst, tval) = NR tail
                             Topt (tLst, value % tval)

            | _ -> (tList, value)
        and NR tList =
            match tList with 
            | Num value :: tail -> (tail, value)
            | Lpar :: tail -> let (tLst, tval) = E tail
                              match tLst with 
                              | Rpar :: tail -> (tail, tval)
                              | _ -> raise (MyError(4455566))
            | _ -> raise (MyError(4455566))
        E tList

    let rec printTList (lst:list<terminal>) : list<string> = 
        match lst with
        head::tail -> Console.Write("{0} ",head.ToString())
                      printTList tail
                  
        | [] -> Console.Write("EOL\n")
                []


    [<EntryPoint>]
    let main argv  =
        Console.WriteLine("Simple Interpreter")
        let input:string = String.concat " " argv
        try
            let oList = lexer input
            let sList = printTList oList;
            let pList = printTList (parser oList)
            let Out = parseNeval oList
            Console.WriteLine("Result = {0}", snd Out)
            snd Out
        with
           | MyError(i) -> i
             //| Error(i) -> 1
           | _ -> reraise()



    // 10 ^ 3* 2, 2 * 10^3

