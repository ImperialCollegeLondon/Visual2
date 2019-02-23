(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.Expressions
    Description: Parse expressions with constants, symbols, and arithmetic
*)


/// Assembler code expressions (symbols, operators, and constants)
module Expressions
    open System.Text.RegularExpressions
    open Errors
    open EEExtensions


    /// Match the start of txt with pat
    /// Return a tuple of the matched text and the rest
    let (|RegexPrefix|_|) pat txt =
        // Match from start, ignore whitespace
        let m = Regex.Match(txt, "^[\\s]*" + pat + "[\\s]*")
        match m.Success with
        | true -> (m.Value, txt.Substring(m.Value.Length)) |> Some
        | false -> None


    /// Remove all whitespace from a matched string
    let removeWs txt = Regex.Replace(txt, "[\\s]*", "")

    /// Active pattern for matching labels
    /// Also removes any whitespace from around the label
    let (|LabelExpr|_|) txt =
        match txt with
        | RegexPrefix "[a-zA-Z][a-zA-Z0-9_]+" (var, rst) ->
            (// Remove whitespace from the label
            removeWs var, rst) |> Some
        | _ -> None



    type SymbolTable = Map<string, uint32>

    // [<CustomEquality; NoComparison>]
    type Expression =
        | BinOp of (uint32 -> uint32 -> uint32) * Expression * Expression
        | Label of string
        | Literal of uint32
        // override _x.Equals (_y) = false

    type Resolvable = Result<uint32, ParseError>


    /// Evaluate exp against the symbol table syms
    /// Returns a list of all errors or the result
    let rec eval (syms : Map<string, uint32>) exp : Result<uint32, ErrCode> =
        let joinErrors a b =
            match a, b with
            | ``Undefined symbol`` a', ``Undefined symbol`` b' ->
                ``Undefined symbol`` (a' @ b') |> Error
            | ``Undefined symbol`` _, a'
            | a', _ -> a' |> Error
        let doBinary op x y =
            match (eval syms x), (eval syms y) with
            | Ok resX, Ok resY -> ((op resX resY) >>> 0) |> Ok
            | Error a, Error b -> joinErrors a b
            | Error a, _ -> a |> Error
            | _, Error b -> b |> Error
        let getSymError x =
            let x' = String.toUpper x
            let symLst = syms |> Map.toList |> List.map fst
            match List.tryFind (fun sym -> x' = String.toUpper sym) symLst with
            | Some sym -> x, sprintf "'%s' which has different case from label '%s'" x sym
            | None -> x, sprintf "'%s' which is not defined as a label" x
        match exp with
        | BinOp(op, x, y) -> doBinary op x y
        | Literal x -> x |> Ok
        | Label x ->
            match (Map.containsKey x syms) with
                | true -> syms.[x] |> Ok
                | false ->
                    ``Undefined symbol`` [ getSymError x ] |> Error


    let to32BitLiteral chars =
        try
            chars
            |> int64
            |> function
               | n when n > (1L <<< 32) || n < (-1L <<< 31) -> Error(``Literal more than 32 bits`` chars)
               | n -> Ok(uint32 n)
        with
            | e -> Error(``Literal is not a valid number`` chars)




    /// Active pattern for matching expressions
    /// Returns an Expression AST
    let rec (|Expr|_|) expTxt =

        let (|Minus|_|) (txt : string) =
            match txt.Length > 0 && txt.[0] = '-' with
            | true -> Some txt.[1..]
            | false -> None

        let (|PosLiteralExpr|_|) txt =
            match txt with
            | RegexPrefix "0[xX][0-9a-fA-F][0-9a-fA-F_]*" (num, rst)
            | RegexPrefix "0[bB][0-1][0-1_]*" (num, rst)
            | RegexPrefix "[0-9][0-9_]*" (num, rst) ->
                try
                    let litNum =
                        num
                        |> String.replace "_" ""
                        |> String.toLower
                        |> int64
                        |> function // check that literal constants are within 32 bit range under FABLE
                           | n when n > ((1L <<< 32) - 1L) ->
                                failwithf "Literal more than 32 bits"
                           | n -> uint32 n
                        |> Literal
                    (litNum, rst) |> Some
                with
                    | _ ->
                        printfn "Exception in Expr: uint32(%A)" num
                        None
            | RegexPrefix "&[0-9a-fA-F]+" (num, rst) ->
                ("0x" + (removeWs num).[1..] |> uint32 |> Literal, rst) |> Some
            | _ -> None

        let (|LiteralExpr|_|) (expTxt : string) =
                match expTxt with
                | PosLiteralExpr(num, txt) -> Some(num, txt)
                | Minus(PosLiteralExpr(num, txt)) -> Some(BinOp((-), Literal 0u, num), txt)
                | _ -> None

        /// Active pattern matching either labels, literals
        /// or a bracketed expression (recursively defined)
        let (|PrimExpr|_|) txt =
            match txt with
            | LabelExpr(lab, rst) -> (Label lab, rst) |> Some
            | LiteralExpr x -> Some x
            | RegexPrefix "\(" (_, Expr(exp, RegexPrefix "\)" (_, rst))) -> (exp, rst) |> Some
            | _ -> None

        /// Higher order active patterns to match lists of the form
        /// x op x op x ... to capture left associativity correctly.
        let rec (|LBinExprList|_|) (|NextExpr|_|) reg op lVal txt =
            match txt with
            | RegexPrefix reg (_, NextExpr(rVal, rst)) ->
                match rst with
                | LBinExprList (|NextExpr|_|) reg op (BinOp(op, lVal, rVal)) (exp, rst')
                    -> Some(exp, rst')
                | _ -> Some(BinOp(op, lVal, rVal), rst)
            | _ -> None

        /// Higher order active pattern for defining binary operators
        /// NextExpr is the active pattern of the operator with the next
        /// highest precedence. reg is the regex which matches this operator
        /// op is the operation it performs
        let (|LBinExpr|_|) (|NextExpr|_|) reg op txt =
            match txt with
            | NextExpr(lVal, rhs) ->
                match rhs with
                | LBinExprList (|NextExpr|_|) reg op lVal x
                    -> Some x
                // Can't nest this AP because its the
                // "pass-through" to the next operator
                | _ -> (lVal, rhs) |> Some
            | _ -> None

        // Define active patterns for the binary operators
        // Order of precedence: Add, Sub, Mul
        let (|MulExpr|_|) = (|LBinExpr|_|) (|PrimExpr|_|) "\*" (*)
        let (|SubExpr|_|) = (|LBinExpr|_|) (|MulExpr|_|) "\-" (-)
        let (|AddExpr|_|) = (|LBinExpr|_|) (|SubExpr|_|) "\+" (+)

        match expTxt with
        | AddExpr x -> Some x
        | _ -> None


    let printUintRes r =
        match r with
        | Ok u -> printfn "OK %d" u; r
        | Error(code, eTxt, eMess) -> printfn "Error:<%s><%s>" eTxt eMess; r

    let parseEvalNumericExpression syms op =
        match removeWs op with
        | Expr(ast, _) -> eval syms ast
        | _ when String.contains "#" op -> makeParseError "Numeric expression (without #)" op ""
        | _ -> makeParseError "Numeric expression" op ""

    let parse syms op =
        match removeWs op with
        | Expr(ast, txt) -> Result.map (fun e -> e, txt) (eval syms ast)
        | _ when String.contains "#" op -> makeParseError "Numeric expression (without #)" op ""
        | _ -> makeParseError "Numeric expression" op ""


    type PartsOfASM = | ALabel | AOpCode | AOperand of int

    /// Code to implement accurate error reporting
    /// Malformed lines will be best effort parsed into parts
    /// Each part will be returned with its position in the original line
    let getASMPart symTab isOpCode (part : PartsOfASM) (line : string) =
        let isSymbol s = Map.containsKey s symTab
        let pack (thing : string) (restOfLine : string) =
            let n = line.Length
            let r = restOfLine.Length
            let ePos = n - r + 1
            let sPos = ePos - thing.Length
            thing, sPos, ePos
        match part, line with
        | ALabel, LabelExpr(lab, rst) when isOpCode lab |> not -> pack lab rst
        | _ -> failwithf "Not implemented"

