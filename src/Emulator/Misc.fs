(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.Misc
    Description: Implement ARM miscellaneous instructions
*)

/// emulate DCD, DCB, FILL, EQU, ADR instructions
module Misc
    open EEExtensions
    open CommonData
    open CommonLex
    open Expressions
    open Helpers
    open Errors
    open DP


    type FILLInstr = { NumBytes : uint32; FillValue : uint32 }

    type ADRInstr = { AReg : RName; AVal : uint32 }

    type Instr =
        | DCD of uint32 list
        | DCB of uint32 list
        | FILL of FILLInstr
        | EQU of uint32
        | ADR of ADRInstr

    let miscRoots = [ "DCD"; "DCB"; "FILL"; "EQU"; "ADR" ]


    /// Errors which can occur during parsing
    // type ErrInstr =
    //     | InvalidExp of string
    //     | InvalidExpList of string
    //     | InvalidFill of string
    //     | LabelRequired
    /// Error types for parsing.

    let commaSplit (x : string) =
        x.Split([| ',' |])
        |> Array.toList
        |> List.map String.trim

    let mergeResults (lst : Result<'T, 'E> list) =
        let folder (st : Result<'T list, 'E>) (r : Result<'T, 'E>) =
            match st with
            | Error st -> Error st
            | Ok st -> Result.map (fun r -> r :: st) r
        List.fold folder (Ok []) lst
        |> Result.map List.rev


    let parseNumExpr ls s =
            Ok((), s)
            |> ResExpr(fun _ e -> e)
            |> ResCheckDone
            |> Result.bind (fun exp ->
                eval ls.SymTab exp)

    let parseExprList symTab lst =
        List.map (parseNumExpr symTab) lst
        |> mergeResults



    let parse (ls : LineData) : Parse<Instr> =
        let (WA la) = ls.LoadAddr // address this instruction is loaded into memory
        let opLst = commaSplit ls.Operands
        let resolvedOpLst = parseExprList ls opLst
        let (|PARSE|_|) op = parseNumExpr ls op |> Some
        let (|RESOLVEALL|_|) lst = match parseExprList ls lst with | Ok ops -> Some ops | _ -> None
        let opCode = ls.OpCode
        let offsetError wb b1 b2 ofs =
            let msg = sprintf "Valid %s offset in range %d..%d. Use 'LDR Rx, =SYMBOL' when offset is larger than this" wb b1 b2
            makeParseError msg (sprintf "Offset of %d" ofs) ""
        let checkAddrOffset (ofs : int) =
            match ofs - 8 with
            | x when x % 4 <> 0 && (x > 264 || x < -248) -> offsetError "byte" -248 264 (ofs - 8)
            | x when (x > 1032 || x < -1016) -> offsetError "word" -1016 1032 (ofs - 8)
            | x -> Ok()

        let labelBinder f =
            match ls.Label with
            | Some lab -> f lab
            | None -> ``Label required`` ("'" + ls.Operands + "' requires a label.")


        let makeDataInstr dataInstrCode = Result.map dataInstrCode resolvedOpLst

        let makeDataDirective dSizeOpt dataInstr =
            { copyDefault ls Cal with
                PInstr = dataInstr
                ISize = 0u
                DSize = dSizeOpt
            }



        let opNum = List.length opLst |> uint32

        let makeFILL ops =
            match resolvedOpLst with
            | Ok [ nBytes ] when nBytes % 4u = 0u ->
                makeDataDirective (Some nBytes) (FILL { NumBytes = nBytes; FillValue = 0u } |> Ok)
            | Ok [ nBytes; fillVal ] when nBytes % 4u = 0u ->
                makeDataDirective (Some nBytes) (FILL { NumBytes = nBytes; FillValue = fillVal } |> Ok)
            | Ok [ nBytes ]
            | Ok [ nBytes; _ ] ->
                let msg = sprintf "%d FILL bytes is invalid. Fill must have a number of bytes divisible by 4" nBytes
                makeDataDirective (Some 0u) (makeInstructionError msg)

            | _ -> makeDataDirective (Some 0u) <|
                       makeInstructionError ("Invalid operands '" + ls.Operands + "'. Fill must have 1 or 2 operands")

        let makeEQU (op : Resolvable) =
            match op with
            | Ok addr -> makeDataDirective (Some 0u) (EQU addr |> Ok)
            | Error e -> makeDataDirective (Some 0u) (Error e)
            |> fun pa -> { pa with PLabel = Option.map (fun lab -> lab, op) ls.Label }

        let pa = copyDefault ls Cal
        match opCode.ToUpper(), opLst with
        | "DCD", RESOLVEALL ops -> makeDataDirective (Some(opNum * 4u)) (makeDataInstr DCD)
        | "DCB", RESOLVEALL ops when ops.Length % 4 = 0 -> makeDataDirective (Some opNum) (makeDataInstr DCB)
        | "DCB", _ ->
            let msg = "Invalid operands: '" + ls.Operands + "'. DCB must have a number of parameters divisible by 4"
            makeInstructionError msg
            |> makeDataDirective (Some 0u)
        | "FILL", RESOLVEALL [ op ] -> makeFILL [ op, 0u ]
        | "FILL", _ -> makeDataDirective None <| makeInstructionError ("Invalid operands for FILL: unresolved symbols")
        | "ADR", RegMatch(Ok rn) :: [ PARSE(Ok addr) ] ->
            match checkAddrOffset (int addr - int la) with
            | Ok _ -> { pa with PInstr = ADR { AReg = rn; AVal = addr } |> Ok; PStall = (if rn = R15 then 2 else 0) }
            | Error e -> { pa with PInstr = Error e }
        | "ADR", _ops ->
            let msg = "Invalid operands: '" + ls.Operands +
                      "'. ADR must have a register followed by a numeric expression operand."
            { pa with PInstr = makeInstructionError msg  }
        | "EQU", [ PARSE op ] when opLst <> [] -> makeEQU op
        | "EQU", x -> { pa with PInstr = makeInstructionError (sprintf "'%A' is an invalid expression for EQU" x) }
        | _, ops -> makeInstructionError ("Invalid instruction: '" + ls.OpCode + " " + ls.Operands + "'")
                    |> makeDataDirective (Some 0u)
        | _ -> failwithf "What? unrecognised Misc opcode %s" opCode



    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) (ls : LineData) =
        if List.contains ls.OpCode miscRoots
        then
            //printfn "IMISC Parsing '%s'" ls.OpCode
            parse ls |> Some
        else
            //printfn "IMISC Not parsing '%s'" ls.OpCode
            None

