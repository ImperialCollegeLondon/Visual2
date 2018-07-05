(* 
    Visual2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compliler )
    Module: Emulator.Misc
    Description: Implement ARM miscellaneous instructions
*)

/// emulate DCD, DCB, FILL, EQU, ADR instructions
module Misc
    open CommonData
    open CommonLex
    open Expressions
    open Helpers
    open Errors
    open DP


    type FILLInstr = {NumBytes : uint32 ; FillValue : uint32}

    type ADRInstr = {AReg : RName ; AVal : uint32}

    type Instr =
        | DCD of uint32 list
        | DCB of uint32 list
        | FILL of FILLInstr
        | EQU of uint32
        | ADR of ADRInstr

    let miscRoots = ["DCD";"DCB";"FILL";"EQU";"ADR"]
  
        
    /// Errors which can occur during parsing
    // type ErrInstr =
    //     | InvalidExp of string
    //     | InvalidExpList of string
    //     | InvalidFill of string
    //     | LabelRequired
    /// Error types for parsing.

    let commaSplit (x : string) =  x.Split([|','|]) |> Array.toList

    let mergeResults (lst : Result<'T,'E> list) =
        let folder (st:Result<'T list,'E>) (r:Result<'T,'E>) = 
            match st with
            | Error st -> Error st
            | Ok st -> Result.map (fun r -> r :: st) r
        List.fold folder (Ok []) lst
        |> Result.map List.rev

    let parseExprList symTab lst =
        List.map (resolveOp symTab) lst
        |> mergeResults

   

    let parse (ls: LineData) : Parse<Instr>  =
        let (WA la) = ls.LoadAddr // address this instruction is loaded into memory
        let opLst = commaSplit ls.Operands
        let resolvedOpLst = parseExprList ls.SymTab opLst
        let (|PARSE|_|) op = resolveOp ls.SymTab op |> Some
        let (|RESOLVEALL|_|) lst = match parseExprList ls.SymTab lst with | Ok ops -> Some ops | _ -> None
        let opCode = ls.OpCode
        let checkAddrOffset (ofs:int) =
            match ofs-8 with
            | x when x % 4 <> 0 && (x > 264 || x < -248) -> 
                makePE ``Invalid offset`` ls.Operands (sprintf "ADR byte offset must be in range -248..264 and is %d" ofs)
            | x when  (x > 1032 || x < -1016) -> 
                makePE ``Invalid offset`` ls.Operands (sprintf "ADR byte offset must be in range -1016..1032 and is %d" ofs)
            | x -> Ok ()

        let labelBinder f = 
            match ls.Label with
            | Some lab -> f lab
            | None -> 
                (ls.OpCode + " " + ls.Operands, " requires a label.")
                ||> makePE ``Label required``
        
        let makeDataInstr dataInstrCode = Result.map dataInstrCode resolvedOpLst
        
        let makeDataDirective dSize dataInstr = 
            { copyDefault ls Cal with
                PInstr = dataInstr
                ISize = 0u
                DSize = dSize
            }
                 


        let opNum = List.length opLst |> uint32

        let makeFILL ops =
            match resolvedOpLst with
            | Ok [nBytes] when nBytes % 4u = 0u -> 
                makeDataDirective nBytes (FILL {NumBytes = nBytes; FillValue = 0u} |> Ok)
            | Ok [nBytes; fillVal] when nBytes % 4u = 0u  ->  
                makeDataDirective nBytes (FILL {NumBytes = nBytes; FillValue = fillVal} |> Ok)
            | Ok [ nBytes]
            | Ok [ nBytes; _] ->
                makeDataDirective 0u (makePE ``Invalid instruction`` ls.Operands <| 
                        sprintf "%d FILL bytes is invalid. Fill must have a number of bytes divisible by 4" nBytes)
            | _ -> makeDataDirective 0u (makePE ``Invalid instruction`` ls.Operands "Fill must have 1 or 2 operands")
       
        let makeEQU (op: Resolvable) =
            match op with
            | Ok addr -> makeDataDirective 0u (EQU addr |> Ok)
            | Error e -> makeDataDirective 0u (Error e)
            |> fun pa -> { pa with PLabel = Option.map (fun lab -> lab , op) ls.Label}
       
        let pa = copyDefault ls Cal
        match opCode, opLst with
        | "DCD", RESOLVEALL ops -> makeDataDirective (opNum*4u) (makeDataInstr DCD) 
        | "DCB", RESOLVEALL ops when ops.Length % 4 = 0 -> makeDataDirective opNum (makeDataInstr DCB) 
        | "DCB", _ -> 
            makePE ``Invalid instruction`` ls.Operands "DCB must have a number of parameters divisible by 4"
            |> makeDataDirective 0u
        | "FILL", RESOLVEALL [op] -> makeFILL [op,0u]
        | "FILL", RESOLVEALL ops  -> makeFILL ops
        | "ADR", RegMatch (Ok rn) :: RESOLVEALL [addr] ->
            match checkAddrOffset (int addr - int la) with
            | Ok _ ->  {pa with PInstr = ADR {AReg=rn ; AVal=addr} |> Ok }
            | Error e -> {pa with PInstr = Error e}
        | "ADR", ops -> {pa with PInstr = makePE ``Invalid instruction`` ls.Operands "Invalid operands for ADR instruction"}
        | "EQU", [PARSE op] -> makeEQU op
        | "EQU", x -> {pa with PInstr = makePE ``Invalid expression`` ls.Operands (sprintf "'%A' is an invalid expression for EQU" x)}
        | _, ops -> makePE ``Invalid instruction`` (ls.OpCode + " " + ls.Operands) "" |> makeDataDirective 0u
        | _ -> failwithf "What? unrecognised Misc opcode %s" opCode
      

        
    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) (ls:LineData) =
        if List.contains ls.OpCode miscRoots 
        then 
            //printfn "IMISC Parsing '%s'" ls.OpCode
            parse ls |> Some 
        else 
            //printfn "IMISC Not parsing '%s'" ls.OpCode
            None
        
