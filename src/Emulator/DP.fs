(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.DP
    Description: Implement ARM DP instructions
*)

/// implement ARM data processing instructions

module DP
open EEExtensions
open CommonData
open Errors
open Expressions
open CommonLex
open Helpers



let failDiag() =
    failwithf "OK so far at %s:%s done!" __SOURCE_FILE__ __LINE__

let pipePrint mess x = printfn "debug %s %A" mess x; x

/// Flags with update info
type UFlags = { F : Flags; CU : bool; VU : bool; NZU : bool; RegU : RName list }
type UCarry = { Ca : bool; CaU : bool }

let toUFlags (fl : Flags) = { F = fl; CU = false; VU = false; NZU = false; RegU = [] }
let toUCarry (carry : bool) = { Ca = carry; CaU = false }

type InstrNegativeLiteralMode =
    | NegatedLit
    | InvertedLit
    | NoNegLit

/// deal with bug in FABLE uint32 handling
let trimUint32 u = u >>> 0

// ///////////// flexible op2 definition and evaluation //////////

type ArmImmediate = { baseVal : byte; rotate : uint32 }
type ArmShiftType = | LSL | ASR | LSR | ROR

/// ARM flexible operand 2
type Op2 =
    | NumberLiteral of K : uint32 * Rot : int * Sub : InstrNegativeLiteralMode
    | RegisterWithShift of RName * ArmShiftType * uint32
    | RegisterWithRRX of RName
    | RegisterWithRegisterShift of RName * ArmShiftType * RName



// ///////////// runtime types ///////////////////////////////////

let makeDPE wanted s = makeParseError wanted s ""

/// instruction type
type Instr = (DataPath -> Result<DataPath * UFlags, ExecuteError>) * Op2

let Executable f = Ok f

// ///////////// datapath manipulation ///////////////////////////

let writeBack result dest (uFlags : UFlags) updateFlags d =
    { Helpers.setReg dest result d with Fl = if updateFlags then uFlags.F else d.Fl },
        { (if updateFlags then uFlags else toUFlags d.Fl) with RegU = [ dest ] }

let evalRegister reg d = Map.find reg d.Regs

let setFlagN value = value > 0x7FFFFFFFu
let setFlagZ value = (value = 0u)

let destStall rn = match rn with | R15 -> 2 | _ -> 0

let addDestStall rn pRes = Result.map (fun (sOp2, pa) -> destStall rn + sOp2, pa) pRes

let op2Stall = function | RegisterWithRegisterShift _ -> 1 | _ -> 0

let getStallFromOk = function | Ok(st, x) -> st, Ok x | Error e -> 0, Error e

/// compute the resulting uint32 from a flexible op2 definition
/// returns (value * carry)
let evalOp2 op2 d =
    let evalRrx (src : uint32) (carry : bool) =
        let msbMask = if carry then (1ul <<< 31) else 0ul
        (src >>> 1) ||| msbMask, { Ca = src &&& 1u = 1u; CaU = true }

    let rotateRight value amt =
        let lowerBits = value >>> amt
        let higherBits = value <<< (32 - amt)
        (higherBits ||| lowerBits)

    let evalShift (src : uint32) (shiftType : ArmShiftType) (shiftBy : uint32) =
        let newVal, carryBit =
            // matching the ARM spec at http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0489g/Cjacbgca.html
            match shiftType with
            | LSL -> if shiftBy >= 32u
                        then 0u, ( if shiftBy = 32u then (src &&& 1u = 1u) else false
                        ) else src <<< int shiftBy, (src >>> (32 - int shiftBy)) &&& 1u = 1u
            | LSR -> if shiftBy >= 32u
                        then 0u, ( if shiftBy = 32u then (src > 0x7FFFFFFFu) else false
                        ) else src >>> int shiftBy, (src >>> (int shiftBy - 1)) &&& 1u = 1u
            | ASR -> if shiftBy >= 32u
                        then uint32 ((int32 src) >>> 31), src > 0x7FFFFFFFu
                        else uint32 ((int32 src) >>> int shiftBy), (src >>> (int shiftBy - 1)) &&& 1u = 1u
            | ROR -> if shiftBy = 32u
                        then src, src > 0x7FFFFFFFu
                        else if shiftBy = 0u
                        then src, false
                        else rotateRight src (int (shiftBy % 32u)), (src >>> int ((shiftBy - 1u) % 32u)) &&& 1u = 1u
        // carry is always unchanged if no shift happens
        newVal, ( if shiftBy <> 0u then { Ca = carryBit; CaU = true } else toUCarry d.Fl.C)

    let evalNumberLiteral (l : uint32) rot sub oldC =
        let u, carry = evalShift (uint32 l) ROR rot
        match sub with
        | NoNegLit -> u
        | InvertedLit -> ~~~u
        | NegatedLit -> (uint32 -(int u))
        |> fun u -> u, ( if rot = 0u then toUCarry oldC else carry)

    match op2 with
    | NumberLiteral(l, rot, sub) -> evalNumberLiteral l (uint32 rot) sub d.Fl.C // carry possibly is changed
    | RegisterWithShift(rSrc, shiftType, shiftBy) -> evalShift d.Regs.[rSrc] shiftType shiftBy
    | RegisterWithRRX rSrc -> evalRrx d.Regs.[rSrc] d.Fl.C
    | RegisterWithRegisterShift(rSrc, shiftType, rShiftBy) -> evalShift d.Regs.[rSrc] shiftType d.Regs.[rShiftBy]


let addNullOp2Info op2Result =
    Result.map (fun op2 -> op2, None)

// ///////////// simulator HOFs //////////////////////////////////

/// higher-order function that executes arbitrary integer arithmetic and logic
let execArithmetic
        (fn : uint32 -> uint32 -> Flags -> UCarry -> Result<uint32 * UFlags, ExecuteError>)
        (updateFlags : bool)
        (dst : RName) (op1 : RName) (op2 : Op2)
        (d : DataPath) =

    let val1 = evalRegister op1 d
    let val2, uCarry = evalOp2 op2 d
    let result = fn val1 (trimUint32 val2) d.Fl uCarry

    result |> Result.map (fun (n, uFlags) -> writeBack n dst uFlags updateFlags d)


/// higher-order function that executes arbitrary comparison instructions
let execComparison
        (fn : uint32 -> uint32 -> Flags -> UCarry -> Result<uint32 * UFlags, ExecuteError>)
        (op1 : RName)
        (op2 : Op2)
        (d : DataPath) =

    // these functions must only update the flags, not the registers
    match (execArithmetic fn true R0 op1 op2 d) with
    | Ok(d', uf) -> Ok({ d with Fl = d'.Fl }, uf)
    | Error e -> Error e


/// function that executes move instructions
let execMove
        (negated : bool)
        (updateFlags : bool)
        (dst : RName)
        (src : Op2)
        (d : DataPath) =

    let op2val, (uCarry : UCarry) = evalOp2 src d
    let result = trimUint32 <| if negated then ~~~op2val else op2val


    let uFlags = {
        F = { d.Fl with N = setFlagN result; Z = setFlagZ result; C = uCarry.Ca };
        NZU = true;
        CU = uCarry.CaU;
        VU = false;
        RegU = [];
        }

    Ok(writeBack result dst uFlags updateFlags d)

/// function that executes adr instruction
let execAdr
        (updateFlags : bool)
        (dst : RName)
        (op2val : uint32)
        (op2uCarry : UCarry)
        (d : DataPath) =

    let flags = { d.Fl with N = setFlagN op2val; Z = setFlagZ op2val; C = op2uCarry.Ca }

    match op2val with
    | op when int64 (d.Regs.[R15] + 8u) - int64 (op) |> abs < 0x400L ->
        Ok(writeBack (trimUint32 op2val) dst { F = flags; VU = false; CU = op2uCarry.CaU; NZU = true; RegU = [] } updateFlags d)
    | _ -> sprintf "Out of range operand %d in ADR instruction" op2val |> Error

// ///////////// simulator functions /////////////////////////////

let simMathWithCarry a b cIn =
    let bit n (x : int64) = (x >>> n) &&& 1L
    let unsignedTrueRes = (int64 (a >>> 0)) + (int64 (b >>> 0)) + (int64 (int cIn))
    let res = trimUint32 (uint32 unsignedTrueRes)
    let signedTrueRes = (int64 (int a)) + (int64 (int b)) + int64 cIn
    let overflow = bit 31 signedTrueRes <> bit 32 signedTrueRes
    let carry = unsignedTrueRes >= (1L <<< 32)
    Ok(res, { F = { N = setFlagN res;
                   Z = setFlagZ res;
                   C = carry;
                   V = overflow }; NZU = true; CU = true; VU = true; RegU = [] })

let to64s (u32 : uint32) = u32 |> int32 |> int64
// basic add
let simADD a b flags op2carry = simMathWithCarry a b 0u
let simSUB a b flags op2carry = simMathWithCarry a (~~~(int64 b) |> uint32) 1u

// add / sub with carry
let simADC a b flags op2carry = simMathWithCarry a b (if flags.C then 1u else 0u)
let simSBC a b flags op2carry = simMathWithCarry a (~~~b) (if flags.C then 1u else 0u)

// reverse subtract
let simRSB a b = simSUB b a
let simRSC a b = simSBC b a

// any bitwise logic (and, orr, eor...)
let simBitwiseLogic op a b flags op2uCarry =
    let mask = (1L <<< 32) - 1L
    let res : int64 = (op (int64 a) (int64 b)) &&& mask
    let ures = uint32 res
    Ok(ures, { F = { flags with N = setFlagN ures; Z = setFlagZ ures; C = op2uCarry.Ca }; NZU = true; CU = op2uCarry.CaU; VU = false; RegU = [] })






/// Map of allowed literals mapped to corresponding (K,rotate)
/// Mutability to reduce startup time
let mutable OkLitMap = None

let makeOkLitMap() =
    match OkLitMap with
    | Some map -> map
    | None ->
        let map =
            let rotateLeft (value : uint32) amt =
                let x1 = ((value >>> (32 - amt)) ||| (value <<< amt))
                let x2 = x1 >>> 0
                //if x1 <> x2 then printfn "Literal bitwise op error: %X : %X" x1 x2
                x2

            let possibleLiterals (k : int32) =
                let K = uint32 k
                [ 0..2..30 ]
                |> List.map (fun rot -> rotateLeft K rot, (K, (32 - rot) % 32))
            [ 255..-1..0 ]
            |> List.collect possibleLiterals
            |> List.groupBy fst
            |> List.map (fun (g, lst) -> g, List.sortBy (fun (x, (K, r)) -> r) lst)
            |> List.map (function | (g, (_, (K, r)) :: _) -> (g), (K, r) | _ -> failwithf "What? Cannot happen!")
            |> Map.ofList
        OkLitMap <- Some map
        map

let makeLitShift shiftStr reg shiftType (n : uint32) =
    match n with
    | n when n < 32u && n >= 0u -> Ok(Op2.RegisterWithShift(reg, shiftType, n))
    | _ -> makeDPE "Valid numeric shift value in range #0 - #31" shiftStr

let parseOp2 (subMode : InstrNegativeLiteralMode) (symTable : SymbolTable) (args : string list) =
    /// make ARM literal from uint32
    let makeImmediate (num' : uint32) =
        let num = num' >>> 0
        let okMap = makeOkLitMap()
        //printfn "makeimmediate num=%d, mask=%d" num mask
        let substitutes : (uint32 * InstrNegativeLiteralMode) list =
            let norm = num, NoNegLit
            match subMode, num with
            | InvertedLit, _ -> [ norm; (~~~num >>> 0, InvertedLit) ]
            | NoNegLit, _
            | NegatedLit, 0u -> [ norm ]
            | NegatedLit, _ -> [ norm; (~~~num + 1u >>> 0, NegatedLit) ]

        let posLits =
            let checkSub (n, sub) =
                Map.tryFind n okMap
                |> function | Some(K, r) -> [ K, r, sub ] | Core.None -> []
            substitutes
            |> List.collect checkSub
            |> List.sortBy (fun (k, r, sub) -> match sub with | NoNegLit -> 0, r | _ -> 1, r)

        //printfn "substitutes=%A" substitutes
        // "Poslits=%A" posLits



        //let isZeroNegated = List.exists (function (0L,_,sub) -> sub = NegatedLit | _ -> false) posLits

        match posLits with
        | [] -> makeDPE "Valid ARM immediate value. Immediates are formed as 32-bit numbers: N ROR (2M), 0 <= N <= 256, 0 <= M <= 15"
                    (sprintf "invalid literal=%d" num)
        | (K, rot, sub) :: _ -> Result.Ok(NumberLiteral(K, rot, sub))


    /// apply shift expression to register
    let parseShiftExpression (str : string) reg =
        let getShiftType str = match Map.tryFind (String.toUpper str) (Map.ofList [ "LSL", LSL; "LSR", LSR; "ASR", ASR; "ROR", ROR ]) with
                                | Some s -> Ok s
                                | None -> makeDPE "Valid shift code LSL,LSR,ASR,ROR" str

        let makeRegShift shiftType r = RegisterWithRegisterShift(reg, shiftType, r)

        if str.ToUpper() = "RRX" then
            Ok(RegisterWithRRX reg)
        else
            match getShiftType (str.ToUpper().Substring(0, 3)), str.ToUpper().Substring(3).Trim() with
            | Ok shiftType, imm when imm.StartsWith("#") ->
                imm.Substring(1)
                |> parseNumberExpression symTable
                |> Result.bind (makeLitShift imm reg shiftType)
            | Ok shiftType, reg when isRegister reg ->
                parseRegister reg
                |> Result.bind (
                    function | R15 -> makeFormatError "Error: operand 2 cannot be PC or R15 if shift is being used" reg ""
                             | rName -> Ok rName)
                |> Result.map (makeRegShift shiftType)
            | Ok shiftType, imm when isValidNumericExpression symTable imm ->
                makeFormatError "Literal constants in instruction operands require '#' prefix" imm ""
            | _ -> makeDPE "Valid flexible op2 shift type" str

    match args with
    | [ imm ] when imm.StartsWith("#") ->
        imm.Substring(1)
        |> parseNumberExpression symTable
        |> Result.bind makeImmediate
    | [ reg ] when isRegister reg ->
        parseRegister reg
        |> Result.map (fun r -> RegisterWithShift(r, LSL, 0u))
    | [ regStr; shiftExpr ] when isRegister regStr ->
        parseRegister regStr
        |> Result.bind (parseShiftExpression shiftExpr)
    | [] -> makeFormatError "Error in flexible op2" "operands ended too early" "flexop2"
    | [ imm ] when isValidNumericExpression symTable imm ->
        makeFormatError "Literal constants in instruction operands require '#' prefix" imm "flexop2"
    | ops -> 
        makeFormatError "Valid op2 consists of: immediate, register, register with shift expression" (String.concat "," ops) "flexop2"


/// make an instruction that has the form "XXX op1, flexop2"
let makeTwoOpInstr hasDest subMode fn symTable operands =
    match operands with
    | op1 :: op2flex ->
        let op1' = parseRegister op1
        let op2' = parseOp2 subMode symTable op2flex

        match op1', op2' with
        | Ok op1'', Ok op2'' ->
            let stall = op2Stall op2'' + if hasDest then destStall op1'' else 0
            Ok(stall, (fn op1'' op2'', op2''))
        | Error e, _ -> Error e
        | _, Error e -> Error e

    | _ -> makeDPE "register, flexible op2" (String.concat "," operands)

/// make an instruction that has the form "XXX dst, op1, flexop2"
let makeThreeOpInstr subMode fn symTable operands =
    match operands with
    | first :: restOfOps ->
        parseRegister first |> Result.bind (fun reg -> addDestStall reg (makeTwoOpInstr false subMode (fn reg) symTable restOfOps))
    | _ -> makeDPE "register, register, flexible operand" ("Not enough operands:'" + String.concat "," operands + "'")



/// return a ready-to-execute comparison function from the given operands
let makeComparisonInstr subMode compType symTable operands _updateFlags =
    makeTwoOpInstr false subMode (execComparison compType) symTable operands

/// return a ready-to-execute move function from the given operands
let makeMoveInstr subMode negated symTable operands updateFlags =
    makeTwoOpInstr true subMode (execMove negated updateFlags) symTable operands

/// return a ready-to-execute adr function from the given operands
let makeAdrInstr symTable operands updateFlags =
    // makeThreeOpInstr (execArithmetic [operands.[0];"#"+operands.[1]+"-(PC+8)"] updateFlags) symTable
    match operands with
    | op1 :: [ op2flex ] ->
        let op1' = parseRegister op1
        let op2' = parseNumberExpression symTable op2flex

        match op1', op2' with
        | Ok op1'', Ok op2'' -> Ok(execAdr updateFlags op1'' op2'')
        | Error e, _ -> Error e
        | _, Error e -> Error e
    | _ -> makeDPE "register, expression" ("Not enough operands: '" + String.concat "," operands + "'")

/// return a ready-to-execute arithmetic function from the given operands
let makeArithmeticInstr subMode opType symTable operands updateFlags =
    makeThreeOpInstr subMode (execArithmetic opType updateFlags) symTable operands

/// return a ready-to-execute shift/rotate function from the given operands
let makeShiftInstr shiftType symTable operands updateFlags =
    match operands with
    | [ dst; src; shiftBy ] ->
        match parseRegister dst, parseRegister src with
        | Ok dst', Ok src' ->
            let op2 =
                let makeLitShift' (n : uint32) =
                    match n with
                    | n when n < 32u && n >= 0u -> Ok(Op2.RegisterWithShift(src', shiftType, n))
                    | _ -> makeDPE "Valid numeric shift value in range #0 - #31" shiftBy
                let makeRegShift r = RegisterWithRegisterShift(src', shiftType, r)
                match shiftBy with
                | cons when cons.StartsWith("#") -> cons.Substring(1) |> parseNumberExpression symTable |> Result.bind (makeLitShift cons src' shiftType)
                | reg when isRegister reg -> parseRegister reg |> Result.map makeRegShift
                | reg when isValidNumericExpression symTable reg ->
                      makeFormatError "Numbers in instruction operands require '#' prefix (#22, #-1)" reg "flexop2"
                | sftTxt -> makeFormatError "Error in shift specification - should be #N or Rx" sftTxt "flexop2"

            Result.map (fun op2' -> op2Stall op2' + destStall dst', (execMove false updateFlags dst' op2', op2')) op2
        | Error e, _ -> Error e
        | _, Error e -> Error e

    | _ -> makeDPE "destination, source, shift value" (String.concat "," operands)

/// return a ready-to-execute RRX function from the given operands
let makeRRXInstr symTable operands updateFlags =
    match operands with
    | [ dst; src ] ->
        match parseRegister dst, parseRegister src with
        | Ok dst', Ok src' -> Ok(destStall dst', (execMove false updateFlags dst' (RegisterWithRRX src'), Op2.RegisterWithRRX src'))
        | Error e, _ -> Error e
        | _, Error e -> Error e

    | _ -> makeDPE "destination, source" (String.concat "," operands)

// ///////////// top-level parsing ///////////////////////////////



/// specification for set of instructions
let spec = [
    "MOV", fun s a b -> makeMoveInstr InvertedLit false s a b
    "MVN", fun s a b -> makeMoveInstr InvertedLit true s a b

    "ADD", fun s a b -> makeArithmeticInstr NegatedLit simADD s a b
    "SUB", fun s a b -> makeArithmeticInstr NegatedLit simSUB s a b
    "ADC", fun s a b -> makeArithmeticInstr InvertedLit simADC s a b
    "SBC", fun s a b -> makeArithmeticInstr InvertedLit simSBC s a b
    "RSB", fun s a b -> makeArithmeticInstr NoNegLit simRSB s a b
    "RSC", fun s a b -> makeArithmeticInstr NoNegLit simRSC s a b

    "AND", fun s a b -> makeArithmeticInstr InvertedLit (simBitwiseLogic (&&&)) s a b
    "ORR", fun s a b -> makeArithmeticInstr NoNegLit (simBitwiseLogic (|||)) s a b
    "EOR", fun s a b -> makeArithmeticInstr NoNegLit (simBitwiseLogic (^^^)) s a b
    "BIC", fun s a b -> makeArithmeticInstr InvertedLit (simBitwiseLogic (fun a b -> a &&& ~~~b)) s a b

    "LSL", fun s a b -> makeShiftInstr LSL s a b
    "LSR", fun s a b -> makeShiftInstr LSR s a b
    "ASR", fun s a b -> makeShiftInstr ASR s a b
    "ROR", fun s a b -> makeShiftInstr ROR s a b
    "RRX", fun s a b -> makeRRXInstr s a b

    "CMN", fun s a b -> makeComparisonInstr NegatedLit simADD s a b
    "CMP", fun s a b -> makeComparisonInstr NegatedLit simSUB s a b
    "TST", fun s a b -> makeComparisonInstr NoNegLit (simBitwiseLogic (&&&)) s a b
    "TEQ", fun s a b -> makeComparisonInstr NoNegLit (simBitwiseLogic (^^^)) s a b
    ]


let dPSpec = {
    InstrC = DP
    Roots = List.map fst spec
    Suffixes = [ ""; "S" ]
 }

/// map of all possible opcodes recognised
let opCodes = opCodeExpand dPSpec

/// Active pattern matching to extract undefined label from error string
let (|UndefLabel|_|) (prefix : string) =
    function | Error(code : ErrCode, txt : string, str : string) ->
                if String.startsWith prefix str then
                    str |> String.substring prefix.Length |> Some
                else None
                | _ -> None

/// main function to parse a line of assembler
/// ls contains the line input
/// and other state needed to generate output
/// the result is None if the opcode does not match
/// otherwise it is Ok Parse or Error (parse error string)
let parse (ls : LineData) : Parse<Instr> option =
    let parse' (instrC, (root, suffix, pCond)) =
        // separate operands
        let operands = ls.Operands.Split(',') |> Array.toList |> List.map String.trim

        // set load address
        let (WA la) = ls.LoadAddr

        // This could be improved
        let pI =
            {
                PInstr = Error ``Unimplemented parse``
                PLabel = ls.Label |> Option.map (fun lab -> lab, Ok la);
                ISize = 4u;
                DSize = Some 0u;
                PCond = pCond
                POpCode = ls.OpCode
                PStall = 0
            }

        // flags should be updated if S suffix is specified
        // or by default on some instructions
        let updateFlags = match root with
                            | "CMP" | "CMN" | "TST" | "TEQ" -> true
                            | _ when suffix = "S" -> true
                            | _ -> false

        // parse operands
        match (Map.ofList spec).[root] ls.SymTab operands updateFlags with
        | Ok(stall, f) ->
            // return parse result with executable line of code
            stall, { pI with PInstr = Executable f }
        | Error e ->
            // return parse error
            0, { pI with PInstr = Error e }
    Map.tryFind ls.OpCode opCodes
    |> Option.map parse'
    |> Option.map (fun (stall, pa) -> { pa with PStall = stall })

let (|IMatch|_|) ld = parse ld

let executeDP = fun (execFn, _dPInfo) dp -> execFn dp


