module DP

open EEExtensions
open CommonData
open Errors
open Expressions
open CommonLex
open System.Xml.Linq

let failDiag() = 
    failwithf "OK so far at %s:%s done!" __SOURCE_FILE__ __LINE__

let pipePrint mess x = printfn "debug %s %A" mess x; x

type InstrNegativeLiteralMode = 
    | NegatedLit
    | InvertedLit
    | NoNegLit

// ///////////// runtime types ///////////////////////////////////

let makeDPE s = makePE ``Invalid syntax`` "" s

/// instruction type
type Instr =  (DataPath -> Result<DataPath, ExecuteError>)
                      
let Executable f = Ok f

// ///////////// datapath manipulation ///////////////////////////

let writeBack result dest flags updateFlags d =
    {d with Regs = Map.add dest result d.Regs ; Fl = if updateFlags then flags else d.Fl}

let evalRegister reg d = Map.find reg d.Regs

let setFlagN value = value > 0x7FFFFFFFu
let setFlagZ value = (value = 0u)


// ///////////// flexible op2 definition and evaluation //////////

type ArmImmediate = { baseVal: byte ; rotate: uint32 }
type ArmShiftType = LSL | ASR | LSR | ROR

/// ARM flexible operand 2
type Op2 =
    | NumberLiteral of K: int64 * Rot: int * Sub: InstrNegativeLiteralMode
    | RegisterWithShift of RName * ArmShiftType * uint32
    | RegisterWithRRX of RName
    | RegisterWithRegisterShift of RName * ArmShiftType * RName

/// compute the resulting uint32 from a flexible op2 definition
/// returns (value * carry)
let evalOp2 op2 d =
    let evalRrx (src : uint32) (carry : bool) =
        let msbMask = if carry then (1ul <<< 31) else 0ul
        (src >>> 1) ||| msbMask, (src &&& 1u = 1u)
        
    let rotateRight value amt =
        let lowerBits = value >>> amt
        let higherBits = value <<< (32 - amt)
        (higherBits ||| lowerBits)

    let evalShift (src : uint32) (shiftType : ArmShiftType) (shiftBy : uint32) =
        let newVal, carryBit =
            // matching the ARM spec at http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0489g/Cjacbgca.html
            match shiftType with
            | LSL -> if shiftBy >= 32u
                        then 0u, if shiftBy = 32u then (src &&& 1u = 1u) else false
                        else src <<< int shiftBy, (src >>> (32 - int shiftBy)) &&& 1u = 1u
            | LSR -> if shiftBy >= 32u
                        then 0u, if shiftBy = 32u then (src > 0x7FFFFFFFu) else false
                        else src >>> int shiftBy, (src >>> (int shiftBy - 1)) &&& 1u = 1u
            | ASR -> if shiftBy >= 32u
                        then uint32 ((int32 src) >>> 31), src > 0x7FFFFFFFu
                        else uint32 ( (int32 src) >>> int shiftBy ), (src >>> (int shiftBy - 1)) &&& 1u = 1u
            | ROR -> if shiftBy = 32u
                        then src, src > 0x7FFFFFFFu
                        else if shiftBy = 0u
                        then src, false
                        else rotateRight src (int (shiftBy % 32u)), (src >>> int ((shiftBy - 1u) % 32u)) &&& 1u = 1u
        // carry is always false if no shift happens
        newVal, if shiftBy <> 0u then carryBit else d.Fl.C

    let evalNumberLiteral (l:int64) rot sub =
        let u, carry = evalShift (uint32 l) ROR rot
        match sub with
        | NoNegLit -> u
        | InvertedLit -> ~~~u
        | NegatedLit -> (uint32 -(int u))
        |> fun u -> u,carry
    
    match op2 with
    | NumberLiteral (l,rot,sub) -> evalNumberLiteral l (uint32 rot) sub // carry possibly is changed
    | RegisterWithShift (rSrc, shiftType, shiftBy) -> evalShift d.Regs.[rSrc] shiftType shiftBy
    | RegisterWithRRX rSrc -> evalRrx d.Regs.[rSrc] d.Fl.C
    | RegisterWithRegisterShift (rSrc, shiftType, rShiftBy) -> evalShift d.Regs.[rSrc] shiftType d.Regs.[rShiftBy]


// ///////////// simulator HOFs //////////////////////////////////
       
/// higher-order function that executes arbitrary integer arithmetic and logic
let execArithmetic
        (fn : uint32 -> uint32 -> Flags -> bool -> Result<uint32 * Flags, ExecuteError>)
        (updateFlags : bool)
        (dst : RName) (op1 : RName) (op2 : Op2)
        (d : DataPath) =

    let val1 = evalRegister op1 d
    let val2, carry = evalOp2 op2 d
    let result = fn val1 val2 d.Fl carry

    result |> Result.map (fun (n, flags) -> writeBack n dst flags updateFlags d)


/// higher-order function that executes arbitrary comparison instructions
let execComparison
        (fn : uint32 -> uint32 -> Flags -> bool -> Result<uint32 * Flags, ExecuteError>)
        (op1 : RName)
        (op2 : Op2)
        (d : DataPath) =

    // these functions must only update the flags, not the registers
    match (execArithmetic fn true R0 op1 op2 d) with
    | Ok d' -> Ok {d with Fl = d'.Fl}
    | Error e -> Error e


/// function that executes move instructions
let execMove
        (negated : bool)
        (updateFlags : bool)
        (dst : RName)
        (src : Op2)
        (d : DataPath) =

    let op2val, carry = evalOp2 src d
    let result = if negated then ~~~op2val else op2val

    let flags = {d.Fl with N = setFlagN result ; Z = setFlagZ result ; C = carry }

    Ok (writeBack result dst flags updateFlags d)

/// function that executes adr instruction
let execAdr
        (updateFlags: bool)
        (dst: RName)
        (op2val: uint32)
        (d: DataPath) =

    let flags = {d.Fl with N = setFlagN op2val ; Z = setFlagZ op2val }

    match op2val with
    | op when int64 (d.Regs.[R15]+8u) - int64 (op) |> abs < 0x400L -> 
        Ok (writeBack op2val dst flags updateFlags d)         
    | _ -> sprintf "Out of range operand %d in ADR instruction" op2val |> Error

// ///////////// simulator functions /////////////////////////////

let simMathWithCarry op_u32 op_u64 op_i64 a b cIn cOutTransform =
    let res = op_u32 (op_u32 a b) cIn |> (&&&) 4294967295u
    let unsignedCarry = op_u64 (op_u64 (uint64 a) (uint64 b)) (uint64 cIn) <> (uint64 res)
    let overflow = op_i64  (op_i64 (int64 (int a)) (int64 (int b)))  (int64 (int cIn)) <> (int64 (int res))
    Ok (res, { N = setFlagN res;
                Z = setFlagZ res;
                C = cOutTransform unsignedCarry;
                V = overflow })

// basic add
let simADD a b flags op2carry = simMathWithCarry (+) (+) (+) a b 0u (fun x -> x)
let simSUB a b flags op2carry = simMathWithCarry (-) (-) (-) a b 0u (not)

// add / sub with carry
let simADC a b flags op2carry = simMathWithCarry (+) (+) (+) a b (if flags.C then 1u else 0u) (fun x -> x)
let simSBC a b flags op2carry = simMathWithCarry (-) (-) (-) a b (if flags.C then 0u else 1u) (not)

// reverse subtract
let simRSB a b = simSUB b a
let simRSC a b = simSBC b a

// any bitwise logic (and, orr, eor...)
let simBitwiseLogic op a b flags op2carry =
    let res = op a b
    Ok (res, {flags with N = setFlagN res ; Z = setFlagZ res ; C = op2carry})


// ///////////// parsing functions ///////////////////////////////

let parseNumberExpression (symTable) (str : string) =
    resolveOp symTable str

let parseRegister (str : string) =
    match Map.tryFind (str.ToUpper().Trim()) regNames with
    | Some r -> Ok r
    | None -> makeDPE <| sprintf "Invalid register name '%s'" str



/// Map of allowed literals mapped to corresponding (K,rotate)
/// Mutability to reduce startup time
let mutable OkLitMap = None

let makeOkLitMap() = 
    match OkLitMap with 
    | Some map -> map
    | None -> 
        let map = 
            let mask = ((1L <<< 32) - 1L)
            let rotateLeft (value:int64) amt = 
                ((value >>> (32 - amt)) ||| (value <<< amt)) &&& mask

            let possibleLiterals K =
                [0..2..30]
                |> List.map (fun rot -> rotateLeft K rot, (K,(32 - rot) % 32))
            [0..255] // workaround FABLE bug with long unsigned int ranges
            |> List.map (fun x -> int64 x)
            |> List.collect possibleLiterals
            |> List.map (fun (x, (K, r)) -> (x, (K,r)))
            |> Map.ofList
        OkLitMap <- Some map
        map

let parseOp2 (subMode: InstrNegativeLiteralMode) (symTable : SymbolTable) (args : string list) =
    /// make ARM literal from uint32
    let makeImmediate (num:int64) =
        let okMap = makeOkLitMap()
        let mask = 0xFFFFFFFFL
        let num64 = int64 (int num) &&& mask    
        let substitutes: (int64 * InstrNegativeLiteralMode) list = 
            let norm = num64,NoNegLit
            match subMode, num with
            | InvertedLit, _ -> [norm ; ((~~~num64) &&& mask , InvertedLit)]
            | NoNegLit, _
            | NegatedLit, 0L -> [norm]
            | NegatedLit, _  -> 
                let u = ( (1L <<< 32) - num64) &&& mask
                [norm ; ( u , NegatedLit)]
         
        let posLits = 
            let checkSub (k,sub) =
                Map.tryFind k okMap
                |> function Some (k,r)-> [(k,r,sub)] | fNone -> []
            substitutes 
            |> List.collect checkSub
            |> List.sortBy (fun (k,r,sub) -> r)


        
        let isZeroNegated = List.exists (function (0L,_,sub) -> sub = NegatedLit | _ -> false) posLits

        match posLits  with
        | [] -> makeDPE <| sprintf "Invalid ARM immediate value %d. Immediates are formed as 32-bit numbers: N ROR (2M), 0 <= N <= 256, 0 <= M <= 15" num
        | _ when isZeroNegated -> Result.Ok (NumberLiteral(0L,0,NoNegLit)) // multiple zero representations
        | [K,rot,sub] -> Result.Ok (NumberLiteral(K,rot,sub))
        | (K,rot,sub) :: _ ->  Result.Ok (NumberLiteral(K,rot,NoNegLit))

    /// apply shift expression to register
    let parseShiftExpression (str : string) reg =
        let getShiftType str = match Map.tryFind str (Map.ofList ["LSL", LSL; "LSR", LSR; "ASR", ASR; "ROR",ROR]) with
                                | Some s -> Ok s
                                | None -> makeDPE <| sprintf "Invalid shift type %s" str
        let makeImmShift shiftType n = RegisterWithShift (reg, shiftType, n)
        let makeRegShift shiftType r = RegisterWithRegisterShift (reg, shiftType, r)

        if str.ToUpper() = "RRX" then
            Ok (RegisterWithRRX reg)
        else
            match getShiftType (str.ToUpper().Substring(0, 3)), str.ToUpper().Substring(3).Trim() with
            | Ok shiftType, imm when imm.StartsWith("#") -> 
                imm.Substring(1) 
                |> parseNumberExpression symTable 
                |> Result.map (makeImmShift shiftType)
            | Ok shiftType, reg -> 
                parseRegister reg 
                |> Result.bind (
                    function | R15 -> makeDPE "Operand 2 cannot be PC or R15 if shift is being used" 
                                | rName -> Ok rName)
                |> Result.map (makeRegShift shiftType)
            | _ -> makeDPE <| sprintf "Invalid flexible op2 shift type %s" str

    match args with
    | [imm] when imm.StartsWith("#") -> 
        imm.Substring(1) 
        |> parseNumberExpression symTable 
        |> Result.map (fun (n:uint32) -> int64 n)
        |> Result.bind makeImmediate
    | [reg] -> 
        parseRegister reg 
        |> Result.map (fun r -> RegisterWithShift (r, LSL, 0u))
    | [regStr ; shiftExpr] -> 
        parseRegister regStr 
        |> Result.bind (parseShiftExpression shiftExpr)
    | [] -> makeDPE "Missing flexible op2"
    | _ -> makeDPE "Too many operands. Valid operand 2 syntaxes consist of: immediate, register, register with shift expression"


/// make an instruction that has the form "XXX op1, flexop2"
let makeTwoOpInstr subMode fn symTable operands =
    match operands with
    | op1 :: op2flex ->
        let op1' = parseRegister op1
        let op2' = parseOp2 subMode symTable op2flex

        match op1', op2' with
        | Ok op1'', Ok op2'' -> Ok (fn op1'' op2'')
        | Error e, _ -> Error e
        | _, Error e -> Error e

    | _ -> makeDPE <|  "Not enough operands: expecting register, flexible operand"

/// make an instruction that has the form "XXX dst, op1, flexop2"
let makeThreeOpInstr subMode fn symTable operands =
    match operands with
    | first :: restOfOps ->
        parseRegister first |> Result.bind (fun reg -> makeTwoOpInstr subMode (fn reg) symTable restOfOps)
    | _ -> makeDPE <| "Not enough operands: expecting register, register, flexible operand"



/// return a ready-to-execute comparison function from the given operands
let makeComparisonInstr subMode compType symTable operands updateFlags =
    makeTwoOpInstr subMode (execComparison compType) symTable operands

/// return a ready-to-execute move function from the given operands
let makeMoveInstr subMode negated symTable operands updateFlags =
    makeTwoOpInstr subMode (execMove negated updateFlags) symTable operands

/// return a ready-to-execute adr function from the given operands     
let makeAdrInstr symTable operands updateFlags =
    // makeThreeOpInstr (execArithmetic [operands.[0];"#"+operands.[1]+"-(PC+8)"] updateFlags) symTable 
    match operands with
    | op1 :: [op2flex] ->
        let op1' = parseRegister op1
        let op2' = parseNumberExpression symTable op2flex

        match op1', op2' with
        | Ok op1'', Ok op2'' -> Ok (execAdr updateFlags op1'' op2'')
        | Error e, _ -> Error e
        | _, Error e -> Error e
    | _ -> makeDPE <| "Not enough operands: expecting register, expression"  

/// return a ready-to-execute arithmetic function from the given operands
let makeArithmeticInstr subMode opType symTable operands updateFlags =
    makeThreeOpInstr subMode (execArithmetic opType updateFlags) symTable operands

/// return a ready-to-execute shift/rotate function from the given operands
let makeShiftInstr shiftType symTable operands updateFlags =
    match operands with
    | [dst ; src ; shiftBy] ->

        match parseRegister dst, parseRegister src with
        | Ok dst', Ok src' ->
            let op2 =
                let makeLitShift n = RegisterWithShift (src', shiftType, n)
                let makeRegShift r = RegisterWithRegisterShift (src', shiftType, r)
                match shiftBy with
                | cons when cons.StartsWith("#") -> cons.Substring(1) |> parseNumberExpression symTable |> Result.map makeLitShift
                | reg -> parseRegister reg |> Result.map makeRegShift

            Result.map (fun op2' -> execMove false updateFlags dst' op2') op2
        | Error e, _ -> Error e
        | _, Error e -> Error e

    | _ -> makeDPE <| "Incorrect number of operands: expecting destination, source, shift value"

/// return a ready-to-execute RRX function from the given operands
let makeRRXInstr symTable operands updateFlags =
    match operands with
    | [dst ; src] ->
        match parseRegister dst, parseRegister src with
        | Ok dst', Ok src' -> Ok (execMove false updateFlags dst' (RegisterWithRRX src'))
        | Error e, _ -> Error e
        | _, Error e -> Error e

    | _ -> makeDPE "Incorrect number of operands: expecting destination, source"

// ///////////// top-level parsing ///////////////////////////////



/// specification for set of instructions
let spec =
            [   "MOV" , fun s a b -> makeMoveInstr InvertedLit false s a b
                "MVN" , fun s a b -> makeMoveInstr InvertedLit true s a b

                "ADD" , fun s a b -> makeArithmeticInstr NegatedLit simADD s a b
                "SUB" , fun s a b -> makeArithmeticInstr NegatedLit simSUB s a b
                "ADC" , fun s a b -> makeArithmeticInstr InvertedLit simADC s a b
                "SBC" , fun s a b -> makeArithmeticInstr InvertedLit simSBC s a b
                "RSB" , fun s a b -> makeArithmeticInstr NoNegLit simRSB s a b
                "RSC" , fun s a b -> makeArithmeticInstr NoNegLit simRSC s a b

                "AND" , fun s a b -> makeArithmeticInstr InvertedLit (simBitwiseLogic (&&&)) s a b
                "ORR" , fun s a b -> makeArithmeticInstr NoNegLit (simBitwiseLogic (|||)) s a b
                "EOR" , fun s a b -> makeArithmeticInstr NoNegLit (simBitwiseLogic (^^^)) s a b
                "BIC" , fun s a b -> makeArithmeticInstr InvertedLit (simBitwiseLogic (fun a b -> a &&& ~~~b)) s a b

                "LSL" , fun s a b -> makeShiftInstr LSL s a b
                "LSR" , fun s a b -> makeShiftInstr LSR s a b
                "ASR" , fun s a b -> makeShiftInstr ASR s a b
                "ROR" , fun s a b -> makeShiftInstr ROR s a b
                "RRX" , fun s a b -> makeRRXInstr s a b

                "CMN" , fun s a b -> makeComparisonInstr NegatedLit simADD s a b
                "CMP" , fun s a b -> makeComparisonInstr NegatedLit simSUB s a b
                "TST" , fun s a b -> makeComparisonInstr NoNegLit (simBitwiseLogic (&&&)) s a b
                "TEQ" , fun s a b -> makeComparisonInstr NoNegLit (simBitwiseLogic (^^^)) s a b
                ]


let dPSpec = {
    InstrC = DP
    Roots = List.map fst spec
    Suffixes = [""; "S"]
}

/// map of all possible opcodes recognised
let opCodes = opCodeExpand dPSpec

/// Active pattern matching to extract undefined label from error string
let (|UndefLabel|_|) (prefix: string) =
    function | Error (code: ErrCode, txt:string, str:string) ->
                if String.startsWith prefix str then
                    str |> String.substring prefix.Length |> Some
                else None
                | _ -> None            

/// main function to parse a line of assembler
/// ls contains the line input
/// and other state needed to generate output
/// the result is None if the opcode does not match
/// otherwise it is Ok Parse or Error (parse error string)
let parse (ls: LineData) : Parse<Instr> option =
    let parse' (instrC, (root,suffix,pCond)) =
        // separate operands
        let operands = ls.Operands.Split(',') |> Array.toList |> List.map String.trim 
            
        // set load address
        let (WA la) = ls.LoadAddr

        // This could be improved
        let pI = 
            {
                PInstr= makeDPE "No instruction parsed in DP"; 
                PLabel = ls.Label |> Option.map (fun lab -> lab, Ok la) ; 
                ISize = 4u; 
                DSize = 0u; 
                PCond = pCond 
            }

        // flags should be updated if S suffix is specified
        // or by default on some instructions
        let updateFlags = match root with
                            | "CMP" | "CMN" | "TST" | "TEQ" -> true
                            | _ when suffix = "S" -> true
                            | _ -> false

        // parse operands
        match (Map.ofList spec).[root] ls.SymTab operands updateFlags with
        | Ok f -> 
            // return parse result with executable line of code
            {pI with PInstr = Executable f}
        | UndefLabel "Undefined label: " uLab ->
            // return parse result with unexecutable line of code due to undefined label         
            {pI with PInstr = makePE ``Undefined symbol`` "" (sprintf "Symbol is %s" uLab)}
        | Error e ->
            // return parse error
            {pI with PInstr = Error e}
    Map.tryFind ls.OpCode opCodes
    |> Option.map parse'

let (|IMatch|_|) ld = parse ld

let executeDP = id

