(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.Memory
    Description: Implement ARM memory instructions
*)

/// Implement ARM memory instructions
module Memory
    open CommonData
    open CommonLex
    open Errors
    open Expressions
    open Helpers

    /// Suffix of LDR and STR instructions
    type MSize =
        | MWord /// LDRB,STRB
        | MByte /// LDR,STR

    type LSType =
        | LOAD /// LDR,LDRB
        | STORE /// STR,STRB

    /// Type of memory instruction
    type MMode =
        | PostIndex /// change base register after memory access
        | PreIndex /// change base register before memory access
        | NoIndex /// no chnage to base register

    type ScaledShiftCode =
        | SASR /// ASR scaling
        | SLSR /// LSR scaling
        | SLSL /// LSL scaling

    /// Single Store/Load memory instruction. LDR, LDRB, STR, STRB
    /// op{type}{cond} Rt, [Rn {, #offset}]        ; immediate offset
    /// op{type}{cond} Rt, [Rn, #offset]!          ; pre-indexed
    /// op{type}{cond} Rt, [Rn], #offset           ; post-indexed
    [<Struct>]
    type InstrMemSingle =
        {
            LSType : LSType
            Rd : RName;
            Rb : RName;
            MAddr : DataPath -> int32 -> int32;
            MemMode : MMode
            MemSize : MSize
        }

    /// Suffixes for LDM and STM
    type MultSuffix =
        | IA | IB | DA | DB
        | FD | ED | FA | EA

    /// Multiple Store/Load memory instruction. LDM, STM
    /// op{addr_mode}{cond} Rn{!}, reglist
    [<Struct>]
    type InstrMemMult = {
        Rn : RName;
        WB : bool
        rList : List<RName>;
        suff : MultSuffix
        }

    type Instr =
        | LDR of InstrMemSingle
        | STR of InstrMemSingle
        | LDM of InstrMemMult
        | STM of InstrMemMult
        | LDREQUAL of RName * uint32


    let memSpecSingle = {
        InstrC = MEM
        Roots = [ "LDR"; "STR" ]
        Suffixes = [ ""; "B" ]
    }

    let memSpecMultiple = {
        InstrC = MEM
        Roots = [ "LDR"; "STR"; "STM"; "LDM" ]
        Suffixes = [ "IA"; "IB"; "DA"; "DB"; "FD"; "ED"; "FA"; "EA" ]
    }


    let memTypeSingleMap =
        Map.ofList [
            "LDR", LDR;
            "STR", STR;
        ]
    let memTypeMultMap =
        Map.ofList [
            "LDM", LDM;
            "STM", STM;
        ]



    /// map of all possible opcodes recognised
    let opCodes =
        let toArr = opCodeExpand >> Map.toArray
        Array.collect toArr [| memSpecSingle; memSpecMultiple |]
        |> Map.ofArray



    /// Contructs an Instruction of InstrMemMult for LDM, STM
    let consMemMult wb reg rLst suffix =
            {
                Rn = reg;
                WB = wb
                rList = rLst;
                suff = suffix;
            }




    /// Where everything happens
    let parse (ls : LineData) : Parse<Instr> option =
        let (WA la) = ls.LoadAddr



        let parseLoad32 pCond =
            let parse =
                Ok((), ls.Operands)
                |> ResREGMATCH(fun _ rn -> rn)
                |> ResREMOVEPREFIX ","
                |> ResREMOVEPREFIX "="
                |> ResExpr(fun rn exp -> rn, exp)
                |> ResCheckDone

            parse
            |> Result.bind (fun (rd, exp) ->
                eval ls.SymTab exp
                |> Result.map (fun lv -> LDREQUAL(rd, lv)))

            |> (fun ins -> copyParse ls ins pCond)
            |> (fun pa -> { pa with PStall = 2 })



        let parseSingle (lsType : LSType) (uRoot : string) (uSuffix : string) pCond : Parse<Instr> =

            let mapOptHeadResult x =
                let f (r, txt) = Result.map x r, txt
                Option.map f

            let mSize =
                match uSuffix with
                | "" -> MWord
                | "B" -> MByte
                | _ -> failwithf "What? Suffix '%s' on LDR or STR is not possible" uSuffix

            /// matches "!" or "" at end of memory instruction
            /// removes initial whitespace
            let (|WRITEBACK|_|) (txt : string) =
                match trim txt with
                | "!" -> Some true
                | "" -> Some false
                | _ -> None

            let (|IMMEXPR|_|) txt =
                match removeWs txt with
                | REMOVEPREFIX "#" (Expr(ast, txt)) ->
                    match eval ls.SymTab ast with
                    | Ok uint32 -> Some(uint32, txt)
                    | _ -> None
                | _ -> None

            let (|ImmExprUnresolved|_|) txt =
                match removeWs txt with
                | REMOVEPREFIX "#" (Expr(ast, txt)) ->
                    match eval ls.SymTab ast with
                    | Error e -> Some e
                    | _ -> None
                | _ -> None

            /// matches the offset part of an addressing mode
            /// removes initial whitespace
            /// "" -> no offset.
            /// ", Rx" -> register offset.
            /// ", #n" -> immediate offset.
            /// ", Rx, XXX #S" -> scaled register offset where XXX = ASR, LSR,LSL.
            /// Returns (offset gen function, restOfTxt)
            let (|OFFSET|_|) (txt : string) =
                let (|SHIFT|_|) (txt : string) =
                    match txt with
                    | REMOVEPREFIXUNCASED "ASR" s -> (SASR, s) |> Some
                    | REMOVEPREFIXUNCASED "LSR" s -> (SLSR, s) |> Some
                    | REMOVEPREFIXUNCASED "LSL" s -> (SLSL, s) |> Some
                    | _ -> None
                let (|COMMA|_|) = (|REMOVEPREFIX|_|) ","
                let (|R|_|) = (|REGMATCH|_|)
                let (|MIMM|_|) immTxt =
                    let memImmBounds =
                        match mSize with
                        | MWord -> 4092, -4092
                        | MByte -> 1023, -1023
                    match memImmBounds, immTxt with
                    | _, IMMEXPR(n, txt) when mSize = MWord && (n % 4u <> 0u) ->
                        (makeParseError "immediate word offset divisible by 4" ("offset=" + (int32 n).ToString()) "ea", txt) |> Some
                    | (bMax, bMin), IMMEXPR(n, txt) when int n <= bMax && int n >= bMin -> (Ok n, txt) |> Some
                    | (bMax, bMin), IMMEXPR(n, txt) ->
                        (makeParseError (sprintf "immediate offset in range %d..%d" bMax bMin) ("offset=" + (int32 n).ToString()) "ea", txt) |> Some
                    | _, ImmExprUnresolved e -> Some(Error e, txt)
                    | _, LITERALNUMB(_, txt) -> (makeFormatError "Numeric offset in LDR/STR must have # prefix (#1000)" immTxt "ea", txt) |> Some
                    | _ -> None
                let (|SHIFTIMM|_|) = function
                    | IMM(n, txt) when int n > 0 && int n < 32 -> (Ok n, txt) |> Some
                    | IMM(n, txt) -> (makeParseError "Scaled register shift must be within range 1..31" ("shift=" + (int32 n).ToString()) "ea", txt) |> Some
                    | _ -> None
                let makeScaled (rx : RName) (shift : ScaledShiftCode) sftAmt =
                    fun (dp : DataPath) ->
                        let x = dp.Regs.[rx]
                        match shift with
                        | SASR -> (int x) >>> int sftAmt
                        | SLSL -> x <<< int sftAmt |> int
                        | SLSR -> x >>> int sftAmt |> int
                match trim txt with
                | "" -> Some(Ok(fun _ -> 0), "")
                | COMMA(R(rx, TRIM s)) when not (s.StartsWith ",") -> (Ok(fun (dp : DataPath) -> int dp.Regs.[rx]), s) |> Some
                | COMMA(MIMM(offset, s)) ->
                    let makeOffsetFunc (offst : uint32) (dp : DataPath) = int offst
                    (Result.map makeOffsetFunc offset, s) |> Some
                | COMMA(R(rx, (TRIM s))) when not (s.StartsWith ",") -> Some(Ok(fun (dp : DataPath) -> int dp.Regs.[rx]), s)
                | COMMA(R(rx, (COMMA(SHIFT(shift, (SHIFTIMM(sftAmt, txt))))))) -> Some(Result.map (makeScaled rx shift) sftAmt, txt)
                | _ -> Some (makeParseError "valid memory offset: ''; 'Ry'; '#N'; 'LSR #N'; 'LSR Rs'"
                                        ("invalid:'" + txt + "'") "list#single-memory-tranfer-instructions", txt)
                // include base register value as second parameter of returned function
                |> mapOptHeadResult (fun fo dp rbv -> fo dp + rbv)

            match ls.Operands with
            | REGMATCH(rd, (REMOVEPREFIX "," txt)) ->
                match PreIndex, PostIndex, txt with
                | indexType, _, BRACKETED '[' ']' (REGMATCH(rb, (OFFSET(spf, ""))), WRITEBACK w)
                | _, indexType, BRACKETED '[' ']' ((REGMATCH(rb, ""), OFFSET(spf, WRITEBACK w))) ->
                    let mode =
                        match w, indexType with
                        | false, PreIndex -> Ok NoIndex
                        | true, PostIndex -> makeParseError "Valid addressing mode" (" '!' is not valid in post-increment addressing:'" + txt + "'") "ea"
                        | _, it -> Ok it
                    match mode, spf with
                    | _, Error e -> Error e
                    | Error e, _ -> Error e
                    | Ok mode', Ok spf' ->
                        Ok {
                            LSType = lsType
                            Rd = rd
                            Rb = rb
                            MAddr = spf'
                            MemMode = mode'
                            MemSize = match uSuffix with | "B" -> MByte | _ -> MWord
                        }
                | _ -> makeParseError "LDR/STR Effective address" txt "list#single-register-memory-transfer-instructions"
            | _ -> makeParseError "LDR/STR register name" ls.Operands "list#single-register-memory-transfer-instructions"
            |> fun ins -> copyParse ls (Result.map memTypeSingleMap.[uRoot] ins) pCond
            |> (fun pa -> { pa with PStall = match uRoot with | "LDR" -> 2 | "STR" -> 1 | _ -> 0 })


        /// parse for LDM, STM
        let parseMult (root : string) suffix pCond : Parse<Instr> =

            /// For matching the list of regs
            let (|RegListMatch|_|) (str : string) =
                match str.ToUpper() with
                | ParseRegex "([rR][0-9]+|PC|LR|SP)}" address -> address |> Some
                | ParseRegex "\[([rR][0-9]+|PC|LR|SP)" address -> address |> Some
                | _ -> None

            /// Regex match the numbers in a hyphen list {r1 - r7}
            /// in order to construct full reg list.
            /// return the two numbers as low, high
            let (|RegListExpand|_|) (str : string) =
                match str.ToUpper() with
                | ParseRegex2 "(R[0-9]+|PC|LR|SP)-(R[0-9]+|PC|LR|SP)" (low, high) when regNames.ContainsKey low && regNames.ContainsKey high ->
                    (regNums.[regNames.[low]], regNums.[regNames.[high]]) |> Some
                | _ -> None

            /// Matches the registers
            let (|RegListMatch|_|) (str : string) =
                /// nice function to make register names from the
                /// high and low values
                /// {r2-r7} -> 2, 7 -> R2,R3,R4,R5,R6,R7
                let optionNumToRegList n =
                    match n with
                    | RegListExpand(low, high) ->
                        let fullRegList = List.map (fun r -> r |> makeRegFn) [ int low..int high ]
                        fullRegList |> Some
                    | _ -> None

                let optionMakeList n =
                    [ n ] |> Some

                match str.ToUpper() with
                | ParseRegex "(([rR][0-9]+|PC|LR|SP)-([rR][0-9]+|PC|LR|SP))" listReg -> optionNumToRegList listReg
                | ParseRegex "([rR][0-9]+|PC|LR|SP)!" bangReg -> bangReg |> optionMakeList
                | ParseRegex "([rR][0-9]+|PC|LR|SP)" reg -> reg |> optionMakeList
                | _ -> None

            /// split the operands at a {
            let splitMult = splitAny ls.Operands '{'

            let checkMultSuffix = function
                | "IA" -> IA |> Ok
                | "IB" -> IB |> Ok
                | "DA" -> DA |> Ok
                | "DB" -> DB |> Ok
                | "FD" -> FD |> Ok
                | "ED" -> ED |> Ok
                | "FA" -> FA |> Ok
                | "EA" -> EA |> Ok
                | "" -> IA |> Ok
                | x -> makeParseError "Valid LDM/STM suffix IA,IB,DA,DB,FD,ED,FA,EA" x "list#multiple-register-memory-transfer-instructions"

            let ops =
                match true, false, ls.Operands with
                | wb, _, REGMATCH(rOp1, (REMOVEPREFIX "!" (REMOVEPREFIX "," (BRACKETED '{' '}' (rl, TRIM "")))))
                | _, wb, REGMATCH(rOp1, (REMOVEPREFIX "," (BRACKETED '{' '}' (rl, TRIM "")))) ->

                    let regList = splitAny rl ','

                    let matcher = function
                        | RegListMatch x -> x
                        | x -> [ x ]

                    let checker = function
                        | RegCheck x -> x
                        | _ -> failwith alwaysMatchesFM

                    let checkedRegs =
                        regList
                        |> List.collect matcher
                        |> List.map checker
                        |> condenseResultList (id)
                        |> Result.bind (fun lst ->
                            let makeListError wanted = makeParseError wanted (String.concat "," regList) ""
                            let lst' = List.distinct lst
                            if lst.Length <> lst'.Length
                            then makeListError "Register list without duplicates"
                            elif List.contains rOp1 lst then makeListError <| sprintf "Register list not containing index register '%A'" rOp1
                            else Ok lst')
                        |> Result.map (List.distinct >> List.sortBy (fun rn -> rn.RegNum))


                    checkedRegs
                    |> Result.map (consMemMult wb rOp1)
                    |> mapErrorApplyResult (checkMultSuffix suffix)
                | _ ->
                    makeParseError "valid LDM/STM operands" ls.Operands "list#single-register-memory-transfer-instructions"

            copyParse ls (Result.map memTypeMultMap.[root] ops) pCond
            |> (fun pa -> { pa with PStall = let rootStall = match root with | "LDM" -> 1 | _ -> 0
                                             match ops with | Ok o -> (max 1 o.rList.Length) + rootStall | _ -> 0 })

        let parse' (_instrC, (root : string, suffix : string, pCond)) =
            let uRoot = root.ToUpper()
            let uSuffix = suffix.ToUpper()
            let singleSuffix = match uSuffix with | "" | "B" -> true | _ -> false
            match singleSuffix, uRoot with
            | true, "LDR" when String.exists ((=) '=') ls.Operands && uSuffix = ""
                -> parseLoad32 pCond
            | true, "LDR" -> parseSingle LOAD uRoot uSuffix pCond
            | true, "STR" -> parseSingle STORE uRoot uSuffix pCond
            | false, "LDM" -> parseMult uRoot uSuffix pCond
            | false, "STM" -> parseMult uRoot uSuffix pCond
            | _ -> failwithf "What? We appear to have an impossible memory root and suffix: %s %s" root suffix

        Map.tryFind (uppercase ls.OpCode) opCodes
        |> Option.map parse'

    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse

//*******************************************************************************************
//                                      Memory execution
//*******************************************************************************************

    let getDataMemWord (ef : uint32) (dp : DataPath) =
        if ef % 4u <> 0u then
            ``Run time error`` (ef, (sprintf "word address %d must be divisible by 4" ef)) |> Error
        else
            match Map.tryFind (WA ef) dp.MM with
            | Some CodeSpace ->
                ``Run time error`` (ef, (sprintf "Memory read of code memory location %x is not allowed" ef)) |> Error
            | Some(Dat d) -> Ok d
            | None -> Ok 0u

    let getDataMemByte (ef : uint32) (dp : DataPath) =
        let wordA = (ef &&& 0xFFFFFFFCu)
        let bitOffset = (ef &&& 0x3u) * 8u |> int
        getDataMemWord wordA dp
        |> Result.map (fun w -> (w >>> bitOffset) &&& 0xFFu)

    let executeLDRSTR (ins : InstrMemSingle) (dp : DataPath) =
        let addr = ins.MAddr dp (int32 dp.Regs.[ins.Rb]) |> uint32
        let rbv = dp.Regs.[ins.Rb]
        let ef =
            match ins.MemMode with
            | NoIndex
            | PreIndex -> uint32 addr
            | PostIndex -> rbv
        //printfn "Executing Rb=%A(%x) addr = %x  ef = %x" ins.Rb dp.Regs.[ins.Rb] addr ef
        dp
        |> updateReg (uint32 (if ins.MemMode = NoIndex then (match ins.Rb with | R15 -> rbv - 4u | _ -> rbv) else addr >>> 0)) ins.Rb
        |> (fun dp ->
                match ins.LSType with
                | LOAD ->
                     match ins.MemSize with
                     | MWord -> (getDataMemWord ef dp)
                     | MByte -> (getDataMemByte ef dp)
                     |> Result.map (fun dat -> updateReg dat ins.Rd dp)
                | STORE ->
                    match ins.MemSize with
                    | MWord -> updateMemData (Dat dp.Regs.[ins.Rd]) ef dp
                    | MByte -> updateMemByte (dp.Regs.[ins.Rd] |> byte) ef dp)

    let offsetList start suffix rl wb isLDM =
        let rec makeOffsetList inlst outlist incr start =
            match inlst with
            | _ :: tail -> (start + incr) |> makeOffsetList tail (start :: outlist) incr
            | [] -> outlist
        let lst, rDiff =
            match suffix with
            | IA -> 0, 4
            | IB -> 4, 4
            | DA -> 0, -4
            | DB -> -4, -4
            | _ ->
                match suffix, isLDM with
                | FD, true | EA, false -> 0, 4
                | ED, true | FA, false -> 4, 4
                | FA, true | ED, false -> 0, -4
                | EA, true | FD, false -> -4, -4
                | _ -> failwithf "What? Cannot happen!"
            |> fun (st, chg) ->
                makeOffsetList rl [] chg (start + st)
                |> if chg < 0 then id else List.rev
                |> (fun lst -> lst, ( if wb then chg * lst.Length else 0)

        ) List.map (fun el -> el |> uint32) lst, rDiff

    let executeMem instr (cpuData : DataPath) =
        /// get multiple memory
        let rec getMemMult addrList contentsLst cpuData =
            match addrList with
            | head :: tail ->
                let addedVal = (getDataMemWord head cpuData) :: contentsLst
                getMemMult tail addedVal cpuData
            | [] -> contentsLst |> List.rev

        let executeLDM wb suffix rn rl cpuData =
            let baseAddrInt = (cpuData.Regs.[rn]) |> int32
            let lst, rEnd = offsetList baseAddrInt suffix rl wb true
            let contents = getMemMult lst [] cpuData
            let condensedContents = condenseResultList (id) contents
            Result.map (fun conts -> setMultRegs rl conts cpuData) condensedContents
            |> Result.map (fun dp -> setReg rn ((dp.Regs.[rn] + uint32 rEnd) &&& 0xFFFFFFFFu) dp)

        let executeSTM wb suffix rn rl cpuData =
            let getReg rn = cpuData.Regs.[rn]
            let baseAddrInt = (getReg rn) |> int32
            let regContentsList = List.map (getReg) rl
            let lst, rEnd = offsetList baseAddrInt suffix rl wb false
            setMultMem (regContentsList |> List.map Dat) lst cpuData
            |> Result.map (fun dp -> setReg rn ((dp.Regs.[rn] + uint32 rEnd) &&& 0xFFFFFFFFu) dp)

        match instr with
        | LDR ins | STR ins -> executeLDRSTR ins cpuData
        | LDM operands ->
            executeLDM operands.WB operands.suff operands.Rn operands.rList cpuData
        | STM operands ->
            executeSTM operands.WB operands.suff operands.Rn operands.rList cpuData
        | LDREQUAL(rn, loadVal) -> setReg rn loadVal cpuData |> Ok
