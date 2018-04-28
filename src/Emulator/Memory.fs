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
            LSType: LSType
            Rd: RName;
            Rb: RName;
            MAddr: DataPath -> int32 -> int32;
            MemMode: MMode
            MemSize: MSize
        }

    /// Suffixes for LDM and STM
    type MultSuffix = 
        | IA | IB | DA | DB
        | FD | ED | FA | EA

    /// Multiple Store/Load memory instruction. LDM, STM
    /// op{addr_mode}{cond} Rn{!}, reglist
    [<Struct>]
    type InstrMemMult = {
        Rn: RName; 
        WB: bool
        rList: List<RName>; 
        suff: Option<MultSuffix>
        }

    type Instr = 
        | LDR of InstrMemSingle
        | STR of InstrMemSingle
        | LDM of InstrMemMult
        | STM of InstrMemMult
        | LDREQUAL of RName * uint32


    let memSpec = {
        InstrC = MEM
        Roots = ["LDR";"STR";"STM";"LDM"]
        Suffixes = [""; "B";"IA";"IB";"DA";"DB";"FD";"ED";"FA";"EA"]
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
    let opCodes = opCodeExpand memSpec


    
    /// Contructs an Instruction of InstrMemMult for LDM, STM
    let consMemMult wb reg rLst suffix =
            {
                Rn = reg;
                WB = wb
                rList = rLst;
                suff = suffix;
            }
 
    let makeRegError txt =
            (txt, notValidRegEM)
            ||> makePE ``Invalid register``


    /// A partially active pattern that returns an error if a register argument is not valid.
    let (|RegCheck|_|) txt =
        match Map.tryFind txt regNames with
        | Some reg ->
            reg |> Ok |> Some
        | _ -> Some <| makeRegError txt

    ///
    let (|REGMATCH|_|) (txt:string) =
        match txt with
        | ParseRegex2 @"\s*([rR][0-9]+|PC|SP|LR)(.*$)" (txt,TRIM rst) -> 
            match Map.tryFind (txt.ToUpper()) regNames with
            | Some rn -> (Some (rn, rst))
            | None -> None
        | _ -> None          

    /// Where everything happens
    let parse (ls: LineData) : Parse<Instr> option =
        let (WA la) = ls.LoadAddr


        let parseLoad32 pCond : Parse<Instr> =
            match ls.Operands with
            | REGMATCH(rd, ( REMOVEPREFIX "," (REMOVEPREFIX "=" (Expr (exp,""))))) -> 
                eval ls.SymTab exp 
                |> Result.map (fun r -> rd,r)
            | _ -> makePE ``Invalid literal`` ls.Operands "Invalid operands for LDR Rn, ="
            |> Result.map (fun (rd, lv) -> LDREQUAL ( rd, lv))
            |> (fun ins -> copyParse ls ins pCond)

            

        let parseSingle (lsType: LSType) (uRoot: string) (uSuffix:string) pCond : Parse<Instr> = 

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
            let (|WRITEBACK|_|) (txt:string) =
                match trim txt with
                | "!" -> Some true
                | "" -> Some false
                | _ -> None

            /// matches the offset part of an addressing mode
            /// removes initial whitespace
            /// "" -> no offset.
            /// ", Rx" -> register offset.
            /// ", #n" -> immediate offset.
            /// ", Rx, XXX #S" -> scaled register offset where XXX = ASR, LSR,LSL.
            /// Returns (offset gen function, restOfTxt)
            let (|OFFSET|_|) (txt:string) =
                let (|SHIFT|_|) txt =
                    match txt with
                    | REMOVEPREFIX "ASR" s->  (SASR, s) |> Some
                    | REMOVEPREFIX "LSR" s -> (SLSR, s) |> Some
                    | REMOVEPREFIX "LSL" s -> (SLSL, s) |> Some
                    | _ -> None
                let (|COMMA|_|) = (|REMOVEPREFIX|_|) ","
                let (|R|_|) = (|REGMATCH|_|)
                let (|MIMM|_|) txt =
                    let memImmBounds =
                        match mSize with
                        | MWord -> 4088 + 8, -4096 + 8
                        | MByte -> 1023 + 8, -1024 + 8
                    match memImmBounds,txt with
                    | (bMax,bMin), IMM (n,txt) when int n <= bMax && int n >= bMin -> (Ok n, txt)  |> Some
                    | (bMax,bMin), IMM (n,txt) -> 
                        (makePE ``Invalid offset`` txt (sprintf "%s immediate offset must be in range %d..%d" (uRoot+uSuffix) bMax bMin), txt) |> Some
                    | _ -> None
                let (|SHIFTIMM|_|) = function
                    | IMM (n,txt) when int n > 0 && int n < 32 -> (Ok n,txt) |> Some
                    | IMM (_,txt) -> (makePE ``Invalid literal`` txt "Scaled register shift must be within raneg 1..31", txt) |> Some
                    | _ -> None
                let makeScaled (rx:RName) (shift:ScaledShiftCode) sftAmt =
                    fun (dp: DataPath)  ->
                        let x = dp.Regs.[rx]
                        match shift with
                        | SASR -> (int x) >>> int sftAmt
                        | SLSL -> x <<< int sftAmt |> int
                        | SLSR -> x >>> int sftAmt |> int
                match trim txt with
                | "" -> Some ( Ok (fun _  -> 0) , "")
                | COMMA (R (rx, TRIM s)) when not (s.StartsWith ",") -> (Ok (fun (dp:DataPath) -> int dp.Regs.[rx]), s) |> Some
                | COMMA( MIMM (offset,s)) -> 
                    let makeOffsetFunc (offst:uint32) (dp:DataPath) = int offst
                    (Result.map makeOffsetFunc offset, s) |> Some
                | COMMA ( R (rx, (TRIM s))) when not (s.StartsWith ",") -> Some (Ok (fun (dp:DataPath) -> int dp.Regs.[rx]),s)
                | COMMA (R (rx, ( COMMA( SHIFT (shift,(SHIFTIMM (sftAmt,txt))))))) -> Some (Result.map (makeScaled rx shift) sftAmt,txt)
                | _ -> Some (makePE ``Invalid second operand`` txt "Can't parse memory offset", txt)
                // include base register value as second parameter of returned function
                |> mapOptHeadResult (fun fo -> fun dp rbv -> fo dp + rbv)

                


            let makeError txt =
                (txt, notValidOffsetEM)
                ||> makePE ``Invalid offset``
                

            match ls.Operands with
            | REGMATCH(rd, ( REMOVEPREFIX "," txt)) -> 
                //printfn "LDRSTR matching %s" txt
                match PreIndex, PostIndex, txt with 
                | indexType, _, BRACKETED '[' ']' (REGMATCH( rb, (OFFSET(spf,""))) ,  WRITEBACK w)
                | _, indexType, BRACKETED '[' ']' ((REGMATCH( rb,""), OFFSET(spf, WRITEBACK w))) -> 
                    let mode =
                        match w, indexType with 
                        | false, PreIndex -> Ok NoIndex
                        | true, PostIndex -> makePE ``Invalid second operand`` txt "Post-increment addressing does not have a '!'"
                        | _, it -> Ok it 
                    match mode,spf with
                    | Error e, _ -> Error e
                    | _, Error e -> Error e
                    | Ok mode', Ok spf' ->
                        Ok {
                            LSType = lsType
                            Rd = rd
                            Rb = rb
                            MAddr = spf'
                            MemMode =  mode'                   
                            MemSize = match uSuffix with "B" -> MByte | _ -> MWord
                        }
                | _ -> makeError txt
            | _ -> printfn "No Rd found"; makeError ls.Operands
            |> fun ins -> copyParse ls (Result.map memTypeSingleMap.[uRoot] ins) pCond

        /// parse for LDM, STM
        let parseMult (root: string) suffix pCond : Parse<Instr> =

            /// For matching the list of regs
            let (|RegListMatch|_|) str =
                match str with 
                | ParseRegex "([rR][0-9]+)}" address -> address |> Some
                | ParseRegex "\[([rR][0-9]+)" address -> address |> Some
                | _ -> None

            /// Regex match the numbers in a hyphen list {r1 - r7}
            /// in order to construct full reg list.
            /// return the two numbers as low, high
            let (|RegListExpand|_|) str =
                match str with
                | ParseRegex2 "[rR]([0-9]+)-[rR]([0-9]+)" (low, high) -> (low, high) |> Some
                | _ -> None

            /// Matches the registers
            let (|RegListMatch|_|) str =
                /// nice function to make register names from the 
                /// high and low values
                /// {r2-r7} -> 2, 7 -> R2,R3,R4,R5,R6,R7
                let optionNumToRegList n = 
                    match n with
                    | RegListExpand (low, high) -> 
                        let fullRegList = List.map (fun r -> r |> makeRegFn) [int low..int high]
                        fullRegList |> Some
                    | _ -> None
                
                let optionMakeList n = 
                    [n] |> Some

                match str with
                | ParseRegex "(([rR][0-9]+)-([rR][0-9]+))" listReg -> optionNumToRegList listReg
                | ParseRegex "([rR][0-9]+)!" bangReg -> bangReg |> optionMakeList
                | ParseRegex "([rR][0-9]+)" reg -> reg |> optionMakeList
                | _ -> None

            /// split the operands at a {
            let splitMult = splitAny ls.Operands '{'
            
            let checkMultSuffix = function
                | "IA" -> Some IA |> Ok
                | "IB" -> Some IB |> Ok
                | "DA" -> Some DA |> Ok
                | "DB" -> Some DB |> Ok
                | "FD" -> Some FD |> Ok
                | "ED" -> Some ED |> Ok
                | "FA" -> Some FA |> Ok
                | "EA" -> Some EA |> Ok
                | ""   -> Some IA |> Ok
                | _ -> 
                    (suffix, notValidSuffixEM)
                    ||> makePE ``Invalid suffix``

            let ops = 
                match true, false, ls.Operands with
                | wb, _, REGMATCH (rOp1, ( REMOVEPREFIX "!" (REMOVEPREFIX "," (BRACKETED '{' '}' (rl,TRIM "")))))
                | _, wb, REGMATCH (rOp1, (REMOVEPREFIX "," (BRACKETED '{' '}' (rl,TRIM "")))) ->

                    let regList = splitAny rl ','

                    let matcher = function
                        | RegListMatch x -> x 
                        | _ -> []
                    
                    let checker = function
                        | RegCheck x -> x
                        | _ -> failwith alwaysMatchesFM

                    let checkedRegs = 
                        regList
                        |> List.collect matcher
                        |> List.map checker
                        |> condenseResultList (id)
                        |> Result.map (List.sortBy (fun rn -> rn.RegNum))


                    checkedRegs 
                    |> Result.map (consMemMult wb rOp1 )
                    |> mapErrorApplyResult (checkMultSuffix suffix)    
                | _ ->
                    (ls.Operands, notValidFormatEM)
                    ||> makePE ``Invalid instruction``

            copyParse ls (Result.map memTypeMultMap.[root] ops) pCond

        let parse' (_instrC, (root : string,suffix : string,pCond)) =
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
    let (|IMatch|_|)  = parse

//*******************************************************************************************
//                                      Memory execution
//*******************************************************************************************

    let getDataMemWord (ef:uint32) (dp:DataPath) =
        if ef % 4u <> 0u then 
            ``Run time error`` (ef, (sprintf "word address %d must be divisible by 4" ef)) |> Error
        else
            match Map.tryFind (WA ef) dp.MM with
            | Some CodeSpace -> 
                ``Run time error`` (ef, (sprintf "Can't read a code memory location %x" ef)) |> Error
            | Some (Dat d) -> Ok d
            | None -> Ok 0u
    
    let getDataMemByte (ef:uint32) (dp:DataPath) =
        let wordA = (ef &&& 0xFFFFFFFCu)
        let bitOffset = (ef &&& 0x3u)*8u |> int
        getDataMemWord wordA dp
        |> Result.map (fun w -> (w >>> bitOffset) &&& 0xFFu)

    let executeLDRSTR (ins:InstrMemSingle) (dp:DataPath) =
        let addr = ins.MAddr dp (int32 dp.Regs.[ins.Rb]) |> uint32
        let rbv = dp.Regs.[ins.Rb]
        let ef = 
            match ins.MemMode with
            | NoIndex 
            | PreIndex -> uint32 addr
            | PostIndex -> rbv
        dp
        |> updateReg (uint32 (if ins.MemMode = NoIndex then rbv else addr &&& 0xFFFFFFFFu)) ins.Rb
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

    let executeMem instr (cpuData: DataPath) =
        
        /// get multiple memory 
        let rec getMemMult addrList contentsLst cpuData = 
            match addrList with
            | head :: tail ->
                let addedVal = (getDataMemWord head cpuData) :: contentsLst
                getMemMult tail addedVal cpuData
            | [] -> contentsLst |> List.rev

        /// make an offset list for ldm and stm by recursively
        /// adding an incr to the address for the length of the list
        let rec makeOffsetList inlst outlist incr start = 
            match inlst with
            | _ :: tail -> (start + incr) |> makeOffsetList tail (start :: outlist) incr
            | [] -> outlist
        
        let executeLDM wb suffix rn rl cpuData =
            let offsetList start = 
                let lst, rDiff =
                    match suffix with
                    | None | Some IA -> 0, 4
                    | Some IB -> 4, 4
                    | Some DA -> 0, -4
                    | Some DB -> -4, -4
                    | Some FD -> 0, 4
                    | Some ED -> 4, 4
                    | Some FA -> 0, -4
                    | Some EA -> -4, -4
                    |> fun (st,chg) -> 
                        makeOffsetList rl [] chg (start + st)
                        |> if chg < 0 then id else List.rev
                        |> (fun lst -> lst, if wb then chg * lst.Length else 0)

                List.map (fun el -> el |> uint32) lst, rDiff
            let baseAddrInt = (cpuData.Regs.[rn]) |> int32
            let lst, rEnd = offsetList baseAddrInt
            let contents = getMemMult lst [] cpuData
            let condensedContents = condenseResultList (id) contents
            Result.map (fun conts -> setMultRegs rl conts cpuData) condensedContents
            |> Result.map (fun dp -> setReg rn ((dp.Regs.[rn] + uint32 rEnd) &&& 0xFFFFFFFFu) dp)

        let executeSTM wb suffix rn rl cpuData = 
            let getReg rn = cpuData.Regs.[rn]
            let offsetList start = 
                let lst,rDiff =
                    match suffix with
                    | None | Some IA -> 0, 4
                    | Some IB ->  4, 4
                    | Some DA -> 0, -4
                    | Some DB -> -4, -4
                    | Some EA -> 0, 4
                    | Some FA -> 4, 4
                    | Some ED -> 0, -4
                    | Some FD -> -4, -4
                    |> fun (st, chg) ->  
                         makeOffsetList rl [] chg (start+st)
                         |> (if chg < 0 then id else List.rev)
                         |> (fun lst -> lst, if wb then chg * lst.Length else 0)

                List.map (fun el -> el |> uint32) lst, rDiff
            let baseAddrInt = (getReg rn) |> int32
            let regContentsList = List.map (getReg) rl
            let lst, rEnd = offsetList baseAddrInt
            setMultMem (regContentsList |> List.map Dat) lst cpuData
            |> Result.map (fun dp -> setReg rn ((dp.Regs.[rn] + uint32 rEnd) &&& 0xFFFFFFFFu) dp)
 

        match instr with
        | LDR ins | STR ins -> executeLDRSTR ins cpuData
        | LDM operands ->
            executeLDM operands.WB operands.suff operands.Rn operands.rList cpuData
        | STM operands ->
            executeSTM operands.WB operands.suff operands.Rn operands.rList cpuData
        | LDREQUAL( rn, loadVal) -> setReg rn loadVal cpuData |> Ok
