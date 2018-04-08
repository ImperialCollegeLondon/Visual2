module Memory
    open CommonData    
    open CommonLex
    open Errors
    open Expressions
    open Helpers

   

    // *********** //
    // LDR AND STR //
    // *********** //

    /// either a number or a register
    type OffsetType =
        | ImmPre of uint32
        | RegPre of RName
    
    /// Address consisting of register for base
    /// and an pre index offset of OffsetType
    /// being either a register or number
    [<Struct>]
    type Address = 
        {
            addrReg: RName; 
            offset: Option<OffsetType>;
        }
    
    /// post index
    /// either register or number
    /// ldr r0, [r1], PostIndex
    type PostIndex =
        | ImmPost of uint32
        | RegPost of RName
    
    /// Suffix of LDR and STR instructions
    type SingleSuffix = 
        | B

    /// Single Store/Load memory instruction. LDR, LDRB, STR, STRB
    /// op{type}{cond} Rt, [Rn {, #offset}]        ; immediate offset
    /// op{type}{cond} Rt, [Rn, #offset]!          ; pre-indexed
    /// op{type}{cond} Rt, [Rn], #offset           ; post-indexed
    [<Struct>]
    type InstrMemSingle = 
        {
            Rn: RName;
            addr: Address;
            postOffset: Option<PostIndex>;
            suff: Option<SingleSuffix>
        }
    
    // *********** //
    // LDM AND STM //
    // *********** //

    /// Suffixes for LDM and STM
    type MultSuffix = 
        | IA | IB | DA | DB
        | FD | ED | FA | EA

    /// Multiple Store/Load memory instruction. LDM, STM
    /// op{addr_mode}{cond} Rn{!}, reglist
    [<Struct>]
    type InstrMemMult = {Rn: RName; rList: List<RName>; suff: Option<MultSuffix>}

    type Instr = 
        | LDR of InstrMemSingle
        | STR of InstrMemSingle
        | LDM of InstrMemMult
        | STM of InstrMemMult


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

    /// Contructs an Instruction of InstrMemSingle for LDR, STR
    let consMemSingle reg mem preoffset postoffset suffix = 
            {
                Rn = reg; 
                addr = {addrReg = mem; offset = preoffset};
                postOffset = postoffset;
                suff = suffix;
            }
    
    /// Contructs an Instruction of InstrMemMult for LDM, STM
    let consMemMult reg rLst suffix =
            {
                Rn = reg;
                rList = rLst;
                suff = suffix;
            }
    
    /// A partially active pattern that returns an error if a register argument is not valid.
    let (|RegCheck|_|) txt =
        match Map.tryFind txt regNames with
        | Some reg ->
            reg |> Ok |> Some
        | _ ->
            (txt, notValidRegEM)
            ||> makePE ``Invalid register``
            |> Some

    /// Where everything happens
    let parse (ls: LineData) : Parse<Instr> option =
        let (WA la) = ls.LoadAddr
        /// Partial Active pattern for matching regexes
        /// Looking for something like [r1] or [r13
        /// For matching the address location 
        let (|MemMatch|_|) str =
            match str with 
            | ParseRegex "\[([rR][0-9]+)\]" address -> address |> makeReg |> Ok |> Some
            | ParseRegex "\[([rR][0-9]+)" address -> address |> makeReg |> Ok |> Some
            | _ -> 
                ("["+str+"]", notValidRegEM)
                ||> makePE ``Invalid register`` 
                |> Some

        /// For matching the list of regs
        let (|RegListMatch|_|) str =
            match str with 
            | ParseRegex "([rR][0-9]+)}" address -> address |> Some
            | ParseRegex "\[([rR][0-9]+)" address -> address |> Some
            | _ -> None
        
        /// Partial active pattern for matching both pre and post indexes
        /// e.g. str r0, [r1], r2   str r0, [r1], #4
        /// e.g. str r0, [r1, r2]   str r0, [r1, #4]
        /// e.g. str r0, [r1, r2]!  str r0, [r1, #4]!
        let (|OffsetMatch|_|) str =

            /// Register Post Index, No Pre Index
            /// e.g. str r0, [r1], r2
            let regNoPrePost = function
                | RegCheck r ->
                    match r with
                    | Ok r' -> 
                        let postInd = RegPost r' |> Some
                        let preInd = None
                        (preInd, postInd) |> Ok
                    | Error e -> Error e
                | _ -> failwith alwaysMatchesFM

            /// Register Pre Index, No Post Index
            /// e.g. str r0, [r1, r2]
            let regPreNoPost = function
                | RegCheck r ->
                    match r with
                    | Ok r' -> 
                        let postInd = None
                        let preInd = RegPre r' |> Some
                        (preInd, postInd) |> Ok
                    | Error e -> Error e
                | _ -> failwith alwaysMatchesFM

            
            /// Register Post Index and Pre Index
            /// e.g. str r0, [r1, r2]!
            let regPreAndPost = function
                | RegCheck r -> 
                    match r with
                    | Ok r' ->
                        let postInd = RegPost r' |> Some
                        let preInd = RegPre r' |> Some
                        (preInd, postInd) |> Ok
                    | Error e -> Error e
                | _ -> failwith alwaysMatchesFM

            /// Immediate Post Index, No Pre Index
            /// e.g. str r0, [r1], #4
            let immNoPrePost n =
                let postInd = ImmPost (uint32 n) |> Some
                let preInd = None
                (preInd, postInd) |> Ok
            
            /// Immediate Pre Index, No Post Index
            /// e.g. str r0, [r1, #4]
            let immPreNoPost n = 
                let postInd = None
                let preInd = ImmPre (uint32 n) |> Some
                (preInd, postInd) |> Ok
            
            /// Immediate Pre and Post Index
            /// e.g. str r0, [r1, #4]!
            let immPreAndPost n =
                let postInd = ImmPost (uint32 n) |> Some
                let preInd = ImmPre (uint32 n) |> Some
                (preInd, postInd) |> Ok


            match str with 
            | ParseRegex "([rR][0-9]+)" preOffReg -> preOffReg |> regNoPrePost |> Some
            | ParseRegex "([rR][0-9]+)\]" preOffReg -> preOffReg |> regPreNoPost |> Some
            | ParseRegex "([rR][0-9]+)\]!" preOffReg -> preOffReg |> regPreAndPost |> Some
            | ParseRegex "#0[xX]([0-9a-fA-F]+)" preOffHex -> ("0x" + preOffHex) |> immNoPrePost |> Some
            | ParseRegex "#([0-9]+)" preOffDec -> preOffDec |> immNoPrePost |> Some
            | ParseRegex "#&([0-9a-fA-F]+)" preOffHex -> ("0x" + preOffHex) |> immNoPrePost |> Some
            | ParseRegex "#0[bB]([0-1]+)" preOffBin -> ("0b" + preOffBin) |> immNoPrePost |> Some
            | ParseRegex "#0[xX]([0-9a-fA-F]+)\]" preOffHex -> ("0x" + preOffHex) |> immPreNoPost |> Some
            | ParseRegex "#([0-9]+)\]" preOffDec -> preOffDec |> immPreNoPost |> Some
            | ParseRegex "#&([0-9a-fA-F]+)\]" preOffHex -> ("0x" + preOffHex) |> immPreNoPost |> Some
            | ParseRegex "#0[bB]([0-1]+)\]" preOffBin -> ("0b" + preOffBin) |> immPreNoPost |> Some
            | ParseRegex "#0[xX]([0-9a-fA-F]+)\]!" preOffHex -> ("0x" + preOffHex) |> immPreAndPost |> Some
            | ParseRegex "#([0-9]+)\]!" preOffDec -> preOffDec |> immPreAndPost |> Some
            | ParseRegex "#&([0-9a-fA-F]+)\]!" preOffHex -> ("0x" + preOffHex) |> immPreAndPost |> Some
            | ParseRegex "#0[bB]([0-1]+)\]!" preOffBin -> ("0b" + preOffBin) |> immPreAndPost |> Some
            | _ -> 
                (str, notValidOffsetEM)
                ||> makePE ``Invalid offset``
                |> Some

        /// parse for LDM, STM
        let parseMult (root: string) suffix pCond : Parse<Instr> =

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
                match splitMult with
                | [rOp1; rlst] -> // LDM, STM
                    let regList = splitAny (rlst.Replace("}", "")) ','
                    let reg = rOp1.Replace(",", "")

                    let matcher = function
                        | RegListMatch x -> x 
                        | _ -> []
                    
                    let checker = function
                        | RegCheck x -> x
                        | _ -> failwith alwaysMatchesFM

                    let rec applyToAll f list =
                        match list with
                        | [] -> []
                        | head :: tail -> f head :: applyToAll f tail

                    let allRegs = regList |> applyToAll matcher |> List.concat
                    let checkedRegs = 
                        allRegs
                        |> (applyToAll checker) 
                        |> condenseResultList (id)
                    match reg with
                    | RegCheck r' -> 
                        combineErrorMapResult r' checkedRegs consMemMult
                        |> mapErrorApplyResult (checkMultSuffix suffix)
                    | _ -> failwith alwaysMatchesFM     
                | _ ->
                    (ls.Operands, notValidFormatEM)
                    ||> makePE ``Invalid instruction``

            copyParse ls (Result.map memTypeMultMap.[root] ops) pCond

        let parseSingle (root: string) suffix pCond : Parse<Instr> =         

            /// split operands at ','
            let splitOps = splitAny ls.Operands ','

            let checkSingleSuffix = function
                | "B" -> Some B |> Ok
                | "" -> None |> Ok
                | _ -> 
                    (suffix, notValidSuffixEM)
                    ||> makePE ``Invalid suffix``
               
            
            let ops =
                match splitOps with
                | [rOp1; addr] -> // str r0, [r1] or str r0, [r1, #4]
                    match rOp1 with
                    | RegCheck rOp1' ->  
                        match addr with
                        | MemMatch addr' ->
                            let partialConsMem = combineErrorMapResult rOp1' addr' consMemSingle
                            partialConsMem
                            |> mapErrorApplyResult (None |> Ok)
                            |> mapErrorApplyResult (None |> Ok)
                            |> mapErrorApplyResult (checkSingleSuffix suffix)
                        | _ -> failwith alwaysMatchesFM
                    | _ -> failwith alwaysMatchesFM
                | [rOp1; addr; postOff] -> // str r0, [r1], #4
                    match rOp1 with
                    | RegCheck rOp1' ->
                        match addr with
                        | MemMatch addr' ->
                            match postOff with
                            | OffsetMatch tuple ->
                                let partialConsMem = combineErrorMapResult rOp1' addr' consMemSingle
                                partialConsMem
                                |> mapErrorApplyResult (Result.map (fst) tuple)
                                |> mapErrorApplyResult (Result.map (snd) tuple)
                                |> mapErrorApplyResult (checkSingleSuffix suffix)
                            | _ -> failwith alwaysMatchesFM
                        | _ -> failwith alwaysMatchesFM
                    | _ -> failwith alwaysMatchesFM
                | _ -> 
                    (ls.Operands, notValidFormatEM)
                    ||> makePE ``Invalid instruction``
     
            copyParse ls (Result.map memTypeSingleMap.[root] ops) pCond

        let parse' (_instrC, (root : string,suffix : string,pCond)) =
            let uRoot = root.ToUpper()
            let uSuffix = suffix.ToUpper()
            match root.ToUpper() with
            | "LDR" -> parseSingle uRoot uSuffix pCond
            | "STR" -> parseSingle uRoot uSuffix pCond
            | "LDM" -> parseMult uRoot uSuffix pCond
            | "STM" -> parseMult uRoot uSuffix pCond
            | _ -> failwith "What? We appear to have an impossible root"
           

        Map.tryFind (uppercase ls.OpCode) opCodes
        |> Option.map parse'

    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|)  = parse


//*******************************************************************************************
// Branch execution
//*******************************************************************************************


    let executeMem instr (cpuData: DataPath) =

        let regContents r = cpuData.Regs.[r]

        /// check if word address is valid and multiple of 4
        let (|Valid|_|) (input: uint32) = 
            if input % word = 0u 
            then Valid |> Some
            else None
        
        let getOffsetType o =
            match o with
            | Some (ImmPre i) -> i
            | Some (RegPre r) -> regContents r
            | None -> 0u
        
        let getPostIndex i =
            match i with 
            | Some (ImmPost i) -> i
            | Some (RegPost r) -> regContents r
            | None -> 0u
        
        /// check if valid, if so return Word Address
        let wordAddress a = 
            match a with
            | Valid -> WA a |> Ok
            | _ -> 
                (a, " is not a valid word address.")
                |> ``Run time error``
                |> Error
        
        /// make an offset list for ldm and stm by recursively
        /// adding an incr to the address for the length of the list
        let rec makeOffsetList inlst outlist incr start = 
            match inlst with
            | _ :: tail -> (start + incr) |> makeOffsetList tail (start :: outlist) incr
            | [] -> outlist
 
        /// STRB to set the correct byte
        let setCorrectByte value addr = 
            let shift = 8u * (addr % word) |> int32
            (value &&& 0x000000FFu) <<< shift

        /// Check if B suffix is presesnt on STR
        let setWordOrByte suffix (value: uint32) addr (cpuData: DataPath) = 
            match suffix with
            | Some B -> updateMemByte (value |> byte) addr cpuData
            | None -> updateMemData (Dat value) addr cpuData

        /// Check if B suffix is presesnt on LDR
        // let getWordOrByte suffix value addr = 
        //     match suffix with
        //     | Some B -> getCorrectByte value addr
        //     | None -> value
        
        let getWordOrByte suffix reg addr (cpuData: DataPath) = 
            match suffix with
            | Some B -> fetchMemByte reg addr cpuData
            | None -> fetchMemData reg addr cpuData
    
        /// get memory stored a address check its not in code segment 
        let getMem addr cpuData = 
            match addr with
            | x when (x < minAddress) ->
                (x, " Trying to access memory where instructions are stored.")
                |> ``Run time error``
                |> Error
            | _ -> 
                let wordAddr = WA addr
                match locExists wordAddr cpuData with
                | true -> 
                    match getMemLoc wordAddr cpuData with
                    | Dat dl ->
                        dl |> Ok
                    | CodeSpace -> 
                        (addr, " Trying to access memory where instructions are stored.")
                        |> ``Run time error``
                        |> Error
                | false -> 0u |> Ok
        
        /// get multiple memory 
        let rec getMemMult addrList contentsLst cpuData = 
            match addrList with
            | head :: tail ->
                let addedVal = (getMem head cpuData) :: contentsLst
                getMemMult tail addedVal cpuData
            | [] -> contentsLst |> List.rev
        
        // let executeLDR suffix rn addr offset cpuData = 
        //     let address = (regContents addr.addrReg + getOffsetType addr.offset)
        //     let alignedAddr = alignAddress address
        //     let contents = getMem alignedAddr cpuData
        //     let value = getWordOrByte suffix contents (regContents addr.addrReg + getOffsetType addr.offset)
        //     let newCpuData = setReg rn value cpuData
        //     setReg addr.addrReg (regContents addr.addrReg + getPostIndex offset) newCpuData
        
        let executeLDR suffix rn addr offset (cpuData: DataPath) = 
            let address = (regContents addr.addrReg + getOffsetType addr.offset)
            let cpuData' = getWordOrByte suffix rn address cpuData
            Result.map (setReg addr.addrReg (regContents addr.addrReg + getPostIndex offset)) cpuData'
            // setReg addr.addrReg (regContents addr.addrReg + getPostIndex offset) cpuData'
                
        let executeSTR suffix rn addr offset (cpuData: DataPath) = 
            let address = (regContents addr.addrReg + getOffsetType addr.offset)
            let cpuData' = setWordOrByte suffix (regContents rn) address cpuData
            Result.map (setReg addr.addrReg (regContents addr.addrReg + getPostIndex offset)) cpuData'

        let executeLDM suffix rn rl cpuData =
            let offsetList start = 
                let lst =
                    match suffix with
                    | None ->
                         start
                        |> makeOffsetList rl [] 4
                        |> List.rev      
                    | Some IA -> 
                        start
                        |> makeOffsetList rl [] 4
                        |> List.rev
                    | Some IB -> 
                        (start + 4)
                        |> makeOffsetList rl [] 4
                        |> List.rev
                    | Some DA -> 
                        start
                        |> makeOffsetList rl [] -4
                    | Some DB ->
                        (start - 4) 
                        |> makeOffsetList rl [] -4
                    | Some FD ->
                        start
                        |> makeOffsetList rl [] 4
                        |> List.rev
                    | Some ED ->
                        (start + 4)
                        |> makeOffsetList rl [] 4
                        |> List.rev
                    | Some FA ->
                        start
                        |> makeOffsetList rl [] -4
                    | Some EA ->
                        (start - 4) 
                        |> makeOffsetList rl [] -4
                List.map (fun el -> el |> uint32) lst
            let baseAddrInt = (regContents rn) |> int32
            let contents = getMemMult (offsetList baseAddrInt) [] cpuData
            let condensedContents = condenseResultList (id) contents
            Result.map (fun conts -> setMultRegs rl conts cpuData) condensedContents

        let executeSTM suffix rn rl cpuData = 
            let offsetList start = 
                let lst =
                    match suffix with
                    | None ->
                        start
                        |> makeOffsetList rl [] 4
                        |> List.rev      
                    | Some IA -> 
                        start
                        |> makeOffsetList rl [] 4
                        |> List.rev
                    | Some IB -> 
                        (start + 4)
                        |> makeOffsetList rl [] 4
                        |> List.rev
                    | Some DA -> 
                        start
                        |> makeOffsetList rl [] -4
                    | Some DB ->
                        (start - 4) 
                        |> makeOffsetList rl [] -4
                    | Some EA ->
                        start
                        |> makeOffsetList rl [] 4
                        |> List.rev
                    | Some FA ->
                        (start + 4)
                        |> makeOffsetList rl [] 4
                        |> List.rev
                    | Some ED ->
                        start
                        |> makeOffsetList rl [] -4
                    | Some FD ->
                        (start - 4) 
                        |> makeOffsetList rl [] -4
                List.map (fun el -> el |> uint32) lst
            let baseAddrInt = (regContents rn) |> int32
            let regContentsList = List.map regContents rl
            setMultMem (regContentsList |> List.map Dat) (offsetList baseAddrInt) cpuData

        let executeInstr (instr: Instr) (cpuData: DataPath) = 
            match instr with
            | LDR operands -> 
                executeLDR operands.suff operands.Rn operands.addr operands.postOffset cpuData
            | STR operands ->
                executeSTR operands.suff operands.Rn operands.addr operands.postOffset cpuData
            | LDM operands ->
                executeLDM operands.suff operands.Rn operands.rList cpuData
            | STM operands ->
                executeSTM operands.suff operands.Rn operands.rList cpuData

        executeInstr instr cpuData



