(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.Helpers
    Description: Functions used in various emulator instruction handling modules
*)

// NB instruction modules were originally written independently and this shows.
// more refactoring with good common code put here would improve quality

/// Emulator Helper functions
module Helpers
    open CommonData
    open System.Text.RegularExpressions
    open CommonLex
    open Errors
    open Expressions

    let pipeShow mess x = printfn "%s:%A" mess x; x


    /// Visuals apparent minimum data address may be useful?
    let minAddress = 0x100u
    /// Word Length 4 bytes
    let word = 0x4u
    /// The Old Chris QuickPrint classic from tick 1
    let qp item = printfn "%A" item
    let qpl lst = List.map (qp) lst



    /// Partial Active pattern for regexes.
    /// Returns the 1st () element in the group
    let (|ParseRegex|_|) (regex : string) (str : string) =
       let m = Regex("^" + regex + "[\\s]*" + "$").Match(str)
       if m.Success
       then Some(m.Groups.[1].Value)
       else None

    /// Partial Active pattern for regexes.
    /// Returns the 1st and 2nd () elements in the group
    let (|ParseRegex2|_|) (regex : string) (str : string) =
       let m = Regex("^" + regex + "[\\s]*" + "$").Match(str)
       if m.Success && m.Groups.Count >= 2
       then Some(m.Groups.[1].Value, m.Groups.[2].Value)
       else None


    /// makes a reg
    let makeReg r = regNames.[r]
    /// Takes a number and converts it into a reg string
    let makeRegFn = (string >> (+) "R") // needed elsewhere

    /// makes a RName from a number
    let makeRegFromNum r =
        r |> makeRegFn |> makeReg

    /// Check if reg is valid by seeing if key is in map
    let regValid r =
        Map.containsKey r regNames

    /// Check all regs in lst are valid
    let regsValid rLst =
        rLst
        |> List.fold (fun b r -> b && (regValid r)) true

    let uppercase (x : string) = x.ToUpper()

    let trim (x : string) = x.Trim()

    /// Split an input string at the provided charater.
    /// Return as a string list.
    let splitAnyKeepSpaces (str : string) char =
        str.Split [| char |]
        //|> Array.map (id)
        |> List.ofArray


    /// Split an input string at the provided charater.
    /// Return as a string list with spaces removed
    let splitAny (str : string) char =
        let nospace = str.Replace(" ", "")
        splitAnyKeepSpaces nospace char

    let (|TRIM|_|) (x : string) = Some(x.Trim())


    /// match bracketed item, ignoring initial space
    /// return (insideBracketsText, textAfterBrackets)
    /// force text after brackets to be "", or start with ' ' or '\t' or '!'
    /// NB - not sure if this is needed...
    /// since bracketed items must have separators
    let (|BRACKETED|_|) bra ket txt =

        let hasSeparator = function
        | "" -> true // in this case no separator is needed
        | s when List.contains s.[0] [ ' '; '\t'; ','; '!' ] -> true
        | _ -> false

        match splitAnyKeepSpaces txt bra with
        | (TRIM "") :: afterBra :: rest ->
            match splitAnyKeepSpaces afterBra ket with
            | inside :: after when hasSeparator <| String.concat (ket.ToString()) after ->
                Some(inside |> trim, String.concat (ket.ToString()) after |> trim)
            | _ -> None
        | _ -> None


    let (|SPLITCOMMAS|_|) txt = Some(splitAnyKeepSpaces txt ',')
    let (|SPLITSPACES|_|) txt = Some(splitAnyKeepSpaces txt ' ')

    /// match and remove specified string, ignoring initial space
    /// return everything after the string
    let (|REMOVEPREFIX|_|) (prefix : string) (txt : string) =
        let trimTxt = trim txt
        if EEExtensions.String.startsWith prefix trimTxt then
            Some trimTxt.[prefix.Length..trimTxt.Length - 1]
        else None

    /// match and remove specified string, ignoring initial space
    /// return everything after the string
    /// The match is case insensitive
    let (|REMOVEPREFIXUNCASED|_|) (prefix : string) (txt : string) =
        let trimTxt = trim txt
        if EEExtensions.String.startsWith (prefix.ToUpper()) (trimTxt.ToUpper()) then
            Some trimTxt.[prefix.Length..trimTxt.Length - 1]
        else None


    /// unusually this match requires no initial space
    let (|LITERALNUMB|_|) txt =
        match txt with
        | Expressions.RegexPrefix "0[xX][0-9a-fA-F]+" (num, rst)
        | Expressions.RegexPrefix "0[bB][0-1]+" (num, rst)
        | Expressions.RegexPrefix "[0-9]+" (num, rst) ->
            try
                ((uint32 (num.ToLower())) >>> 0, rst) |> Some
            with
                | e -> failwithf "Exception in Expr: uint32(%A)" num
        | Expressions.RegexPrefix "&[0-9a-fA-F]+" (num, rst) ->
            ("0x" + (num.Trim()).[1..] |> uint32, rst) |> Some
        | _ -> None


    /// match a #n immediate, ignoring space
    /// no check on size of immediate
    let (|IMM|_|) (txt : string) =
        match trim txt with
        | REMOVEPREFIX "#" (LITERALNUMB(n, txt)) -> Some(n, txt)
        | REMOVEPREFIX "#-" (LITERALNUMB(n, txt)) -> Some(uint32 ((0 - (int n)) &&& 0xFFFFFFFF), txt)
        | _ -> None




    let checkValid2 opList =
        match opList with
        | h1 :: h2 :: _ when (regsValid [ h1; h2 ]) -> true
        | _ -> false


    /// A partially active pattern to check validity of a register passed as a string,
    /// and return an `RName` if it is valid.
    let (|RegMatch|_|) txt =
        match Map.tryFind ((trim txt).ToUpper()) regNames with
        | Some reg ->
            reg |> Ok |> Some
        | _ ->
            None

    /// A partially active pattern that returns an error if a register argument is not valid.
    let (|RegCheck|_|) (txt : string) =
        match Map.tryFind ((trim txt).ToUpper()) regNames with
        | Some reg ->
            reg |> Ok |> Some
        | _ -> makeParseError "register name" txt "" |> Some

    /// A partilly active pattern to extract a register name, returning it paired with the rest of the string, if possible
    let (|REGMATCH|_|) (txt' : string) =
        match txt' with
        | ParseRegex2 @"\s*([rR][0-9]+|[Pp][Cc]|[Ss][Pp]|[Ll][Rr]|[Pp][Cc]|[Ss][Pp]|[Ll][Rr])(.*$)" (txt, TRIM rst) ->
            match Map.tryFind (txt.ToUpper()) regNames with
            | Some rn -> (Some(rn, rst))
            | None -> None
        | _ -> None

    /// <summary> Convert a partial active pattern function into a function that operates on a Result<AstSoFar*string, E'> monad.
    /// Pipelining the output functions makes AP failure at any stage throw a monadic error.</summary>
    /// <param name=ap> active pattern style function that operates on a txt input to parse something </param>
    /// <param name=needed> string describing the text or construct needed for successful parse </param>
    /// <param name=adapt> converts astSoFar, and the output of ap, to the astSoFar passed out </param>
    let resultify ap needed adapt resTxt =
        let (|AP|_|) txt = ap txt
        match resTxt with
        | Ok(ast, AP(ast', txt)) -> Ok(adapt ast ast', txt)
        | Ok(_, txt) -> makeParseError needed txt ""
        | Error e -> Error e





    /// version of APs that always match and return a Result monad
    let ResExpr adapt rTxt = resultify Expressions.``|Expr|_|`` "a numeric expression" adapt rTxt
    let ResREGMATCH adapt rTxt = resultify (|REGMATCH|_|) "a register name" adapt rTxt
    let ResREMOVEPREFIX prefix rTxt =
        resultify ((|REMOVEPREFIX|_|) prefix
        >> Option.map (fun txt -> (), txt)) ("'" + prefix + "'") (fun r _ -> r) rTxt
    let ResCheckDone x = Result.bind (function | r, "" -> Ok r | _, txt -> makeFormatError "Error: unexpected characters found at end of instruction" txt "") x

    /////////////// parsing functions ///////////////////////////////

    let parseNumberExpression (symTable) (str : string) =
        parseEvalNumericExpression symTable str

    let isValidNumericExpression symTable str =
        match parseEvalNumericExpression symTable str with
        | Ok _ -> true
        | _ -> false

    let parseRegister (str : string) =
        match Map.tryFind (str.ToUpper().Trim()) regNames with
        | Some r -> Ok r
        | None -> makeParseError "valid register name" str ""

    let isRegister (str : string) =
        match Map.tryFind (str.ToUpper().Trim()) regNames with
        | Some r -> true
        | None -> false

//********************************************************************************
//
//                          EXECUTION HELPERS
//
//********************************************************************************
    let setPCOffset = 8u - 4u // from pipelining + increment after instruction adjustments

    /// Function for setting a register
    /// Takes RName and value and returns
    /// new DataPath with that register set.
    /// correctly adjusts to set real value of PC during execution
    let setReg reg contents cpuData =
        let adjContents =
            match reg with
            | R15 -> contents + setPCOffset
            | _ -> contents
        { cpuData with Regs = Map.add reg (adjContents >>> 0) cpuData.Regs }

    let setRegRaw reg contents cpuData =
        { cpuData with Regs = Map.add reg contents cpuData.Regs }

    /// Recursive function for setting multiple registers
    /// Need to check that the lists provided are the same length
    let rec setMultRegs regLst contentsLst cpuData =
        match regLst, contentsLst with
        | rhead :: rtail, chead :: ctail when (List.length regLst = List.length contentsLst) ->
            let newCpuData = setReg rhead chead cpuData
            setMultRegs rtail ctail newCpuData
        | [], [] -> cpuData
        | _ -> failwith "Lists given to setMultRegs function were of different sizes."


    /// gets real PC value during instruction execution
    /// compensates for +8 pipelining
    let getPC (cpuData : DataPath) =
        cpuData.Regs.[R15] - 8u
    /// sets PC during execution from real value
    /// correctly adjusts for pipelining and increment after instruction
    let setPC (addr : uint32) (cpuData : DataPath) =
        setReg R15 addr cpuData

    let getMemLoc addr cpuData =
        match Map.containsKey addr cpuData.MM with
        | true -> cpuData.MM.[addr]
        | false -> failwithf "Attempted lookup:addr=%A, MM=%A" addr cpuData.MM

    let locExists m cpuData =
        Map.containsKey m cpuData.MM

    /// Tom's condExecute instruction as he made it first (don't reinvent the wheel)
    let condExecute (cond : Condition) (cpuData : DataPath) =
        let n, c, z, v = (cpuData.Fl.N, cpuData.Fl.C, cpuData.Fl.Z, cpuData.Fl.V)
        match cond with
        | Cal -> true
        | Cnv -> false
        | Ceq -> z
        | Cne -> (not z)
        | Chs -> c
        | Clo -> (not c)
        | Cmi -> n
        | Cpl -> (not n)
        | Cvs -> v
        | Cvc -> (not v)
        | Chi -> (c && not z)
        | Cls -> (not c || z)
        | Cge -> (n = v)
        | Clt -> (n <> v)
        | Cgt -> (not z && (n = v))
        | Cle -> (z || (n <> v))

    /// Return a new datapath with reg rX set to value
    let updateReg value rX dp =
        setReg rX value dp

    let validateWA addr =
        match addr % word with
        | 0u -> true
        | _ -> false


    // Update the whole word at addr with value in dp
    // let updateMem value (addr : uint32) dp =
    //     match addr % 4u with
    //     | 0u -> {dp with MM = Map.add (WA addr) value dp.MM}
    //     | _ -> failwithf "Trying to update memory at unaligned address"

    let updateMemData value (a : uint32) (dp : DataPath) =
        let addr = a >>> 0
        match value with
        | Data.Dat d -> if d > 0xFFFFFFFFu then failwithf "What? Bad data value %A" d
        | _ -> ()
        match validateWA addr with
        | false ->
            (addr, " Trying to update memory at unaligned address.")
            |> ``Run time error``
            |> Error
        | true ->
        match Map.tryFind (WA addr) dp.MM with
        | None
        | Some(Dat _) -> Ok { dp with MM = Map.add (WA addr) value dp.MM }
        | Some CodeSpace ->
            (addr, " Updating a byte in instruction memory space.")
            |> ``Run time error``
            |> Error


    /// Return the next aligned address after addr
    let alignAddress addr = (addr / word) * word

    /// Update a single byte in memory (Little Endian)
    // let updateMemByte (value : byte) (addr : uint32) dp =
    //     let baseAddr = alignAddress (addr)
    //     let shft = (int ((addr % 4u)* 8u))
    //     let mask = 0xFFu <<< shft |> (~~~)
    //     let oldVal =
    //         match Map.containsKey (WA baseAddr) dp.MM with
    //         | true -> dp.MM.[WA baseAddr]
    //         | false -> DataLoc 0u // Uninitialised memory is zeroed
    //     let newVal =
    //         match oldVal with
    //         | DataLoc x -> (x &&& mask) ||| ((uint32 value) <<< shft)
    //         | _ -> failwithf "Updating byte at instruction address"
    //     updateMem (DataLoc newVal) baseAddr dp

    let updateMemByte (value : byte) (a : uint32) (dp : DataPath) =
        let addr = a >>> 0
        let baseAddr = alignAddress addr
        let shft = (int ((addr % word) * 8u))
        let mask = 0xFFu <<< shft |> (~~~)
        let oldVal =
            match Map.containsKey (WA baseAddr) dp.MM with
            | true -> dp.MM.[WA baseAddr]
            | false -> Dat 0u // Uninitialised memory is zeroed
        if (value |> uint32) &&& 0xFFFFFF00u <> 0u then
            failwithf "Bad byte value:%d" (value |> uint32)
        match oldVal with
        | Dat x ->
            let newVal = (x &&& mask) ||| ((uint32 value) <<< shft)
            updateMemData (Dat newVal) baseAddr dp
        | CodeSpace ->
            (addr, " Updating a byte in instruction memory space.")
            |> ``Run time error``
            |> Error

   /// LDRB to load the correct byte
    let getCorrectByte value addr =
        let shift = 8u * (addr % word) |> int32
        ((0x000000FFu <<< shift) &&& value) >>> shift

    let fetchMemData reg addr (cpuData : DataPath) =
        match validateWA addr with
        | true ->
            match addr with
            | a when (a < minAddress) ->
                (a, " Trying to access memory where instructions are stored.")
                |> ``Run time error``
                |> Error
            | _ ->
                let baseAddr = alignAddress addr
                let wordAddr = WA baseAddr
                match locExists wordAddr cpuData with
                | true ->
                    match getMemLoc wordAddr cpuData with
                    | Dat dl ->
                        setReg reg dl cpuData |> Ok
                    | CodeSpace ->
                        (addr, " Trying to access memory where instructions are stored.")
                        |> ``Run time error``
                        |> Error
                | false -> setReg reg 0u cpuData |> Ok
        | false ->
            (addr, " Trying to update memory at unaligned address.")
            |> ``Run time error``
            |> Error

    let fetchMemByte reg addr (cpuData : DataPath) =
        let baseAddr = alignAddress addr
        let wordAddr = WA baseAddr
        match locExists wordAddr cpuData with
        | true ->
            match getMemLoc wordAddr cpuData with
            | Dat dl ->
                let byteValue = getCorrectByte dl addr
                setReg reg byteValue cpuData |> Ok
            | CodeSpace ->
                (addr, " Trying to access memory where instructions are stored.")
                |> ``Run time error``
                |> Error
        | false ->
            // (addr |> string, " You have not stored anything at this address, value is set to 0.")
            // ||> makeError
            // |> ``Run time warning``
            // |> Error
            setReg reg 0u cpuData |> Ok

    /// Recursive function for storing multiple values at multiple memory addresses
    /// Need to check that the lists provided are the same length
    let rec setMultMem contentsLst addrLst cpuData : Result<DataPath, ExecuteError> =
        match addrLst, contentsLst with
        | mhead :: mtail, chead :: ctail when (List.length addrLst = List.length contentsLst) ->
            let newCpuData = updateMemData chead mhead cpuData
            Result.bind (setMultMem ctail mtail) newCpuData
        | [], [] -> cpuData |> Ok
        | _ -> failwith "Lists given to setMultMem function were of different sizes."



    let fillRegs (vals : uint32 list) =
        List.zip [ 0..15 ] vals
        |> List.map (fun (r, v) -> (register r, v))
        |> Map.ofList

    let emptyRegs =
        [ 0..15 ]
        |> List.map (fun _ -> 0u)
        |> fillRegs

    let initialDp() = {
            Fl = { N = false; C = false; Z = false; V = false };
            Regs = emptyRegs;
            MM = Map.ofList []
        }
