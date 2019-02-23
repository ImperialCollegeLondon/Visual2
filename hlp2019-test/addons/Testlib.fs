(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.TestLib
    Description: Low-level library types and functions for implementing file-based testbenches
*)

module TestLib

open EEExtensions
open Helpers
open CommonData
open ExecutionTop

type TbCheckResult = { Actual : uint32; Check : tbCheck; Spec : TbSpec }

type TestResult = {
    TestCoverage : int Set
    TestCycles : int64
    TestMsgs : string list
    TestLines : string list
    TestOk : bool
    TestNum : int
    TestName : string
    }

let maxTestLength = 100000L

let parseTbLine lNum (lin : string) =

    let apcsRegs rLst =
        let rnd = System.Random()
        rLst
        |> List.map (fun rn -> rn, rnd.Next(-100, 100) |> uint32)

    let (|RESOLVEREGS|_|) lst =
        let ops =
            String.concat " " lst
            |> String.splitString [| "," |]
            |> Array.map String.trim
            |> Array.toList
        let parseL = List.map parseRegister ops
        let regs = List.okList parseL
        if regs.Length = parseL.Length then Some regs else None

    let (|RESOLVE|_|) lst =
        let ops =
            String.concat " " lst
            |> String.splitString [| "," |]
            |> Array.map String.trim
            |> Array.toList
        let parseLiteral = function | LITERALNUMB(lit, "") -> [ lit ] | _ -> []
        let parseL = List.map parseLiteral ops
        if List.exists ((=) []) parseL then None
        else
            List.concat parseL |> Some
    /// Active pattern converts a string to uppercase - for case-insensitive matches
    let (|UPPER|_|) (name : string) = Some(name.ToUpper())
    let (|Defs|_|) words =
        match words with
        | [ RegMatch(Ok rn); UPPER "IS"; LITERALNUMB(lit, "") ] -> (TbRegEquals(lNum, rn, lit)) |> Some
        | RegMatch(Ok rn) :: UPPER "PTR" :: RESOLVE lits -> (TbRegPointsTo(lNum, rn, 0u, lits)) |> Some
        | _ -> None
    let commentStrippedLine =
        match String.split [| ';' |] lin |> Array.toList with
        | [] -> ""
        | lin :: _ -> lin.Trim()
    match commentStrippedLine |> String.splitOnWhitespace |> Array.toList with
    | [ "" ] -> []
    | UPPER "IN" :: Defs tbSpec -> [ Ok(TbIn, tbSpec) ]
    | UPPER "OUT" :: Defs tbSpec -> [ Ok(TbOut, tbSpec) ]
    | UPPER "STACKPROTECT" :: _ -> [ Ok(TbOut, TbStackProtected 0u) ]
    | UPPER "DATAAREA" :: LITERALNUMB(lit, "") :: _ -> [ Ok(TbIn, TbSetDataArea lit) ]
    | UPPER "PERSISTENTREGS" :: RESOLVEREGS regs ->
        let regVals = apcsRegs regs
        [ Ok(TbIn, APCS regVals); Ok(TbOut, APCS regVals) ]
    | [ UPPER "RANDOMISEINITVALS" ] -> [ Ok(TbIn, RandomiseInitVals) ]
    | [ UPPER "BRANCHTOSUB"; subName ] -> [ Ok(TbIn, BranchToSub subName) ]
    | [ UPPER "RELABEL"; symName; newSymName ] -> [ Ok(TbIn, Relabel(symName, newSymName)) ]
    | [ UPPER "APPENDCODE"; num ] -> [
        System.Int32.TryParse num
        |> function | false, _ -> Error(lNum, "AppendCode expects an integer representing the code block (#BLOCK) to append")
                    | true, num -> Ok(TbIn, AddCode(num, []))
      ]
    | _ ->
        [ Error(lNum, "Parse Error in testbench") ]

/// Return pseudo-random register values for input seed./// Use own generator for deterministic compatiblity F# and FABLE
let calcInitRands seed =
    let ms seed =
        let state = ref seed
        (fun (_ : unit) ->
            state := (214013 * !state + 2531011) &&& System.Int32.MaxValue
            !state / (1 <<< 16))
    let rnd = ms seed
    [ 0..15 ] |> List.map (fun n -> register n, ((rnd() % 1000) |> uint32)) |> Map.ofList




/// Make the initial dataPath (containing test inputs) from image data and Test.
/// test: the Test.
/// lim: the initial LoadImage created from the memory image of the program being tested.
/// Return result because a test requiring a subroutine may fail at this stage.
let initTestDP (mm : DataMemory, symTab : Expressions.SymbolTable) test =
    let initRands = calcInitRands test.TNum
    let setRegRandom dp rn =
        Map.add rn initRands.[rn] dp
    let dp = {
                Fl = {
                        C = false
                        V = false
                        N = false
                        Z = false
                    };
                Regs = initialRegMap;
                MM = mm
             }
    let rMap' = { dp with Regs = Map.add R13 test.InitSP dp.Regs }
    let ldSpecs = [
                    test.Ins
                    |> List.sortBy (function | APCS _ | RandomiseInitVals -> 0 | _ -> 1)
                    |> List.map (fun sp -> TbIn, sp)

                    test.Outs
                    |> List.map (fun sp -> TbOut, sp)
                  ] |> List.concat
    let addSpec dp (inout, spec) =
        match inout, spec with
        | TbOut, TbRegEquals(_, rn, u) -> Ok dp
        | TbIn, TbRegEquals(_, rn, u) -> Ok { dp with Regs = Map.add rn u dp.Regs }
        | tbio, TbRegPointsTo(_, rn, start, uLst) ->
            let mm' = ExecutionTop.addWordDataListToMem start dp.MM (uLst |> List.map Dat)
            let dp' = Map.add rn start dp.Regs
            Ok { dp with Regs = dp'; MM = (match tbio with | TbIn -> mm' | TbOut -> dp.MM) }
        | _, TbStackProtected _u -> Ok dp
        | TbIn, APCS rLst ->
            let rm = List.fold (fun regs (rn, u) -> setRegRandom regs rn) dp.Regs rLst
            Ok { dp with Regs = rm }
        | _, TbSetDataArea u -> Ok dp
        | _, RandomiseInitVals ->
            Ok { dp with Regs = List.fold setRegRandom dp.Regs [ R0; R1; R2; R3; R4; R5; R6; R7; R8; R9; R10; R11; R12 ] }
        | _, BranchToSub subName ->
            let subAddr = Map.tryFind subName symTab
            match subAddr with
            | None -> Error(sprintf "Can't find subroutine '%s' required by test specification" subName)
            | Some subA ->
                dp
                |> Helpers.setRegRaw R15 subA
                |> Ok
        | _, _ -> Ok dp
    let addSpecBound dpRes spec = match dpRes with | Ok dp -> addSpec dp spec | e -> e
    List.fold addSpecBound (Ok rMap') ldSpecs
    |> Result.map (setReg R14 0xFFFFFFFCu)



/// Process test specs in textual order linking allocated data addresses, and reordering specs where needed
/// Initstack - stack initial value.
/// Start - current data address for allocation
let linkSpecs initStack start specs =
    let addSpec (start, linkedSpecs) (inOut, spec) =
        match spec with
        | TbRegPointsTo(lNum, rn, _start, uLst) ->
            let rnOtherPntOpt = List.collect (function
                                              | _, TbRegPointsTo(_, rn', pnt, _) when rn' = rn -> [ pnt ]                                                      
                                              | _ -> []) linkedSpecs
            let start', pnt' =
                match rnOtherPntOpt with
                | pnt :: _ -> start, pnt
                | [] -> start + 4u * (uint32 uLst.Length), start
            start', (inOut, TbRegPointsTo(lNum, rn, pnt', uLst)) :: linkedSpecs
        | TbStackProtected _ -> start, (inOut, TbStackProtected initStack) :: linkedSpecs
        | TbSetDataArea u -> u, (inOut, TbSetDataArea u) :: linkedSpecs
        // no linkage required for these specs
        | TbRegEquals _
        | APCS _
        | RandomiseInitVals _
        | AddCode _
        | Relabel _
        | BranchToSub _ -> start, (inOut, spec) :: linkedSpecs
    specs
    |> List.fold addSpec (start, [])
    |> snd
    |> List.rev
    |> (fun spL -> spL)
            //let regs = initTestDp (Map.empty,Map.empty) spL

/// Parse lines defining a single test.
/// Return Result is Test object, or list of line numbers and error messages.
let parseOneTest initStack dataStart testNum testName lines =

    let checkRes, tbLines =
        lines
        |> List.partition (snd >> String.trim >> String.startsWith ">>")

    let testLines, testErrors =
        tbLines
        |> List.collect (fun (i, lin) -> parseTbLine (i + 1) lin)
        |> List.splitResult

    match testErrors with
    | [] ->
        let linkedLines = linkSpecs initStack dataStart testLines
        {
            TNum = testNum;
            TName = String.trim testName
            TAppendCode = []
            TRelabelSymbols = []
            Ins = List.collect (function | (TbIn, x) -> [ x ] | _ -> []) linkedLines
            Outs = List.collect (function | (TbOut, x) -> [ x ] | _ -> []) linkedLines
            CheckLines = (checkRes |> List.map snd)
            InitSP = initStack
            TestLines = lines |> List.map snd
        }
        |> (fun test -> // use final register input values to set PERSISTENTREGS checks
                initTestDP (Map.empty, Map.empty)
                           { test with Ins = List.collect (function | BranchToSub _ -> [] | sp -> [ sp ]) test.Ins }
                |> Result.map (fun dp ->
                    let setAPCSVals = List.map (fun (rn, uv) -> rn, dp.Regs.[rn])
                    let outs' = List.map (function | APCS rvL -> APCS(setAPCSVals rvL) | spec -> spec) test.Outs
                    { test with Outs = outs' })
                |> Result.mapError (fun s ->
                    printfn "initDP failed with: %s" s
                    failwithf "What? Should never get error from early eval of dp"))
    | errors -> Error errors

/// Parse lines defining a single code block.
/// Return Result is Test object.
let parseOneBlock blockNum blockName lines =
    match lines with
    | [ n, _hdr ] -> Error <| [ n, sprintf "Code block %d must have at least one line!" blockNum ]
    | _ ->
        {
            TNum = blockNum;
            TName = String.trim blockName
            TAppendCode = lines |> List.map snd
            TRelabelSymbols = []
            Ins = []
            Outs = []
            CheckLines = []
            InitSP = 0u
            TestLines = lines |> List.map snd
        } |> Ok



/// Parse testbench file returning as result list of Tests or errors
let parseTests initStack dStart lines =
    let parseChunk initStack dStart chunk =
        List.head chunk
        |> snd
        |> String.splitOnWhitespace
        |> Array.toList
        |> (function | "#TEST" :: LITERALNUMB(n, testName) :: _ ->
                         parseOneTest initStack dStart (int n) testName (List.tail chunk)
                         
                     | "#BLOCK" :: LITERALNUMB(n, blockName) :: _ ->
                         parseOneBlock (int n) blockName chunk
                         
                     | x -> Error [ 1, sprintf "Can't parse test header '%A'" (List.truncate 2 x) ])
    lines
    |> List.map String.trim
    |> List.indexed
    |> List.filter (snd >> (<>) "")
    |> List.tail
    |> List.chunkAt (snd >> (fun lin -> String.startsWith "#TEST" lin || String.startsWith "#BLOCK" lin))
    |> List.map (fun chunk -> parseChunk initStack dStart chunk)
    |> (fun lst ->
        let tests = List.filter (function | Ok { TAppendCode = [] } | Error _ -> true | _ -> false) lst
        let getBlock n = List.tryFind (function | Ok { TNum = n'; TAppendCode = x } when x <> [] && n' = n -> true
                                                | _ -> false) lst
        let resolveBlocks (tst : Test) =
            let specs =
                tst.Ins |> List.collect (
                    function | AddCode(n, body) ->
                                getBlock n
                                |> function | None | Some(Error _) -> [ Error(0, sprintf "Can't find #BLOCK %d" n) ]
                                            | Some(Ok tst) -> [ AddCode(n, tst.TAppendCode) |> Ok ]
                             | x -> [ Ok x ])
            let errors = List.errorList specs
            if errors <> [] then Error errors else Ok { tst with Ins = List.okList specs }
        tests
        |> List.map (Result.bind resolveBlocks))





/// Create a list of TbCheckResult from a test specification and output DataPath of the tested program.
/// test: the test specification (which will have generated program inputs)
/// dp: the Datapath to check against the specification outputs
let checkTestResults (test : Test) (outDp : DataPath) (subRet : bool) =
    let specs = test.Outs
    let isSubTest =
        List.exists (function | BranchToSub _ -> true | _ -> false) test.Ins
    let exitTest =
        //printfn "isSubTest=%A, subRet=%A" isSubTest subRet
        match isSubTest, subRet with
        | false, false | true, true -> []
        | false, true -> [ 0u, TbRet "\t>>- Subroutine return detected when normal program end expected", BranchToSub "" ]
        | true, false -> [ 0u, TbRet "\t>>- Normal program end detected when subroutine return expected", BranchToSub "" ]
        |> List.map (fun (v, c, sp) -> { Actual = v; Check = c; Spec = sp })
    let checkSpec spec =
        let checkOneLoc (ma, u) =
            match Map.tryFind (WA ma) outDp.MM with
            | Some(Dat u') when u = u' -> []
            | Some(Dat u') -> [ { Actual = u; Check = TbMem(ma, Some u'); Spec = spec } ]
            | _ -> [ { Actual = u; Check = TbMem(ma, None); Spec = spec } ]
        match spec with
        | TbStackProtected sp when outDp.Regs.[R13] <> sp ->
            [ { Actual = 0u; Check = TbVal outDp.Regs.[R13]; Spec = spec } ]
        | TbStackProtected sp ->
            Map.toList outDp.MM
            |> List.filter (fun ((WA u), mm) -> u >= sp)
            |> List.collect (fun ((WA u), mm) -> match mm with | Dat m -> [ u, m ] | CodeSpace -> [])
            |> List.map (fun (u, m) -> { Actual = 0u; Check = TbMem(u, Some m); Spec = spec })
        | TbRegEquals(lNum, rn, u) when outDp.Regs.[rn] = u -> []
        | TbRegEquals(lNum, rn, u) -> [ { Actual = u; Check = TbVal outDp.Regs.[rn]; Spec = spec } ]
        | TbRegPointsTo(_, rn, start, uLst) ->
            uLst
            |> List.indexed
            |> List.map (fun (n, u) -> (start + uint32 (4 * n), u))
            |> List.collect checkOneLoc
        | APCS rLst ->
            rLst
            |> List.collect (fun (rn, u') ->
                    match u' = outDp.Regs.[rn] with
                    | true -> []
                    | false -> [ { Actual = u'; Check = TbVal outDp.Regs.[rn]; Spec = APCS [ rn, u' ] } ])
        | _ -> []
    specs |> List.collect checkSpec
    |> (fun lst -> exitTest @ lst)



/// Return code transformed by test.
/// Transformation is idempotent.
let transformCodeByTest (code : string list) (test : Test) =
    let startsWith (lab:string) (s:string) =
       let s' = s.Trim()
       s = lab || s.StartsWith (lab + " ") || s.StartsWith(lab + "\t") 
    let transformHdr = ";##TRANSFORM----------code appended by testbench---------"
    if List.exists (fun s -> String.trim s = transformHdr) code then code
    else
        let labelChanges = List.collect (function | Relabel(ol, nl) -> [ ol, nl ] | _ -> []) test.Ins
        let changeLabels code (oldLab, newLab) =
            List.map (fun lin -> if startsWith oldLab lin
                                 then newLab + lin.[oldLab.Length..lin.Length - 1]
                                 else lin) code
        let appends =
            match test.TAppendCode with
            | [] -> []
            | x -> transformHdr :: x
        let code' = List.fold changeLabels code labelChanges @ appends
        if code' <> code then
            let hdrComment =
                [ "; Code modified by testbench before testing!------" ] @
                (labelChanges |> List.map (fun (ol, nl) -> sprintf ";Testbench has changed label '%s' to '%s'" ol nl)) @
                [ ";------student code (with label changes)----------" ]
            hdrComment @ code'
        else
            code
    |> (fun code -> 
        //printfn "%s" (String.concat "\r\n" code); 
        //System.Console.ReadKey() 
        //|> ignore; 
        code)

/// Take assembler code and run test on it (transforming it if needed as per test).
/// Return Error if code parse fails.
/// Return Ok runInfo if parse succeeds.
let parseCodeAndRunTest (code : string list) (test : Test) =
    let lim = reLoadProgram (transformCodeByTest code test)
    if lim.Errors <> []
    then
        Error(sprintf "%A" lim.Errors)
    else
        let dp = initTestDP (lim.Mem, lim.SymInf.SymTab) test
        match dp with
        | Ok dp -> Ok <| getRunInfoFromImageWithInits NoBreak lim dp.Regs dp.Fl Map.empty dp.MM
        | Error e -> Error <| ">>-" + e
        |> Result.map (asmStep maxTestLength)

/// Generate a text line explaining a TbCheckResult error.
/// Input: TbCheckResult.
/// Output: string containing error message.
let displayError { Actual = u : uint32; Check = check : tbCheck; Spec = spec : TbSpec } =
    match check, spec with
    | TbVal spAct, TbStackProtected sp ->
        sprintf "\t>>- Unbalanced Stack. SP: Actual: %x, Expected: %x" spAct sp
    | TbMem(adr, act), TbStackProtected sp ->
        let actTxt = match act with | None -> "None" | Some a -> sprintf "%d" a
        sprintf "\t>>- Caller Stack [%x] -> Actual: %s." adr actTxt
    | TbVal act, TbRegEquals(n, reg, v) ->
        sprintf "\t>>- %A: Actual: %d, Expected: %d" reg act v
    | TbMem(adr, act), TbRegPointsTo(n, ptr, start, uLst) ->
        let actTxt = match act with | None -> "None" | Some a -> sprintf "%d" a
        let offset = int (adr - start)
        sprintf "\t>>- [%A,#%d] -> Actual: %s. expected: %d" ptr offset actTxt uLst.[offset / 4]
    | TbVal act, APCS [ rn, exptd ] ->
        sprintf "\t>>- Persistent Register %A -> Actual: %d. expected: %d" rn (int32 act) (int32 exptd)
    | TbRet errMess, _ -> errMess
    | _ -> failwithf "What?: inconsistent specs and check results"

/// Run a single test and generate pass/fail and text lines summarising result
/// Return (passed, messages) : (bool * string list).
/// passed: true if test passes
/// messages: list of lines suitable for testbench buffer insertion
/// dp: DataPath after test simulation ends.
let computeTestResults (test : Test) (dp : DataPath) =
    let subRet = dp.Regs.[R15] = 0xFFFFFFFCu
    let errorLines =
        checkTestResults test dp subRet
        |> List.map displayError
    let resultLines =
        errorLines
        |> function | [] -> [ sprintf ">>; \t\tTest %d PASSED." test.TNum ]
                    | errMess -> sprintf ">>- \t\tTest %d FAILED." test.TNum :: errMess
    errorLines = [], resultLines

/// generate test results by simulating code (possibly transformed by test).
/// cache overall result for efficiency reasons
/// Note that simulation results are cached independently of this.
let runTestOnCode =
    let runTest' ((test, code) : Test * string list) =
            let testRes = parseCodeAndRunTest code test
            let tr = {
                    TestCoverage = Set []
                    TestCycles = 0L
                    TestOk = false
                    TestLines = test.TestLines
                    TestMsgs = []
                    TestNum = test.TNum
                    TestName = test.TName
                }
            match testRes with
            | Error e -> { tr with TestMsgs = [ e ] }
            | Ok {State = ProgState.PSError (Errors.``Run time error``(n,s))} -> 
                {tr with TestMsgs = [sprintf ">>- Run time error at memory address 0x%x: %s" n s]}
            | Ok ri ->
                let ok, msgs = computeTestResults test (fst ri.dpCurrent)
                { tr with
                        TestCoverage = ri.Coverage
                        TestCycles = ri.CyclesDone
                        TestOk = ok
                        TestMsgs = msgs
                }
    let rt = cacheLastN 200 runTest'
    fun test code -> rt (test, code)
