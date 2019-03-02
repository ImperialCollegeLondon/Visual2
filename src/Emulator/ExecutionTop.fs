(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.ExecutionTop
    Description: Top-level code to execute instructions and programs
*)

/// execute ARM programs. Also calls parser, and implements code indentation
module ExecutionTop
open EEExtensions
open Errors
open CommonData
open DP
open Memory
open Branch
open Misc
open CommonLex
open Helpers
open Branch
open Errors
open Expressions
open ParseTop
open System.Drawing

//**************************************************************************************
//                            HELPER FUNCTIONS
//**************************************************************************************

/// memoises objFunc by storing the last N results and 
/// returning a stored result if the same input is presented
let cacheLastN N objFunc =
    let mutable cache = []
    fun inDat ->
        match List.tryFind (fun (inD, _) -> inD = inDat) cache with
        | Some(_, res) -> res
        | _ ->
            let res = objFunc inDat
            if cache.Length > N then
                cache <- List.take N ((inDat, res) :: cache)
                else cache <- (inDat, res) :: cache
            res



let isArithmeticOpCode opc =
    let arithmeticOpcRoots = [ "ADD"; "ADC"; "SUB"; "SBC"; "RSB"; "RSC"; "CMP"; "CMN" ]
    List.exists (fun s -> String.startsWith s opc) arithmeticOpcRoots



//**************************************************************************************
//                     TOP LEVEL EXECUTION FUNCTIONS
//**************************************************************************************
let historyMaxGap = 500L
let maxProgramCacheSize = 100

let mutable minDataStart : uint32 = 0x200u

type ErrResolveBase = { lineNumber : uint32; error : ParseError }

/// Used to track current code and data section load position when loading
/// instructions and data into memory from a program to make a loadImage
type LoadPos = {
    PosI : uint32;
    PosD : uint32 option;
    DStart : uint32
    }

/// Differentiate symbol type according to how it is defined in assembly
type SymbolType =
    | CalculatedSymbol // label defined by EQU - code be code or data
    | DataSymbol // label of DCD etc in data space
    | CodeSymbol // label of code in code space


/// resolved symbol table mapping symbol name to its type and value
type AnnotatedSymbolTable = Map<string, uint32 * SymbolType>

/// symbol info used during the parse and symbol resolution process
/// forward references get resolved if this is possible by multiple
/// loadProgram calls made by reLoadProgram
/// each time loadprogram is called the initial symbol table contains
/// more defined symbols. Calls continue until no more symbols 
/// can be defined
type SymbolInfo = {
    SymTab : SymbolTable;
    SymTypeTab : Map<string, SymbolType>
    Refs : (string * int * string) list
    Defs : (string * (SymbolType * int) * int) list
    Unresolved : (string * SymbolType * int) list
    }

type LoadImage = {
    LoadP : LoadPos
    Mem : DataMemory
    Code : CodeMemory<CondInstr * int>
    Errors : (ParseError * int * string) list
    SymInf : SymbolInfo
    Indent : int
    Source : string list
    EditorText : string list
    }

type StackI = { Target : uint32; SP : uint32; RetAddr : uint32 }


type Step = {
    NumDone : int64
    NumCycDone : int64
    Dp : DataPath * UFlags
    SI : StackI list
    }

type ProgState = | PSExit | PSRunning | PSError of ExecuteError | PSBreak

type TbSpec =
    | TbRegEquals of TNum : int * Register : RName * Data : uint32
    | TbRegPointsTo of TNum : int * Pointer : RName * Address : uint32 * MemData : uint32 list
    | TbStackProtected of uint32
    | TbSetDataArea of uint32
    | APCS of (RName * uint32) list
    | RandomiseInitVals
    | BranchToSub of string
    | Relabel of string * string
    | AddCode of int * string list

type tbCheck =
    | TbVal of Actual : uint32
    | TbMem of ActualAddress : uint32 * ActualData : uint32 option
    | TbRet of string

type TbInOut = | TbIn | TbOut

type Test = {
        TNum : int;
        TName : string;
        TAppendCode : string list;
        TRelabelSymbols : (string * string) list
        Ins : TbSpec list;
        Outs : TbSpec list;
        CheckLines : string list;
        InitSP : uint32
        TestLines : string list
    }

type TestBenchState = | NoTest | Testing of Test list

type BreakCondition =
    | NoBreak
    | ToSubroutine
    | ToReturn


type RunInfo = {
    dpInit : DataPath
    IMem : CodeMemory<CondInstr * int>
    st : AnnotatedSymbolTable
    StepsDone : int64
    CyclesDone : int64
    dpCurrent : DataPath * DP.UFlags
    State : ProgState
    LastDP : (DataPath * DP.UFlags) option
    Source : string list
    EditorText : string list
    History : Step list
    StackInfo : StackI list
    TestState : TestBenchState
    Coverage : int Set
    BreakCond : BreakCondition
    }


type RunState = | Running | Paused | Stopping

type RunMode =
    | ResetMode
    | ParseErrorMode
    | RunErrorMode of RunInfo
    | ActiveMode of RunState * RunInfo
    | FinishedMode of RunInfo



let dataSectionAlignment = 0x200u

let initLoadImage dStart symTab =
    {
        LoadP = { PosI = 0u; PosD = Some dStart; DStart = dStart }
        Mem = [] |> Map.ofList
        Code = [] |> Map.ofList
        Errors = []
        SymInf =
            {
                SymTab = symTab
                SymTypeTab = Map.empty
                Refs = []
                Defs = []
                Unresolved = []
            }
        Indent = 7
        Source = [ "" ]
        EditorText = []
    }

let makeLocI (pa : Parse<Instr>) =
    match pa.PInstr with
    | Ok ins -> Code(pa.PCond, ins)
    | Error _ -> failwithf "What? can't put invalid instruction into memory"

let makeLocD (pa : Parse<Instr>) : Data list =
    let makeW (bl : uint32 list) =
        let rec makeW' (b : uint32 list) locs =
            let CH (b : uint32) n = (b &&& 0xffu) <<< n
            match b with
            | [] ->
                List.rev locs
            | bls :: b1 :: b2 :: bms :: rest ->
                makeW' rest ((CH bls 0 ||| CH b1 8 ||| CH b2 16 ||| CH bms 24) :: locs)
            | r -> failwithf "What? can't make words from list with length %d not divisible by 4: %A" (List.length bl) bl
        makeW' bl [] |> List.map Dat

    match pa.PInstr, pa.DSize with
    | Ok(IMISC(DCD dl)), Some ds ->
        //printfn "DCD dl=%A" dl
        if List.length dl * 4 <> int ds then
            failwithf "What? DCD Data size %d does not match data list %A" ds dl
        dl |> List.map Dat
    | Ok(IMISC(DCB dl)), Some ds ->
        //printfn "DCB dl=%A" dl
        if List.length dl <> int ds then
            failwithf "What? DCB Data size %d does not match data list %A" ds dl
        dl |> makeW
    | Ok(IMISC(FILL { NumBytes = fNum; FillValue = fVal })), Some ds ->
        if fNum <> ds then
            failwithf "What? Data size %d does not match FILL size %d" ds fNum
        List.init (int fNum / 4) (fun _ -> 0u) |> List.map Dat
    | Ok _, _ -> failwithf "What? Undefined data directive!"
    | Error _, _ -> failwithf "What? Can't load data memory from error!"

let addWordDataListToMem (dStart : uint32) (mm : DataMemory) (dl : Data list) =
    let folder mm (n, loc) = Map.add n loc mm
    dl
    |> List.indexed
    |> List.map (fun (n, loc) -> (uint32 (n * 4) + dStart) |> WA, loc)
    |> List.fold folder mm


let loadLine (lim : LoadImage) ((line, lineNum) : string * int) =
    let addSymbol sym typ symList = (sym, typ, lineNum) :: symList
    match lim.LoadP.PosD with
    | None ->
        printfn "Data area undefined"
        { lim with SymInf = {
                lim.SymInf with Unresolved = ("_FILL_UNDEFINED", DataSymbol, 0) :: lim.SymInf.Unresolved
        } } // short-circuit parse
    | Some posD ->
        let pa = parseLine lim.SymInf.SymTab (lim.LoadP.PosI, posD) line
        let labType =
            match pa.PInstr with
            | Ok EMPTY -> CodeSymbol
            | Ok(IMISC(EQU _)) -> CalculatedSymbol
            | Ok(IMISC(DCD _)) | Ok(IMISC(DCB _)) | Ok(IMISC(FILL _)) -> DataSymbol
            | _ -> CodeSymbol
        let si' =
            let si = lim.SymInf
            match pa.PLabel with
            | Some(lab, Ok addr) ->
                    { si with
                        Defs = addSymbol lab (labType, lineNum) si.Defs
                        SymTab = Map.add lab addr si.SymTab
                        SymTypeTab = Map.add lab labType si.SymTypeTab
                    }
            | None -> si
            | Some(lab, Error _) ->
                { si with Unresolved = addSymbol lab labType si.Unresolved }

            |> (fun si ->
                match pa.PInstr with
                | Error(``Undefined symbol`` syms) ->
                    { si with
                        Refs =
                            (syms
                            |> List.map (fun (s, msg) -> (s, lineNum, msg)))
                            @ si.Refs
                    }
                | _ -> si)

        let lp = lim.LoadP
        let ins = pa.PInstr
        let updatePos dPos dIncr =
            match dPos, dIncr with
            | None, _ -> None
            | _, None -> None
            | Some a, Some b -> Some(a + b)
        let m, c =
            match pa.ISize, pa.DSize with
            | 0u, Some 0u -> lim.Mem, lim.Code
            | 0u, Some _ ->
                //printfn "makeLoc output: %A" (makeLocD pa)
                addWordDataListToMem posD lim.Mem (makeLocD pa), lim.Code
            | 0u, None -> lim.Mem, lim.Code
            | 4u, Some 0u ->
                    match pa.PInstr with
                    | Ok pai ->
                        lim.Mem, Map.add (WA lp.PosI) ({ Cond = pa.PCond; InsExec = pai; InsOpCode = pa.POpCode; Cycles = int64 pa.PStall }, lineNum) lim.Code
                    | _ -> lim.Mem, lim.Code
            | i, d -> failwithf "What? Unexpected sizes (I=%d ; D=%A) in parse load" i d

        {
            SymInf = si'
            LoadP = { lp with PosI = lp.PosI + pa.ISize; PosD = updatePos lp.PosD pa.DSize }
            Mem = m
            Code = c
            Errors =
                match ins with
                    | Error x -> (x, lineNum, pa.POpCode) :: lim.Errors
                    | _ -> lim.Errors
            Indent =
                match pa.PLabel with
                | Some(lab, _) -> max lim.Indent (lab.Length + 1)
                | _ -> lim.Indent
            Source = [ "" ]
            EditorText = []
        }

let addTermination (lim : LoadImage) =
    let insLst = Map.toList lim.Code |> List.sortByDescending fst
    match insLst with
    | (_, ({ Cond = Cal; InsExec = IBRANCH END; InsOpCode = "END" }, _)) :: _ -> lim
    | [] -> loadLine lim ("END", 1)
    | _ -> loadLine lim ("END", -1) // used if no line for END

/// loadProgram creates a new loadImage from the symbol table in the 
/// old loadImage and the pogram text in lines.
/// Multiple loadProgram calls are made by reLoadProgram.
/// each time loadProgram is called the initial symbol table contains
/// more defined symbols. Calls continue until no more symbols 
/// can be defined
let loadProgram (lines : string list) (lim : LoadImage) =
    let roundUpHBound n =
        let blkSize = dataSectionAlignment
        match n / blkSize, int n % int blkSize with
        | hb, 0 -> blkSize * hb
        | hb, _ -> blkSize * (hb + 1u)
        |> max minDataStart
    //printfn "POSI= %A" lim.LoadP
    let setDStart lim = { lim with LoadP = { lim.LoadP with DStart = roundUpHBound lim.LoadP.PosI } }
    let initLim = initLoadImage (setDStart lim).LoadP.DStart lim.SymInf.SymTab
    List.fold loadLine initLim (lines |> List.indexed |> List.map (fun (i, s) -> s, i + 1))
    |> addTermination

/// Returns the program in lines with nice uniform indentation
/// Uses symbol table from lim.
/// Required the program to parse correctly.
let indentProgram lim lines =
    let spaces n = if n >= 0 then String.init n (fun _ -> " ") else " "
    let opCols = 8
    let leftAlign (n : int) (s : string) =
        s + spaces (n - s.Length)
    let n = lim.Indent
    let isSymbol s = Map.containsKey s lim.SymInf.SymTab
    let instr lis =
        match lis with
        | [] -> ""
        | [ opc ] -> opc
        | opc :: rest -> leftAlign opCols opc + String.concat " " rest
    let indentLine (line : string) =
        match splitIntoWords line with
        | [] -> ""
        | lab :: rest when isSymbol lab ->
            leftAlign n lab + instr rest
        | lab :: rest -> spaces n + instr (lab :: rest)
        | [ lab ] when isSymbol lab -> lab
        | [ lab ] -> spaces n + lab
    List.map indentLine lines

/// Version of assembly line with whitespace removed that allows
/// indented code to be compared with original code
let invariantOfLine =
    String.splitRemoveEmptyEntries [| ' '; '\t'; '\f' |]
    >> String.concat " "
    >> String.trim



let mutable programCache : Map<string list, LoadImage> = Map.empty

let makeDupSymParseErrors (sym, defLst) =
    let lNos dLst = dLst |> List.map (fun (_, (_, lineNo), _) -> lineNo)
    let eLines = lNos defLst
    defLst
    |> lNos
    |> List.map (fun lineNo -> ``Duplicate symbol`` (sym, eLines), lineNo, "")

/// Top level program which parses and loads a program into a loadimage ready for
/// execution.
let reLoadProgram (lines : string list) =
    let findDuplicateSymbols (lim : LoadImage) =
        let symDefs = lim.SymInf.Defs
        let symGrps = List.groupBy (fun (sym, (_typ, _lineNo), _sVal) -> sym) symDefs
        symGrps
        |> List.filter (fun (sym, defLst) -> defLst.Length > 1)
        |> List.collect makeDupSymParseErrors
    let reLoadProgram' (lines : string list) =
        let addCodeMarkers (lim : LoadImage) =
            match lim.LoadP.PosD with
            | None -> lim
            | _ ->
                let addCodeMark map (WA a, _) =
                    match Map.tryFind (WA a) map with
                    | None -> Map.add (WA a) CodeSpace map
                    | _ -> failwithf "Code and Data conflict in %x" a
                List.fold addCodeMark (lim.Mem) (lim.Code |> Map.toList)
                |> (fun markedMem -> { lim with Mem = markedMem })
        let lim1 = initLoadImage dataSectionAlignment ([] |> Map.ofList)
        let next = loadProgram lines
        let unres lim = lim.SymInf.Unresolved |> List.length
        let errs lim = lim.Errors |> List.map (fun (e, n, opc) -> n)
        let rec pass lim1 lim2 =
            //printfn "LoadP in pass = %A, n1=%d, n2=%d" lim2.LoadP (unres lim1) (unres lim2)
            match unres lim1, unres lim2 with
            | n1, n2 when (n2 = 0 && lim2.LoadP.PosI <= lim2.LoadP.DStart) || (n2 <> 0 && n1 = n2)
                -> lim2
            | n1, n2 when n1 = n2 && List.isEmpty lim2.Errors && lim2.LoadP.PosI <= lim2.LoadP.DStart->
                failwithf "What? %d unresolved refs in load image with no errors" n1
            | _ -> pass lim2 (next lim2)
        let final =
            pass lim1 (next lim1)
            |> next
            |> addCodeMarkers
        let src = indentProgram final lines
        let lim = { final with Source = src; EditorText = lines; Errors = (findDuplicateSymbols final) @ final.Errors }
        lim
    cacheLastN 10 reLoadProgram' lines


let executeADR (ai : ADRInstr) (dp : DataPath) =
    setReg ai.AReg ai.AVal dp

/// Execute next instruction as determined by PC using dp as current
/// register + mem state and code as program
let dataPathStep (dp : DataPath, code : CodeMemory<CondInstr * int>) =
    let addToPc a dp = { dp with Regs = Map.add R15 ((uint32 a + dp.Regs.[R15]) >>> 0) dp.Regs }
    let pc = dp.Regs.[R15]
    let dp' = addToPc 8 dp // +8 during instruction execution so PC reads correct (pipelining)
    let uFl = DP.toUFlags dp'.Fl
    let noFlagChange = Result.map (fun d -> d, uFl)
    let thisInstr = Map.tryFind (WA pc) code
    match thisInstr with
    | None when pc = 0xFFFFFFFCu ->
        (// special case to implement testbenches, terminate on branch to 0xFFFFFFFC
        -1, None), Error TBEXIT
    | None ->
        (-1, None), (NotInstrMem pc |> Error)
    | Some({ Cond = cond; InsExec = instr } as condIns, line) ->
        match condExecute cond dp' with
        | true ->
            match instr with
            | IDP instr' ->
                executeDP instr' dp'
            | IMEM instr' ->
                executeMem instr' dp' |> noFlagChange
            | IBRANCH instr' ->
                executeBranch instr' dp' |> noFlagChange
            | IMISC(Misc.ADR adrInstr) ->
                //printfn "Executing ADR"
                executeADR adrInstr dp' |> Ok |> noFlagChange
            | IMISC x -> (``Run time error`` (dp.Regs.[R15], sprintf "Can't execute %A" x)) |> Error
            | ParseTop.EMPTY _ -> failwithf "Shouldn't be executing empty instruction"
            |> fun res -> (line, Some condIns), res


        | false -> (line, None), ((dp', uFl) |> Ok)
        // NB because PC is incremented after execution all exec instructions that write PC must in fact
        // write it as (+8-4) of real value. setReg does this.
        |> fun (lci, res) ->
                lci, Result.map (fun (dp, uF) ->
                    addToPc (4 - 8) dp, uF) res // undo +8 for pipelining added before execution.
                                                // Add +4 to advance to next instruction

/// <summary> <para> Top-level function to run an assembly program.
/// Will run until error, program end, or numSteps instructions have been executed. </para>
/// <para> Previous runs are typically contained in a linked list of RunInfo records.
/// The function will find the previous result with StepsDone as large as possible but
/// smaller than numSteps and use this as starting point </para> </summary>
/// <param name="numSteps"> max number instructions from ri.dpInit before stopping </param>
/// <param name="ri"> runtime info with initial data path and instructions</param>
/// <result> <see cref="RunInfo">RunInfo Record</see> with final PC, instruction Result,
/// and number of steps successfully executed </result>
let asmStep (numSteps : int64) (ri : RunInfo) =
        // Can't use a tail recursive function here since FABLE will maybe not optimise stack.
        // We need this code to be fast and possibly execute for a long time
        // so use this ugly while loop with mutable variables!
        let mutable dp = ri.dpInit, toUFlags ri.dpInit.Fl // initial dataPath
        let mutable stepsDone = 0L // number of instructions completed without error
        let mutable cyclesDone = 0L // number of cycles completed
        let mutable state = PSRunning
        let mutable lastDP = None
        let mutable stackInfo = []
        let mutable running = true // true if no error has yet happened
        let mutable coverage = ri.Coverage

        /// record branches and returns for display and break conditions.
        /// stackInfo is list of StackI that record sp and return or target address
        let recordStack dp' dp (opc : string) lNum =
            let guessComputedBranch opc dp' dp =
                (String.startsWith "B" opc |> not) && dp'.Regs.[R15] - dp.Regs.[R15] <> 4u
                
            let opc' = opc.ToUpper()
            
            if opc'.Length <> 3 && String.startsWith "BL" opc' then
                if ri.BreakCond = ToSubroutine && stepsDone > ri.StepsDone then
                    state <- PSBreak
                    running <- false
                let ret = dp.Regs.[R15] + 4u
                stackInfo <- { Target = dp'.Regs.[R15]; SP = dp.Regs.[R13]; RetAddr = ret } :: stackInfo
            elif guessComputedBranch opc dp' dp then
                match stackInfo with
                | { RetAddr = ra } :: rest when ra = dp'.Regs.[R15] ->
                    stackInfo <- rest
                    if ri.BreakCond = ToReturn && stepsDone > ri.StepsDone then
                        state <- PSBreak
                        running <- false
                | _ -> ()

        let runFrom = match ri.BreakCond with | NoBreak -> numSteps | _ -> ri.StepsDone
        let (future, past) = List.partition (fun (st : Step) -> st.NumDone >= runFrom) ri.History
        let mutable history = past
        match past with
        | step :: _ ->
            dp <- step.Dp
            stepsDone <- step.NumDone
            cyclesDone <- step.NumCycDone
            stackInfo <- step.SI;
        | _ -> ()
        //printf "Stepping before while Done=%d num=%d dp=%A" stepsDone numSteps  dp
        if stepsDone >= numSteps then lastDP <- Some dp;
        while stepsDone < numSteps && running do
            //printfn "stepsDone=%d, SI=%A" stepsDone stackInfo
            let historyLastRecord = match history with | [] -> 0L | h :: _ -> h.NumDone
            if (stepsDone - historyLastRecord) > historyMaxGap then
                history <- { Dp = dp; SI = stackInfo; NumDone = stepsDone; NumCycDone = cyclesDone } :: history
            let ti, stepRes = dataPathStep (fst dp, ri.IMem)
            match stepRes with
            | Result.Ok(dp', uF') ->
                lastDP <- Some dp; dp <- dp', uF'; stepsDone <- stepsDone + 1L;
            | Result.Error EXIT -> running <- false; state <- PSExit;
            | Result.Error e -> running <- false; state <- PSError e; lastDP <- Some dp;
            cyclesDone <- cyclesDone + match ti with | _, Some { Cycles = cyc } -> cyc + 1L | _, None -> 1L
            match ti, stepRes, lastDP with
            | (lNum, Some ci), Result.Ok(dp', _), Some(dp, _) ->
                coverage <- Set.add lNum coverage
                recordStack dp' dp ci.InsOpCode lNum
            | (lNum, _), _, _ -> coverage <- Set.add lNum coverage

        {
            ri with
                dpCurrent = dp
                State = state
                LastDP = lastDP
                StepsDone = stepsDone
                CyclesDone = cyclesDone
                StackInfo = stackInfo
                BreakCond = if state = PSBreak then NoBreak else ri.BreakCond
                Coverage = coverage
                History =
                    future @ history
                    |> List.distinctBy (fun st -> st.NumDone)
                    |> List.sortByDescending (fun st -> st.NumDone)
        }

/// Sensible initial value of R13 so that code with subroutines works as expected
let initStackPointer = 0xff000000u

/// initial value of all registers (note special case for SP R13)
let initialRegMap : Map<CommonData.RName, uint32> =
    [ 0..15 ]
    |> List.map (CommonData.register >> function | R13 -> R13, initStackPointer | rn -> rn, 0u)
    |> Map.ofList

/// Return initial RunInfo context from a LoadImage
let getRunInfoFromImageWithInits breakCond (lim : LoadImage) regsInit flagsInit mMap mm =
    let getSymTyp sym =
        match Map.tryFind sym lim.SymInf.SymTypeTab with
        | Some typ -> typ
        | None -> failwithf "What? No type info for symbol %s" sym

    let getData map mm : Map<WAddr, Data> =
        let dLocs = map |> Map.toList
        List.fold (fun mem (a, x) ->
            if a > 0xFFFFFFFFu then failwithf "What? invalid address in memory image location: %d: %d" a x
            if x > 0xFFFFFFFFu then failwithf "What? invalid data value in memory image locatio: %d: %d" a x
            Map.add (WA a) (Dat x) mem) mm dLocs
    let dp = {
                Fl = flagsInit
                Regs = regsInit
                MM = getData mMap mm
             }
    {
        dpInit = dp;
        dpCurrent = dp, DP.toUFlags dp.Fl
        State = PSRunning
        st =
            lim.SymInf.SymTab
            |> Map.map (fun sym addr -> addr, getSymTyp sym)
        IMem = lim.Code;
        LastDP = None
        StepsDone = 0L
        CyclesDone = 0L
        Source = lim.Source
        EditorText = lim.EditorText
        History = []
        StackInfo = []
        Coverage = Set []
        TestState = NoTest
        BreakCond = breakCond
    }





