module ExecutionTop
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



//**************************************************************************************
//                     TOP LEVEL EXECUTION FUNCTIONS
//**************************************************************************************

type ErrResolveBase = {lineNumber : uint32 ; error : ParseError}

type LoadPos = { 
    PosI: uint32; 
    PosD: uint32 ; 
    DStart: uint32
    }

type SymbolInfo = {
    SymTab: SymbolTable ; 
    Refs: (string * int) list ; 
    Defs: (string * int) list; 
    Unresolved: (string * int) list
    }

type LoadImage = {
    LoadP: LoadPos
    Mem: DataMemory
    Code: CodeMemory<CondInstr * int>
    Errors: (ParseError * int) list
    SymInf: SymbolInfo
    Indent: int
    Source: string list
    }

type Step = {
    NumDone: int64
    Dp: DataPath
    }

type ProgState = | PSExit | PSRunning | PSError of ExecuteError

type RunInfo = {
    dpInit: DataPath
    IMem: CodeMemory<CondInstr * int>
    st: SymbolTable
    StepsDone: int64
    dpCurrent: DataPath
    State: ProgState
    LastPC: uint32 option
    Source: string list
    History: Step list
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
        LoadP = {PosI=0u ; PosD= dStart; DStart = dStart}
        Mem = [] |> Map.ofList
        Code = [] |> Map.ofList
        Errors = []
        SymInf =
            {
                SymTab = symTab
                Refs = []
                Defs = []
                Unresolved = []
            }
        Indent = 7
        Source = [""]
    }

let makeLocI (pa: Parse<Instr>) = 
    match pa.PInstr with
    | Ok ins -> Code (pa.PCond, ins)
    | Error _ -> failwithf "What? can't put invalid instruction into memory"

let makeLocD (pa: Parse<Instr>) : Data list =
    let makeW (bl:uint32 list) =
        let rec makeW' (b: uint32 list) locs =
            let CH (b:uint32) n = (b &&& 0xffu) <<< n
            match b with
            | [] -> 
                List.rev locs
            | bls :: b1 :: b2 :: bms :: rest ->
                makeW' rest ((CH bls 0 ||| CH b1 8 ||| CH b2 16 ||| CH bms 24) :: locs)
            | r -> failwithf "What? can't make words from list with length %d not divisible by 4: %A" (List.length bl) bl
        makeW' bl []  |> List.map Dat  
        
    match pa.PInstr with
    | Ok (IMISC (DCD dl)) ->
        //printfn "DCD dl=%A" dl
        if List.length dl * 4 <> int pa.DSize then 
            failwithf "What? DCD Data size %d does not match data list %A" pa.DSize dl
        dl |> List.map Dat
    | Ok (IMISC (DCB dl)) -> 
        //printfn "DCB dl=%A" dl
        if List.length dl <> int pa.DSize then 
            failwithf "What? DCB Data size %d does not match data list %A" pa.DSize dl
        dl |> makeW 
    | Ok (IMISC (FILL {NumBytes=fNum ; FillValue=fVal})) ->
        if fNum <> pa.DSize then
            failwithf "What? Data size %d does not match FILL size %d" pa.DSize fNum
        List.init (int fNum/4) (fun _ -> 0u) |> List.map Dat
    | Ok _ -> failwithf "What? Undefined data directive!"
    | Error _ -> failwithf "What? Can't load data memory from error!"

let addWordDataListToMem (dStart:uint32) (mm: DataMemory) (dl:Data list) = 
    let folder mm (n,loc) = Map.add n loc mm
    dl
    |> List.indexed
    |> List.map (fun (n,loc) -> (uint32 (n*4) + dStart) |> WA,loc)
    |> List.fold folder mm 
    

let loadLine (lim:LoadImage) ((line,lineNum) : string * int) =
    let addSymbol sym symList = (sym,lineNum) :: symList
    let pa = parseLine lim.SymInf.SymTab (lim.LoadP.PosI,lim.LoadP.PosD) line
    let si' =
        let si = lim.SymInf
        match pa.PLabel with
        | Some (lab,Ok addr) -> 
            { si with 
                Defs = addSymbol lab si.Defs
                SymTab = Map.add lab addr si.SymTab
            }
        | None -> si
        | Some (lab, Error _) -> 
            { si with Unresolved = addSymbol lab si.Unresolved }

        |>  (fun si ->
            match pa.PInstr with
            | Error ( ``Undefined symbol``,_,syms) ->
                { si with
                    Refs =
                        (syms.Split([|','|])
                        |> Array.toList 
                        |> List.map (fun s -> (s , lineNum)))
                        @ si.Refs
                }
            | _ -> si)

    let lp = lim.LoadP
    let ins = pa.PInstr
    let m,c =
        match pa.ISize, pa.DSize with
        | 0u,0u -> lim.Mem, lim.Code
        | 0u, _ -> 
            //printfn "makeLoc output: %A" (makeLocD pa)
            addWordDataListToMem   lp.PosD lim.Mem (makeLocD pa), lim.Code
        | 4u, 0u -> 
                match pa.PInstr with
                | Ok pai -> 
                    lim.Mem, Map.add (WA lp.PosI) ((pa.PCond,pai) , lineNum) lim.Code
                | _ -> lim.Mem, lim.Code
        | i, d -> failwithf "What? Unexpected sizes (I=%d ; D=%d) in parse load" i d

    {
        SymInf = si'
        LoadP = { lp with PosI=lp.PosI+pa.ISize ; PosD = lp.PosD+pa.DSize}
        Mem = m
        Code = c
        Errors = 
            match ins with 
                | Error x -> (x,lineNum) :: lim.Errors 
                | _ -> lim.Errors
        Indent = 
            match pa.PLabel with 
            | Some (lab,_) -> max lim.Indent (lab.Length+1) 
            | _ -> lim.Indent
        Source = [""]
    }

let addTermination (lim:LoadImage) =
    let insLst = Map.toList lim.Code |> List.sortByDescending fst
    match insLst with
    | (_, ((Cal,IBRANCH END),_)) :: _ -> lim 
    | []
    | _ -> loadLine lim ("END",1)

    
let loadProgram (lines: string list) (lim: LoadImage)   =
    let roundUpHBound n = 
        let blkSize = dataSectionAlignment
        match n/blkSize , int n % int blkSize with
        | hb, 0 -> blkSize * hb
        | hb, _ -> blkSize * (hb+1u)
    let setDStart lim = {lim with LoadP = {lim.LoadP with DStart = roundUpHBound lim.LoadP.PosI}}
    let initLim = initLoadImage (setDStart lim).LoadP.DStart lim.SymInf.SymTab
    List.fold loadLine initLim (lines |> List.indexed |> List.map (fun (i,s) -> s,i+1))
    |> addTermination


let indentProgram lim lines =
    let spaces n = if n >= 0 then String.init n (fun _ -> " ") else " "
    let opCols = 7
    let leftAlign (n:int) (s:string) =
        s + spaces (n - s.Length)
    let n = lim.Indent
    let isSymbol s = Map.containsKey s lim.SymInf.SymTab
    let instr lis =
        match lis with
        | [] -> ""
        | [opc] -> opc
        | opc :: rest -> leftAlign opCols opc + String.concat " " rest
    let indentLine (line:string) =
        match splitIntoWords line with
        | [] -> ""
        | lab :: rest when isSymbol lab ->
            leftAlign n lab + instr rest
        | lab :: rest -> spaces n + instr (lab :: rest)
        | [lab] when isSymbol lab -> lab
        | [lab] -> spaces n + lab
    List.map indentLine lines


let reLoadProgram (lines: string list) =
    let addCodeMarkers (lim: LoadImage) =
        let addCodeMark map (WA a, _) = 
               match Map.tryFind (WA a) map with
                | None -> Map.add (WA a) CodeSpace map
                | _ -> failwithf "Code and Data conflict in %x" a
        List.fold addCodeMark (lim.Mem) (lim.Code |> Map.toList)
        |> (fun markedMem -> {lim with Mem = markedMem})
    let lim1 = initLoadImage dataSectionAlignment ([] |> Map.ofList)
    let next = loadProgram lines
    let unres lim = lim.SymInf.Unresolved |> List.length
    let errs lim = lim.Errors |> List.map snd
    let rec pass lim1 lim2 =
        match unres lim1, unres lim2 with
        | n1,n2 when (n2=0 && (lim1.LoadP.PosI <= lim2.LoadP.DStart)) || n1 = n2
            -> lim2
        | n1,n2 when n1 = n2 && lim2.Errors = [] -> failwithf "What? %d unresolved refs in load image with no errors" n1
        | _ -> pass lim2 (next lim2)
    let final = 
        pass lim1 (next lim1) 
        |> next 
        |> next
        |> addCodeMarkers
    let src = indentProgram final lines
    {final with Source=src}, src


let executeADR (ai:ADRInstr) (dp:DataPath) =
    setReg ai.AReg ai.AVal dp


let dataPathStep (dp : DataPath, code:CodeMemory<CondInstr*int>) = 
    let addToPc a dp = {dp with Regs = Map.add R15 ((uint32 a + dp.Regs.[R15]) &&& 0xffffffffu) dp.Regs}
    let pc = dp.Regs.[R15]
    let dp' = addToPc 8 dp // +8 during instruction execution so PC reads correct (pipelining)
    match Map.tryFind (WA pc) code with
    | None ->
        NotInstrMem pc |> Error
    | Some ((cond,instr),line) ->
        match condExecute cond dp' with
        | true -> 
            match instr with
            | IDP instr' ->
                executeDP instr' dp'
            | IMEM instr' ->
                executeMem instr' dp'
            | IBRANCH instr' ->
                executeBranch instr' dp'
            | IMISC (Misc.ADR adrInstr) ->
                //printfn "Executing ADR"
                executeADR adrInstr dp' |> Ok
            | IMISC ( x) -> (``Run time error`` ( dp.Regs.[R15], sprintf "Can't execute %A" x)) |> Error
            | ParseTop.EMPTY _ -> failwithf "Shouldn't be executing empty instruction"
        | false -> dp' |> Ok
        // NB because PC is incremented after execution all exec instructions that write PC must in fact 
        // write it as (+8-4) of real value. setReg does this.
        |> Result.map (addToPc (4 - 8)) // undo +8 for pipelining added before execution. Add +4 to advance to next instruction

/// <summary> <para> Top-level function to run an assembly program.
/// Will run until error, program end, or numSteps instructions have been executed. </para>
/// <para> Previous runs are typically contained in a linked list of RunInfo records.
/// The function will find the previous result with StepsDone as large as possible but
/// smaller than numSteps and use this as starting point </para> </summary>
/// <param name="numSteps"> max number instructions from ri.dpInit before stopping </param>
/// <param name="ri"> runtime info with initial data path and instructions</param>
/// <result> <see cref="RunInfo">RunInfo Record</see> with final PC, instruction Result, 
/// and number of steps successfully executed </result>
let asmStep (numSteps:int64) (ri:RunInfo) =
        // Can't use a tail recursive function here since FABLE will maybe not optimise stack.
        // We need this code to be fast and possibly execute for a long time
        // so use this ugly while loop with mutable variables!
        let mutable dp = ri.dpInit // initial dataPath
        let mutable stepsDone = 0L // number of instructions completed without error
        let mutable state = PSRunning
        let mutable lastPC = None
        let setPrecomputedResult =
            ri.History
            |> List.tryFind (fun (step:Step) -> step.NumDone < numSteps)
            |> function 
                | None -> () 
                | Some step -> 
                    dp <- step.Dp ; stepsDone <- step.NumDone
        setPrecomputedResult       
        //printf "Stepping before while Done=%d num=%d dp=%A" stepsDone numSteps  dp
        let mutable running = true // true if no error has yet happened
        while stepsDone < numSteps && running do
            lastPC <- Some dp.Regs.[R15];
            match dataPathStep (dp,ri.IMem) with
            | Result.Ok dp' ->  dp <- dp' ; stepsDone <- stepsDone + 1L;
            | Result.Error EXIT -> running <- false ; state <- PSExit;
            | Result.Error e ->  running <- false ; state <- PSError e
        //printf "stepping after while PC=%d, dp=%A, done=%d --- err'=%A" dp.Regs.[R15] dp stepsDone (dataPathStep (dp,ri.IMem))
        {
            ri with 
                dpCurrent = dp
                State = 
                    match  dataPathStep (dp,ri.IMem) with
                    | Ok dp -> PSRunning
                    | Error EXIT -> PSExit
                    | Error e -> PSError e
                    
                LastPC = lastPC
                StepsDone=stepsDone
                History = 
                    match ri.History with 
                    | [] -> [{Dp=dp; NumDone=stepsDone}]
                    | h :: _ as hist when stepsDone > h.NumDone -> {Dp=dp; NumDone=stepsDone} :: hist
                    | hist -> hist
        } 


            
    