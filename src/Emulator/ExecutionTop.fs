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

type LoadPos = { PosI: uint32; PosD: uint32 ; DStart: uint32}
type SymbolInfo = {SymTab: SymbolTable ; Refs: (string * int) list ; Defs: (string * int) list; Unresolved: (string * int) list}

type LoadImage = {
    LoadP: LoadPos
    Mem: DataMemory
    Code: CodeMemory<CondInstr * int>
    Errors: (ParseError * int) list
    SymInf: SymbolInfo
    Indent: int
    }

type RunInfo = {
    dp: DataPath
    IMem: CodeMemory<CondInstr * int>
    RunErr: ExecuteError option
    st: SymbolTable
    }

let dataSectionAlignment = 0x100u

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
    }

let makeLocI (pa: Parse<Instr>) = 
    match pa.PInstr with
    | Ok ins -> Code (pa.PCond, ins)
    | Error _ -> failwithf "What? can't put invalid instruction into memory"

let makeLocD (pa: Parse<Instr>) : Data list=
    let makeW (bl:uint32 list) =
        let rec makeW' (b: uint32 list) locs =
            let CH (b:uint32) n = (b &&& 0xffu) <<< n
            match b with
            | [] -> List.rev locs
            | bls :: b1 :: b2 :: bms :: rest ->
                makeW' rest ((CH bls 0 ||| CH bls 8 ||| CH bls 16 ||| CH bls 24) :: locs)
            | r -> failwithf "What? can't make words from list with length %d not divisible by 4: %A" (List.length bl) bl
        makeW' bl []  |> List.map Dat  
        
    match pa.PInstr with
    | Ok (IMISC (DCD dl)) ->
        if List.length dl * 4 <> int pa.DSize then 
            failwithf "What? DCD Data size %d does not match data list %A" pa.DSize dl
        dl |> List.map Dat
    | Ok (IMISC (DCB dl)) -> 
        if List.length dl <> int pa.DSize then 
            failwithf "What? DCB Data size %d does not match data list %A" pa.DSize dl
        dl |> makeW 
    | Ok (IMISC (FILL {NumBytes=fNum ; FillValue=fVal})) ->
        if fNum <> pa.DSize then
            failwithf "What? Data size %d does not match FILL size %d" pa.DSize fNum
        List.init (int fNum/4) (fun _ -> 0u) |> List.map Dat
    | Ok _ -> failwithf "What? Undefined data directive!"
    | Error _ -> failwithf "What? Can't load data memory from error!"

let addDataListToMem (dStart:uint32) (mm: DataMemory) (dl:Data list) = 
    let folder mm (n,loc) = Map.add n loc mm
    dl
    |> List.indexed
    |> List.map (fun (n,loc) -> (uint32 n + dStart) |> WA,loc)
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
        | 0u, _ -> addDataListToMem  lp.PosD lim.Mem (makeLocD pa), lim.Code
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
    let initLim = initLoadImage (setDStart lim).LoadP.DStart ([] |> Map.ofList)
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
    let lim1 = initLoadImage dataSectionAlignment ([] |> Map.ofList)
    let next = loadProgram lines
    let unres lim = lim.SymInf.Unresolved |> List.length
    let errs lim = lim.Errors |> List.map snd
    let rec pass lim1 lim2 =
        match unres lim1, unres lim2 with
        | n1,n2 when (n2=0 && lim1.LoadP.PosI <= lim2.LoadP.DStart) || n1 = n2
            -> lim2
        | n1,n2 when n1 = n2 && lim2.Errors = [] -> failwithf "What? %d unresolved refs in load image with no errors" n1
        | _ -> pass lim2 (next lim2)
    let final = pass lim1 (next lim1)
    final, indentProgram final lines


let executeADR (ai:ADRInstr) (dp:DataPath) =
    setReg ai.AReg ai.AVal dp


let dataPathStep (dp : DataPath, code:CodeMemory<CondInstr*int>) = 
    match Map.tryFind (WA dp.Regs.[R15]) code with
    | None ->
        NotInstrMem dp.Regs.[R15] |> Error
    | Some ((cond,instr),line) ->
        match condExecute cond dp with
        | true -> 
            match instr with
            | IDP instr' ->
                executeDP instr' dp
            | IMEM instr' ->
                executeMem instr' dp
            | IBRANCH instr' ->
                executeBranch instr' dp
            | IMISC (Misc.ADR adrInstr) ->
                executeADR adrInstr dp |> Ok
            | IMISC ( x) -> (``Run time error`` ( dp.Regs.[R15], sprintf "Can't execute %A" x)) |> Error
            | ParseTop.EMPTY _ -> failwithf "Shouldn't be executing empty instruction"
        | false -> dp |> Ok
        |> Result.map updatePC

    