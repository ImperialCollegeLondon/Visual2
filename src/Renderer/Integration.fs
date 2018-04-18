module Integration

open Tabs
open Update
open Helpers
open CommonData
open Expressions
open CommonLex
open ParseTop
open ExecutionTop
open DP

open Errors
open Ref

open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Core

open Fable.Import.Electron
open Node.Exports
open System.IO



let errUnpacker (eName, eTxt, eMess) tId lineNo =
    makeErrorInEditor tId lineNo (""+eName + "\n\n" + eTxt + "\n\n" + eMess) 

let highlightErrorParse ((err:ParseError), lineNo) tId = 
    let errCode, errStr, errMess = err
    let getErrNames = 
        match errCode with
        | ``Undefined symbol`` -> "Undefined symbol"
        | ``Invalid literal`` -> "Invalid literal"
        | ``Invalid second operand`` -> "Invalid second operand"
        | ``Invalid flexible second operand``  -> "Invalid flexible second operand"
        | ``Invalid memory address``  -> "Invalid memory address"
        | ``Invalid offset``  -> "Invalid offset"
        | ``Invalid register``  -> "Invalid register"
        | ``Invalid shift``  -> "Invalid shift"
        | ``Invalid suffix``  -> "Invalid suffix"
        | ``Invalid instruction``  -> "Invalid instruction"
        | ``Invalid expression`` -> "Invalid expression"
        | ``Invalid expression list``  -> "Invalid expression list"
        | ``Invalid fill``  -> "Invalid fill"
        | ``Label required``  -> "Label required"
        | ``Unimplemented instruction`` -> "Invalid instruction"
        | e -> sprintf "Error Code without getErrNames entry: %A" e
    errUnpacker (getErrNames,errStr, errMess) tId lineNo
    setErrorStatus ()


let makeMemoryMap mm =
    Map.toList mm
    |> List.map (fun (WA addr, value) ->
           match value with
           | Dat x -> Some (addr, x)
           | CodeSpace -> fNone)
    |> List.choose id
    |> Map.ofList

let getMemoryMap () : Map<WAddr,MemLoc<CondInstr*int>> =
    memoryMap
    |> Map.toList
    |> List.map (fun (a,v) -> WA a , DataLoc v)
    |> Map.ofList

let setRegs regs =
    regMap <- regs
    updateRegisters()

let getRegs() = regMap
    
let setFlags flags =
    setFlag "N" flags.N
    setFlag "C" flags.C
    setFlag "Z" flags.Z
    setFlag "V" flags.V

let getFlags() =
    {
        N = getFlag "N"
        C = getFlag "C"
        V = getFlag "V"
        Z = getFlag "Z"
    }

let showInfo (ri:RunInfo) =
    symbolMap <- ri.st
    updateSymTable()
    memoryMap <- makeMemoryMap ri.dp.MM
    updateMemory()
    setRegs ri.dp.Regs
    setFlags ri.dp.Fl

let handleRunTimeError e (pInfo:RunInfo) =
    match e with
    | EXIT ->
        showInfo pInfo
        setExecutionCompleteStatus ()
    | NotInstrMem x -> 
        Browser.window.alert(sprintf "Trying to access non-instruction memory 0x%x" x)
        setErrorStatus ()
    | ``Run time error`` (pos,msg) ->
        Browser.window.alert(sprintf "Error on line %d: %s" pos msg)
        setErrorStatus ()
    | ``Unknown symbol runtime error`` undefs ->
        Browser.window.alert(sprintf "What? Undefined symbols: %A" undefs)

    

let rec pExecute (numSteps: int) (ri:RunInfo) =
    let stepsBeforeDisplay = 50000
    let mutable currentStep = numSteps
    let lastStep = numSteps - stepsBeforeDisplay
    let mutable pi = {ri with RunErr=FSharp.Core.Option.None}
    let mutable dp = ri.dp
    let mutable err = fNone
    let setState() =
        showInfo pi
        updateRegisters()
    while currentStep <> 0 do
        dp <- ri.dp
        match dataPathStep (pi.dp,pi.IMem) with
        | Result.Error e -> 
            err <- Some e
            handleRunTimeError e pi
            currentStep <- 0
        | Result.Ok ndp ->
            pi <- { pi with dp = ndp}
            currentStep <- currentStep - 1
            if currentStep = lastStep then
                setState()
                Browser.window.setTimeout(
                    (fun () -> pExecute currentStep pi), 0) |> ignore
    setState()
    //printfn "Pexecute dp=%A" pi.dp
    {pi with RunErr = err}


let rec pTestExecute more numSteps ri =
    if more then printfn "Initial\nregs=%A\nflags=%A\nMem=%A\n" ri.dp.Regs ri.dp.Fl ri.IMem
    match numSteps, dataPathStep (ri.dp,ri.IMem) with
    | 0, _ -> 
        if more then printfn "Terminating with no error\nregs=%A\nflags=%A\n" ri.dp.Regs ri.dp.Fl
        { ri with RunErr = fNone}
    | _, Result.Error e -> 
        if more then printfn "Terminating\nregs=%A\nflags=%A\nError=%A\n" ri.dp.Regs ri.dp.Fl e
        {ri with RunErr = Some e }
    | _, Result.Ok ndp ->
        pTestExecute more (numSteps - 1) { ri with dp = ndp}
   
    
let tryParseCode tId =

    let asm = 
        getCode tId 
        |> (fun (x : string) -> x.Split [|'\n'|]) 
        |> Array.toList

    let lim, indentedAsm = reLoadProgram asm

    // See if any errors exist, if they do display them
    match lim with
    | {Errors=[]} as lim -> 
        //Browser.console.log(sprintf "%A" lim)
        let editor = editors.[tId]
        (editor?setValue (String.concat "\n"indentedAsm)) |> ignore
        (lim, indentedAsm) |> Some
    | lim -> 
        List.map (fun x -> highlightErrorParse x tId) lim.Errors |> ignore
        fNone


let getRunInfoFromState (lim:LoadImage) =
    let getData map mm : Map<WAddr,Data> =
        let dLocs = map |> Map.toList
        List.fold (fun mem -> fun (a, x) -> Map.add (WA a) (Dat x) mem) mm dLocs
    let dp = {
                Fl = getFlags()
                Regs = getRegs()
                MM = getData memoryMap lim.Mem
             } 
    {dp=dp; st=lim.SymInf.SymTab; IMem = lim.Code; RunErr=fNone}
   
let runInfo code dp st = {dp=dp ; st = st ; RunErr = fNone; IMem = code}

let highlightCurrentIns (pInfo:RunInfo) tId =
    removeEditorDecorations tId
    match Map.tryFind (WA pInfo.dp.Regs.[R15]) pInfo.IMem with
    | Some (ci, lineNo) -> highlightLine tId lineNo
    | Option.None
    | Some _ -> failwithf "What? Current PC value (%x) is not an instruction: this should be impossible!" pInfo.dp.Regs.[R15]


let runCode () =

    let tId = currentFileTabId
    removeEditorDecorations tId
    match tryParseCode tId with
    | Some (lim, txt) -> 
        disableEditors()
        match pExecute maxStepsToRun (lim |> getRunInfoFromState) with
        | {RunErr = Some e;  dp=dp} as ri -> handleRunTimeError e ri
        | {dp=dp} as ri -> highlightCurrentIns ri tId
        ()
    | _ -> ()

let mutable currentPInfo : RunInfo option = fNone



let doCodeStep pi tId =
    let newDp = dataPathStep (pi.dp,pi.IMem)
    match newDp with
    | Result.Error e ->
        handleRunTimeError e pi
    | Result.Ok ndp ->
        let newP = {pi with dp = ndp}
        highlightCurrentIns pi tId
        currentPInfo <- Some newP
        showInfo newP

let stepCode () =
    let tId = currentFileTabId
    removeEditorDecorations tId
    match currentPInfo with
    | Some pInfo -> doCodeStep pInfo tId
    | _ ->
        currentPInfo <- 
            tryParseCode tId 
            |> Core.Option.map (fst >> getRunInfoFromState) 
        match currentPInfo with
        | Some pi -> 
            disableEditors()
            doCodeStep pi tId
        | fNone -> ()


let resetEmulator () =
    removeEditorDecorations currentFileTabId
    enableEditors()   
    memoryMap <- initialMemoryMap
    symbolMap <- initialSymbolMap
    regMap <- initialRegMap
    updateMemory()
    updateSymTable()
    updateRegisters ()
    resetRegs()
    resetFlags()
    currentPInfo <- fNone
    setNoStatus ()

