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




/// Generate the hover error box
/// the text generated is full GH markdown
/// TODO: rationalise ename,eTxt,eMess and generate better messages
let errUnpacker (eName, eTxt, eMess) tId lineNo =
    // TODO - add proper error messages with links to HTML documentation
    makeErrorInEditor tId lineNo [
        eName ; 
        eTxt ;  
        eMess
        "[Assembler Guide](https://github.com/tomcl/Visual2/wiki/Assembler)"
        ]

let highlightErrorParse ((err:ParseError), lineNo) tId = 
    let errCode, errStr, errMess = err
    let getErrNames = 
        match errCode with
        | ``Invalid syntax`` -> "Invalid syntax"
        | ``Undefined symbol`` -> "Undefined symbol"
        | ``Invalid literal`` -> "Invalid literal"
        | ``Invalid second operand`` -> "Invalid second operand"
        | ``Invalid offset``  -> "Invalid offset"
        | ``Invalid register``  -> "Invalid register"
        | ``Invalid suffix``  -> "Invalid suffix"
        | ``Invalid instruction``  -> "Invalid instruction"
        | ``Invalid expression`` -> "Invalid expression"
        | ``Label required``  -> "Label required"
        | ``Unimplemented instruction`` -> "Invalid instruction"
        | e -> sprintf "Error Code without getErrNames entry: %A" e
    errUnpacker (getErrNames,errStr, errMess) tId lineNo
    setErrorStatus "Errors in code"


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

let highlightCurrentIns classname pInfo pc tId  =
    removeEditorDecorations tId
    match Map.tryFind (WA pc) pInfo.IMem with
    | Some (ci, lineNo) -> highlightLine tId lineNo classname
    | Option.None
    | Some _ -> failwithf "What? Current PC value (%x) is not an instruction: this should be impossible!" pc


let handleRunTimeError e (pInfo:RunInfo) lastPC =
    let getCodeLineMess pInfo pos =
        match Map.tryFind (WA lastPC) pInfo.IMem with
        | Some (_, lineNo) -> sprintf "on line %d" lineNo
        | _ -> ""
    match e with
    | EXIT ->
        showInfo pInfo
        setExecutionCompleteStatus ()
    | NotInstrMem x -> 
        Browser.window.alert(sprintf "Trying to access non-instruction memory 0x%x" x)
        setErrorStatus "Run time error"
    | ``Run time error`` (pos,msg) ->
        let lineMess = getCodeLineMess pInfo pos
        highlightCurrentIns "editor-line-highlight-error" pInfo lastPC currentFileTabId
        updateRegisters()
        Browser.window.setTimeout( (fun () ->                
            Browser.window.alert(sprintf "Error %s: %s" lineMess msg)
            setErrorStatus "Run time error"), 100, []) |> ignore
    | ``Unknown symbol runtime error`` undefs ->
        Browser.window.alert(sprintf "What? Undefined symbols: %A" undefs)



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

let mutable currentPInfo : RunInfo option = fNone

/// Parses and runs the assembler program in the current tab
/// Aborts after steps instructions, unless steps is 0
let runEditorTab steps =
    let tId = currentFileTabId
    removeEditorDecorations tId
    match tryParseCode tId with
    | Some (lim, _) -> 
        disableEditors()
        let rec asmStepDisplay steps ri =
            if steps <= 50000 then
                let ri,lastPC = asmStep steps ri
                currentPInfo <- Some ri
                match ri with
                | {RunErr = Option.None} ->  highlightCurrentIns "editor-line-highlight" ri lastPC tId
                | {RunErr = Some e } -> handleRunTimeError e ri lastPC
                showInfo ri
            else
                match asmStep 50000 ri with
                | {RunErr = Some e },lastPC -> handleRunTimeError e ri lastPC
                | ri, _ -> 
                  updateRegisters()
                  Browser.window.setTimeout( (fun () -> 
                    asmStepDisplay (steps-50000) ri), 0, []) |> ignore               
        asmStepDisplay  steps (lim |> getRunInfoFromState)
    | _ -> ()

let runCode () = runEditorTab maxStepsToRun

let stepCode() = runEditorTab 1


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

