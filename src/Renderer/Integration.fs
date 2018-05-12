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
    setMode ParseErrorMode


let makeMemoryMap mm =
    Map.toList mm
    |> List.map (fun (WA addr, value) ->
           match value with
           | Dat x -> Some (addr, x)
           | CodeSpace -> Core.Option.None)
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

let showInfo () =
    match runMode with
    | FinishedMode ri
    | SteppingMode ri
    | RunErrorMode ri ->
        symbolMap <- ri.st
        updateSymTable()
        let dp = dpAfterExec ri
        memoryMap <- makeMemoryMap dp.MM
        updateMemory()
        setRegs dp.Regs
        setFlags dp.Fl
    | _ -> ()

let highlightCurrentIns classname pInfo tId  =
    removeEditorDecorations tId
    match Map.tryFind (WA pInfo.LastPC) pInfo.IMem with
    | Some (ci, lineNo) -> highlightLine tId lineNo classname
    | Option.None
    | Some _ -> failwithf "What? Current PC value (%x) is not an instruction: this should be impossible!" pInfo.LastPC


let handleRunTimeError e (pInfo:RunInfo)  =
    let getCodeLineMess pInfo pos =
        match Map.tryFind (WA pInfo.LastPC) pInfo.IMem with
        | Some (_, lineNo) -> sprintf "on line %d" lineNo
        | _ -> ""
    match e with
    | EXIT ->
        setMode (FinishedMode pInfo)
        showInfo ()
    | NotInstrMem x -> 
        Browser.window.alert(sprintf "Trying to access non-instruction memory 0x%x" x)
        setMode (RunErrorMode pInfo)
    | ``Run time error`` (pos,msg) ->
        let lineMess = getCodeLineMess pInfo pos
        highlightCurrentIns "editor-line-highlight-error" pInfo currentFileTabId
        updateRegisters()
        Browser.window.setTimeout( (fun () ->                
            Browser.window.alert(sprintf "Error %s: %s" lineMess msg)
            RunErrorMode pInfo), 100, []) |> ignore
    | ``Unknown symbol runtime error`` undefs ->
        Browser.window.alert(sprintf "What? Undefined symbols: %A" undefs)


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
        Core.Option.None


let getRunInfoFromState (lim:LoadImage) =
    let getData map mm : Map<WAddr,Data> =
        let dLocs = map |> Map.toList
        List.fold (fun mem -> fun (a, x) -> Map.add (WA a) (Dat x) mem) mm dLocs
    let dp = {
                Fl = getFlags()
                Regs = getRegs()
                MM = getData memoryMap lim.Mem
             } 
    {dpInit=dp; st=lim.SymInf.SymTab; IMem = lim.Code; LastPC = dp.Regs.[R15]; dpResult=Result.Ok dp; StepsDone=0}
   
let rec asmStepDisplay steps ri =
    let mode r = SteppingMode r
    printfn "exec with steps=%d and R0=%d" steps (dpAfterExec ri).Regs.[R0]
    if steps <= 50000 then
        let ri = asmStep steps ri
        setMode (mode ri)
        match ri.dpResult with
        | Result.Ok dp ->  
            highlightCurrentIns "editor-line-highlight" ri currentFileTabId
            if steps = 1 then setMode (SteppingMode ri)
        | Result.Error (e,_) -> handleRunTimeError e ri
        showInfo ()
    else
        setMode (mode ri)
        match asmStep 25000 ri with
        | {dpResult = Result.Error (e,_) } -> handleRunTimeError e ri
        | {dpResult = Result.Ok _} -> 
            setMode (mode ri)
            showInfo ()
            Browser.window.setTimeout( (fun () -> 
            asmStepDisplay (steps-25000) ri), 0, []) |> ignore               



/// Parses and runs the assembler program in the current tab
/// Aborts after steps instructions, unless steps is 0
let runEditorTab steps =
    match runMode with
    | ResetMode
    | ParseErrorMode 
    | RunErrorMode _ ->
        let tId = currentFileTabId
        removeEditorDecorations tId
        match tryParseCode tId with
        | Some (lim, _) -> 
            disableEditors()
            asmStepDisplay  steps (lim |> getRunInfoFromState)
        | _ -> ()
    | SteppingMode ri
    | FinishedMode ri ->
        asmStepDisplay  steps ri


let runCode () = runEditorTab maxStepsToRun

let stepCode() =
    match runMode with
    | SteppingMode ri -> runEditorTab (ri.StepsDone + 1)
    | FinishedMode _ -> ()
    | _ -> runEditorTab 1

let stepCodeBackBy numSteps =
    match runMode with
    | SteppingMode ri
    | RunErrorMode ri
    | FinishedMode ri ->
        match asmStepBack numSteps ri with
        | Option.None ->
            printfn "Mode=%A" runMode
            Browser.window.alert( sprintf "Can't step back by %d instructions" numSteps)
        | Some ri' -> 
            runMode <- SteppingMode ri'
            match ri'.dpResult with
            | Result.Ok dp ->  
                highlightCurrentIns "editor-line-highlight" ri' currentFileTabId
                setMode (SteppingMode ri')
            | Result.Error (e,_) -> failwithf "What? Error can't happen when stepping backwards!"
            showInfo ()

    | ParseErrorMode -> Browser.window.alert( sprintf "Can't execute when code has errors")
    | ResetMode -> Browser.window.alert( sprintf "Execution has not started")

let stepCodeBack () = stepCodeBackBy 1

let resetEmulator () =
    removeEditorDecorations currentFileTabId
    enableEditors()   
    memoryMap <- initialMemoryMap
    symbolMap <- initialSymbolMap
    regMap <- initialRegMap
    setMode ResetMode
    updateMemory()
    updateSymTable()
    updateRegisters ()
    resetRegs()
    resetFlags()

