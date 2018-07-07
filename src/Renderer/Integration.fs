(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Integration
    Description: Code to integrate the emulator with the renderer
*)

/// integrate emulator code with renderer
module Integration

open Tabs
open Views
open CommonData
open ParseTop
open ExecutionTop
open Errors
open Refs
open Editors

open Fable.Core.JsInterop
open Fable.Import

let maxStepsBeforeCheckEvents: int64 = 5000L
let maxStepsBeforeSlowDisplay: int64 = 100000L
let slowDisplayThreshold: int64 = 20000L

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

let setState runState ri =
    setMode (ActiveMode (runState,ri))

let showInfo () =
    let isStopped = match runMode with | ActiveMode(Running,_) -> true | _ -> false
    match runMode with
    | FinishedMode ri
    | ActiveMode (_, ri)
    | RunErrorMode ri ->
        symbolMap <- ri.st
        updateSymTable()
        let dp = ri.dpCurrent
        memoryMap <- makeMemoryMap dp.MM
        if currentView = Refs.Views.Memory || isStopped then
            updateMemory()
        setRegs dp.Regs
        setFlags dp.Fl
        updateRegisters()
    | _ -> ()

let highlightCurrentIns classname pInfo tId  =
    removeEditorDecorations tId
    match pInfo.LastPC with
    | None -> ()
    | Some pc ->
        match Map.tryFind (WA pc) pInfo.IMem with
        | Some (ci, lineNo) -> 
            highlightLine tId lineNo classname
            Editors.revealLineInWindow tId lineNo
        | Option.None
        | Some _ -> failwithf "What? Current PC value (%x) is not an instruction: this should be impossible!" pc
    

let handleRunTimeError e (pInfo:RunInfo)  =
    let getCodeLineMess pInfo pos =
        match pInfo.LastPC with
        | None -> ""
        | Some pc ->
            match Map.tryFind (WA pc) pInfo.IMem with
            | Some (_, lineNo) -> sprintf "on line %d" lineNo
            | _ -> ""
    match e with
    | EXIT ->
        let prevInstr (pInfo:RunInfo) = { pInfo with LastPC = Option.map (fun n -> n - 4u) pInfo.LastPC }
        setMode (FinishedMode pInfo)
        highlightCurrentIns "editor-line-highlight" (pInfo) currentFileTabId
        enableEditors()

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
        setMode (RunMode.RunErrorMode pInfo)
       

    | ``Unknown symbol runtime error`` undefs ->
        Browser.window.alert(sprintf "What? Undefined symbols: %A" undefs)
        setMode (RunMode.RunErrorMode pInfo)
    showInfo()

let textOfTId tId =
    Files.getCode tId 
    |> (fun (x : string) -> x.Split [|'\n'|]) 
    |> Array.toList

let imageOfTId = textOfTId >> reLoadProgram


let currentFileTabIsChanged (pInfo:RunInfo) =
    let txt = textOfTId currentFileTabId
    txt <> pInfo.EditorText


let tryParseCode tId =
    let lim, indentedAsm = imageOfTId tId

    // See if any errors exist, if they do display them
    match lim with
    | {Errors=[]} as lim -> 
        //Browser.console.log(sprintf "%A" lim)
        let editor = editors.[tId]
        let newCode = String.concat "\n" indentedAsm
        if Files.getCode tId <> newCode then 
            (editor?setValue newCode) |> ignore
        (lim, indentedAsm) |> Some
    | lim -> 
        List.map (fun x -> highlightErrorParse x tId) lim.Errors |> ignore
        Core.Option.None

let getRunInfoFromState (lim:LoadImage) =
    let getSymTyp sym = 
        match Map.tryFind sym lim.SymInf.SymTypeTab with
        | Some typ -> typ
        | None -> failwithf "What? No type info for symbol %s" sym

    let getData map mm : Map<WAddr,Data> =
        let dLocs = map |> Map.toList
        List.fold (fun mem -> fun (a, x) -> Map.add (WA a) (Dat x) mem) mm dLocs

    let dp = {
                Fl = getFlags()
                Regs = getRegs()
                MM = getData memoryMap lim.Mem
             } 
    {
        dpInit=dp; 
        dpCurrent = dp
        State = PSRunning
        st = 
            lim.SymInf.SymTab
            |> Map.map (fun sym addr -> addr, getSymTyp sym) 
        IMem = lim.Code; 
        LastPC = None
        StepsDone=0L
        Source = lim.Source
        EditorText = lim.EditorText
        History = []
    }

let mutable lastDisplayStepsDone = 0L

let loopMessage() = 
    let steps = Refs.vSettings.SimulatorMaxSteps
    sprintf "WARNING Possible infinite loop: max number of steps (%s) exceeded. To disable this warning use Edit -> Preferences" steps
   
let rec asmStepDisplay steps ri =
    let displayState ri' running =
            match ri'.State with
            | PSRunning ->  
                highlightCurrentIns "editor-line-highlight" ri' currentFileTabId
                showInfo()
                if ri.StepsDone < slowDisplayThreshold || (ri.StepsDone - lastDisplayStepsDone) >  maxStepsBeforeSlowDisplay then
                    lastDisplayStepsDone <- ri.StepsDone
                    showInfo()
                if running && (int64 Refs.vSettings.SimulatorMaxSteps) <> 0L then  Browser.window.alert( loopMessage() )
            | PSError e -> handleRunTimeError e ri'
            | PSExit -> handleRunTimeError EXIT ri'

    match runMode with
    | ActiveMode (Stopping,ri') -> 
        setState RunState.Paused ri'
        showInfo ()
        highlightCurrentIns "editor-line-highlight" ri' currentFileTabId
    | ResetMode -> ()
    | _ ->
        let stepsNeeded = steps - ri.StepsDone
        let running = stepsNeeded <> 1L
        let stepsMax = maxStepsBeforeCheckEvents
        //printfn "exec with steps=%d and R0=%d" ri.StepsDone ri.dpCurrent.Regs.[R0]
        if stepsNeeded <= stepsMax then
            let ri' = asmStep steps ri
            setState Paused ri'
            displayState ri' running
        else
            let ri' = asmStep (stepsMax+ri.StepsDone - 1L) ri
            setState RunState.Running ri'
            showInfo()
            match  ri'.State with
            | PSRunning -> 
                 Browser.window.setTimeout( (fun () -> 
                        asmStepDisplay steps ri'), 0, []) |> ignore
            | _ -> displayState ri' true

    
let prepareModeForExecution() =
    match runMode with
    | FinishedMode ri
    | RunErrorMode ri
    | ActiveMode (_,ri) ->
        if currentFileTabIsChanged ri then
            Browser.window.alert "Resetting emulator for new execution" |> ignore
            setMode ResetMode
    | _ -> ()

/// Parses and runs the assembler program in the current tab
/// Aborts after steps instructions, unless steps is 0
let runEditorTab steps =
    prepareModeForExecution()
    match runMode with
    | ResetMode
    | ParseErrorMode _ ->
        let tId = currentFileTabId
        removeEditorDecorations tId
        match tryParseCode tId with
        | Some (lim, _) -> 
            disableEditors()
            let ri = lim |> getRunInfoFromState
            setState RunState.Running ri
            asmStepDisplay steps ri
        | _ -> ()
    | ActiveMode (RunState.Paused,ri) -> asmStepDisplay  (steps + ri.StepsDone) ri
    | ActiveMode _
    | RunErrorMode _ 
    | FinishedMode _ -> ()



let runCode () = 
    match runMode with
    | FinishedMode _ 
    | RunErrorMode _ -> Files.resetEmulator()
    | _ -> ()
    match runMode with
    | ActiveMode(RunState.Running,ri) -> setState(RunState.Stopping) ri
    | _ ->
        runEditorTab <|
                match int64 Refs.vSettings.SimulatorMaxSteps with
                | 0L -> System.Int64.MaxValue
                | n when n > 0L -> n
                | _ -> System.Int64.MaxValue

let stepCode() = 
    runEditorTab 1L

let stepCodeBackBy numSteps =
    match runMode with
    | ActiveMode (Paused,ri)
    | RunErrorMode ri
    | FinishedMode ri -> 
        if  currentFileTabIsChanged ri then
            Browser.window.alert "can't step backwards because execution state is no longer valid"
        else
            //printf "Stepping back with done=%d  PC=%A" ri.StepsDone ri.dpCurrent
            let target = 
                match runMode with
                | RunErrorMode ri -> ri.StepsDone + 1L - numSteps
                | _ -> ri.StepsDone - numSteps
            setState RunState.Running ri

            if target <= 0L then
                Files.resetEmulator()
                removeEditorDecorations currentFileTabId
                showInfo()
            else
                printfn "Stepping back to step %d" target
                setState RunState.Paused ri 
                let ri' = asmStep target ri
                setState Paused ri'
                disableEditors()
                match ri'.State with
                | PSRunning ->  
                    highlightCurrentIns "editor-line-highlight" ri' currentFileTabId
                | PSError _ | PSExit -> failwithf "What? Error can't happen when stepping backwards!"
                showInfo ()
    | ParseErrorMode -> Browser.window.alert( sprintf "Can't execute when code has errors")
    | ResetMode -> Browser.window.alert( sprintf "Execution has not started")
    | _ -> ()

let stepCodeBack () = stepCodeBackBy 1L
