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

/// Number of execution steps before checking if button has been pressed
/// and updating displayed state
let maxStepsBeforeCheckEvents: int64 = 5000L

/// Number of execution steps before slowing down display update
let maxStepsBeforeSlowDisplay: int64 = 100000L
/// Number of execution steps before updating displayed state on long runs.
let slowDisplayThreshold: int64 = 20000L
/// URL to display top-level Instruction Help Guide
let guideHTML = "https://tomcl.github.io/visual2.github.io/guide.html"

let fstOf3 (x,_,_) = x
let dpAndInfo (dp,_,dpi) = dp,dpi


/// Generate the hover error box.
/// Decorate the line with error indication.
/// The text generated is full GH markdown.
let makeHover tId lineNo opc (linkOpt, lines, gLines) =
    let mLink = Refs.visualDocsPage linkOpt
    makeErrorInEditor tId lineNo (
        lines @ 
        [ sprintf "[more](%s)" mLink ])  gLines   

/// Process an editor line parse error. Generate a hover message and line decoration. Set Parse Error Mode
let highlightErrorParse ((err:ParseError), lineNo) tId opc = 
    let ML = EEExtensions.String.split [|'\n'|] >> Array.toList
    let codeLines = Refs.getCode tId |> EEExtensions.String.split [|'\n'|]
    let (gHover,range) =
        if opc <> "" then
            ErrorDocs.getOpcHover "" opc codeLines.[lineNo-1]
        else ([||], (1,1))
    let link, hover = 
        match err with
        | ``Invalid syntax`` (wanted, found, page) ->
            page, (ML <| "Parse error\nLooking for: " + wanted) @ (ML <| "Found: " + found)
        | ``Invalid format`` (error, found, page) ->
            page, (ML <| "Format error\n" + error) @ (ML <| "Found: " + found)
        | ``Invalid instruction`` reason ->
            "", ML "This instruction is not valid" @ ML reason
        | ``Label required`` reason ->
            "", ML "This line needs a label" @ ML reason
        | ``Unimplemented parse`` ->
            "", ML "Unimplemented parse: this is an unexpected error, please inform project maintainers"
        | ``Undefined symbol`` syms ->
            "", ML <| "This line contains an expression with assembler labels '" + syms + "' that have not been defined"
        | ``Invalid opCode`` (root, cond, suffix) ->
            "", sprintf "This opcode: %A%A%A is not valid" root cond suffix |> ML
        | ``Unimplemented instruction`` opcode ->
            "", sprintf "%s is not a valid UAL instruction" opcode |> ML
    let gLink = [ sprintf "[UAL Guide](%s)" (visualDocsPage "list") ]
    makeHover tId lineNo opc (link, hover, (gHover |> Array.toList) @ gLink)
    setMode ParseErrorMode

/// Make map of all data memory locs
let makeDataLocMemoryMap mm =
    Map.toList mm
    |> List.map (fun (WA addr, value) ->
           match value with
           | Dat x -> Some (addr, x)
           | CodeSpace -> Core.Option.None)
    |> List.choose id
    |> Map.ofList

/// Get simulation data locations from current stored map
let getMemoryMap () : Map<WAddr,MemLoc<CondInstr*int>> =
    memoryMap
    |> Map.toList
    |> List.map (fun (a,v) -> WA a , DataLoc v)
    |> Map.ofList

/// Set current stored register values
let setRegs regs =
    regMap <- regs
    updateRegisters()

/// Get current stored register values
let getRegs() = regMap

/// Set all current stored flags    
let setFlags (uFlags:DP.UFlags) =
    let flags = uFlags.F
    setFlag "N" flags.N uFlags.NZU
    setFlag "C" flags.C uFlags.CU
    setFlag "Z" flags.Z uFlags.NZU
    setFlag "V" flags.V uFlags.VU

/// Get all current stored flags
let getFlags() =
    {
        N = getFlag "N"
        C = getFlag "C"
        V = getFlag "V"
        Z = getFlag "Z"
    }

/// Set current stored runMode active
let setCurrentModeActiveFromInfo runState ri =
    setMode (ActiveMode (runState,ri))

let resetEmulator () =
    printfn "Resetting..."
    Editors.removeEditorDecorations currentFileTabId
    Editors.enableEditors()   
    memoryMap <- Map.empty
    symbolMap <- Map.empty
    regMap <- initialRegMap
    setMode ResetMode
    updateMemory()
    updateSymTable()
    updateRegisters ()
    resetRegs()
    resetFlags()
    updateClockTime 0uL
/// Display current execution state in GUI from stored runMode
let showInfoFromCurrentMode () =
    let isStopped = match runMode with | ActiveMode(Running,_) -> true | _ -> false
    match runMode with
    | FinishedMode ri
    | ActiveMode (_, ri)
    | RunErrorMode ri ->
        symbolMap <- ri.st
        updateSymTable()
        let dp,uFl = ri.dpCurrent
        memoryMap <- makeDataLocMemoryMap dp.MM
        if currentView = Refs.Views.Memory || isStopped then
            updateMemory()
        setRegs dp.Regs
        setFlags uFl
        updateRegisters()
        updateClockTime (ri.StepsDone |> uint64) |> ignore
    | _ -> ()

/// Apply GUI decorations to instruction line of last PC and current PC.
/// Move current instruction line to middle of window if not visible.
let highlightCurrentAndNextIns classname pInfo tId  =
    removeEditorDecorations tId
    Tooltips.deleteAllContentWidgets()
    match pInfo.LastDP with
    | None -> ()
    | Some (dp,_uFl) ->
        match Map.tryFind (WA dp.Regs.[R15]) pInfo.IMem with
        | Some (condInstr, lineNo) -> 
            highlightLine tId lineNo classname 
            Editors.revealLineInWindow tId lineNo
            Editors.toolTipInfo (lineNo-1) dp condInstr
        | Option.None
        | Some _ -> failwithf "What? Current PC value (%x) is not an instruction: this should be impossible!" dp.Regs.[R15]
    let pc = (fst pInfo.dpCurrent).Regs.[R15]
    match Map.tryFind (WA pc) pInfo.IMem with
    | Some (condInstr, lineNo) -> 
        highlightNextInstruction tId lineNo
        Editors.toolTipInfo (lineNo-1) (fst pInfo.dpCurrent) condInstr
    | _ -> ()
    
/// Update GUI after a runtime error. Highlight error line (and make it visible).
/// Set suitable error message hover.
let UpdateGUIWithRunTimeError e (pInfo:RunInfo)  =
    let getCodeLineMess pInfo pos =
        match pInfo.LastDP with
        | None -> ""
        | Some (dp,_) ->
            match Map.tryFind (WA dp.Regs.[R15]) pInfo.IMem with
            | Some (_, lineNo) -> sprintf "on line %d" lineNo
            | _ -> ""
    match e with
    | EXIT ->
        setMode (FinishedMode pInfo)
        highlightCurrentAndNextIns "editor-line-highlight" (pInfo) currentFileTabId
        enableEditors()

    | NotInstrMem x -> 
        Browser.window.alert(sprintf "Trying to access non-instruction memory 0x%x" x)
        setMode (RunErrorMode pInfo)

    | ``Run time error`` (pos,msg) ->
        let lineMess = getCodeLineMess pInfo pos
        highlightCurrentAndNextIns "editor-line-highlight-error" pInfo currentFileTabId
        updateRegisters()
        Browser.window.setTimeout( (fun () ->                
            Browser.window.alert(sprintf "Error %s: %s" lineMess msg)
            RunErrorMode pInfo), 100, []) |> ignore
        setMode (RunMode.RunErrorMode pInfo)
       

    | ``Unknown symbol runtime error`` undefs ->
        Browser.window.alert(sprintf "What? Undefined symbols: %A" undefs)
        setMode (RunMode.RunErrorMode pInfo)
    showInfoFromCurrentMode()



/// Return executable image of program in editor tab
let imageOfTId = textOfTId >> reLoadProgram

/// Return true if program in current editor tab has changed from that in pInfo
let currentFileTabProgramIsChanged (pInfo:RunInfo) =
    let txt = textOfTId currentFileTabId
    let txt' = pInfo.EditorText
    txt.Length <> txt'.Length ||
    List.zip txt txt'
    |> List.exists (fun (a,b) ->invariantOfLine a <> invariantOfLine b)



/// Parse text in tId as program. If parse is OK, indent the program.
/// If parse fails decorate the buffer with error info.
/// Return LoadImage on parse success or None.
let tryParseAndIndentCode tId =
    let lim = imageOfTId tId
    let editorASM = lim.EditorText
    // See if any errors exist, if they do display them
    match lim with
    | {Errors=[]} as lim -> 
        //Browser.console.log(sprintf "%A" lim)
        let editor = editors.[tId]
        let newCode = String.concat "\n" lim.Source
        if Refs.getCode tId <> newCode then 
            (editor?setValue newCode) |> ignore
        (lim, lim.Source) |> Some
    | lim -> 
        let processParseError (pe, lineNo, opCode) =
            highlightErrorParse (pe,lineNo) tId opCode
        List.map processParseError lim.Errors |> ignore
        Core.Option.None

/// Return initial RunInfo context from a LoadImage
let getRunInfoFromImage (lim:LoadImage) =
    let getSymTyp sym = 
        match Map.tryFind sym lim.SymInf.SymTypeTab with
        | Some typ -> typ
        | None -> failwithf "What? No type info for symbol %s" sym

    let getData map mm : Map<WAddr,Data> =
        let dLocs = map |> Map.toList
        List.fold (fun mem -> fun (a, x) -> 
            if a > 0xFFFFFFFFu then failwithf "What? invalid address in memory image location: %d: %d" a x
            if x > 0xFFFFFFFFu then failwithf "What? invalid data value in memory image locatio: %d: %d" a x
            Map.add (WA a) (Dat x) mem) mm dLocs

    let dp = {
                Fl = getFlags()
                Regs = getRegs()
                MM = getData memoryMap lim.Mem
             } 
    {
        dpInit=dp; 
        dpCurrent = dp, DP.toUFlags dp.Fl
        State = PSRunning
        st = 
            lim.SymInf.SymTab
            |> Map.map (fun sym addr -> addr, getSymTyp sym) 
        IMem = lim.Code; 
        LastDP = None
        StepsDone=0L
        Source = lim.Source
        EditorText = lim.EditorText
        History = []
    }

/// Execution Step number at which GUI was last updated
let mutable lastDisplayStepsDone = 0L


/// Run the simulation from state ri for steps instructions. 
/// Steps can be positive or negative, for forward or backward stepping.
/// Keep GUI updated from time to time if steps is large positive.
/// Always update GUI at end.
/// Stored history means that backward stepping will always be fast.
let rec asmStepDisplay steps ri =
    let loopMessage() = 
        let steps = Refs.vSettings.SimulatorMaxSteps
        sprintf "WARNING Possible infinite loop: max number of steps (%s) exceeded. To disable this warning use Edit -> Preferences" steps
    let displayState ri' running =
            match ri'.State with
            | PSRunning ->  
                highlightCurrentAndNextIns "editor-line-highlight" ri' currentFileTabId
                showInfoFromCurrentMode()
                if ri.StepsDone < slowDisplayThreshold || (ri.StepsDone - lastDisplayStepsDone) >  maxStepsBeforeSlowDisplay then
                    lastDisplayStepsDone <- ri.StepsDone
                    showInfoFromCurrentMode()
                if running && (int64 Refs.vSettings.SimulatorMaxSteps) <> 0L then  Browser.window.alert( loopMessage() )
            | PSError e -> UpdateGUIWithRunTimeError e ri'
            | PSExit -> UpdateGUIWithRunTimeError EXIT ri'

    match runMode with
    | ActiveMode (Stopping,ri') -> 
        setCurrentModeActiveFromInfo RunState.Paused ri'
        showInfoFromCurrentMode ()
        highlightCurrentAndNextIns "editor-line-highlight" ri' currentFileTabId
    | ResetMode -> ()
    | _ ->
        let stepsNeeded = steps - ri.StepsDone
        let running = stepsNeeded <> 1L
        let stepsMax = maxStepsBeforeCheckEvents
        //printfn "exec with steps=%d and R0=%d" ri.StepsDone ri.dpCurrent.Regs.[R0]
        if stepsNeeded <= stepsMax then
            let ri' = asmStep steps ri
            setCurrentModeActiveFromInfo Paused ri'
            displayState ri' running
        else
            let ri' = asmStep (stepsMax+ri.StepsDone - 1L) ri
            setCurrentModeActiveFromInfo RunState.Running ri'
            showInfoFromCurrentMode()
            match  ri'.State with
            | PSRunning -> 
                 Browser.window.setTimeout( (fun () -> 
                        asmStepDisplay steps ri'), 0, []) |> ignore
            | _ -> displayState ri' true

/// If program has changed reset execution    
let prepareModeForExecution() =
    match runMode with
    | FinishedMode ri
    | RunErrorMode ri
    | ActiveMode (_,ri) ->
        if currentFileTabProgramIsChanged ri then
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
        match tryParseAndIndentCode tId with
        | Some (lim, _) -> 
            disableEditors()
            let ri = lim |> getRunInfoFromImage
            setCurrentModeActiveFromInfo RunState.Running ri
            asmStepDisplay steps ri
        | _ -> ()
    | ActiveMode (RunState.Paused,ri) -> asmStepDisplay  (steps + ri.StepsDone) ri
    | ActiveMode _
    | RunErrorMode _ 
    | FinishedMode _ -> ()


/// Top-level simulation execute
let runCode () = 
    match runMode with
    | FinishedMode _ 
    | RunErrorMode _ -> resetEmulator()
    | _ -> ()
    match runMode with
    | ActiveMode(RunState.Running,ri) -> setCurrentModeActiveFromInfo(RunState.Stopping) ri
    | _ ->
        runEditorTab <|
                match int64 Refs.vSettings.SimulatorMaxSteps with
                | 0L -> System.Int64.MaxValue
                | n when n > 0L -> n
                | _ -> System.Int64.MaxValue

/// Step simulation forward by 1
let stepCode() = 
    runEditorTab 1L

/// Step simulation back by numSteps
let stepCodeBackBy numSteps =
    match runMode with
    | ActiveMode (Paused,ri)
    | RunErrorMode ri
    | FinishedMode ri -> 
        if  currentFileTabProgramIsChanged ri then
            Browser.window.alert "can't step backwards because execution state is no longer valid"
        else
            //printf "Stepping back with done=%d  PC=%A" ri.StepsDone ri.dpCurrent
            let target = 
                match runMode with
                | RunErrorMode ri -> ri.StepsDone + 1L - numSteps
                | _ -> ri.StepsDone - numSteps
            setCurrentModeActiveFromInfo RunState.Running ri

            if target <= 0L then
                resetEmulator()
                removeEditorDecorations currentFileTabId
                showInfoFromCurrentMode()
            else
                printfn "Stepping back to step %d" target
                setCurrentModeActiveFromInfo RunState.Paused ri 
                let ri' = asmStep target ri
                setCurrentModeActiveFromInfo Paused ri'
                disableEditors()
                match ri'.State with
                | PSRunning ->  
                    highlightCurrentAndNextIns "editor-line-highlight" ri' currentFileTabId
                | PSError _ | PSExit -> failwithf "What? Error can't happen when stepping backwards!"
                showInfoFromCurrentMode ()
    | ParseErrorMode -> Browser.window.alert( sprintf "Can't execute when code has errors")
    | ResetMode -> Browser.window.alert( sprintf "Execution has not started")
    | _ -> ()

/// Step simulation back by 1 instruction
let stepCodeBack () = stepCodeBackBy 1L
