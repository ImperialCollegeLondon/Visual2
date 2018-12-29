(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Integration
    Description: Code to integrate the emulator with the renderer
*)

/// integrate emulator code with renderer
module Integration

open EEExtensions
open Tabs
open Views
open CommonData
open ParseTop
open ExecutionTop
open Errors
open Refs
open Editors
open TestLib
open Testbench
open Fable.Core.JsInterop
open Fable.Import


/// Number of execution steps before checking if button has been pressed
/// and updating displayed state
let maxStepsBeforeCheckEvents : int64 = 5000L

/// Number of execution steps before slowing down display update
let maxStepsBeforeSlowDisplay : int64 = 100000L
/// Number of execution steps before updating displayed state on long runs.
let slowDisplayThreshold : int64 = 20000L
/// URL to display top-level Instruction Help Guide
let guideHTML = "https://tomcl.github.io/visual2.github.io/guide.html"

let fstOf3 (x, _, _) = x
let dpAndInfo (dp, _, dpi) = dp, dpi


/// Process an editor line parse error. Generate a hover message and line decoration. Set Parse Error Mode
let highlightErrorParse ((err : ParseError), lineNo) tId opc =
    let ML = EEExtensions.String.split [| '\n' |] >> Array.toList
    let codeLines = Refs.getCode tId |> EEExtensions.String.split [| '\n' |]
    let (gHover, range) =
        if opc <> "" then
            ErrorDocs.getOpcHover "" opc codeLines.[lineNo - 1]
        else ([], (1, 1))
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
            let symsMsg =
                match syms with
                | [ sym, msg ] -> sprintf ": %s" msg
                | lst -> List.map snd lst |> String.concat "\n" |> sprintf "s:\n%s"
            "", ML <| "This line contains an expression with undefined symbol" + symsMsg
        | ``Invalid opCode`` (root, cond, suffix) ->
            "", sprintf "This opcode: %A%A%A is not valid" root cond suffix |> ML
        | ``Unimplemented instruction`` opcode ->
            "", sprintf "%s is not a valid UAL instruction" opcode |> ML
        | ``Duplicate symbol`` (sym, lines) ->
            let lineMsg = String.concat ", " (List.map (sprintf "%d") lines)
            "", ML(sprintf "%s: duplicate labels on lines: %s\nDuplicate label names are not allowed" sym lineMsg)
        | ``Literal more than 32 bits`` lit
        | ``Literal is not a valid number`` lit -> "", sprintf "%s is not a valid literal" lit |> ML

    let gLink = []
    let mLink = [ sprintf "[more](%s)" (Refs.visualDocsPage link) ]
    let mHover = hover @ [ "More: see \u26a0" ]
    match err with
    | ``Duplicate symbol`` (sym, lines) -> makeErrorInEditor tId lineNo hover hover
    | _ -> makeErrorInEditor tId lineNo mHover (gHover @ hover @ mLink @ gLink)

    setMode ParseErrorMode

/// Make map of all data memory locs
let makeDataLocMemoryMap mm =
    Map.toList mm
    |> List.map (fun ((WA addr), value) ->
           match value with
           | Dat x -> Some(addr, x)
           | CodeSpace -> Core.Option.None)
    |> List.choose id
    |> Map.ofList

/// Get simulation data locations from current stored map
let getMemoryMap() : Map<WAddr, MemLoc<CondInstr * int>> =
    memoryMap
    |> Map.toList
    |> List.map (fun (a, v) -> WA a, DataLoc v)
    |> Map.ofList

/// Set current stored register values
let setRegs regs =
    regMap <- regs
    updateRegisters()

/// Get current stored register values
let getRegs() = regMap

/// Set all current stored flags
let setFlags (uFlags : DP.UFlags) =
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
    setMode (ActiveMode(runState, ri))

let resetEmulator() =
    printfn "Resetting..."
    Tooltips.deleteAllContentWidgets()
    Editors.removeEditorDecorations currentFileTabId
    Editors.enableEditors()
    memoryMap <- Map.empty
    symbolMap <- Map.empty
    regMap <- initialRegMap
    setMode ResetMode
    updateMemory()
    updateSymTable()
    resetRegs()
    resetFlags()
    updateRegisters()
    updateClockTime (0uL, 0uL)

/// Display current execution state in GUI from stored runMode
let showInfoFromCurrentMode() =
    let isStopped = match runMode with | ActiveMode(Running, _) -> true | _ -> false
    match runMode with
    | FinishedMode ri
    | ActiveMode(_, ri)
    | RunErrorMode ri ->
        symbolMap <- ri.st
        updateSymTable()
        let dp, uFl = ri.dpCurrent
        memoryMap <- makeDataLocMemoryMap dp.MM
        if currentView = Refs.Views.Memory || isStopped then
            updateMemory()
        setRegs dp.Regs
        setFlags uFl
        updateRegisters()
        updateClockTime ((ri.StepsDone |> uint64), (ri.CyclesDone |> uint64)) |> ignore
    | _ -> ()

/// Apply GUI decorations to instruction line of last PC and current PC.
/// Move current instruction line to middle of window if not visible.
let highlightCurrentAndNextIns classname pInfo tId =
    removeEditorDecorations tId
    Tooltips.deleteAllContentWidgets()
    match pInfo.LastDP with
    | None -> ()
    | Some(dp, _uFl) ->
        match Map.tryFind (WA dp.Regs.[R15]) pInfo.IMem with
        | Some(condInstr, lineNo) ->
            highlightLine tId lineNo classname
            Editors.revealLineInWindow tId lineNo
            Editors.toolTipInfo (lineNo - 1, "top") dp condInstr
        | Option.None
        | Some _ ->
            if dp.Regs.[R15] <> 0xFFFFFFFCu then
                failwithf "What? Current PC value (%x) is not an instruction: this should be impossible!" dp.Regs.[R15]
            else
                () // special case of return from testbench call
    let pc = (fst pInfo.dpCurrent).Regs.[R15]
    match Map.tryFind (WA pc) pInfo.IMem with
    | Some(condInstr, lineNo) ->
        highlightNextInstruction tId lineNo
        Editors.toolTipInfo (lineNo - 1, "bottom") (fst pInfo.dpCurrent) condInstr
    | _ -> ()


let handleTest (pInfo : RunInfo) =
    match pInfo.TestState, pInfo.State with
    | _, PSExit
    | _, PSError EXIT
    | _, PSError TBEXIT ->
        match pInfo.TestState with
        | NoTest -> printfn "No test!"; []
        | Testing [] ->
            showAlert "Bad TestState: Testing []" "";
            resetEmulator()
            []
        | Testing(test :: rest) ->
            printfn "Test %d finished!" test.TNum
            let dp = fst pInfo.dpCurrent
            let passed = addResultsToTestbench test dp
            match passed, rest with
            | true, [] ->
                showVexAlert "Tests all passed!"
                resetEmulator()
                []
            | true, rest -> rest
            | false, _ ->
                showVexAlert (sprintf "Test %d has errors!" test.TNum)
                resetEmulator()
                match Testbench.getTBWithTab() with
                | Ok(tbTab, _) -> Tabs.selectFileTab tbTab
                | _ -> ()
                []
    | NoTest, _ -> []
    | _ -> showVexAlert "Test terminated because program has runtime error"
           []

/// Update GUI after a runtime error or exit. Highlight error line (and make it visible).
/// Set suitable error message hover. Update GUI to 'finished' state on program exit.
/// If running a testbench check results on finish and start next test if passed.
let UpdateGUIFromRunState(pInfo : RunInfo) =
    let getCodeLineMess pInfo =
        match pInfo.LastDP with
        | None -> ""
        | Some(dp, _) ->
            match Map.tryFind (WA dp.Regs.[R15]) pInfo.IMem with
            | Some(_, lineNo) -> sprintf "on line %d" lineNo
            | _ -> ""
    match pInfo.State with
    | PSError EXIT
    | PSError TBEXIT
    | PSExit ->
        setMode (FinishedMode pInfo)
        highlightCurrentAndNextIns "editor-line-highlight" (pInfo) currentFileTabId
        enableEditors()

    | PSBreak ->
        setMode (ActiveMode(Paused, pInfo))
        highlightCurrentAndNextIns "editor-line-highlight" (pInfo) currentFileTabId
        enableEditors()

    | PSError(NotInstrMem x) ->
        showVexAlert (sprintf "Trying to access non-instruction memory 0x%x" x)
        setMode (RunErrorMode pInfo)

    | PSError(``Run time error`` (_pos, msg)) ->
        let lineMess = getCodeLineMess pInfo
        highlightCurrentAndNextIns "editor-line-highlight-error" pInfo currentFileTabId
        updateRegisters()
        Browser.window.setTimeout ((fun () ->
            showVexAlert (sprintf "Error %s: %s" lineMess msg)
            RunErrorMode pInfo), 100, []) |> ignore
        setMode (RunMode.RunErrorMode pInfo)

    | PSError(``Unknown symbol runtime error`` undefs) ->
        showVexAlert (sprintf "What? Undefined symbols: %A" undefs)
        setMode (RunMode.RunErrorMode pInfo)
    | PSRunning -> failwithf "What? Invalid pInfo.State=PSRunning. Can't update GUI here if still running"
    showInfoFromCurrentMode()



/// Return executable image of program in editor tab
let imageOfTId = textOfTId >> reLoadProgram

/// Return true if program in current editor tab has changed from that in pInfo
let currentFileTabProgramIsChanged (pInfo : RunInfo) =
    let txt = textOfTId currentFileTabId
    let txt' = pInfo.EditorText
    txt.Length <> txt'.Length ||
    List.zip txt txt'
    |> List.exists (fun (a, b) -> invariantOfLine a <> invariantOfLine b)



/// Parse text in tId as program. If parse is OK, indent the program.
/// If parse fails decorate the buffer with error info.
/// Return LoadImage on parse success or None.
let tryParseAndIndentCode tId =
    let lim = imageOfTId tId
    let editorASM = lim.EditorText
    // See if any errors exist, if they do display them
    match lim with
    | { Errors = [] } as lim ->
        //Browser.console.log(sprintf "%A" lim)
        let editor = editors.[tId]
        let trimmed line = String.trimEnd [| '\r'; '\n' |] line
        let newCode = List.map trimmed lim.Source
        let oldCode = List.map trimmed (Refs.textOfTId tId)
        if oldCode <> newCode then
            if debugLevel > 0 then
                if oldCode.Length <> newCode.Length then
                    printfn "Lengths of indented and old code do not match!"
            (editor?setValue (String.concat "\n" newCode)) |> ignore
        (lim, lim.Source) |> Some
    | lim ->
        let processParseError (pe, lineNo, opCode) =
            highlightErrorParse (pe, lineNo) tId opCode
        List.map processParseError lim.Errors |> ignore
        Core.Option.None



let getRunInfoFromImage bc (lim : LoadImage) =
    getRunInfoFromImageWithInits bc lim (getRegs()) (getFlags()) memoryMap lim.Mem


/// Execution Step number at which GUI was last updated
let mutable lastDisplayStepsDone = 0L

let getTestRunInfo test codeTid breakCond =
    match tryParseAndIndentCode codeTid with
    | Some(lim, _) ->
        let dp = initTestDP (lim.Mem, lim.SymInf.SymTab) test
        Editors.disableEditors()
        match dp with
        | Ok dp -> getRunInfoFromImageWithInits breakCond lim dp.Regs dp.Fl Map.empty dp.MM |> Ok |> Some
        | Error e -> Error e |> Some
    | None -> None

let runThingOnCode thing =
    match Testbench.getTBWithTab() with
    | Ok(tbTab, _) ->
        match fileTabList |> List.filter (fun id -> id <> tbTab) with
        | [] -> showVexAlert "Can't run Tests because no assembly file is loaded!"
        | [ id ] ->
            Tabs.selectFileTab id
            thing()
        | _ -> showVexAlert "Can't run Tests because more than one assembler file is currently loaded. Select the file you wish to test and use Run-> Tests."
    | Error e -> showVexAlert e


let runTests startTest tests stepFunc =
    let codeTid = Refs.currentFileTabId
    match tests with
    | test :: _ ->
        printfn "Running tests"
        match getTestRunInfo test codeTid NoBreak with
        | Some(Ok ri) ->
            let ri' = { ri with TestState = if startTest then NoTest else Testing tests }
            setCurrentModeActiveFromInfo RunState.Running ri'
            stepFunc (if startTest then 1L else System.Int64.MaxValue) ri'
        | Some(Error eMess) -> showVexAlert eMess
        | _ -> showVexAlert "Can't run tests: current tab must have valid assembler"
    | [] -> ()


/// Run the simulation from state ri for steps instructions.
/// Steps can be positive or negative, for forward or backward stepping.
/// Keep GUI updated from time to time if steps is large positive.
/// Always update GUI at end.
/// Stored history means that backward stepping will always be fast.
let rec asmStepDisplay (breakc : BreakCondition) steps ri' =
    let ri = { ri' with BreakCond = breakc }
    let loopMessage() =
        let steps = Refs.vSettings.SimulatorMaxSteps
        sprintf "WARNING Possible infinite loop: max number of steps (%s) exceeded. To disable this warning use Edit -> Preferences" steps
    /// Main subfunction that updates GUI after a bit of simulation.
    /// running: true if trying to run program to end, false if pausing or single stepping.
    /// ri': simulator state including whether we have a program end or break or error termination.
    let displayState ri' running =
            match ri'.State with
            | PSRunning -> // simulation stopped without error or exit
                highlightCurrentAndNextIns "editor-line-highlight" ri' currentFileTabId
                showInfoFromCurrentMode()
                if ri.StepsDone < slowDisplayThreshold || (ri.StepsDone - lastDisplayStepsDone) > maxStepsBeforeSlowDisplay then
                    lastDisplayStepsDone <- ri.StepsDone
                    showInfoFromCurrentMode()
                if running && (int64 Refs.vSettings.SimulatorMaxSteps) <> 0L then showVexAlert (loopMessage())
            | PSError e -> // Something went wrong causing a run-time error
                UpdateGUIFromRunState ri'
            | PSBreak // execution met a valid break condition
            | PSExit -> // end-of-program termination (from END or implicit drop off code section)
                UpdateGUIFromRunState ri'

    match runMode with
    | ActiveMode(Stopping, ri') -> // pause execution from GUI button
        setCurrentModeActiveFromInfo RunState.Paused ri'
        showInfoFromCurrentMode()
        highlightCurrentAndNextIns "editor-line-highlight" ri' currentFileTabId
    | ResetMode -> () // stop everything after reset
    | _ -> // actually do some simulation
        let stepsNeeded = steps - ri.StepsDone // the number of steps still required
        let running = stepsNeeded <> 1L // false if we are single-stepping - a special case
        let stepsMax = maxStepsBeforeCheckEvents // maximum steps to do before updating GUI
        //printfn "exec with steps=%d and R0=%d" ri.StepsDone ri.dpCurrent.Regs.[R0]
        if stepsNeeded <= stepsMax then // in this case we are running to some defined step as part of stepping back, or stepping forward by 1
            let ri' = asmStep steps { ri with BreakCond = NoBreak } // finally run the simulator!
            setCurrentModeActiveFromInfo Paused ri' // mark the fact that we have paused
            displayState ri' running // update GUI
            highlightCurrentAndNextIns "editor-line-highlight" ri' currentFileTabId
        else // in this case we are running indefinitely, or else for a long time
             // if indefinitely, we could stop on display update timeout, or error, or end of program exit
            let ri' = asmStep (stepsMax + ri.StepsDone - 1L) ri // finally run the simulator!
            setCurrentModeActiveFromInfo RunState.Running ri' // mark the fact that we are running
            showInfoFromCurrentMode() // main function to update GUI
            match ri'.State with
            | PSRunning -> //
                 Browser.window.setTimeout ((fun () ->
                        // schedule more simulation in the event loop allowing button-press events
                        asmStepDisplay ri'.BreakCond steps ri'), 0, []) |> ignore
            | _ ->
                displayState ri' false // update GUI
                highlightCurrentAndNextIns "editor-line-highlight" ri' currentFileTabId
                match handleTest ri' with
                | [] -> ()
                | tests -> runTests false tests (asmStepDisplay NoBreak)



/// If program has changed reset execution
let prepareModeForExecution() =
    match runMode with
    | FinishedMode ri
    | RunErrorMode ri
    | ActiveMode(_, ri) ->
        if currentFileTabProgramIsChanged ri then
            showVexAlert "Resetting emulator for new execution" |> ignore
            resetEmulator()
    | _ -> ()

/// Parses and runs the assembler program in the current tab
/// Aborts after steps instructions, unless steps is 0, or
/// if breackCondition happens
let runEditorTab breakCondition steps =
    if currentFileTabId = -1 then
        showVexAlert "No file tab in editor to run!"
        ()
    else
        prepareModeForExecution()
        match runMode with
        | ResetMode
        | ParseErrorMode _ ->
            let tId = currentFileTabId
            removeEditorDecorations tId
            match tryParseAndIndentCode tId with
            | Some(lim, _) ->
                disableEditors()
                let ri = lim |> getRunInfoFromImage breakCondition
                setCurrentModeActiveFromInfo RunState.Running ri
                asmStepDisplay breakCondition steps ri
            | _ -> ()
        | ActiveMode(RunState.Paused, ri) ->
            asmStepDisplay breakCondition (steps + ri.StepsDone) ri
        | ActiveMode _
        | RunErrorMode _
        | FinishedMode _ -> ()



/// Step simulation forward by 1
let stepCode() =
    match currentTabIsTB() with
    | false -> runEditorTab NoBreak 1L
    | true -> showVexAlert "Current file is a testbench: switch to an assembly tab"

/// Step simulation back by numSteps
let stepCodeBackBy numSteps =
    match runMode with
    | ActiveMode(Paused, ri')
    | RunErrorMode ri'
    | FinishedMode ri' ->
        let ri = { ri' with BreakCond = NoBreak }
        if currentFileTabProgramIsChanged ri then
            showVexAlert "can't step backwards because execution state is no longer valid"
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
                | PSError _ | PSExit | PSBreak -> failwithf "What? Error can't happen when stepping backwards!"
                showInfoFromCurrentMode()
    | ParseErrorMode -> showVexAlert (sprintf "Can't execute when code has errors")
    | ResetMode -> showVexAlert (sprintf "Execution has not started")
    | _ -> ()

/// Step simulation back by 1 instruction
let stepCodeBack() = stepCodeBackBy 1L


let runEditorTabOnTests (tests : Test list) =
        if tests = [] then showVexAlert "There are no Tests to run in the testbench!"
        let runT() = runTests false tests (asmStepDisplay NoBreak)
        prepareModeForExecution()
        match runMode with
        | ResetMode
        | ParseErrorMode _ ->
            let tId = Refs.currentFileTabId
            Editors.removeEditorDecorations tId
            runT()
        | ActiveMode _
        | RunErrorMode _
        | FinishedMode _ ->
            resetEmulator();
            runT()

let runTestbench() =
    match getParsedTests 0x80000000u with
    | Error(mess) ->
        showVexAlert mess
    | Ok(tabId, tests) when Refs.currentFileTabId = tabId ->
        showVexAlert "Please select the program tab which you want to test - not the testbench"
    | Ok(_, tests) ->
        printfn "Running %d Tests" tests.Length
        runEditorTabOnTests tests

let runTestbenchOnCode() =
    runThingOnCode runTestbench


let startTest test =
    runThingOnCode (fun () -> runTests true [ test ] (asmStepDisplay NoBreak))

/// Top-level simulation execute
/// If current tab is TB run TB if this is possible
let runCode breakCondition () =
    match currentTabIsTB() with
    | true -> runTestbenchOnCode()
    | false ->
        match runMode with
        | FinishedMode _
        | RunErrorMode _ -> resetEmulator()
        | _ -> ()
        match runMode with
        | ActiveMode(RunState.Running, ri) -> setCurrentModeActiveFromInfo (RunState.Stopping) ri
        | _ ->
            runEditorTab breakCondition <|
                match int64 Refs.vSettings.SimulatorMaxSteps with
                | 0L -> System.Int64.MaxValue
                | n when n > 0L -> n
                | _ -> System.Int64.MaxValue
