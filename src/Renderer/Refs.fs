
(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Refs
    Description: F# references to elements in the DOM + some user settings handling
*)

/// F# References to static parts of renderer DOM
module Refs
open CommonData
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Microsoft.FSharp.Collections
open Node.Exports
open EEExtensions


// **********************************************************************************
//                                  App Version
// **********************************************************************************

let appVersion = "1.06.9"

// **********************************************************************************
//                               Types used in this module
// **********************************************************************************

/// Bases to display data in for all Views
/// Udec = unsigned ecimal
type Representations =
    | Hex
    | Bin
    | Dec
    | UDec

/// Select View in RH window
type Views =
    | Registers
    | Memory
    | Symbols

type VSettings = {
    EditorFontSize : string
    SimulatorMaxSteps : string
    EditorTheme : string
    EditorWordWrap : string
    EditorRenderWhitespace : string
    CurrentFilePath : string
    RegisteredKey : string
    OnlineFetchText : string
    }



// ***********************************************************************************************
//                             Top-level Interfaces to Javascript libraries
// ***********************************************************************************************

//------------------------------------------TIPPY.JS----------------------------------------------

/// top-level function from tippy.js to make tooltips
let tippy (rClass : string, tippyOpts : obj) : unit = importDefault "tippy.js"

//-------------------------------------------VEX-JS-----------------------------------------------

/// interfaces to vex.js
[<Emit("require('vex-js');")>]
let vex : obj = jsNative

[<Emit("require('vex-dialog');")>]
let vexDialog : obj = jsNative

vex?defaultOptions?className <- "vex-theme-default"

vex?registerPlugin vexDialog

let vButton (caption : string) =
    let b = createObj [
        "text" ==> caption
        "click" ==> fun () -> "abc"
    ]
    b
let showVexConfirm (htmlMessage : string) (callBack : bool -> unit) =
    vex?dialog?confirm (createObj [
        "unsafeMessage" ==> htmlMessage;
        "callback" ==> callBack ])
    ()

let showVexAlert (htmlMessage : string) =
    vex?dialog?alert (createObj [ "unsafeMessage" ==> htmlMessage ])
    ()

let showVexPrompt (placeHolder : string) (callBack : string -> unit) (htmlMessage : string) =
    vex?dialog?prompt (createObj [
        "unsafeMessage" ==> htmlMessage
        "placeholder" ==> placeHolder
        "callback" ==> callBack
        ])
    ()

let validPosInt s =
    match s with
    | null -> Error ""
    | x ->
        printfn "Error %s" x
        x
        |> System.Int32.TryParse
        |> function
            | true, n when n > 0 -> Ok n
            | true, n -> Error "number must be greater than 0"
            | false, _ -> Error "Input a positive integer"

let showVexValidatedPrompt (placeHolder : string) (validator : string -> Result<'T, string>) (callBack : 'T -> unit) (htmlMessage : string) =
    let cb (s : string) =
        match (unbox s) with
        | false -> ()
        | _ ->
            validator s
            |> function
                | Ok valid -> callBack valid
                | Error errMess -> showVexAlert errMess
    showVexPrompt placeHolder cb htmlMessage

let showSimpleVexPrompt (callBack : string -> unit) (htmlMessage : string) =
    showVexPrompt "" callBack htmlMessage

let testVex() : unit =
    printfn "%A" vex?dialog?buttons?NO
    showVexPrompt "Test!" (fun value -> printfn "Callback value = %A" value) "My new Prompt"

//---------------------------------------------SVG------------------------------------------------

// SVG (scalable vector graphics) library is interfaced via built-in FABLE libraries:
// Fable.Helpers.React
// Fable.Helpers.React.Props
// see tooltips.fs for examples and some documentation of SVG helpers

// ***********************************************************************************************
//                                  Mini DSL for creating DOM objects
// ***********************************************************************************************

let ELEMENT elName classes (htmlElements : HTMLElement list) =
    let ele = document.createElement elName
    ele.classList.add (classes |> List.toArray)
    List.iter (ele.appendChild >> ignore) htmlElements
    ele

let INNERHTML html (ele : HTMLElement) = (ele.innerHTML <- html); ele
let STYLE (name, value) (ele : HTMLElement) = ele.style.setProperty (name, value); ele

let ID name (ele : HTMLElement) = (ele.id <- name); ele
let CLICKLISTENER (listener : unit -> unit) (ele : HTMLElement) = (ele.addEventListener_click (fun _ -> listener() |> ignore; createObj [])); ele

let DIV = ELEMENT "div"

let BR() = document.createElement "br"

let FORM classes contents =
    let form = ELEMENT "form" classes contents
        // disable form submission
    form.onsubmit <- (fun _ -> false :> obj)
    form

let TABLE = ELEMENT "table"

let toDOM text = ELEMENT "span" [] [] |> INNERHTML text

let TROW = ELEMENT "tr" []

let TD x = ELEMENT "td" [] <| [ x ]

// *************************************************************************************
//                               References to DOM elements
// *************************************************************************************

/// look up a DOM element
let getHtml = Browser.document.getElementById
//--------------------------- Buttons -------------------------------

let openFileBtn = getHtml "explore" :?> HTMLButtonElement
let saveFileBtn = getHtml "save" :?> HTMLButtonElement
let runSimulationBtn : HTMLButtonElement = getHtml "run" :?> HTMLButtonElement
let resetSimulationBtn = getHtml "reset" :?> HTMLButtonElement
let stepForwardBtn = getHtml "stepf" :?> HTMLButtonElement
let stepBackBtn = getHtml "stepb" :?> HTMLButtonElement
/// get byte/word switch button element
let byteViewBtn = getHtml "byte-view"

/// get reverse direction element
let reverseViewBtn = getHtml "reverse-view"

/// get memory list element
let memList = getHtml "mem-list"

/// get symbol table View element
let symView = getHtml "sym-view"

/// get symbol table element
let symTable = getHtml "sym-table"

//---------------------File tab elements-------------------------------

/// get element containing all tab headers
let fileTabMenu = getHtml "tabs-files"

/// get last (invisible) tab header
let newFileTab = getHtml "new-file-tab"
// ***********************************************************************************
//                       Functions Relating to Right-hand View Panel
// ***********************************************************************************

/// used to get ID of button for each representation
let repToId =
    Map.ofList [
        Hex, "rep-hex";
        Bin, "rep-bin";
        Dec, "rep-dec";
        UDec, "rep-udec";
    ]

/// used to get ID used in DOM for each View
let viewToIdView =
    Map.ofList [
        Registers, "view-reg";
        Memory, "view-mem";
        Symbols, "view-sym";
    ]
/// used to get Tab ID in DOM for each View
let viewToIdTab =
    Map.ofList [
        Registers, "tab-reg";
        Memory, "tab-mem";
        Symbols, "tab-sym"
    ]
/// Get Flag display element from ID ("C", "V", "N", "Z")
let flag id = getHtml <| sprintf "flag_%s" id

/// get button for specific representation
let representation rep = getHtml repToId.[rep]

/// get View pane element from View
let viewView view = getHtml viewToIdView.[view]

/// get View Tab element from view
let viewTab view = getHtml viewToIdTab.[view]


/// get ID of Tab for Tab number tabID
let fileTabIdFormatter tabID = sprintf "file-tab-%d" tabID

/// get element corresponding to file tab tabID
let fileTab tabID = getHtml <| fileTabIdFormatter tabID

/// get ID of editor window containing file
let fileViewIdFormatter = sprintf "file-view-%d"

/// get element of editor window containing file
let fileView id = getHtml <| fileViewIdFormatter id

/// get pane element containing for tab menu and editors
let fileViewPane = getHtml "file-view-pane"

/// get id of element containing tab name as dispalyed
let tabNameIdFormatter = sprintf "file-view-name-%d"

/// get element containing tab name as displayed
let fileTabName id = getHtml <| tabNameIdFormatter id

/// get id of element containing file path
let tabFilePathIdFormatter = sprintf "file-view-path-%d"
/// get (invisible)  element containing file path
let tabFilePath id = getHtml <| tabFilePathIdFormatter id
/// get the editor window overlay element

//--------------- Simulation Indicator elements----------------------

let darkenOverlay = getHtml "darken-overlay"
/// get element for status-bar button (and indicator)
let statusBar = getHtml "status-bar"

/// Set the background of file panes.
/// This is done based on theme (light or dark) to prevent flicker
let setFilePaneBackground color =
    fileViewPane.setAttribute ("style", sprintf "background: %s" color)

let updateClockTime (n : uint64, m : uint64) =
    getHtml "clock-time"
    |> INNERHTML(if n = 0uL then "-" else sprintf "%d : %d" n m) |> ignore

// ************************************************************************************
//                         Utility functions used in this module
// ************************************************************************************
[<Emit "'0x' + ($0 >>> 0).toString(16).toUpperCase()">]
let hexFormatter _ : string = jsNative

[<Emit "'u' + ($0 >>> 0).toString(10)">]
let uDecFormatter _ : string = jsNative

// Returns a formatter for the given representation
let formatterWithWidth width rep =
 // TODO: Use binformatter from testformats.fs
    let binFormatter width fmt x =
        let bin a =
            [ 0..width - 1 ]
            |> List.fold (fun s x ->
                match ((a >>> x) % 2u), x with
                | 1u, 7 | 1u, 15 | 1u, 23 -> "_1" + s
                | 0u, 7 | 0u, 15 | 0u, 23 -> "_0" + s
                | 1u, _ -> "1" + s
                | 0u, _ -> "0" + s
                | _ -> failwithf "modulo is broken"
            ) ""
        sprintf fmt (bin x)
    match rep with
    | Hex -> hexFormatter
    | Bin -> (binFormatter width "0b%s")
    | Dec -> (int32 >> sprintf "%d")
    | UDec -> uDecFormatter


let formatter = formatterWithWidth 32


/// Determine whether JS value is undefined
[<Emit("$0 === undefined")>]
let isUndefined (_ : 'a) : bool = jsNative

[<Emit("__dirname")>]

let appDirName : string = jsNative

/// compare input with last input: if different, or no last input, execute function
let cacheLastWithActionIfChanged actionFunc =
    let mutable cache : 'a option = None
    fun inDat ->
        match cache with
        | Some i when i = inDat -> ()
        | Some _
        | None ->
            cache <- Some inDat
            actionFunc inDat





/// A reference to the settings for the app
/// persistent using electron-settings
let settings : obj = electron.remote.require "electron-settings"

let mutable vSettings = {
    EditorFontSize = "16"
    SimulatorMaxSteps = "20000"
    EditorTheme = "solarised-dark"
    EditorWordWrap = "off"
    EditorRenderWhitespace = "none"
    CurrentFilePath = Fable.Import.Node.Exports.os.homedir()
    RegisteredKey = ""
    OnlineFetchText = ""
    }

let themes = [
                "one-dark-pro", "One Dark Pro";
                "one-light-pro", "One Light Pro";
                "solarised-dark", "Solarised Dark";
                "solarised-light", "Solarised Light";
              ]
let minFontSize = 6L
let maxFontSize = 60L
let checkPath (p : string) =
    let p' = path.dirname p
    try
        let stat' = fs.statSync (U2.Case1 p')
        let stat = fs.statSync (U2.Case1 p)
        match (stat.isDirectory(), stat'.isDirectory()) with
        | true, _ -> p
        | false, true -> p'
        | _ -> os.homedir()
    with
        | e -> os.homedir()


let checkSettings (vs : VSettings) =
    let vso = vSettings
    try
        let checkNum (n : string) (min : int64) (max : int64) (def : string) =
            match int64 n with
            | x when x > max -> def
            | x when x < min -> def
            | x -> x.ToString()
        {
        vs with
            EditorTheme =
                match List.tryFind (fun (th, _) -> (th = vs.EditorTheme)) themes with
                | Some _ -> vs.EditorTheme
                | _ -> printfn "Setting theme to default"
                       vSettings.EditorTheme
            SimulatorMaxSteps =
                checkNum vs.SimulatorMaxSteps 0L System.Int64.MaxValue vso.SimulatorMaxSteps
            EditorFontSize =
                checkNum vs.EditorFontSize minFontSize maxFontSize vso.EditorFontSize
            CurrentFilePath = checkPath vs.CurrentFilePath
        }
    with
        | _ -> printf "Error parsing stored settings: %A" vs
               vs








let setJSONSettings() =
    let setSetting (name : string) (value : string) =
        printf "Saving JSON: %A" value
        settings?set (name, value) |> ignore
    printfn "Saving settings to this PC: %A" vSettings
    setSetting "JSON" (Fable.Import.JS.JSON.stringify vSettings)


let getJSONSettings() =
    let json = settings?get ("JSON", "undefined")
    printfn "Getting settings"
    match json = "undefined" with
    | true ->
            printfn "No JSON settings found on this PC"
            setJSONSettings()
            vSettings
    | false ->
        try
            let vs = (Fable.Import.JS.JSON.parse json) :?> VSettings
            vs
        with
        | e ->
            printfn "Parse failed: using default settings"
            vSettings

let showMessage1 (callBack : int -> unit) (message : string) (detail : string) (buttons : string list) =
    let rem = electron.remote
    let retFn = unbox callBack
    rem.dialog.showMessageBox (
       (let opts = createEmpty<Fable.Import.Electron.ShowMessageBoxOptions>
        opts.title <- FSharp.Core.Option.None
        opts.message <- message |> Some
        opts.detail <- detail |> Some
        opts.``type`` <- "none" |> Some
        opts.buttons <- buttons |> List.toSeq |> ResizeArray |> Some
        opts), retFn)
    |> ignore

let showAlert (message : string) (detail : string) =
    showVexAlert <|
        sprintf """<p style="text-align:center"><b>%s</b><br>%s</p>""" detail message

let showAlert1 (message : string) (detail : string) =
    let rem = electron.remote
    rem.dialog.showMessageBox (
       (let opts = createEmpty<Fable.Import.Electron.ShowMessageBoxOptions>
        opts.title <- FSharp.Core.Option.None
        opts.message <- message |> Some
        opts.detail <- detail |> Some
        opts.``type`` <- "error" |> Some
        opts))
    |> ignore

/// extract CSS custom variable value
let getCustomCSS (varName : string) =
    let styles = window.getComputedStyle Browser.document.documentElement
    styles.getPropertyValue varName
/// set a custom CSS variable defined in :root pseudoclass
let setCustomCSS (varName : string) (content : string) =
    let element = Browser.document.documentElement
    element.style.setProperty (varName, content)

/// set the CSS variable that determines dashboard width
let setDashboardWidth (width) =
    setCustomCSS "--dashboard-width" width

/// Element in Register view representing register rNum
let register rNum = getHtml <| sprintf "R%i" rNum

let visualDocsPage name =
    match EEExtensions.String.split [| '#' |] name |> Array.toList with
    | [ "" ] -> @"https://tomcl.github.io/visual2.github.io/guide.html#content"
    | [ page ] -> sprintf "https://tomcl.github.io/visual2.github.io/%s.html#content" page
    | [ page; tag ] -> sprintf @"https://tomcl.github.io/visual2.github.io/%s.html#%s" page tag
    | _ -> failwithf "What? Split must return non-empty list!"

/// Run an external URL url in a separate window.
/// Second parameter triggers action (for use in menus)
let runPage url () =
    printf "Running page %s" url
    let rem = electron.remote
    let options = createEmpty<Electron.BrowserWindowOptions>
    // Complete list of window options
    // https://electronjs.org/docs/api/browser-window#new-browserwindowoptions
    options.width <- Some 1200.
    options.height <- Some 800.
    //options.show <- Some false
    let prefs = createEmpty<Electron.WebPreferences>
    prefs.devTools <- Some false
    prefs.nodeIntegration <- Some false
    options.webPreferences <- Some prefs

    options.frame <- Some true
    options.hasShadow <- Some true
    options.backgroundColor <- None
    options.icon <- Some(U2.Case2 "app/visual.ico")
    let window = rem.BrowserWindow.Create(options)
    window.setMenuBarVisibility true
    window.loadURL url
    window.show()

let runExtPage url () =
    electron.shell.openExternal url |> ignore


let writeToFile str path =
    let errorHandler _err = // TODO: figure out how to handle errors which can occur
        ()
    fs.writeFile (path, str, errorHandler)


// ***********************************************************************************************
//                                       Mutable state
// ***********************************************************************************************



let initialFlags = { N = false; Z = false; C = false; V = false }
/// File Tab currently selected (and therefore visible)
let mutable currentFileTabId = -1 // By default no tab is open
/// List of all in use file tabs
let mutable fileTabList : int list = []
/// tab containing current testbench specification (if testbench is loaded)
let mutable testbenchTab : int option = None
/// Map tabIds to the editors which are contained in them
let mutable editors : Map<int, obj> = Map.ofList []
/// Map of content widgets currently on editor, indexed by id
let mutable currentTabWidgets : Map<string, obj> = Map.empty
/// id of tab containing settings form, if this exists
let mutable settingsTab : int option = None
/// The current number representation being used
let mutable currentRep = Hex
/// indicates what the current DOM symbols display representation is
let mutable displayedCurrentRep = Hex
/// The current View in the right-hand pane
let mutable currentView = Registers
/// Whether the Memory View is byte of word based
let mutable byteView = false
/// direction of memory addresses
let mutable reverseDirection = false
/// Number of instructions imulated before break. If 0 run forever
let mutable maxStepsToRun = 50000
/// Contents of data memory
let mutable memoryMap : Map<uint32, uint32> = Map.empty
/// Contents of CPU registers
let mutable regMap : Map<CommonData.RName, uint32> = ExecutionTop.initialRegMap
/// Contents of CPU flags
let mutable flags : CommonData.Flags = initialFlags
/// Values of all Defined Symols
let mutable symbolMap : Map<string, uint32 * ExecutionTop.SymbolType> = Map.empty
/// version of symbolMap currently displayed
let mutable displayedSymbolMap : Map<string, uint32 * ExecutionTop.SymbolType> = Map.empty

/// Current state of simulator
let mutable runMode : ExecutionTop.RunMode = ExecutionTop.ResetMode

/// Global debug level set from main process.
/// 0 => production. 1 => development. 2 => debug parameter.
let mutable debugLevel = 0


/// Online data matched time
let mutable lastOnlineFetchTime : Result<System.DateTime, System.DateTime> =
    Result.Error System.DateTime.Now


//--------------------------------Helper functions to read mutable state-----------------------------------


/// Return the text in tab id tId as a string
let getCode tId : string =
    if tId < 0 then failwithf "No current Editor!"
    let editor = editors.[tId]
    editor?getValue ()

/// Return list of lines in editor tab tId
let textOfTId tId =
    getCode tId
    |> (fun (x : string) -> x.Split [| '\n' |])
    |> Array.toList

let currentTabText() =
    if currentFileTabId < 0 then None
    else
        Some(textOfTId currentFileTabId)



let setRegister (id : CommonData.RName) (value : uint32) =
    let el = register id.RegNum
    el.innerHTML <- formatter currentRep value

let updateRegisters() =
    Map.iter setRegister regMap

let resetRegs() =
    [ 0..15 ]
    |> List.map (fun x ->
        setRegister (CommonData.register x) (
            match x with
            | 13 -> 0xFF000000u
            | _ -> 0u))
    |> ignore

