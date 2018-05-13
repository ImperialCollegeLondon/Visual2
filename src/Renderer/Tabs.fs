// Code that is specific to individual tabs, e.g. individual editor settings, tab switching etc.
module Tabs

open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Core
open CommonData
open ExecutionTop
open Editor



let initialMemoryMap : Map<uint32, uint32> = Map.ofList []
let initialSymbolMap : Map<string, uint32> = Map.ofList []

let initialRegMap : Map<CommonData.RName, uint32> = 
    [0..15]
    |> List.map ( CommonData.register >> fun rn -> rn,0u)
    |> Map.ofList

let initialFlags =   { N=false ; Z=false; C=false; V=false}  
let mutable currentFileTabId = -1 // By default no tab is open
let mutable fileTabList : int list = []

// Map tabIds to the editors which are contained in them
let mutable editors : Map<int, obj> = Map.ofList []

let mutable settingsTab : int option = Microsoft.FSharp.Core.option.None

// The current number representation being used
let mutable currentRep = Ref.Hex
let mutable currentView = Ref.Registers
let mutable byteView = false
let mutable maxStepsToRun = 50000
let mutable memoryMap : Map<uint32, uint32> = Map.ofList []
let mutable regMap : Map<CommonData.RName,uint32> = initialRegMap
let mutable flags: CommonData.Flags = initialFlags
let mutable symbolMap : Map<string, uint32> = Map.ofList []
let mutable runMode: RunMode = ResetMode

[<Emit "'0x' + ($0 >>> 0).toString(16)">]
let hexFormatter _ : string = jsNative

[<Emit "'u' + ($0 >>> 0).toString(10)">]
let uDecFormatter _ : string = jsNative

// Returns a formatter for the given representation
let formatter rep = 
// TODO: Use binformatter from testformats.fs
    let binFormatter fmt x =
        let bin a =
            [0..31]
            |> List.fold (fun s x -> 
                match ((a >>> x) % 2u) with
                | 1u -> "1" + s
                | 0u -> "0" + s
                | _ -> failwithf "modulo is broken"
            ) ""
        sprintf fmt (bin x)
    match rep with
    | Ref.Hex -> hexFormatter
    | Ref.Bin -> (binFormatter "0b%s")
    | Ref.Dec -> (int32 >> sprintf "%d")
    | Ref.UDec -> uDecFormatter


let setRegister (id: RName) (value: uint32) =
    let el = Ref.register id.RegNum
    el.innerHTML <- formatter currentRep value

let updateRegisters () =
    Map.iter setRegister regMap


let getFlag (id: string) =
    let el = Ref.flag id
    match  el.innerHTML with
    | "1" -> true
    | _ -> false

let setFlag (id: string) (value: bool) =
    let el = Ref.flag id
    match value with
        | false ->
            el.setAttribute("style", "background: #fcfcfc")
            el.innerHTML <- sprintf "%i" 0
        | true ->
            el.setAttribute("style", "background: #4285f4")
            el.innerHTML <- sprintf "%i" 1

let resetRegs () =
    [0..15]
    |> List.map (fun x -> setRegister (CommonData.register x) 0u)
    |> ignore

let resetFlags () =
    setFlag "N" false
    setFlag "C" false
    setFlag "Z" false
    setFlag "V" false

let setStatusButton msg (className:string)=
    let classes = [| "btn-positive";"btn-negative";"btn-primary"|]
    Ref.statusBar.classList.remove classes
    Ref.statusBar.classList.add(className)
    Ref.statusBar.innerHTML <- msg


let setErrorStatus msg = setStatusButton msg "btn-negative"

let setExecutionCompleteStatus () = 
    setStatusButton "Execution Complete" "btn-positive"

let setStepExecutionStatus () = setStatusButton "Stepping" "btn-primary"

let setNoStatus () =
    Ref.statusBar.classList.remove("btn-negative")
    Ref.statusBar.classList.remove("btn-positive")
    Ref.statusBar.classList.remove("btn-primary")
    Ref.statusBar.innerHTML <- "-"

let setMode (rm:RunMode) =
    match rm with
    | ParseErrorMode -> setErrorStatus "Errors in Code"
    | RunErrorMode _ -> setErrorStatus "Runtime Error"
    | ResetMode -> setNoStatus()
    | SteppingMode ri -> setStepExecutionStatus ()
    | FinishedMode ri -> setExecutionCompleteStatus ()
    runMode <- rm




let getSettingsTabId () =
    match settingsTab with
    | Some x -> x
    | _ -> failwithf "No settings tab exists"


let uniqueTabId () =
    // Look in fileTabList and find the next unique id
    match List.isEmpty fileTabList with
    | true -> 0
    | false -> (List.last fileTabList) + 1
let selectFileTab id =
    // Hacky match, but otherwise deleting also attempts to select the deleted tab
    match List.contains id fileTabList || id < 0 with
    | true ->
        Browser.console.log(sprintf "Switching to tab #%d" id)

        // Only remove active from the previously selected tab if it existed
        match currentFileTabId < 0 with
        | false ->
            let oldTab = Ref.fileTab currentFileTabId
            oldTab.classList.remove("active")
            let oldView = Ref.fileView currentFileTabId
            oldView.classList.add("invisible")
        | true -> ()

        // If the new id is -1, no tab is selected
        match id < 0 with
        | true -> ()
        | false ->
            let newTab = Ref.fileTab id
            newTab.classList.add("active")
            let newView = Ref.fileView id
            newView.classList.remove("invisible")

        currentFileTabId <- id
    | false -> ()

let getTabName id = 
    (Ref.fileTabName id).innerHTML

// Determines if a tab of a given id is unsaved
let isTabUnsaved id = 
    (Ref.fileTab id).lastElementChild.classList.contains("unsaved")

let deleteFileTab id =
    let isSettingsTab =
        match settingsTab with
        | Microsoft.FSharp.Core.option.None -> false
        | Some tab when tab = id -> true
        | _ -> false

    // Confirm delete message is slightly different for the settings menu
    let tabName =
        match isSettingsTab with
        | true -> "settings"
        | false -> sprintf "'%s" (getTabName id)

    let confirmDelete = 
        match isTabUnsaved id with
        | false -> true
        | true -> Browser.window.confirm(
                    sprintf "You have unsaved changes, are you sure you want to close %s?" tabName
                    )

    match confirmDelete with
    | false -> ()
    | true ->
        fileTabList <- List.filter (fun x -> x <> id) fileTabList
        match currentFileTabId with
        | x when x = id ->
            selectFileTab
                <| match List.isEmpty fileTabList with
                   | true -> -1
                   | false -> List.last fileTabList
        | _ -> ()
        Ref.fileTabMenu.removeChild(Ref.fileTab id) |> ignore
        Ref.fileViewPane.removeChild(Ref.fileView id) |> ignore
        match isSettingsTab with
        | true -> 
            settingsTab <- Microsoft.FSharp.Core.option.None
        | false ->
            let editor = editors.[id]
            editor?dispose() |> ignore // Delete the Monaco editor
            editors <- Map.remove id editors
    
let setTabUnsaved id = (Ref.fileTabName id).classList.add("unsaved")
let setTabSaved id = (Ref.fileTabName id).classList.remove("unsaved")

let setTabName id name = (Ref.fileTabName id).innerHTML <- name

// Create a new tab of a particular name and then return its id
let createTab name =
    let tab = document.createElement("div")
    tab.classList.add("tab-item")
    tab.classList.add("tab-file")

    let defaultFileName = document.createElement("span")
    defaultFileName.classList.add("tab-file-name")

    let cancel = document.createElement("span")
    cancel.classList.add("icon")
    cancel.classList.add("icon-cancel")
    cancel.classList.add("icon-close-tab")

    let id = uniqueTabId ()
    tab.id <- Ref.fileTabIdFormatter id

    // Create an empty span to store the filepath of this tab
    let filePath = document.createElement("span")
    filePath.classList.add("invisible")
    filePath.id <- Ref.tabFilePathIdFormatter id

    // Add the necessary elements to create the new tab
    tab.appendChild(filePath) |> ignore
    tab.appendChild(cancel) |> ignore
    tab.appendChild(defaultFileName) |> ignore

    defaultFileName.innerHTML <- name

    defaultFileName.id <- Ref.tabNameIdFormatter id

    cancel.addEventListener_click(fun _ -> 
        Browser.console.log(sprintf "Deleting tab #%d" id)
        deleteFileTab id
    )

    tab.addEventListener_click (fun _ -> selectFileTab id)

    fileTabList <- fileTabList @ [id]

    Ref.fileTabMenu.insertBefore(tab, Ref.newFileTab) |> ignore
    setTabSaved id
    id

let createNamedFileTab name =
    let id = createTab name

    // Create the new view div
    let fv = document.createElement("div")
    fv.classList.add("editor")
    fv.classList.add("invisible")    
    fv.id <- Ref.fileViewIdFormatter id

    Ref.fileViewPane.appendChild(fv) |> ignore

    let editor = window?monaco?editor?create(fv, editorOptions())
    
    // Whenever the content of this editor changes
    editor?onDidChangeModelContent(fun _ ->
        setTabUnsaved id // Set the unsaved icon in the tab
    ) |> ignore

    editors <- Map.add id editor editors
    // Return the id of the tab we just created
    id

let createFileTab () = 
    createNamedFileTab "Untitled.S" 
    |> selectFileTab // Switch to the tab we just created

let deleteCurrentTab () =
    match currentFileTabId >= 0 with
    | false -> ()
    | true -> deleteFileTab currentFileTabId
    
let updateEditor tId =
    editors.[tId]?updateOptions(editorOptions()) |> ignore

let setTheme theme = 
    window?monaco?editor?setTheme(theme)

let updateAllEditors () =
    editors
    |> Map.iter (fun tId _ -> updateEditor tId)
    setTheme (editorOptions())?theme |> ignore

// Disable the editor and tab selection during execution
let disableEditors () = 
    Ref.fileTabMenu.classList.add("disabled-click")
    (Ref.fileView currentFileTabId).classList.add("disabled-click")
    Ref.fileViewPane.onclick <- (fun _ ->
        Browser.window.alert("Cannot use editor pane during execution")
    )
    Ref.darkenOverlay.classList.remove("invisible")

// Enable the editor once execution has completed
let enableEditors () =
    Ref.fileTabMenu.classList.remove("disabled-click")
    (Ref.fileView currentFileTabId).classList.remove("disabled-click")
    Ref.fileViewPane.onclick <- ignore
    Ref.darkenOverlay.classList.add("invisible")

let mutable decorations : obj list = []
let mutable lineDecorations : obj list = []

[<Emit "new monaco.Range($0,$1,$2,$3)">]
let monacoRange _ _ _ _ = jsNative

[<Emit "$0.deltaDecorations($1, [
    { range: $2, options: $3},
  ]);">]
let lineDecoration _editor _decorations _range _name = jsNative

[<Emit "$0.deltaDecorations($1, [{ range: new monaco.Range(1,1,1,1), options : { } }]);">]
let removeDecorations _editor _decorations = 
    jsNative

// Remove all text decorations associated with an editor
let removeEditorDecorations tId =
    List.iter (fun x -> removeDecorations editors.[tId] x) decorations
    decorations <- []

let editorLineDecorate editor number decoration =
    let model = editor?getModel()
    let lineWidth = model?getLineMaxColumn(number)
    let newDecs = lineDecoration editor
                    decorations
                    (monacoRange number 1 number lineWidth)
                    decoration
    decorations <- List.append decorations [newDecs]

// highlight a particular line
let highlightLine tId number className = 
    editorLineDecorate 
        editors.[tId]
        number
        (createObj[
            "isWholeLine" ==> true
            "inlineClassName" ==> className
        ])

/// Decorate a line with an error indication and set up a hover message
/// Distinct message lines must be elements of markdownLst
/// markdownLst: string list - list of markdown paragraphs
/// tId: int - tab identifier
/// lineNumber: int - line to decorate, starting at 1
let makeErrorInEditor tId lineNumber (markdownLst:string list) = 
    let makeMarkDown textLst =
        textLst
        |> List.toArray
        |> Array.map (fun txt ->  createObj [ "isTrusted" ==> true; "value" ==> txt ])

    editorLineDecorate 
        editors.[tId]
        lineNumber 
        (createObj[
            "isWholeLine" ==> true
            "isTrusted" ==> true
            "inlineClassName" ==> "editor-line-error"
            "hoverMessage" ==> makeMarkDown markdownLst
        ])