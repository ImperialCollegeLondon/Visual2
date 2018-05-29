// Code that is specific to individual tabs, e.g. individual editor settings, tab switching etc.
module Tabs

open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Core
open EEExtensions
open CommonData
open Refs





[<Emit "'0x' + ($0 >>> 0).toString(16)">]
let hexFormatter _ : string = jsNative

[<Emit "'u' + ($0 >>> 0).toString(10)">]
let uDecFormatter _ : string = jsNative

// Returns a formatter for the given representation
let formatterWithWidth width rep = 
// TODO: Use binformatter from testformats.fs
    let binFormatter width fmt x =
        let bin a =
            [0..width-1]
            |> List.fold (fun s x -> 
                match ((a >>> x) % 2u),x with
                | 1u,7 | 1u,15 | 1u,23 -> "_1" + s
                | 0u,7 | 0u,15 | 0u,23 -> "_0" + s
                | 1u,_ -> "1" + s
                | 0u,_ -> "0" + s
                | _ -> failwithf "modulo is broken"
            ) ""
        sprintf fmt (bin x)
    match rep with
    | Refs.Hex -> hexFormatter
    | Refs.Bin -> (binFormatter width "0b%s")
    | Refs.Dec -> (int32 >> sprintf "%d")
    | Refs.UDec -> uDecFormatter

let formatter = formatterWithWidth 32

let setRegister (id: RName) (value: uint32) =
    let el = Refs.register id.RegNum
    el.innerHTML <- formatter Refs.currentRep value

let updateRegisters () =
    Map.iter setRegister Refs.regMap


let getFlag (id: string) =
    let el = Refs.flag id
    match  el.innerHTML with
    | "1" -> true
    | _ -> false

let setFlag (id: string) (value: bool) =
    let el = Refs.flag id
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
    Refs.statusBar.classList.remove classes
    Refs.statusBar.classList.add(className)
    Refs.statusBar.innerHTML <- msg


let setErrorStatus msg = setStatusButton msg "btn-negative"

let setExecutionCompleteStatus () = 
    setStatusButton "Execution Complete" "btn-positive"

let setStepExecutionStatus () = setStatusButton "Stepping" "btn-primary"

let setNoStatus () =
    Refs.statusBar.classList.remove("btn-negative")
    Refs.statusBar.classList.remove("btn-positive")
    Refs.statusBar.classList.remove("btn-primary")
    Refs.statusBar.innerHTML <- "-"

let setRunButton (mode:ExecutionTop.RunMode) =
    match mode with 
    | ExecutionTop.ActiveMode (ExecutionTop.Running,_) ->
        Refs.runSimulationBtn.innerText <- "Pause"; 
    |_ -> 
        Refs.runSimulationBtn.innerText <- "Run"

let setMode (rm:ExecutionTop.RunMode) =
    match rm with
    | ExecutionTop.ParseErrorMode -> setErrorStatus "Errors in Code"
    | ExecutionTop.RunErrorMode _ -> setErrorStatus "Runtime Error"
    | ExecutionTop.ResetMode -> setNoStatus()
    | ExecutionTop.ActiveMode (_,_) -> setStepExecutionStatus ()
    | ExecutionTop.FinishedMode _ -> setExecutionCompleteStatus ()
    setRunButton rm
    Refs.runMode <- rm


    


let getSettingsTabId () =
    match Refs.settingsTab with
    | Some x -> x
    | _ -> failwithf "No settings tab exists"


let uniqueTabId () =
    // Look in fileTabList and find the next unique id
    match List.isEmpty Refs.fileTabList with
    | true -> 0
    | false -> (List.last Refs.fileTabList) + 1

let selectFileTab id =
    // Hacky match, but otherwise deleting also attempts to select the deleted tab
    match List.contains id Refs.fileTabList || id < 0 with
    | true ->
        // Only remove active from the previously selected tab if it existed
        match Refs.currentFileTabId < 0 with
        | false ->
            (Refs.fileTab Refs.currentFileTabId).classList.remove("active")
            (Refs.fileView Refs.currentFileTabId).classList.add("invisible")
        | true -> ()

        // If the new id is -1, no tab is selected
        match id < 0 with
        | true -> ()
        | false ->
            (Refs.fileTab id).classList.add("active")
            (Refs.fileView id).classList.remove("invisible")

        Refs.currentFileTabId <- id
    | false -> ()

let getTabName id = 
    (Refs.fileTabName id).innerHTML

// Determines if a tab of a given id is unsaved
let isTabUnsaved id = 
    (Refs.fileTab id).lastElementChild.classList.contains("unsaved")

let deleteFileTab id =
    let isSettingsTab =
        match Refs.settingsTab with
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
                    sprintf "You have unsaved changes, are you sure you want to close %s?" (String.replace "*" "" tabName)
                    )

    match confirmDelete with
    | false -> ()
    | true ->
        Refs.fileTabList <- List.filter (fun x -> x <> id) Refs.fileTabList
        match Refs.currentFileTabId with
        | x when x = id ->
            selectFileTab
                <| match List.isEmpty Refs.fileTabList with
                   | true -> -1
                   | false -> List.last Refs.fileTabList
        | _ -> ()
        Refs.fileTabMenu.removeChild(Refs.fileTab id) |> ignore
        Refs.fileViewPane.removeChild(Refs.fileView id) |> ignore
        match isSettingsTab with
        | true -> 
            Refs.settingsTab <- Microsoft.FSharp.Core.option.None
        | false ->
            let editor = Refs.editors.[id]
            editor?dispose() |> ignore // Delete the Monaco editor
            Refs.editors <- Map.remove id Refs.editors
    
let setTabUnsaved id = 
    let tabName = Refs.fileTabName id
    tabName.classList.add("unsaved")
    if tabName.innerText.EndsWith " *" |> not then
        tabName.innerText <- tabName.innerText + " *"

let setTabSaved id = 
    let tabName = Refs.fileTabName id
    tabName.classList.remove("unsaved")
    tabName.innerText <- 
        let txt = tabName.innerText
        if txt.EndsWith " *"
        then txt.[0..txt.Length - 3]
        else txt 

let setTabName id name = (Refs.fileTabName id).innerHTML <- name

// Create a new tab of a particular name and then return its id
let createTab name =
    let id = uniqueTabId ()

    DIV ["tab-item";"tab-file"] [
        // filepath element (invisible)
        ELEMENT "span" ["invisible"] []
        |> ID  (Refs.tabFilePathIdFormatter id)
        // cancel icon (visible on hover)
        ELEMENT "span" ["icon";"icon-cancel";"icon-close-tab"] []
        |> CLICKLISTENER (fun _ -> deleteFileTab id)
        // filename element (visible)
        ELEMENT "span" ["tab-file-name"] []
        |> INNERHTML name
        |> ID (Refs.tabNameIdFormatter id)
    ]
    |> ID (Refs.fileTabIdFormatter id)
    |> CLICKLISTENER (fun _ -> selectFileTab id)
    |> (fun tab -> Refs.fileTabMenu.insertBefore(tab, Refs.newFileTab))
    |> ignore

    Refs.fileTabList <- Refs.fileTabList @ [id]
    setTabSaved id
    id

let findNamedFile (name:string) =
    let normalisePath (path:string) =
        path.Split [| '/' ; '\\'|]
        |> Array.toList
        |> List.map String.toLower

    Refs.fileTabList
    |> List.map (fun id -> (Refs.tabFilePath id).innerText, id)
    |> List.tryFind (fun (path,_) -> 
        normalisePath path = normalisePath name)
    |> Core.Option.map (fun (_,id) -> id)
    

let createNamedFileTab fName fPath=
    let unusedTab = 
        Refs.fileTabList 
        |> List.filter (fun tid -> getTabName tid = "Untitled.s")
    match findNamedFile fPath, unusedTab with
    | Some id,_ -> 
        // Return existing tab id
        printfn "Found tab %A" id
        selectFileTab id
        id 
    | _, [tId] ->
        printfn "Found unused tab %A" tId
        selectFileTab tId
        (Refs.fileTabName tId).innerHTML <- fName
        (Refs.tabFilePath tId).innerHTML <- fPath
        tId   
    | Option.None, _ -> 
        let id = createTab fName

        let addEditor (fv ) =
            let editor = window?monaco?editor?create(fv, Refs.editorOptions())   
                    // Whenever the content of this editor changes
            editor?onDidChangeModelContent(fun _ ->
                setTabUnsaved id // Set the unsaved icon in the tab
                ) |> ignore
            Refs.editors <- Map.add id editor Refs.editors

        // Create the new fileView div
        let fv =
            DIV ["editor";"invisible"] []
            |> ID (Refs.fileViewIdFormatter id)
        Refs.fileViewPane.appendChild fv |> ignore
        addEditor fv
        id

let createFileTab () = 
    createNamedFileTab "Untitled.s" ""
    |> selectFileTab // Switch to the tab we just created

let deleteCurrentTab () =
    match Refs.currentFileTabId >= 0 with
    | false -> ()
    | true -> deleteFileTab Refs.currentFileTabId

let unsavedTabs() =
    Refs.fileTabList
    |> List.filter isTabUnsaved

