(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Tabs
    Description: handle editor tabs: each can contain a distinct assembly file
*)

/// implement Monaco editor file tabs

module Tabs
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Core
open EEExtensions
open CommonData
open Refs
open Editors

/// Get flag as stored and displayed in GUI
let getFlag (id : string) =
    let el = Refs.flag id
    match el.innerHTML with
    | "1" -> true
    | _ -> false

/// Set flag as stored and displayed in GUI
let setFlag (id : string) (value : bool) (hasChanged : bool) =
    let el = Refs.flag id
    match value with
        | false ->
            el.innerHTML <- sprintf "%i" 0
        | true ->
            el.innerHTML <- sprintf "%i" 1
    match hasChanged with
        | false ->
            el.setAttribute ("style", "background: #fcfcfc")
        | true ->
            el.setAttribute ("style", "background: #4285f4")


/// initialise stored and displayed flags to 0
let resetFlags() =
    setFlag "N" false false
    setFlag "C" false false
    setFlag "Z" false false
    setFlag "V" false false

let setStatusButton msg (className : string) =
    let classes = [| "btn-positive"; "btn-negative"; "btn-primary" |]
    Refs.statusBar.classList.remove classes
    Refs.statusBar.classList.add (className)
    Refs.statusBar.innerHTML <- msg

let setErrorStatus msg = setStatusButton msg "btn-negative"

let setExecutionCompleteStatus() =
    setStatusButton "Execution Complete" "btn-positive"

let setStepExecutionStatus() = setStatusButton "Stepping" "btn-primary"

let setNoStatus() =
    Refs.statusBar.classList.remove ("btn-negative")
    Refs.statusBar.classList.remove ("btn-positive")
    Refs.statusBar.classList.remove ("btn-primary")
    Refs.statusBar.innerHTML <- "-"

let setRunButton (mode : ExecutionTop.RunMode) =
    match mode with
    | ExecutionTop.ActiveMode(ExecutionTop.Running, _) ->
        Refs.runSimulationBtn.innerText <- "Pause";
    | _ ->
        Refs.runSimulationBtn.innerText <- "Run"

let setMode (rm : ExecutionTop.RunMode) =
    match rm with
    | ExecutionTop.ParseErrorMode -> setErrorStatus "Errors in Code"
    | ExecutionTop.RunErrorMode _ -> setErrorStatus "Runtime Error"
    | ExecutionTop.ResetMode ->
        setNoStatus()
        Tooltips.deleteAllContentWidgets()
    | ExecutionTop.ActiveMode(_, _) -> setStepExecutionStatus()
    | ExecutionTop.FinishedMode _ -> setExecutionCompleteStatus()
    setRunButton rm
    Refs.runMode <- rm

let getSettingsTabId() =
    match Refs.settingsTab with
    | Some x -> x
    | _ -> failwithf "No settings tab exists"

let uniqueTabId() =
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
        | true -> Browser.window.confirm (
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
        Tooltips.deleteAllContentWidgets()
        Refs.fileTabMenu.removeChild (Refs.fileTab id) |> ignore
        Refs.fileViewPane.removeChild (Refs.fileView id) |> ignore
        match isSettingsTab with
        | true ->
            Refs.settingsTab <- Microsoft.FSharp.Core.option.None
        | false ->
            let editor = Refs.editors.[id]
            editor?dispose () |> ignore // Delete the Monaco editor
            Refs.editors <- Map.remove id Refs.editors

let setTabUnsaved id =
    let tabName = Refs.fileTabName id
    tabName.classList.add ("unsaved")
    if tabName.innerText.EndsWith " *" |> not then
        tabName.innerText <- tabName.innerText + " *"

let setTabSaved id =
    let tabName = Refs.fileTabName id
    tabName.classList.remove ("unsaved")
    tabName.innerText <-
        let txt = tabName.innerText
        if txt.EndsWith " *"
        then txt.[0..txt.Length - 3]
        else txt

let setTabName id name = (Refs.fileTabName id).innerHTML <- name

// Create a new tab of a particular name and then return its id
let createTab name =
    let id = uniqueTabId()

    DIV [ "tab-item"; "tab-file" ] [
        // filepath element (invisible)
        ELEMENT "span" [ "invisible" ] []
        |> ID(Refs.tabFilePathIdFormatter id)
        // cancel icon (visible on hover)
        ELEMENT "span" [ "icon"; "icon-cancel"; "icon-close-tab" ] []
        |> CLICKLISTENER(fun _ -> deleteFileTab id)
        // filename element (visible)
        ELEMENT "span" [ "tab-file-name" ] []
        |> INNERHTML name
        |> ID(Refs.tabNameIdFormatter id)
    ]
    |> ID(Refs.fileTabIdFormatter id)
    |> CLICKLISTENER(fun _ -> selectFileTab id)
    |> (fun tab -> Refs.fileTabMenu.insertBefore (tab, Refs.newFileTab))
    |> ignore

    Refs.fileTabList <- Refs.fileTabList @ [ id ]
    setTabSaved id
    id

let findNamedFile (name : string) =
    let normalisePath (path : string) =
        path.Split [| '/'; '\\' |]
        |> Array.toList
        |> List.map String.toLower
    if name = "Untitled.s" || name = "" then Core.Option.None
    else
        Refs.fileTabList
        |> List.map (fun id -> (Refs.tabFilePath id).innerText, id)
        |> List.tryFind (fun (path, _) ->
            normalisePath path = normalisePath name)
        |> Core.Option.map (fun (_, id) -> id)

let createNamedFileTab fName fPath =
    match findNamedFile fPath with
    | Some id ->
        // Return existing tab id
        printfn "Found tab %A" id
        selectFileTab id
        id
    | Option.None ->
        let id = createTab fName
        let addEditor (fv) =
            let editor = window?monaco?editor?create (fv, Editors.editorOptions false)
                    // Whenever the content of this editor changes
            editor?onDidChangeModelContent (fun _ ->
                setTabUnsaved id // Set the unsaved icon in the tab
                ) |> ignore
            Refs.editors <- Map.add id editor Refs.editors

        // Create the new fileView div
        let fv =
            DIV [ "editor"; "invisible" ] []
            |> ID(Refs.fileViewIdFormatter id)
        Refs.fileViewPane.appendChild fv |> ignore
        addEditor fv
        id

let createFileTab() =
    createNamedFileTab "Untitled.s" ""
    |> (fun tId -> selectFileTab tId; tId) // Switch to the tab we just created

let deleteCurrentTab() =
    match Refs.currentFileTabId >= 0 with
    | false -> ()
    | true -> deleteFileTab Refs.currentFileTabId

let unsavedTabs() =
    Refs.fileTabList
    |> List.filter isTabUnsaved

