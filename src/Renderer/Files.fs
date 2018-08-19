(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Files
    Description: IDE file Load and store via electrom main process dialogs
*)

/// implement load and save of assembler files
module Files

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Node.Exports
open Fable.PowerPack
open EEExtensions

open Fable.Import.Browser


open Refs
open Fable
open Settings
open Tabs
open Views

open CommonData
open ExecutionTop




    


//*************************************************************************************
//                           FILE LOAD AND STORE
//*************************************************************************************

let setTabFilePath id path =
    let fp = (tabFilePath id)
    fp.innerHTML <- path
 
let getTabFilePath id =
    let fp = (tabFilePath id)
    fp.innerHTML
/// get file name and extension from path
let baseFilePath (path : string) =
    path.Split [|'/';'\\'|]
    |> Array.last




/// If x is undefined, return errCase, else return Ok x
let resultUndefined errCase x =
    match isUndefined x with
    | true -> Result.Error errCase
    | false -> Result.Ok x

let fileFilterOpts = 
    ResizeArray[ 
        createObj [
            "name" ==> "Assembly Code"
            "extensions" ==> ResizeArray ["s"]
        ]
    ] |> Some

let updateCurrentPath p =
            let dir = p |> String.split [|'\\';'/'|]
            if dir.Length > 1 then
                let path = path.join dir.[0..dir.Length-2]
                printfn "Made path=%A" path
                if (fs.statSync (U2.Case1 path)).isDirectory() then
                    printf "Changing current path from %A to %A" vSettings.CurrentFilePath path
                    vSettings <- {vSettings with CurrentFilePath = path}
                    Refs.setJSONSettings()
            p


let updateCurrentPathFromList (res : string list) =
        printfn "Updating path to: %A" res
        match res with
        | p :: _ ->updateCurrentPath p |> ignore
        | _ -> ()
        res





let writeCurrentCodeToFile path = (Refs.writeToFile (getCode currentFileTabId) path)

let saveFileAs () =
    // Don't do anything if the user tries to save as the settings tab
    match settingsTab with
    | Some x when x = currentFileTabId -> ()
    | _ ->
        let options = createEmpty<SaveDialogOptions>
        options.filters <- fileFilterOpts
        let currentPath = getTabFilePath currentFileTabId
        
        // If a path already exists for this file, open it
        match currentPath with
        | "" -> ()
        | _ -> options.defaultPath <- Some currentPath

        let result = electron.remote.dialog.showSaveDialog(options)

        // Performs op on resPart if x is Ok resPart, then returns x
        let resultIter op x =
            Result.map op x |> ignore
            x

        result
        |> resultUndefined ()
        |> resultIter writeCurrentCodeToFile
        |> resultIter (setTabFilePath currentFileTabId)
        |> resultIter updateCurrentPath
        |> Result.map baseFilePath
        |> Result.map (setTabName currentFileTabId)
        |> Result.map (fun _ -> setTabSaved (currentFileTabId))
        |> ignore

// If a path already exists for a file, write it straight to disk without the dialog
let saveFile () =
    // Save the settings if the current tab is the settings tab
    match settingsTab with
    | Some x when x = currentFileTabId -> 
        getFormSettings()
        setTabSaved (currentFileTabId)
    | _ ->
        match getTabFilePath currentFileTabId with
        | "" -> saveFileAs () // No current path exists
        | path -> 
            writeCurrentCodeToFile path
            setTabSaved (currentFileTabId)
// Figure out if any of the tabs are unsaved
let unsavedFiles () =
    fileTabList
    |> List.map isTabUnsaved
    |> List.fold (||) false

let editorFind () =
    let action = editors.[currentFileTabId]?getAction("actions.find")
    action?run() |> ignore
 
let editorFindReplace () =
    let action = editors.[currentFileTabId]?getAction("editor.action.startFindReplaceAction")
    action?run() |> ignore

let editorUndo () =
    editors.[currentFileTabId]?trigger("Update.fs", "undo") |> ignore

let editorRedo () =
    editors.[currentFileTabId]?trigger("Update.fs", "redo") |> ignore

let editorSelectAll () = 
    editors.[currentFileTabId]?trigger("Update.fs", "selectAll") |> ignore


