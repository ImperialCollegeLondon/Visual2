(* 
    High Level Programming @ Imperial College London # Spring 2018
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compliler )
    Contributors: Angelos Filos
    Module: Renderer.Update
    Description: Event helper functions for `HTML` elements in `index.html`.
*)

module Update

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Node.Exports
open Fable.PowerPack

open Fable.Import.Browser


open Ref
open Fable
open Settings
open Tabs

open CommonData
open ExecutionTop




let fontSize (size: int) =
    let options = createObj ["fontSize" ==> size]
    window?code?updateOptions options
    

let setRepresentation rep =
    // Disable the other button
    (representation currentRep).classList.remove("btn-rep-enabled")
    |> ignore

    // Enable the newly pressed button
    let btnNew = representation rep
    btnNew.classList.add("btn-rep-enabled");

    // Reassign currentRep, ew mutability required
    currentRep <- rep

    updateRegisters()

let setView view =
    // Change the active tab
    (viewTab currentView).classList.remove("active")
    (viewTab view).classList.add("active")

    // Change the visibility of the views
    (viewView currentView).classList.add("invisible")
    (viewView view).classList.remove("invisible")

    // ew mutability again, update the variable
    currentView <- view

let toggleByteView () = 
    byteView <- not byteView
    match byteView with
    | true -> 
        byteViewBtn.classList.add("btn-byte-active")
        byteViewBtn.innerHTML <- "Disable Byte View"
    | false -> 
        byteViewBtn.classList.remove("btn-byte-active")
        byteViewBtn.innerHTML <- "Enable Byte View"

// Converts a memory map to a list of lists which are contiguous blocks of memory
let contiguousMemory (mem : Map<uint32, uint32>) =
    Map.toList mem
    |> List.fold (fun state (addr, value) -> 
        match state with
        | [] -> [[(addr, value)]]
        | hd :: tl ->
            match hd with
            | [] -> failwithf "Contiguous memory never starts a new list with no elements"
            | hd' :: _ when fst hd' = addr - 4u -> 
                ((addr, value) :: hd) :: tl // Add to current contiguous block
            | _ :: _ -> [(addr, value)] :: state // Non-contiguous, add to new block
    ) [] 
    |> List.map List.rev // Reverse each list to go back to increasing
    |> List.rev // Reverse the overall list

// Converts a list of (uint32 * uint32) to a byte addressed
// memory list of (uint32 * uint32) which is 4 times longer
// LITTLE ENDIAN
let lstToBytes (lst : (uint32 * uint32) list) =
    lst
    |> List.collect (fun (addr, value) -> 
        [
            addr, value |> byte |> uint32
            addr + 1u, (value >>> 8) |> byte |> uint32
            addr + 2u, (value >>> 16) |> byte |> uint32;
            addr + 3u, (value >>> 24) |> byte |> uint32;
        ]
    )

// Creates the html to format the memory table in contiguous blocks
let updateMemory () =
    let makeRow (addr : uint32, value : uint32) =
        let tr = document.createElement("tr")
        tr.classList.add("tr-head-mem")

        let tdAddr = document.createElement("td")
        tdAddr.classList.add("selectable-text")
        tdAddr.innerHTML <- sprintf "0x%X" addr

        let tdValue = document.createElement("td")
        tdValue.classList.add("selectable-text")
        tdValue.innerHTML <- formatter currentRep value

        tr.appendChild(tdAddr) |> ignore
        tr.appendChild(tdValue) |> ignore
        tr

    let makeContig (lst : (uint32 * uint32) list) = 
        let li = document.createElement("li")
        li.classList.add("list-group-item")
        li.style.padding <- "0px"

        let table = document.createElement("table")
        table.classList.add("table-striped")

        let tr = document.createElement("tr")

        let thAddr = document.createElement("th")
        thAddr.classList.add("th-mem")
        thAddr.innerHTML <- "Address"

        let thValue = document.createElement("th")
        thValue.classList.add("th-mem")
        thValue.innerHTML <- "Value"

        tr.appendChild(thAddr) |> ignore
        tr.appendChild(thValue) |> ignore

        table.appendChild(tr) |> ignore

        let byteSwitcher = 
            match byteView with
            | true -> lstToBytes
            | false -> id

        // Add each row to the table from lst
        lst
        |> byteSwitcher
        |> List.map (makeRow >> (fun html -> table.appendChild(html)))
        |> ignore

        li.appendChild(table) |> ignore
        li
    
    // Clear the old memory list
    memList.innerHTML <- ""

    // Add the new memory list
    memoryMap
    |> contiguousMemory
    |> List.map (makeContig >> (fun html -> memList.appendChild(html)))
    |> ignore

let updateSymTable () =
    let makeRow ((sym : string), value : uint32) =
        let tr = document.createElement("tr")
        tr.classList.add("tr-head-sym")

        let tdSym = document.createElement("td")
        tdSym.classList.add("selectable-text")
        tdSym.innerHTML <- sym

        let tdValue = document.createElement("td")
        tdValue.classList.add("selectable-text")
        tdValue.innerHTML <- formatter currentRep value

        tr.appendChild(tdSym) |> ignore
        tr.appendChild(tdValue) |> ignore
        tr

    // Clear the old symbol table
    symTable.innerHTML <- ""

    let tr = document.createElement("tr")
    let thSym = document.createElement("th")
    let thVal = document.createElement("th")

    thSym.innerHTML <- "Symbol"
    thVal.innerHTML <- "Value"

    thSym.classList.add("th-mem")
    thVal.classList.add("th-mem")

    tr.appendChild(thSym) |> ignore
    tr.appendChild(thVal) |> ignore

    symTable.appendChild(tr) |> ignore

    (List.map (makeRow >> (fun x -> symTable.appendChild(x))) (symbolMap
    |> Map.toList))
    |> ignore


let resetEmulator () =
    printfn "Resetting..."
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

let setTabFilePath id path =
    let fp = (tabFilePath id)
    fp.innerHTML <- path
 
let getTabFilePath id =
    let fp = (tabFilePath id)
    fp.innerHTML

let baseFilePath (path : string) =
    path.Split [|'/';'\\'|]
    |> Array.last

// Load the node Buffer into the specified tab
let loadFileIntoTab tId (fileData : Node.Buffer.Buffer) =
    if currentFileTabId = tId then
        resetEmulator()
    let editor = editors.[tId]
    editor?setValue(fileData.toString("utf8")) |> ignore
    setTabSaved tId

// Return the code in tab id tId as a string
let getCode tId =
    let editor = editors.[tId]
    editor?getValue() :?> string

// If x is undefined, return errCase, else return Ok x
let resultUndefined errCase x =
    match isUndefined x with
    | true -> Result.Error errCase
    | false -> Result.Ok x

let fileFilterOpts = 
    ResizeArray[ 
        createObj [
            "name" ==> "Assembly Code"
            "extensions" ==> ResizeArray ["S"]
        ]
    ] |> Some

let openFile () =
    let options = createEmpty<OpenDialogOptions>
    options.properties <- ResizeArray(["openFile"; "multiSelections"]) |> Some
    options.filters <- fileFilterOpts
    options.defaultPath <- Some (Editor.getSetting "current-file-path")
    let readPath (path, tId) = 
        fs.readFile(path, (fun err data -> // TODO: find out what this error does
            loadFileIntoTab tId data
        ))
        |> ignore
        tId // Return the tab id list again to open the last one

    let makeTab path =
        let tId = createNamedFileTab (baseFilePath path)
        setTabFilePath tId path
        (path, tId)

    let checkResult (res : ResizeArray<string>) =
        match isUndefined res with
        | true -> Result.Error () // No files were opened, so don't do anything
        | false -> Result.Ok (res.ToArray())

    electron.remote.dialog.showOpenDialog(options)
    |> resultUndefined ()
    |> Result.map (fun x -> x.ToArray())
    |> Result.map Array.toList
    |> Result.map (List.map (makeTab >> readPath))
    |> Result.map List.last
    |> Result.map selectFileTab
    |> ignore


let writeToFile str path =
    let errorHandler _err = // TODO: figure out how to handle errors which can occur
        ()
    fs.writeFile(path, str, errorHandler)

let writeCurrentCodeToFile path = (writeToFile (getCode currentFileTabId) path)

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
        |> Result.map baseFilePath
        |> Result.map (setTabName currentFileTabId)
        |> Result.map (fun _ -> setTabSaved (currentFileTabId))
        |> ignore

// If a path already exists for a file, write it straight to disk without the dialog
let saveFile () =
    // Save the settings if the current tab is the settings tab
    match settingsTab with
    | Some x when x = currentFileTabId -> 
        saveSettings()
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


