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

/// Set View to view
let setView view =
    // Change the active tab
    (viewTab currentView).classList.remove("active")
    (viewTab view).classList.add("active")

    // Change the visibility of the views
    (viewView currentView).classList.add("invisible")
    (viewView view).classList.remove("invisible")

    // ew mutability again, update the variable
    currentView <- view

/// Toggle byte / word view
let toggleByteView () = 
    byteView <- not byteView
    match byteView with
    | true -> 
        byteViewBtn.classList.add("btn-byte-active")
        byteViewBtn.innerHTML <- "Disable Byte View"
    | false -> 
        byteViewBtn.classList.remove("btn-byte-active")
        byteViewBtn.innerHTML <- "Enable Byte View"

/// Converts a memory map to a list of lists which are contiguous blocks of memory
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

/// Converts a list of (uint32 * uint32) to a byte addressed
/// memory list of (uint32 * uint32) which is 4 times longer
/// LITTLE ENDIAN
let lstToBytes (lst : (uint32 * uint32) list) =
    let byteInfo (dat:uint32) =
        let b = dat &&& 0xFFu
        match b with
        | _ when  b >= 32u && b <= 126u -> sprintf "'%c'" (char b), b
        | _ -> "", b
    lst
    |> List.collect (fun (addr, value) -> 
        [
            addr, value |> byteInfo
            addr + 1u, (value >>> 8) |> byteInfo
            addr + 2u, (value >>> 16) |> byteInfo
            addr + 3u, (value >>> 24) |> byteInfo
        ]
    )

/// make an HTML element
/// id = element name
/// css = css class names to add to classlist
/// inner = inner HTML (typically text) for element
let makeElement (id:string)  (css:string) (inner:string) =
        let el = document.createElement id
        el.classList.add css
        el.innerHTML <- inner
        el

/// make an HTML element
/// id = element name
/// css = css class names to add to classlist
let makeEl (id:string)  (css:string) =
        let el = document.createElement id
        el.classList.add css
        el
/// appends child node after last child in parent node, returns parent
/// operator is left associative
/// child: child node
/// node: parent node.
let (&>>) (node:Node) child = 
    node.appendChild child |> ignore
    node

let createDOM (parentID: string) (childList: Node list) = 
    let parent = document.createElement parentID
    List.iter (fun ch -> parent &>> ch |>  ignore) childList
    parent

let addToDOM (parent: Node) (childList: Node list) =
    List.iter (fun ch -> parent &>> ch |>  ignore) childList
    parent    

/// Update Memory view based on byteview, memoryMap, symbolMap
/// Creates the html to format the memory table in contiguous blocks
let updateMemory () =
    let chWidth = 13
    let memPanelShim = 50
    let onlyIfByte x = if byteView then [x] else []

    let invSymbolMap = 
        symbolMap
        |> Map.toList
        |> List.distinctBy (fun (sym,addr) ->addr)
        |> List.map (fun (sym,addr) -> (addr,sym))
        |> Map.ofList

    let lookupSym addr = 
            match Map.tryFind addr invSymbolMap with
            | option.None -> ""
            | Some sym -> sym
    
    let maxTableWidth = 
        memoryMap
        |> Map.map (fun addr dat -> 
                    (lookupSym addr |> String.length) + 
                    (formatter currentRep dat).Length +
                    (sprintf "0x%X" addr).Length
           )
        |> Map.fold (fun x k v -> max x v) 0
        |> (fun w -> w*chWidth + memPanelShim)
       
    let makeRow (addr : uint32, (chRep:string, value : uint32)) =

        let tr = makeEl "tr" "tr-head-mem"

        let rowDat = 
            [
                lookupSym addr
                sprintf "0x%X" addr
                (if byteView then 
                    formatterWithWidth 8 currentRep value + 
                    (chRep |> function | "" -> "" | chr -> sprintf " (%s)" chr)
                else formatter currentRep value)   
            ]

        let makeNode txt = makeElement "td" "selectable-text" txt :> Node

        addToDOM tr (List.map makeNode rowDat)

    let makeContig (lst : (uint32 * uint32) list) = 

        let table = makeEl "table" "table-striped"

        let makeNode txt = makeElement "th" "th-mem" txt :> Node

        let tr = createDOM "tr" <| List.map makeNode ([ "Symbol" ; "Address"; "Value"])

        let byteSwitcher = 
            match byteView with
            | true -> lstToBytes
            | false -> List.map (fun (addr,dat) -> (addr,("",dat)))

        // Add each row to the table from lst
        let rows = 
            lst
            |> byteSwitcher
            |> List.map makeRow

        addToDOM table <| [tr] @ rows 
        |> ignore

        let li = makeEl "li" "list-group-item"
        li.style.padding <- "0px"

        addToDOM li  [table]
    
    // Clear the old memory list
    memList.innerHTML <- ""

    // Add the new memory list
    memoryMap
    |> contiguousMemory
    |> List.map (makeContig >> (fun html -> memList.appendChild(html)))
    |> ignore

/// Update symbol table View using currentRep and symbolMap
let updateSymTable () =

    let makeRow ((sym : string), value : uint32) =
        let tr = makeEl "tr" "tr-head-sym"
        addToDOM tr [
            makeElement "td" "selectable-text" sym
            makeElement "td" "selectable-text" (formatter currentRep value)
            ]

    let tr = 
        createDOM "tr" [
            makeElement "th" "th-mem" "Symbol"
            makeElement "th" "th-mem" "Value"
            ]

    let symTabRows =
        symbolMap
        |> Map.toList
        |> List.map makeRow

    // Clear the old symbol table
    symTable.innerHTML <- ""
    // Add the new one
    addToDOM symTable ([tr] @ symTabRows) |> ignore
 
 //************************************************************************************
 //                          CHANGE EMULATOR STATE
 //************************************************************************************


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

//*************************************************************************************
//                           FILE LOAD AND STORE
//*************************************************************************************

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
        let tId = createNamedFileTab (baseFilePath path) path
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


