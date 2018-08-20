(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.MenuBar
    Description: Code for the app menus
*)

/// implement menu functions
module MenuBar

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Node.Base
open Refs
open Settings
open Tabs

(****************************************************************************************************
 *
 *                                      MENU OPERATIONS
 *
 ****************************************************************************************************)

 /// Load the node Buffer into the specified tab
let loadFileIntoTab tId (fileData : Node.Buffer.Buffer) =
    if currentFileTabId = tId then
        Integration.resetEmulator()
    let editor = editors.[tId]
    editor?setValue(fileData.toString("utf8")) |> ignore
    setTabSaved tId

let openFile () =
    let options = createEmpty<OpenDialogOptions>
    options.properties <- ResizeArray(["openFile"; "multiSelections"]) |> Some
    options.filters <- Files.fileFilterOpts
    options.defaultPath <- Some vSettings.CurrentFilePath
    let readPath (path, tId) = 
        Node.Exports.fs.readFile(path, (fun err data -> // TODO: find out what this error does
            loadFileIntoTab tId data
        ))
        |> ignore
        tId // Return the tab id list again to open the last one

    let makeTab path =
        let tId = createNamedFileTab (Files.baseFilePath path) path
        Files.setTabFilePath tId path
        (path, tId)

    electron.remote.dialog.showOpenDialog(options)
    |> Files.resultUndefined ()
    |> Result.map (fun x -> x.ToArray())
    |> Result.map Array.toList
    |> Result.map Files.updateCurrentPathFromList
    |> Result.map (List.map (makeTab >> readPath))
    |> Result.map List.last
    |> Result.map selectFileTab
    |> ignore


let loadDemo () =
    Tabs.createFileTab()
    let tId = Refs.currentFileTabId
    let sampleFileName = Tests.sampleDir + "karatsuba.s"
    printfn "Reading sample file: %s" sampleFileName
    Node.Exports.fs.readFile( sampleFileName, (fun _ data -> // TODO: find out what this error does
            loadFileIntoTab  tId data
        ))
    Tabs.setTabSaved tId



/// Check if there is any unsaved info. Display dialog asking for confirmation if there is.
/// Otherwise exit.   
let ExitIfOK() =
    let close() = electron.ipcRenderer.send "doClose" |> ignore
    let callback (result:int) =
        match int result with
        | 0 -> ()
        | _ -> close()
    let tabL = Tabs.unsavedTabs()
    let showQuitMessage (callBack:int ->unit) =
        let rem = electron.remote
        let mess = "You have unsaved changes. Would you like to save them first?"
        let detail = "Your changes will be lost if you don\'t save them."
        let buttons =  [ "Save" ; "Dont Save" ] 
        Refs.showMessage callBack mess detail buttons
    if tabL <> [] then
        showQuitMessage callback
    else close()

(****************************************************************************************************
 *
 *                                  MENU HELPER FUNCTIONS
 *
 ****************************************************************************************************)
       

let menuSeparator = 
    let sep = createEmpty<MenuItemOptions>
    sep.``type`` <- Some Separator
    sep

/// Make action menu item from name, opt key to trigger, and action.
let makeItem (label:string) (accelerator: string option)  (iAction: unit -> unit) =
    let handlerCaster f = System.Func<MenuItem, BrowserWindow, unit> f |> Some
    let item = createEmpty<MenuItemOptions>
    item.label <- Some label
    item.accelerator <- accelerator
    item.click <- handlerCaster (fun _ _ -> iAction())
    item

/// Make role menu from name, opt key to trigger, and action. 
let makeRoleItem label accelerator role = 
    let item = makeItem label accelerator id
    item.role <- U2.Case1 role |> Some
    item

/// Make a new menu from a a list of menu items
let makeMenu (name:string) (table:MenuItemOptions list) =
    let subMenu = createEmpty<MenuItemOptions>
    subMenu.label <- Some name
    subMenu.submenu <-
        table
        |> ResizeArray<MenuItemOptions>
        |> U2.Case2 |> Some
    subMenu

(****************************************************************************************************
 *
 *                                         MENUS
 *
 ****************************************************************************************************)
let fileMenu =
    makeMenu "File" [
            makeItem "New"      (Some "CmdOrCtrl+N")        createFileTab
            menuSeparator
            makeItem "Save"     (Some "CmdOrCtrl+S")        Files.saveFile
            makeItem "Save As"  (Some "CmdOrCtrl+Shift+S")  Files.saveFileAs
            makeItem "Open"     (Some "CmdOrCtrl+O")        openFile
            menuSeparator
            makeItem "Close"    (Some "Ctrl+W")             deleteCurrentTab
            menuSeparator
            makeItem "Quit"     (Some "Ctrl+Q")             ExitIfOK
        ]

/// menu action to create a settings tab
let optCreateSettingsTab() =
    match runMode with
    | ExecutionTop.ResetMode 
    | ExecutionTop.ParseErrorMode -> createSettingsTab ()
    | _ -> Browser.window.alert "Can't change preferences while simulator is running" |> ignore

let editMenu = 
    makeMenu "Edit" [
        makeItem "Undo" (Some "CmdOrCtrl+Z") Files.editorUndo
        makeItem "Redo" (Some "CmdOrCtrl+Shift+Z") Files.editorRedo
        menuSeparator
        makeRoleItem "Cut" (Some "CmdOrCtrl+X") MenuItemRole.Cut
        makeRoleItem "Copy" (Some "CmdOrCtrl+C") MenuItemRole.Copy   
        makeRoleItem "Paste" (Some "CmdOrCtrl+V")  MenuItemRole.Paste
        menuSeparator
        makeItem "Select All"  (Some "CmdOrCtrl+A")  Files.editorSelectAll
        menuSeparator
        makeItem "Find" (Some "CmdOrCtrl+F") Files.editorFind              
        makeItem "Replace"  (Some "CmdOrCtrl+H") Files.editorFindReplace
        menuSeparator
        makeItem "Increase Font Size" (Some "CmdOrCtrl+.") (fun () -> Settings.alterFontSize 2)
        makeItem "Decrease Font Size" (Some "CmdOrCtrl+,") (fun () -> Settings.alterFontSize -2)
        makeItem  "Preferences"  Option.None optCreateSettingsTab
    ]

let viewMenu = 
        let devToolsKey = if Node.Globals.``process``.platform = NodeJS.Platform.Darwin then "Alt+Command+I" else "Ctrl+Shift+I"
        makeMenu "View" [
            makeRoleItem "Toggle Fullscreen"  (Some "F11")  MenuItemRole.Togglefullscreen
            menuSeparator
            makeRoleItem  "Zoom In" (Some "CmdOrCtrl+Plus") MenuItemRole.Zoomin 
            makeRoleItem  "Zoom Out"  (Some "CmdOrCtrl+-") MenuItemRole.Zoomout 
            makeRoleItem  "Reset Zoom"  (Some "CmdOrCtrl+0") MenuItemRole.Resetzoom
            menuSeparator
            makeItem "Toggle Dev Tools" (Some "F12") (electron.remote.getCurrentWebContents()).toggleDevTools
        ]

let helpMenu =
        let runPage page = Refs.runPage page
        makeMenu "Help" [
            makeItem "UAL Instruction Guide" Core.Option.None (runPage <| visualDocsPage "guide#content")
            makeItem "VisUAL2 web pages" Core.Option.None (runPage <| visualDocsPage "")
            makeItem "Official ARM documentation" Core.Option.None (runPage "http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0234b/i1010871.html")
            makeItem "Run Emulator Tests" Core.Option.None Tests.runAllEmulatorTests
            makeItem "Load Complex Demo Code" Core.Option.None loadDemo
            makeItem "Run dev tools FABLE checks" Core.Option.None Playground.check1
            makeItem "About" Core.option.None ( fun () -> 
                printfn "Directory is:%s" (Stats.dirOfSettings())
                electron.remote.dialog.showMessageBox (
                      let opts = createEmpty<ShowMessageBoxOptions>
                      opts.title <- sprintf "Visual2 ARM Simulator v%s" Refs.appVersion |> Some
                      opts.message <- "(c) 2018, Imperial College" |> Some
                      opts.detail <- 
                            "Acknowledgements: Salman Arif (VisUAL), HLP 2018 class" +
                            " (F# reimplementation), with special mention to Thomas Carrotti," +
                            " Lorenzo Silvestri, and HLP Team 10" |> Some
                      opts
                ) |> ignore; () )
         ]   


/// Make all app menus
let mainMenu() =
    let template = 
        ResizeArray<MenuItemOptions> [
            fileMenu
            editMenu
            viewMenu
            helpMenu
        ]
    template
    |> electron.remote.Menu.buildFromTemplate
    |> electron.remote.Menu.setApplicationMenu