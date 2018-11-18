(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Main.Main
    Description: Electron Main Process
*)

/// This single module is compiled to JS to make the electron main process that runs node directly on teh host PC and starts up the app.
/// This process is also used for any native resource access (files etc) via electron IPC calls
module Main



open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Node.Exports
open System.Diagnostics

let printHelpMessage() =
    printfn """
Visual2 command line options
----------------------------
-h, --help      - print this help message
-d, --debug     - run with browser dev tools initially open, for startup debug info logged to console
                - <F12> to open or close dev tools after startup
"""

let args = 
    Fable.Import.Node.Globals.``process``.argv 
    |> Seq.toList
    |> List.map (fun s -> s.ToLower())
/// returns true if any of flags are present as command line argument    
let argFlagIsOn (flags:string list) = 
    let fl = List.map (fun (s:string) -> s.ToLower()) flags
    List.exists (fun flag -> List.contains flag args) fl

/// returns true if fl is recognised as valid?
let isValidFlag fl =
    List.contains fl ["-h";"--help";"-d";"--debug";"-w";".";]

/// returns true if we need to disable startup and print help message
let hasHelpArgs() =
    argFlagIsOn ["--help";"-h"] || List.exists (fun arg -> not (isValidFlag arg)) (List.tail args)

if hasHelpArgs() && not (argFlagIsOn ["--help";"-h"]) then
    printfn "Bad arguments: %A" args
 
 /// returns true if we need to add debugging info and printout
let hasDebugArgs() = argFlagIsOn ["--debug";"-d"]

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mutable mainWindow: BrowserWindow option = Option.None

/// ensure that if app is started again the first instance is focussed and the second quits
let shouldQuit = electron.app.makeSingleInstance( fun _ _ ->
        // Someone tried to run a second instance, we should focus our window.
        match  mainWindow with
        | Some win ->  
            if (win.isMinimized()) then
                win.restore();
            win.focus();
        | Core.Option.None -> ())
     
if (shouldQuit) then 
    electron.app.quit()


// Used for right-click context menu
[<Emit("require('electron-context-menu')({});")>]
let contextMenu () = jsNative

let settings:obj = importDefault "electron-settings"

let dTrace fmt s = if hasDebugArgs() then printfn fmt s

dTrace "settings=%A" (settings?get "editor-theme")

let enableHotReload (window:BrowserWindow) =
        printfn "Enabling development hot reload..."
        fs.watch(path.join(Node.Globals.__dirname, "/main.js"), fun _ _ ->
            window.webContents.reloadIgnoringCache()
        ) |> ignore
        fs.watch(path.join(Node.Globals.__dirname, "/app/js"), fun _ _ ->
            window.webContents.reloadIgnoringCache()
        ) |> ignore
        fs.watch(path.join(Node.Globals.__dirname, "/app/css"), fun _ _ ->
            window.webContents.reloadIgnoringCache()
        ) |> ignore
        fs.watch(path.join(Node.Globals.__dirname, "/app/index.html"), fun _ _ ->
            window.webContents.reloadIgnoringCache()
        ) |> ignore


/// create main renderer window of app
let createMainWindow () =
    if hasHelpArgs() then printHelpMessage()
    else 
        if hasDebugArgs() then printfn "Starting to create app window..."
        let options = createEmpty<BrowserWindowOptions>
        // Complete list of window options
        // https://electronjs.org/docs/api/browser-window#new-browserwindowoptions
        options.width <- Some 1200.
        options.height <- Some 800.
        options.show <- Some false
        let prefs = createEmpty<WebPreferences>
        prefs.devTools <- Some (argFlagIsOn ["-w"; "-d"; "--debug"])   
        options.webPreferences <- Some prefs
    
        options.frame <- Some true
        options.hasShadow <- Some true
        options.backgroundColor <-  Some "#5F9EA0"
        options.icon <- Some (U2.Case2  "app/visual.ico")

        let window = electron.BrowserWindow.Create(options)
        if hasDebugArgs() then  window.webContents.openDevTools();
        // Load the index.html of the app.
        let opts = createEmpty<Node.Url.Url<obj>>
        opts.pathname <- Some <| path.join(Node.Globals.__dirname, "/app/index.html")
        opts.protocol <- Some "file:"
        dTrace "Loading HTML: %A" opts.pathname
        window.loadURL(url.format(opts))
        dTrace "%s" "load complete"       
        let mutable closeAfterSave = false
        window.on("close", 
             unbox (fun e ->
                  if not closeAfterSave then
                       dTrace "%s" "Close event received!"
                       e?preventDefault () |> ignore
                       window.webContents.send "closingWindow"
             )) |> ignore
        // Emitted when the window is closed.
        window.on("closed", unbox(fun () ->
            // Dereference the window object, usually you would store windows
            // in an array if your app supports multi windows, this is the time
            // when you should delete the corresponding element.
            mainWindow <- Option.None
            )) |> ignore

        window.on("resize",
            unbox ( fun _ ->
                window.webContents.send "resizeWindow")) |> ignore

        window.webContents.on("new-window",  (fun e x ->
            printfn "Opening new window! %A %A" e x
            e?preventDefault();
            electron.shell.openExternal x |> ignore
        )) |> ignore
        
        electron.ipcMain?on ("doClose", unbox (fun () ->
            closeAfterSave <- true
            dTrace "%s" "Closing window NOW!"
            window?close()
            )) |> ignore


        // Maximize the window
        //window.maximize()

        // Clear the menuBar, this is overwritten by the renderer process
        let template = ResizeArray<MenuItemOptions> [
                            createEmpty<MenuItemOptions>
                        ]
        electron.Menu.setApplicationMenu(electron.Menu.buildFromTemplate(template))

        window.on("ready-to-show", (fun () -> 
            window.show() 
            options.backgroundColor <- Some "#F0F0F0"
            window.focus()
            dTrace "%s" "Window on!"
            if argFlagIsOn ["-w"] then enableHotReload window)
         ) |> ignore

        mainWindow <- Some window

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
electron.app.on("ready", unbox createMainWindow) |> ignore

// Quit when all windows are closed.
electron.app.on("window-all-closed", unbox(fun () ->
    // On OS X it is common for applications and their menu bar
    // to stay active until the user quits explicitly with Cmd + Q
   // if Node.Globals.``process``.platform <> Node.Base.NodeJS.Darwin then
        electron.app.quit()
)) |> ignore

electron.app.on("activate", unbox(fun () ->
    // On OS X it's common to re-create a window in the app when the
    // dock icon is clicked and there are no other windows open.
    if mainWindow.IsNone  then
        createMainWindow()
)) |> ignore

