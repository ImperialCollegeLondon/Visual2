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

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mutable mainWindow: BrowserWindow option = Option.None

// Used for right-click context menu
[<Emit("require('electron-context-menu')({});")>]

let contextMenu () = jsNative

let settings:obj = importDefault "electron-settings"

printfn "settings=%A" (settings?get "editor-theme")

let createMainWindow () =
    printfn "Starting to create app window..."
    let options = createEmpty<BrowserWindowOptions>
    // Complete list of window options
    // https://electronjs.org/docs/api/browser-window#new-browserwindowoptions
    options.width <- Some 1200.
    options.height <- Some 800.
    options.show <- Some false
    let prefs = createEmpty<WebPreferences>
    prefs.devTools <- Some true    
    options.webPreferences <- Some prefs
    
    options.frame <- Some true
    options.hasShadow <- Some true
    options.backgroundColor <- Some "#F0F0F0" //"#5F9EA0"
    options.icon <- Some (U2.Case2  "app/visual.ico")
    let window = electron.BrowserWindow.Create(options)
    //window.webContents.openDevTools();
    // Load the index.html of the app.
    let opts = createEmpty<Node.Url.Url<obj>>
    opts.pathname <- Some <| path.join(Node.Globals.__dirname, "/app/index.html")
    opts.protocol <- Some "file:"
    // printfn "Loading HTML: %A, icon=%A" opts.pathname options.icon
    window.loadURL(url.format(opts))
    //window.show()
    printfn "load complete"

      
    #if WATCH
    printfn "Adding extra DEBUG Code..."
    
    fs.watch(path.join(Node.Globals.__dirname, "/main.js"), fun _ _ ->
        window.webContents.reloadIgnoringCache()
    ) |> ignore

    fs.watch(path.join(Node.Globals.__dirname, "/app/js"), fun _ _ ->
        window.webContents.reloadIgnoringCache()
    ) |> ignore
    fs.watch(path.join(Node.Globals.__dirname, "/app/css"), fun _ _ ->
        window.webContents.reloadIgnoringCache()
    ) |> ignore
    fs.watch(path.join(Node.Globals.__dirname, "/app"), fun _ _ ->
        window.webContents.reloadIgnoringCache()
    ) |> ignore
    
    #endif 
    let mutable closeAfterSave = false
    window.on("close", 
                unbox (fun e ->
                    if not closeAfterSave then
                        printfn "Close event received!"
                        e?preventDefault () |> ignore
                        window.webContents.send "closingWindow")) |> ignore
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
    
    electron.ipcMain?on ("doClose", unbox (fun () ->
                 closeAfterSave <- true
                 printfn "Closing window NOW!"
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
        window.focus())
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
    if mainWindow.IsNone then
        createMainWindow()
)) |> ignore







