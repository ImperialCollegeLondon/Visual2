(* 
    High Level Programming @ Imperial College London # Spring 2018
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Renderer
    Description: Top-level Electron Renderer Process
*)

/// Top-level renderer process function: calls everything else
module Renderer

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

// open DevTools to see the message
// Menu -> View -> Toggle Developer Tools
Browser.console.log "Hi from renderer.fs" |> ignore

open Refs

/// Hack to provide a constant global variable
/// set from command line arguments of main process.
/// 0 => production. 1 => dev. 2 => debug.
let setDebugLevel() =
    let argV = 
        electron.remote.``process``.argv 
        |> Seq.toList 
        |> List.tail
        |> List.map (fun s -> s.ToLower())
    let isArg s = List.contains s argV
    debugLevel <-
        if isArg "--debug" || isArg "-d" then 2
        elif isArg "-w" then 1
        else 0



    

/// Attach a click event on each of the map elements to a function f
/// which accepts the map element as an argument
let mapClickAttacher map (refFinder : 'a -> HTMLElement) f =
    let attachRep ref = (refFinder ref).addEventListener_click(fun _ -> f ref)
    map
    |> Map.toList
    |> List.map (fst >> attachRep)
    |> ignore

/// Called via IPC message from main process whenever main window is resized.
/// Work out any CSS dimensions that must change in response and set
/// them. Note use of CSS custom variables to control multiple
/// CSS properties
let resizeGUI() =
    let headerHeight = (getHtml "vis-header").offsetHeight
    setCustomCSS "--header-height" (sprintf "%.1fpx" headerHeight)

/// Initialization after `index.html` is loaded.
/// Equivalent of main() function
let init () =

    setDebugLevel()
    printfn "Debug level = %d" debugLevel
    // Show the body once we are ready to go!
    document.getElementById("vis-body").classList.remove("invisible")
    
    // Set up window close interlock using IPC from/to main process
    electron.ipcRenderer.on("closingWindow", (fun (event) ->
        MenuBar.ExitIfOK ()        
        )) |> ignore
    
    electron.ipcRenderer.on("resizeWindow", (fun (event) ->
        resizeGUI ()        
        )) |> ignore


    // Actions for the buttons
    Refs.openFileBtn.addEventListener_click(fun _ ->
        MenuBar.openFile ()
    )
    Refs.saveFileBtn.addEventListener_click(fun _ ->
        Files.saveFile ()
    )
    Refs.runSimulationBtn.addEventListener_click(fun _ ->
        Integration.runCode ()
    )
    stepForwardBtn.addEventListener_click(fun _ ->
        Integration.stepCode ()
    )
    stepBackBtn.addEventListener_click(fun _ ->
        Integration.stepCodeBack ()
    )

    resetSimulationBtn.addEventListener_click(fun _ ->
        Integration.resetEmulator()
    )

    mapClickAttacher repToId Refs.representation (fun rep ->
        Browser.console.log (sprintf "Representation changed to %A" rep) |> ignore
        Views.setRepresentation rep |> ignore
        Views.updateMemory ()
        Views.updateSymTable ()
    )

    mapClickAttacher viewToIdTab Refs.viewTab (fun view ->
        Browser.console.log (sprintf "View changed to %A" view) |> ignore
        Views.setView view
    )

    (Refs.byteViewBtn).addEventListener_click(fun _ ->
        Browser.console.log "Toggling byte view" |> ignore
        Views.toggleByteView ()
        Views.updateMemory ()
    )

    (Refs.newFileTab).addEventListener_click(fun _ ->
        Browser.console.log "Creating a new file tab" |> ignore
        Tabs.createFileTab ()
    )

    // create electron menus
    MenuBar.mainMenu()

    // Create an empty tab to start with
    Tabs.createFileTab ()
    printfn "Ending renderer init"
    vSettings <- checkSettings (getJSONSettings())
    Editors.updateAllEditors false

    Tooltips.addFixedToolTips()

/// top-level function that runs the renderer code
let handleMonacoReady (_: Event) = init ()

document.addEventListener("monaco-ready", U2.Case1 handleMonacoReady)
