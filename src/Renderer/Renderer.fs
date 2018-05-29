(* 
    High Level Programming @ Imperial College London # Spring 2018
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compliler )
    Contributors: Angelos Filos, Thomas Clarke
    Module: Main
    Description: Electron Renderer Process - Top-level F# Script to execute after `index.html` is loaded.
*)

module Renderer

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

// open DevTools to see the message
// Menu -> View -> Toggle Developer Tools
Browser.console.log "Hi from the renderer.js" |> ignore

open Refs
open Views
open MenuBar
open Tabs


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
    // Show the body once we are ready to go!
    document.getElementById("vis-body").classList.remove("invisible")
    
    // Set up window close interlock using IPC from/to main process
    electron.ipcRenderer.on("closingWindow", (fun (event) ->
        checkOKToClose ()        
        )) |> ignore
    
    electron.ipcRenderer.on("resizeWindow", (fun (event) ->
        resizeGUI ()        
        )) |> ignore


    // Actions for the buttons
    Refs.openFileBtn.addEventListener_click(fun _ ->
        Files.openFile ()
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
        Files.resetEmulator()
    )

    mapClickAttacher repToId Refs.representation (fun rep ->
        Browser.console.log (sprintf "Representation changed to %A" rep) |> ignore
        setRepresentation rep |> ignore
        updateMemory ()
        updateSymTable ()
    )

    mapClickAttacher viewToIdTab Refs.viewTab (fun view ->
        Browser.console.log (sprintf "View changed to %A" view) |> ignore
        setView view
    )

    (Refs.byteViewBtn).addEventListener_click(fun _ ->
        Browser.console.log "Toggling byte view" |> ignore
        toggleByteView ()
        updateMemory ()
    )

    (Refs.newFileTab).addEventListener_click(fun _ ->
        Browser.console.log "Creating a new file tab" |> ignore
        createFileTab ()
    )

    // Create an empty tab to start with
    createFileTab ()
    Settings.vSettings <- Settings.getVisualSettings()
    updateAllEditors()


setMainMenu Tests.runAllEmulatorTests // pass this out-of-order dependency in to the menu code.

let handleMonacoReady (_: Event) = init ()

let listener: U2<EventListener, EventListenerObject> = U2.Case1 handleMonacoReady

document.addEventListener("monaco-ready", listener)
