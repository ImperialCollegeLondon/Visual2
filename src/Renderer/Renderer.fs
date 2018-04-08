(* 
    High Level Programming @ Imperial College London # Spring 2018
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compliler )
    Contributors: Angelos Filos
    Module: Main
    Description: Electron Renderer Process - Script to executed after `index.html` is loaded.
*)

module Renderer

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

// open DevTools to see the message
// Menu -> View -> Toggle Developer Tools
Browser.console.log "Hi from the renderer.js" |> ignore

open Ref
open Update
open MenuBar
open Tabs
open Integration

// Attach a click event on each of the map elements to a function f
// Which accepts the map element as an argument
let mapClickAttacher map (refFinder : 'a -> HTMLElement) f =
    let attachRep ref = (refFinder ref).addEventListener_click(fun _ -> f ref)
    map
    |> Map.toList
    |> List.map (fst >> attachRep)
    |> ignore

/// Initialization after `index.html` is loaded
let init () =
    // Show the body once we are ready to go!
    document.getElementById("vis-body").classList.remove("invisible")

    // TODO: Implement actions for the buttons
    Ref.explore.addEventListener_click(fun _ ->
        openFile ()
    )
    Ref.save.addEventListener_click(fun _ ->
        saveFile ()
    )
    Ref.run.addEventListener_click(fun _ ->
        runCode ()
    )
    stepfBtn.addEventListener_click(fun _ ->
        stepCode ()
    )
    resetBtn.addEventListener_click(fun _ ->
        resetEmulator()
    )

    mapClickAttacher repToId Ref.representation (fun rep ->
        Browser.console.log (sprintf "Representation changed to %A" rep) |> ignore
        setRepresentation rep |> ignore
        updateMemory ()
        updateSymTable ()
    )

    mapClickAttacher viewToIdTab Ref.viewTab (fun view ->
        Browser.console.log (sprintf "View changed to %A" view) |> ignore
        setView view
    )

    (Ref.byteViewBtn).addEventListener_click(fun _ ->
        Browser.console.log "Toggling byte view" |> ignore
        toggleByteView ()
        updateMemory ()
    )

    (Ref.newFileTab).addEventListener_click(fun _ ->
        Browser.console.log "Creating a new file tab" |> ignore
        createFileTab ()
    )

    // Create an empty tab to start with
    createFileTab ()


setMainMenu ()

let handleMonacoReady (_: Event) = init ()

let listener: U2<EventListener, EventListenerObject> = !^handleMonacoReady

document.addEventListener("monaco-ready", listener)
