//*******************************************************************************
//                        Interaction with Editors   
//*******************************************************************************

module Editors

open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Core
open EEExtensions
open CommonData
open ExecutionTop
open Refs
open Tabs

let updateEditor tId =
    editors.[tId]?updateOptions(editorOptions()) |> ignore

let setTheme theme = 
    window?monaco?editor?setTheme(theme)


let updateAllEditors () =
    editors
    |> Map.iter (fun tId _ -> updateEditor tId)
    let theme = getSetting("editor-theme")
    Refs.setFilePaneBackground (match theme with | "vs-light" -> "white" | _ -> "black")
    setTheme (theme) |> ignore
   

// Disable the editor and tab selection during execution
let disableEditors () = 
    Refs.fileTabMenu.classList.add("disabled-click")
    (Refs.fileView currentFileTabId).classList.add("disabled-click")
    Refs.fileViewPane.onclick <- (fun _ ->
        Browser.window.alert("Cannot use editor pane during execution")
    )
    Refs.darkenOverlay.classList.remove("invisible")

// Enable the editor once execution has completed
let enableEditors () =
    Refs.fileTabMenu.classList.remove("disabled-click")
    (Refs.fileView currentFileTabId).classList.remove("disabled-click")
    Refs.fileViewPane.onclick <- ignore
    Refs.darkenOverlay.classList.add("invisible")

let mutable decorations : obj list = []
let mutable lineDecorations : obj list = []

[<Emit "new monaco.Range($0,$1,$2,$3)">]
let monacoRange _ _ _ _ = jsNative

[<Emit "$0.deltaDecorations($1, [
    { range: $2, options: $3},
  ]);">]
let lineDecoration _editor _decorations _range _name = jsNative

[<Emit "$0.deltaDecorations($1, [{ range: new monaco.Range(1,1,1,1), options : { } }]);">]
let removeDecorations _editor _decorations = 
    jsNative

// Remove all text decorations associated with an editor
let removeEditorDecorations tId =
    List.iter (fun x -> removeDecorations editors.[tId] x) decorations
    decorations <- []

let editorLineDecorate editor number decoration =
    let model = editor?getModel()
    let lineWidth = model?getLineMaxColumn(number)
    let newDecs = lineDecoration editor
                    decorations
                    (monacoRange number 1 number lineWidth)
                    decoration
    decorations <- List.append decorations [newDecs]

// highlight a particular line
let highlightLine tId number className = 
    editorLineDecorate 
        editors.[tId]
        number
        (createObj[
            "isWholeLine" ==> true
            "inlineClassName" ==> className
        ])

/// Decorate a line with an error indication and set up a hover message
/// Distinct message lines must be elements of markdownLst
/// markdownLst: string list - list of markdown paragraphs
/// tId: int - tab identifier
/// lineNumber: int - line to decorate, starting at 1
let makeErrorInEditor tId lineNumber (markdownLst:string list) = 
    let makeMarkDown textLst =
        textLst
        |> List.toArray
        |> Array.map (fun txt ->  createObj [ "isTrusted" ==> true; "value" ==> txt ])

    editorLineDecorate 
        editors.[tId]
        lineNumber 
        (createObj[
            "isWholeLine" ==> true
            "isTrusted" ==> true
            "inlineClassName" ==> "editor-line-error"
            "hoverMessage" ==> makeMarkDown markdownLst
        ])