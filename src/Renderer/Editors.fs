(* 
    Visual2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compliler )
    Module: Renderer.Editors
    Description: Interface with Monaco editor buffers
*)

/// Interface with monaco editor buffers
module Editors

open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Core
open EEExtensions

let editorOptions (readOnly:bool) = 
    let vs = Refs.vSettings
    createObj [

                        // User defined settings
                        "theme" ==> vs.EditorTheme
                        "renderWhitespace" ==> vs.EditorRenderWhitespace
                        "fontSize" ==> vs.EditorFontSize
                        "wordWrap" ==> vs.EditorWordWrap

                        // Application defined settings
                        "value" ==> "";
                        "fontFamily" ==> "fira-code"
                        "fontWeight" ==> "bold"
                        "language" ==> "arm";
                        "roundedSelection" ==> false;
                        "scrollBeyondLastLine" ==> false;
                        "readOnly" ==> readOnly;
                        "automaticLayout" ==> true;
                        "minimap" ==> createObj [ "enabled" ==> false ]
              ]


let updateEditor tId readOnly =
    let eo = editorOptions readOnly
    printfn "Options: %A" eo
    Refs.editors.[tId]?updateOptions(eo) |> ignore

let setTheme theme = 
    window?monaco?editor?setTheme(theme)


let updateAllEditors readOnly =
    Refs.editors
    |> Map.iter (fun tId _ -> if tId = Refs.currentFileTabId then  readOnly else false
                              |> updateEditor tId)
    let theme = Refs.vSettings.EditorTheme
    Refs.setFilePaneBackground (
        match theme with 
        | "one-light-pro" | "solarised-light" -> "white" 
        | _ -> "black")
    setTheme (theme) |> ignore
   

// Disable the editor and tab selection during execution
let disableEditors () = 
    Refs.fileTabMenu.classList.add("disabled-click")
    Refs.fileTabMenu.onclick <- (fun _ ->
        Browser.window.alert("Cannot change tabs during execution")
    )
    updateEditor Refs.currentFileTabId true
    Refs.darkenOverlay.classList.remove("invisible")
    Refs.darkenOverlay.classList.add([|"disabled-click"|])

// Enable the editor once execution has completed
let enableEditors () =
    Refs.fileTabMenu.classList.remove("disabled-click")
    Refs.fileTabMenu.onclick <- ignore
    updateEditor Refs.currentFileTabId false
    Refs.darkenOverlay.classList.add([|"invisible"|])

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
    List.iter (fun x -> removeDecorations Refs.editors.[tId] x) decorations
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
        Refs.editors.[tId]
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
        Refs.editors.[tId]
        lineNumber 
        (createObj[
            "isWholeLine" ==> true
            "isTrusted" ==> true
            "inlineClassName" ==> "editor-line-error"
            "hoverMessage" ==> makeMarkDown markdownLst
        ])

let revealLineInWindow tId (lineNumber: int) =
    Refs.editors.[tId]?revealLineInCenterIfOutsideViewport(lineNumber) |> ignore