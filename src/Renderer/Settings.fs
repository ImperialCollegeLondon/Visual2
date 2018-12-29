(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Settings
    Description: Code to handle persistent settings stored on PC as a file under user data
*)

/// implement electron-style persistent settings via electron-settings module

module Settings
open Fable.Import.Browser
open Fable.Core.JsInterop

open Refs
open Tabs
open Editors


let editorFontSize = "editor-font-size"
let simulatorMaxSteps = "simulator-max-steps"
let editorTheme = "editor-theme"
let editorWordWrap = "editor-word-wrap"
let editorRenderWhitespace = "editor-render-whitespace"




let setSettingsUnsaved = (fun _ -> setTabUnsaved (getSettingsTabId()))


let getIntSetting mini maxi (defi : string) setting =
    let (|INT|_|) (n : string) =
        try
            Some(int64 n)
        with
            | e -> Some(defi |> int64)
    match setting with
    | INT n when n >= mini && n <= maxi -> n |> string
    | INT n when n < mini -> mini |> string
    | INT n when n > maxi -> maxi |> string
    | _ -> defi

let getFormSettings() =
    let getS (name : string) =
        let input = document.getElementById (name) :?> HTMLInputElement
        input.value
    let vs = {
            SimulatorMaxSteps = getIntSetting 0L 1000000000L vSettings.SimulatorMaxSteps (getS simulatorMaxSteps)
            EditorFontSize = getIntSetting 6L 100L vSettings.EditorFontSize (getS editorFontSize)
            EditorTheme = getS editorTheme
            EditorWordWrap = getS editorWordWrap
            EditorRenderWhitespace = getS editorRenderWhitespace
            CurrentFilePath = vSettings.CurrentFilePath
            RegisteredKey = vSettings.RegisteredKey
            OnlineFetchText = vSettings.OnlineFetchText
        }
    let vs1 = checkSettings vs
    printfn "Checked settings are: %A" vs1
    vSettings <- vs1
    printfn "Saving settings: %A" vSettings
    setJSONSettings()

let initFormSettings() =
    let setS (name : string) (v : string) =
        let input = document.getElementById (name) :?> HTMLInputElement
        printfn "name=%A,dom=%A, value=%A" name input v
        input.value <- v
    let vs = vSettings
    setS simulatorMaxSteps <| (uint64 vs.SimulatorMaxSteps).ToString()
    setS editorFontSize <| (uint64 vs.EditorFontSize).ToString()
    setS editorTheme vs.EditorTheme
    setS editorWordWrap vs.EditorWordWrap
    setS editorRenderWhitespace vs.EditorRenderWhitespace







let makeInputVal inType name (min : int, steps : int, max : int) defi =
    let fi = document.createElement_input()
    fi.``type`` <- inType
    fi.id <- name
    fi.classList.add [| "settings-input" |]
    fi.min <- min.ToString()
    fi.max <- max.ToString()
    fi.step <- steps.ToString()
    fi.value <- defi.ToString()
    // Whenever a form input is changed, set the settings tab unsaved
    fi.onchange <- fun e -> setSettingsUnsaved e :> obj
    //fi.onerror <- (fun _ -> printfn "Error")
    //fi.onreset <- (fun _ -> printfn "Reset")
    fi

let makeInputSelect options name defV =
    let makeOption (optionValue, optionName) =
        let opt = document.createElement_option()
        opt.innerHTML <- optionName
        opt.value <- optionValue
        opt

    let select = document.createElement_select()
    select.classList.add ([| "form-control"; "settings-select" |])
    select.id <- name

    List.map (makeOption >> (fun x -> select.appendChild (x))) options |> ignore

    select.value <- defV

    select.onchange <- fun e -> setSettingsUnsaved e :> obj

    select

let makeInputCheckbox name trueVal falseVal defVal =
    let checkbox = document.createElement_input()
    checkbox.``type`` <- "checkbox"
    checkbox.id <- name

    let setValue() =
        checkbox.value <- match checkbox.``checked`` with
                            | true -> trueVal
                            | false -> falseVal

    // When the checkbox is ticked, update its value
    checkbox.addEventListener_click (fun _ -> setValue() :> obj)

    checkbox.``checked`` <- match defVal.ToString() with
                            | x when x = trueVal -> true
                            | _ -> false

    setValue()

    checkbox.onchange <- (fun e -> setSettingsUnsaved e :> obj)
    checkbox






let makeFormGroup label input =
    DIV [ "form-group" ] [
        ELEMENT "lab" [ "settings-label" ] [] |> INNERHTML label
        BR()
        input
    ]

// HTML description for the settings menu
let settingsMenu() =
    FORM [ "settings-menu"; "editor" ] [

        DIV [ "float-left" ] [
            ELEMENT "h4" [] [] |> INNERHTML "Editor"
            makeFormGroup "Font Size" <|
                makeInputVal "number" editorFontSize (int minFontSize, 2, int maxFontSize) vSettings.EditorFontSize
            makeFormGroup "Theme" (makeInputSelect themes editorTheme vSettings.EditorTheme)
            makeFormGroup "Word Wrap" (makeInputCheckbox editorWordWrap "on" "off" "on")
            makeFormGroup "Render Whitespace Characters"
                (makeInputCheckbox editorRenderWhitespace "all" "none" "none")
        ]
        DIV [] [
            ELEMENT "h4" [] [] |> INNERHTML "Simulator"
            makeFormGroup "Max Steps <br> (0 for no max) "
                (makeInputVal "number" simulatorMaxSteps (0, 100, 10000000) vSettings.SimulatorMaxSteps)
        ]

        DIV [ "after" ] []
        DIV [] [
            ELEMENT "button" [ "btn"; "btn-default" ] []
            |> INNERHTML "Save and Close Settings"
            |> CLICKLISTENER(fun _ ->
                        getFormSettings()
                        setTabSaved (getSettingsTabId())
                        Tabs.deleteCurrentTab()
                        Editors.updateAllEditors false)
        ]
    ]

let createSettingsTab() =
    // If the settings tab already exists, just switch to it, else create it
    match settingsTab with
    | Some tab -> selectFileTab tab
    | Microsoft.FSharp.Core.option.None ->
        let id = createTab " &nbsp  Settings"
        settingsTab <- Some id

        let tabName = fileTabName id
        tabName.classList.add ("icon")
        tabName.classList.add ("icon-cog")

        let sv = settingsMenu()

        sv.classList.add ("invisible")
        sv.id <- fileViewIdFormatter id

        fileViewPane.appendChild (sv) |> ignore
        initFormSettings()
        selectFileTab id

let alterFontSize (n : int) =
    let fontSize = int64 Refs.vSettings.EditorFontSize
    let newSize = fontSize + int64 n
    Refs.vSettings <- checkSettings
        { Refs.vSettings with
            EditorFontSize = (int64 newSize).ToString()
        }
    Refs.setJSONSettings()
    updateAllEditors false
