module Settings

open Fable.Import.Browser

open Ref
open Tabs
open Editor

let editorFontSize = "editor-font-size"
let simulatorMaxSteps = "simulator-max-steps"
let editorTheme = "editor-theme"
let editorWordWrap = "editor-word-wrap"
let editorRenderWhitespace = "editor-render-whitespace"

let inputSettings = [
                        simulatorMaxSteps
                        editorFontSize
                        editorTheme
                        editorWordWrap
                        editorRenderWhitespace
                    ]


let themes = [
                "vs-light", "Light"; 
                "vs-dark", "Dark"; 
                "one-dark-pro", "One Dark Pro";
              ]

let setSettingsUnsaved = (fun _ -> setTabUnsaved (getSettingsTabId ()))

let getSettingInput (name : string) =
    let input = document.getElementById(name) :?> HTMLInputElement
    input.value


type VSettings = {
    EditorFontSize : uint32
    SimulatorMaxSteps : int64
    EditorTheme: string
    EditorWordWrap: string
    EditorRenderWhitespace: string
    }

let mutable vSettings = {
    EditorFontSize = 16u
    SimulatorMaxSteps = 200000L
    EditorTheme = "one-dark-pro"
    EditorWordWrap = "off"
    EditorRenderWhitespace = "None"
    }

let getInt mini maxi defi settingName = 
    let (|INT|_|) = function
        | Helpers.LITERALNUMB (n,"") -> Some n 
        | _ -> Core.Option.None
    match getSettingInput settingName with
    | INT n when n >= mini && n <= maxi -> n
    | INT n when n < mini -> mini
    | INT n when n > maxi -> maxi
    | _ -> defi

let getVisualSettings() =
    printfn "getting settings"
    {
        EditorFontSize = getInt 8u 100u 12u editorFontSize
        SimulatorMaxSteps = getInt 0u 1000000000u 10000u simulatorMaxSteps |> uint64 |> int64
        EditorTheme = getSettingInput editorTheme
        EditorWordWrap = "off"
        EditorRenderWhitespace = "None"
    } 
    |> Helpers.pipeShow "New settings:"


let setSettingInput (name : string) =
    setSetting name (getSettingInput name)

// Go through the form extracting all of the relevant settings
let saveSettings () =
    List.map setSettingInput inputSettings |> ignore
    vSettings <- getVisualSettings()
    updateAllEditors()

let makeFormGroup label input =
    let fg = document.createElement("div")
    fg.classList.add("form-group")

    let lab = document.createElement("label")
    lab.innerHTML <- label
    lab.classList.add("settings-label")

    let br = document.createElement("br")

    fg.appendChild(lab) |> ignore
    fg.appendChild(br) |> ignore
    fg.appendChild(input) |> ignore

    fg

let makeInputVal inType name =
    let fi = document.createElement_input()
    fi.``type`` <- inType
    fi.id <- name
    fi.value <- (getSetting name).ToString()
    // Whenever a form input is changed, set the settings tab unsaved
    fi.onchange <- setSettingsUnsaved
    fi

let makeInputSelect options name =
    let makeOption (optionValue, optionName) =
        let opt = document.createElement_option()
        opt.innerHTML <- optionName
        opt.value <- optionValue
        opt

    let select = document.createElement_select()
    select.classList.add("form-control")
    select.classList.add("settings-select")
    select.id <- name

    List.map (makeOption >> (fun x -> select.appendChild(x))) options |> ignore

    select.value <- (getSetting name).ToString()

    select.onchange <- setSettingsUnsaved

    select

let makeInputCheckbox name trueVal falseVal =
    let checkbox = document.createElement_input()
    checkbox.``type`` <- "checkbox"
    checkbox.id <- name

    let setValue() = 
        checkbox.value <- match checkbox.``checked`` with
                            | true -> trueVal
                            | false -> falseVal

    // When the checkbox is ticked, update its value
    checkbox.addEventListener_click (fun _ -> setValue())

    checkbox.``checked`` <- match (getSetting name).ToString() with
                            | x when x = trueVal -> true
                            | _ -> false
    
    setValue()

    checkbox.onchange <- setSettingsUnsaved
    checkbox

let editorForm () =
    let form = document.createElement("form")

    let makeAdd label input =
        let group = makeFormGroup label input
        form.appendChild(group) |> ignore

    let fontSizeInput = makeInputVal "number" editorFontSize
    makeAdd "Font Size" fontSizeInput

    let themeSelect = makeInputSelect themes editorTheme
    makeAdd "Theme" themeSelect

    let wordWrapCheck = makeInputCheckbox editorWordWrap "on" "off"
    makeAdd "Word Wrap" wordWrapCheck

    let renderWhitespace = makeInputCheckbox editorRenderWhitespace "all" "none"
    makeAdd "Render Whitespace Characters" renderWhitespace

    form

let simulatorForm () =
    let form = document.createElement("form")

    let makeAdd label input =
        let group = makeFormGroup label input
        form.appendChild(group) |> ignore

    let maxStepInput = makeInputVal "number" simulatorMaxSteps
    makeAdd "Max steps (0 for no max) " maxStepInput

    form

// HTML description for the settings menu
let settingsMenu () =
    let menu = document.createElement("div")
    menu.classList.add("settings-menu")
    menu.classList.add("editor")
    let simulatorHeading = document.createElement("h3")
    simulatorHeading.innerHTML <- "Simulator"
    let editorHeading = document.createElement("h3")
    editorHeading.innerHTML <- "Editor"
 
    menu.appendChild(simulatorHeading) |> ignore
    menu.appendChild(simulatorForm()) |> ignore
    menu.appendChild(editorHeading) |> ignore
    menu.appendChild(editorForm()) |> ignore

    let saveButton = document.createElement("button")
    saveButton.classList.add("btn")
    saveButton.classList.add("btn-default")

    saveButton.innerHTML <- "Save"
    saveButton.addEventListener_click(fun _ -> 
        saveSettings()
        setTabSaved (getSettingsTabId ())
        Tabs.deleteCurrentTab()
        )

    menu.appendChild(saveButton) |> ignore

    menu

let createSettingsTab () =   
    // If the settings tab already exists, just switch to it, else create it
    match settingsTab with
    | Some tab -> selectFileTab tab
    | Microsoft.FSharp.Core.option.None ->
        let id = createTab " Settings"
        settingsTab <- Some id

        let tabName = fileTabName id
        tabName.classList.add("icon")
        tabName.classList.add("icon-cog")

        let sv = settingsMenu ()

        sv.classList.add("invisible")
        sv.id <- fileViewIdFormatter id

        fileViewPane.appendChild(sv) |> ignore
        selectFileTab id