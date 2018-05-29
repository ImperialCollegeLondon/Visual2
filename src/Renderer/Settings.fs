module Settings

open Fable.Import.Browser

open Refs
open Tabs


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

let getIntSetting mini maxi defi settingName = 
    let (|INT|_|) = function
        | Helpers.LITERALNUMB (n,"") -> Some n 
        | _ -> Core.Option.None
    match getSetting settingName with
    | INT n when n >= mini && n <= maxi -> n
    | INT n when n < mini -> mini
    | INT n when n > maxi -> maxi
    | _ -> defi

let getVisualSettings() =
    printfn "getting settings"
    {
        EditorFontSize = getIntSetting 8u 100u 12u editorFontSize
        SimulatorMaxSteps = getIntSetting 0u 1000000000u 10000u simulatorMaxSteps |> uint64 |> int64
        EditorTheme = getSetting editorTheme
        EditorWordWrap = "off"
        EditorRenderWhitespace = "None"
    } 



let setSettingInput (name : string) =
    setSetting name (getSettingInput name)

// Go through the form extracting all of the relevant settings
let saveSettings () =
    List.map setSettingInput inputSettings |> ignore
    vSettings <- getVisualSettings()
    updateAllEditors()



let makeInputVal inType name (min:int,steps:int,max:int)=
    let fi = document.createElement_input()
    fi.``type`` <- inType
    fi.id <- name
    fi.min <- min.ToString()
    fi.max <- max.ToString()
    fi.step <- steps.ToString()
    fi.value <- (getSetting name).ToString()
    // Whenever a form input is changed, set the settings tab unsaved
    fi.onchange <- setSettingsUnsaved 
    //fi.onerror <- (fun _ -> printfn "Error")
    //fi.onreset <- (fun _ -> printfn "Reset")
    fi

let makeInputSelect options name =
    let makeOption (optionValue, optionName) =
        let opt = document.createElement_option()
        opt.innerHTML <- optionName
        opt.value <- optionValue
        opt

    let select = document.createElement_select()
    select.classList.add ([| "form-control";"settings-select"|] )
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



let ELEMENT elName classes (htmlElements: HTMLElement list) =
    let ele = document.createElement elName
    ele.classList.add (classes |> List.toArray)
    List.iter (ele.appendChild >> ignore) htmlElements
    ele

let INNERHTML html (ele:HTMLElement) = (ele.innerHTML <- html) ; ele
let ID name (ele:HTMLElement) = (ele.id <- name) ; ele
let CLICKLISTENER listener (ele:HTMLElement) = (ele.addEventListener_click listener) ; ele

let DIV = ELEMENT "div"

let BR() = document.createElement "br"

let FORM classes contents = 
    let form = ELEMENT "form" classes contents
        // disable form submission
    form.onsubmit <- ( fun _ -> false)
    form


let makeFormGroup label input =
    DIV ["form-group"] [
        ELEMENT "lab" ["settings-label"] [] |> INNERHTML label
        BR()
        input
    ]

// HTML description for the settings menu
let settingsMenu () =
    FORM ["settings-menu";"editor"] [  
    
        DIV ["float-left"] [
            ELEMENT "h4" [] [] |> INNERHTML "Editor" 
            makeFormGroup "Font Size" (makeInputVal "number" editorFontSize (6,2,50))
            makeFormGroup "Theme" (makeInputSelect themes editorTheme)
            makeFormGroup "Word Wrap" (makeInputCheckbox editorWordWrap "on" "off")
            makeFormGroup "Render Whitespace Characters" 
                (makeInputCheckbox editorRenderWhitespace "all" "none")
        ]
        DIV [] [
            ELEMENT "h4" [] [] |> INNERHTML "Simulator"
            makeFormGroup "Max steps <br> (0 for no max) " 
                (makeInputVal "number" simulatorMaxSteps (0, 100,10000000))
        ]
        
        DIV ["after"] []
        DIV [] [
            ELEMENT "button" ["btn";"btn-default"] []
            |> INNERHTML "Save"
            |> CLICKLISTENER (fun _ ->                                
                        saveSettings()
                        setTabSaved ( getSettingsTabId () )
                        Tabs.deleteCurrentTab() )
        ]
    ]    

let createSettingsTab () =   
    // If the settings tab already exists, just switch to it, else create it
    match settingsTab with
    | Some tab -> selectFileTab tab
    | Microsoft.FSharp.Core.option.None ->
        let id = createTab " &nbsp  Settings"
        settingsTab <- Some id

        let tabName = fileTabName id
        tabName.classList.add("icon")
        tabName.classList.add("icon-cog")

        let sv = settingsMenu ()

        sv.classList.add("invisible")
        sv.id <- fileViewIdFormatter id

        fileViewPane.appendChild(sv) |> ignore
        selectFileTab id