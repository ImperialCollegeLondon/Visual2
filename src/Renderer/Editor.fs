// Code that is generic for all editors
module Editor

open Fable.Core.JsInterop
open Fable.Core
open Fable.Import.Node.Exports
open Fable.Import
open Ref

// Default settings if they haven't already been defined by electron-settings
let defaultSettings = Map.ofList [
                            "simulator-max-steps" ==> "200000"
                            "editor-font-size" ==> "16"
                            "editor-theme" ==> "vs-dark-pro"
                            "editor-word-wrap" ==> "off"
                            "editor-render-whitespace" ==> "none"
                            "current-file-path" ==> os.homedir()
                        ]

let getSetting (name : string) =
    let setting = settings?get(name)
    match isUndefined setting with
    | true -> defaultSettings.[name]
    | false -> setting
    |> (fun x -> x.ToString())

let setSetting (name : string) (value : obj) =
    settings?set(name, value) |> ignore

let editorOptions () = createObj [

                        // User defined settings
                        "theme" ==> getSetting "editor-theme";
                        "renderWhitespace" ==> getSetting "editor-render-whitespace"
                        "fontSize" ==> getSetting "editor-font-size";
                        "wordWrap" ==> getSetting "editor-word-wrap";

                        // Application defined settings
                        "value" ==> "";
                        "language" ==> "arm";
                        "roundedSelection" ==> false;
                        "scrollBeyondLastLine" ==> false;
                        "automaticLayout" ==> true;
                        "minimap" ==> createObj [ "enabled" ==> false ]
                    ]
    
let showMessage (callBack:int ->unit) (message:string) (detail:string) (buttons:string list) =
    let rem = electron.remote
    let retFn = unbox callBack
    rem.dialog.showMessageBox(
       (let opts = createEmpty<Fable.Import.Electron.ShowMessageBoxOptions>
        opts.title <- Option.None
        opts.message <- message |> Some
        opts.detail <- detail |> Some
        opts.``type`` <- "none" |> Some
        opts.buttons <- buttons |> List.toSeq |> ResizeArray |> Some
        opts), retFn)   
    |> ignore
