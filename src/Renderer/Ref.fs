(* 
    High Level Programming @ Imperial College London # Spring 2018
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compliler )
    Contributors: Angelos Filos
    Module: Ref
    Description: References to `HTML` elements from `index.html`.
*)

module Ref

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Microsoft.FSharp.Collections

[<Emit("$0 === undefined")>]
let isUndefined (_: 'a) : bool = jsNative



type Representations =
    | Hex
    | Bin
    | Dec
    | UDec

let repToId = 
    Map.ofList [
        Hex, "rep-hex";
        Bin, "rep-bin";
        Dec, "rep-dec";
        UDec, "rep-udec";
    ]

type Views =
    | Registers
    | Memory
    | Symbols

let viewToIdView = 
    Map.ofList [
        Registers, "view-reg";
        Memory, "view-mem";
        Symbols, "view-sym";
    ]

let viewToIdTab = 
    Map.ofList [
        Registers, "tab-reg";
        Memory, "tab-mem";
        Symbols, "tab-sym"
    ]

// A reference to the settings for the app
let settings : obj = importDefault "electron-settings"

let getHtml = Browser.document.getElementById

let fontSize = getHtml "font-size" :?> HTMLSelectElement
let register id = getHtml <| sprintf "R%i" id
let explore = getHtml "explore" :?> HTMLButtonElement
let save = getHtml "save" :?> HTMLButtonElement
let run: HTMLButtonElement = getHtml "run" :?> HTMLButtonElement
let resetBtn = getHtml "reset" :?> HTMLButtonElement
let stepfBtn = getHtml "stepf" :?> HTMLButtonElement
let stepbBtn = getHtml "stepb" :?> HTMLButtonElement
let flag id = getHtml <| sprintf "flag_%s" id
let representation rep = getHtml repToId.[rep]
let viewView view = getHtml viewToIdView.[view]
let viewTab view = getHtml viewToIdTab.[view]
let byteViewBtn = getHtml "byte-view"
let memList = getHtml "mem-list"
let symView = getHtml "sym-view"
let symTable = getHtml "sym-table"
let fileTabMenu = getHtml "tabs-files"
let newFileTab = getHtml "new-file-tab"
let fileTabIdFormatter = sprintf "file-tab-%d"
let fileTab id = getHtml <| fileTabIdFormatter id
let fileViewIdFormatter = sprintf "file-view-%d"
let fileView id = getHtml <| fileViewIdFormatter id
let fileViewPane = getHtml "file-view-pane"
let tabNameIdFormatter = sprintf "file-view-name-%d"
let fileTabName id = getHtml <| tabNameIdFormatter id
let tabFilePathIdFormatter = sprintf "file-view-path-%d"
let tabFilePath id = getHtml <| tabFilePathIdFormatter id
let darkenOverlay = getHtml "darken-overlay"
let statusBar = getHtml "status-bar"
