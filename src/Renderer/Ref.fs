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


/// Bases to display data in for all Views
/// Udec = unsigned ecimal
type Representations =
    | Hex
    | Bin
    | Dec
    | UDec

/// used to get ID of button for each representation
let repToId = 
    Map.ofList [
        Hex, "rep-hex";
        Bin, "rep-bin";
        Dec, "rep-dec";
        UDec, "rep-udec";
    ]
/// Select View in RH window
type Views =
    | Registers
    | Memory
    | Symbols

/// used to get ID used in DOM for each View
let viewToIdView = 
    Map.ofList [
        Registers, "view-reg";
        Memory, "view-mem";
        Symbols, "view-sym";
    ]
/// used to get Tab ID in DOM for each View
let viewToIdTab = 
    Map.ofList [
        Registers, "tab-reg";
        Memory, "tab-mem";
        Symbols, "tab-sym"
    ]

/// A reference to the settings for the app
/// persistent using electron-settings
let settings : obj = importDefault "electron-settings"

/// look up a DOM element
let getHtml = Browser.document.getElementById

/// set a custom CSS variable defined in :root pseudoclass
let setCustomCSS (varName:string) (content:string) =
    let element = Browser.document.documentElement
    element.style.setProperty(varName, content)

/// set the CSS variable that determines dashboard width
let setDashboardWidth (widthInPixels:float)=
    setCustomCSS "--dashboard-width" (sprintf "%.1fpx" widthInPixels)

let fontSize = getHtml "font-size" :?> HTMLSelectElement
let register id = getHtml <| sprintf "R%i" id

//----------------BUTTONS on ToolBar--------------------
let explore = getHtml "explore" :?> HTMLButtonElement
let save = getHtml "save" :?> HTMLButtonElement
let run: HTMLButtonElement = getHtml "run" :?> HTMLButtonElement
let resetBtn = getHtml "reset" :?> HTMLButtonElement
let stepfBtn = getHtml "stepf" :?> HTMLButtonElement
let stepbBtn = getHtml "stepb" :?> HTMLButtonElement

/// Get Flag display element from ID ("C", "V", "N", "Z")
let flag id = getHtml <| sprintf "flag_%s" id

/// get button for specific representation
let representation rep = getHtml repToId.[rep]

/// get View pane element from View
let viewView view = getHtml viewToIdView.[view]

/// get View Tab element from view
let viewTab view = getHtml viewToIdTab.[view]

/// get byte/word switch button element
let byteViewBtn = getHtml "byte-view"

/// get memory list element
let memList = getHtml "mem-list"

/// get symbol table View element
let symView = getHtml "sym-view"

/// get symbol table element
let symTable = getHtml "sym-table"

/// get element containing all tab headers
let fileTabMenu = getHtml "tabs-files"

/// get last (invisible) tab header
let newFileTab = getHtml "new-file-tab"

/// get ID of Tab for Tab number tabID
let fileTabIdFormatter tabID = sprintf "file-tab-%d" tabID

/// get element corresponding to file tab tabID
let fileTab tabID = getHtml <| fileTabIdFormatter tabID

/// get ID of editor window containing file
let fileViewIdFormatter = sprintf "file-view-%d"

/// get element of editor window containing file
let fileView id = getHtml <| fileViewIdFormatter id

/// get pane element containing for tab menu and editors
let fileViewPane = getHtml "file-view-pane"

/// get id of element containing tab name as dispalyed
let tabNameIdFormatter = sprintf "file-view-name-%d"

/// get element containing tab name as displayed
let fileTabName id = getHtml <| tabNameIdFormatter id

/// get id of element containing file path
let tabFilePathIdFormatter = sprintf "file-view-path-%d"

// get (invisible)  element containing file path
let tabFilePath id = getHtml <| tabFilePathIdFormatter id


let darkenOverlay = getHtml "darken-overlay"

/// get element for status-bar button (and indicator)
let statusBar = getHtml "status-bar"

let setFilePaneBackground color =
    fileViewPane.setAttribute("style", sprintf "background: %s" color)


