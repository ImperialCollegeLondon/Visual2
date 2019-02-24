module GuiExamples
open Refs // for HTML (DOM) interface

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser


(* HTML/CSS reference: https://www.w3schools.com/css/css_dropdowns.asp
>>>>>>>>>>>>>>>>>>>>>CSS>>>>>>>>>>>>>>>>>>>>>>>>
<style>
.dropdown-example{
  position: relative;
  display: inline-block;
}

.dropdown-content-example {
  display: none;
  position: absolute;
  background-color: #f9f9f9;
  min-width: 160px;
  box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2);
  padding: 12px 16px;
  z-index: 1;
}

.dropdown-example:hover .dropdown-content-example {
  display: block;
}
</style>
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<CSS<<<<<<<<<<<<<<<<<<<<<<<<

--------------------------HTML--------------------------
<div class="dropdown-example">
  <span>Mouse over me</span>
  <div class="dropdown-content-example">
    <p>Hello World!</p>
  </div>
</div>
--------------------------HTML---------------------------
*)

//-------------------DEFINITIONS FROM VIEWS.FS-----------

/// make an HTML element
/// id = element name
/// css = css class names to add to classlist
let makeEl (id : string) (css : string) =
        let el = Fable.Import.Browser.document.createElement id
        el.classList.add css
        el
/// appends child node after last child in parent node, returns parent
/// operator is left associative
/// child: child node
/// node: parent node.
let (&>>) (node : Fable.Import.Browser.Node) child =
    node.appendChild child |> ignore
    node

let createDOM (parentID : string) (childList : Fable.Import.Browser.Node list) =
    let parent = Fable.Import.Browser.document.createElement parentID
    List.iter (fun ch -> parent &>> ch |> ignore) childList
    parent

let addToDOM (parent : Fable.Import.Browser.Node) (childList : Fable.Import.Browser.Node list) =
    List.iter (fun ch -> parent &>> ch |> ignore) childList
    parent


//----------------Definitions from Refs.fs---------------------

let ELEMENT elName classes (htmlElements : HTMLElement list) =
    let ele = document.createElement elName
    ele.classList.add (classes |> List.toArray)
    List.iter (ele.appendChild >> ignore) htmlElements
    ele

let INNERHTML html (ele : HTMLElement) = (ele.innerHTML <- html); ele
let STYLE (name, value) (ele : HTMLElement) = ele.style.setProperty (name, value); ele

let ID name (ele : HTMLElement) = (ele.id <- name); ele
let CLICKLISTENER (listener : unit -> unit) (ele : HTMLElement) = 
    (ele.addEventListener_click (fun _ -> listener() |> ignore; createObj [])); ele

let DIV = ELEMENT "div"

let BR() = Fable.Import.Browser.document.createElement "br"

let FORM classes contents =
    let form = ELEMENT "form" classes contents
        // disable form submission
    form.onsubmit <- (fun _ -> false :> obj)
    form

let TABLE = ELEMENT "table"

let toDOM text = ELEMENT "span" [] [] |> INNERHTML text

let TROW = ELEMENT "tr" []

let TD x = ELEMENT "td" [] <| [ x ]

/// look up a DOM element from its ID
let getHtml = Browser.document.getElementById


// Implement DOM elements in example dropdown
(*
<div class="dropdown-example">
  <span>Mouse over me</span>
  <div class="dropdown-content-example">
    <p>Hello World!</p>
  </div>
</div>
*)

/// make a new convenience function to create a <p> text node
let toDOMP text = ELEMENT "p" [] [] |> INNERHTML text

///create a dropbox (must be enabled on hover using CSS)
let dropBox =
    DIV ["dropdown-example"] [
        toDOM "Mouse over me"
        DIV ["dropdown-content-example"] [toDOMP "Hello World"]
    ]

let R15box = Refs.getHtml "R15"

