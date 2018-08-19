(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Refs
    Description: F# references to elements in the DOM + some user settings handling
*)

/// F# References to static parts of renderer DOM
module Tooltips
open System
open Fable.Import
open Fable.Import.Browser
open Fable.Core
open Fable.Core.JsInterop

open Microsoft.FSharp.Collections
open Node.Exports
open EEExtensions
open Refs



// ***********************************************************************************************
//                                  SVG Graphics using React
// ***********************************************************************************************

(*
// SVG elements
let inline svg b c = svgEl "svg" b c
let inline circle b c = svgEl "circle" b c
let inline clipPath b c = svgEl "clipPath" b c
let inline defs b c = svgEl "defs" b c
let inline ellipse b c = svgEl "ellipse" b c
let inline g b c = svgEl "g" b c
let inline image b c = svgEl "image" b c
let inline line b c = svgEl "line" b c
let inline linearGradient b c = svgEl "linearGradient" b c
let inline mask b c = svgEl "mask" b c
let inline path b c = svgEl "path" b c
let inline pattern b c = svgEl "pattern" b c
let inline polygon b c = svgEl "polygon" b c
let inline polyline b c = svgEl "polyline" b c
let inline radialGradient b c = svgEl "radialGradient" b c
let inline rect b c = svgEl "rect" b c
let inline stop b c = svgEl "stop" b c
let inline text b c = svgEl "text" b c
let inline tspan b c = svgEl "tspan" b c


    type SVGAttr =
        | ClipPath of string
        | Cx of obj
        | Cy of obj
        | D of string
        | Dx of obj
        | Dy of obj
        | Fill of string
        | FillOpacity of obj
        | FontFamily of string
        | FontSize of obj
        | Fx of obj
        | Fy of obj
        | GradientTransform of string
        | GradientUnits of string
        | Height of obj
        | MarkerEnd of string
        | MarkerMid of string
        | MarkerStart of string
        | Offset of obj
        | Opacity of obj
        | PatternContentUnits of string
        | PatternUnits of string
        | Points of string
        | PreserveAspectRatio of string
        | R of obj
        | Rx of obj
        | Ry of obj
        | SpreadMethod of string
        | StopColor of string
        | StopOpacity of obj
        | Stroke of string
        | StrokeDasharray of string
        | StrokeLinecap of string
        | StrokeMiterlimit of string
        | StrokeOpacity of obj
        | StrokeWidth of obj
        | TextAnchor of string
        | Transform of string
        | Version of string
        | ViewBox of string
        | Width of obj
        | X1 of obj
        | X2 of obj
        | X of obj
        | XlinkActuate of string
        | XlinkArcrole of string
        | XlinkHref of string
        | XlinkRole of string
        | XlinkShow of string
        | XlinkTitle of string
        | XlinkType of string
        | XmlBase of string
        | XmlLang of string
        | XmlSpace of string
        | Y1 of obj
        | Y2 of obj
        | Y of obj

*)

//---------------------------------------------------------------------------------------------------------------------------
//
// Very Basic DSL for Writing React SVG in F#
//
// React SVG attribute constructors usually need qualified names (SVGAttr.X) to disambiguate from HTML attribute constructors
// The SVG diagram is converted from React to HTML at top-level
//
// Attributes not available as constructors can be made manually:
// SVGAttr.Attr1Name "attr-val" => !!("attr-name", "attr-value")
//
// SVG elements not available as inline constructors can be made using svgEl, e.g. "g" element:
// svgEl "g" [attribute list] [react element list]
//
//---------------------------------------------------------------------------------------------------------------------------

open Fable.Helpers.React
open Fable.Helpers.React.Props


let arrowMarker mId color =
    defs [] [
                svgEl "marker" [
                    !!("id", mId); 
                    !!("markerWidth","5");
                    !!("markerHeight","5");
                    !!("refX","0");
                    !!("refY","1.5");
                    !!("orient","auto");
                    !!("markerUnits","strokeWidth")
                    SVGAttr.Stroke color
                ] [ path [ D "M0,0 L0,3 L4.5,1.5 z"; SVGAttr.Fill color] [] ]
            ]

/// Include all markers used here for SVG diagrams
/// This function must be inserted in SVG just once before other descriptions.
let svgMarkerDefs() =
    arrowMarker "arrowHead" "black"

let arrow (x1,y1) (x2,y2) =
    let head = 1.
    let al = sqrt((x1-x2)**2. + (y1-y2)**2.)
    let headX = head * (x2-x1) / al
    let headY = head * (y2-y1) / al
    let fS f = sprintf "%.2f" f
    line [X1  (fS x1) ; Y1  (fS y1); X2 (fS (x2-headX)); Y2  (fS (y2-headY)); SVGAttr.StrokeWidth (fS (head/5.));  SVGAttr.Stroke "black"; SVGAttr.MarkerEnd "url(#arrowHead)"] []

let arrowCurve pathCmds =
    path [D pathCmds; SVGAttr.Stroke "black"; SVGAttr.Fill "transparent"; SVGAttr.MarkerEnd "url(#arrowHead)" ] []

let textInBox (width,height) (boxClass:string) (txtClass:string) (rhTopX,rhTopY) txt =
    let fS f = sprintf "%.2f" f
    svgEl "g" [] [
        rect [ 
            X (fS rhTopX)
            Y (fS rhTopY)
            SVGAttr.Height (fS height)
            SVGAttr.Width (fS width)
            !!("dominant-baseline","middle") //align vertically on centre
            SVGAttr.TextAnchor "middle" // align horizontally on centre
            !!("class", boxClass)
        ] []
        text [ 
            X (rhTopX+width/2.0 |> fS); 
            Y (rhTopY+height/2.0 |> fS) ; 
            !!("dominant-baseline","middle"); 
            SVGAttr.TextAnchor "middle"
            !!("class", txtClass)
        ] [ ofString txt ]
    ]

let register boxClass txtClass (boxW,boxH) (posX,posY) (bits:int list) =
    let box xp yp b =
        let txt = sprintf "%d" b
        textInBox (boxW,boxH) boxClass txtClass (xp,yp) txt
    let boxes =
        bits
        |> List.indexed
        |> List.rev
        |> List.map (fun (n,b) -> 
            let xp = (float n)*boxW + posX
            box xp posY b)
    svgEl "g" [] boxes

let arrowSet (x,y) (dx,dy) pitch num =
    let arrow' n = arrow (x+(float n)*pitch,y) (x+dx+(float n)*pitch,y+dy)
    svgEl "g" [] (List.map arrow' [0..num-1])

let makeHtmlFromSVG re =
    let ele = ELEMENT "div" [] []
    ReactDom.render(re, ele)
    ele

/// generate an SVG diagram for shifts as HTML DOM
let displayShiftDiagram rn beforeNum (afterNum, afterUf) (shiftT: DP.ArmShiftType option) shiftNum =
    let boxW,boxH = 2.7, 2.7
    let posX,posY = 7., 10.
    let sepY = 35.
    let boxClass = "tooltip-shift-reg-box"
    let txtClass = "tooltip-shift-reg-txt"
    let arrowXOff = if shiftNum < 0 then - (float shiftNum) * boxW else 0.
    let arrowXOff2 = if shiftNum > 0 then (32. - float shiftNum) * boxW else 0.
    let getBits num = 
        [0..31] 
        |> List.map (fun n -> match (num &&& (1 <<< n)) with | 0 -> 0 | _ -> 1)
    let reg = register boxClass txtClass (boxW,boxH)
    svg
        [ ViewBox "0 0 100 50"; unbox ("width", "600px") ]
        [
            svgMarkerDefs() // used to define arrow heads
            reg (posX,posY) (getBits (beforeNum |> int))
            reg (posX,posY+sepY) (getBits afterNum)
            arrowSet (posX + boxW/2. + arrowXOff,posY+boxH) (boxW*(float shiftNum),(sepY-boxW)) boxW (32 - abs shiftNum)
            arrowSet (posX + arrowXOff2 + boxW/2. , posY+boxH) ((if shiftNum > 0 then -arrowXOff2 else (32. + float shiftNum)*boxW) , (sepY-boxW)) boxW (abs shiftNum)
        ]
    |> makeHtmlFromSVG




let demoSVG ()  =
    svg 
      [ ViewBox "0 0 100 100"; unbox ("width", "700x") ]
      [ circle 
          [ Cx (!! "50"); Cy (!! "50"); R (!! "45"); !! ("fill", "#0B79CE") ] 
          []
        text [X "25"; Y "25" ] [ofString "a"]
        text [X "50"; Y "50"] [ofString "b"]
        svgMarkerDefs()
        line [X1  "50"; Y1  "50"; X2 "25"; Y2  "25"; SVGAttr.Stroke "red"; SVGAttr.MarkerEnd "url(#arrow)"] []
        rect [X "40.5"; Y "40.5"; SVGAttr.Width "50"; SVGAttr.Height "20"; SVGAttr.Stroke "red"; SVGAttr.Fill "white"; !!("class","tooltip-shift-reg-box")] []
        text [ X "65"; Y "50" ; !!("dominant-baseline","middle"); SVGAttr.TextAnchor "middle"; !!("class","tooltip-shift-reg-txt")] [ ofString "1" ]
      ]
      



   


// ***********************************************************************************************
//                                  Tooltips using Tippy.js
// ***********************************************************************************************

/// top-level function from tippy.js to make tooltips
let tippy(rClass:string, tippyOpts:obj):unit = importDefault "tippy.js"

/// Position of widget on editor buffer character grid.
/// AboveBelow => offset form position, Exact => centered on position
type WidgetPlace =
    | AboveBelow of HPos: int * VPos: int
    | Exact of HPos: int * VPos: int

/// <summary> Make an editor content widget to display over editor. 
/// Position in editor: pos. HTML to display: dom.</summary>
/// <param name="name"> Widget id (tracked by editor) name</param>
/// <param name="dom"> HTML to display in widget </param>
/// <param name="pos">  Position of widget on editor character grid </param>
let makeContentWidget (name: string) (dom:HTMLElement) (pos:WidgetPlace) =
    let h,v = match pos with | AboveBelow(h,v) -> (h,v) | Exact(h,v) -> (h,v)
    let widget = createObj  [
                  "domNode" ==> dom
                  "getDomNode" ==> fun () -> dom
                  "getId" ==> fun () -> name
                  "getPosition" ==> 
                       fun () -> 
                        createObj [ "position" ==>  
                                        createObj [
                                             "lineNumber" ==> v
                                             "column" ==> h
                                        ]
                                    "preference" ==>   
                                        match pos with 
                                        | Exact _ -> [|0|]
                                        | AboveBelow _ -> [|1;2|]
                                  ]
                  ] 
    editors.[currentFileTabId]?addContentWidget widget |> ignore
    currentTabWidgets <- Map.add name widget currentTabWidgets 

/// delete content widget with ID = name on current editor.
let deleteContentWidget name =
    match Map.tryFind name currentTabWidgets with
    | None -> ()
    | Some w ->
        editors.[currentFileTabId]?removeContentWidget w |> ignore
        currentTabWidgets <- Map.remove name currentTabWidgets

let deleteAllContentWidgets() =
    Array.iter deleteContentWidget (Map.keys currentTabWidgets) 

let tippyTheme() =
    match vSettings.EditorTheme with
    | "one-light-pro" | "solarised-light" -> "dark"
    | _ -> "light"

/// <summary> Tooltip generator using tippy.js https://atomiks.github.io/tippyjs/ </summary>
/// <param name="theme"> Tippy theme as defined in CSS: dark, light, etc</param>
/// <param name = "placement"> Tippy placement (top, bottom, etc) </param>
/// <param name = "clickable"> true => click to display, false => hover to display </param>
/// <param name = "button"> true => delay tooltip and make it close on click </param>
/// <param name = "domID"> ID of DOM element to which tooltip is attached </param>
let makeTooltip (theme:string) (placement:string) (clickable:bool) (button:bool) (domID:string) (tooltip: HTMLElement) =
    tippy( "#"+domID, createObj <| 
        [ 
            "html" ==> tooltip 
            "hideOnClick" ==> if clickable then true :> obj else button :> obj
            "interactive" ==> not clickable
            "delay" ==> if button then "[1300,0]" else "0"
            "arrow" ==> true
            "trigger" ==> if clickable then "click" else "mouseenter"
            "arrowType"==> "round"
            "theme" ==> theme
            "placement" ==> placement
        ])

        
/// <summary>
/// Make an info button with associated hover tooltip.
/// </summary>
/// <param name = "h"> horizontal char position for LH edge of button in editor </param>
/// <param name = "v"> line number in editor buffer on which to place button (starting from 0 = top)</param>
/// <param name = "buttonText"> label on button</param>
/// <param name = "toolTipDOM"> DOM to display inside tooltip box </param>
let makeEditorInfoButton (clickable:bool) h v (buttonText:string) (toolTipDOM:HTMLElement) = 
    /// Ratio of char width / char size for editor buffer font.
    /// TODO: work this out properly from a test
    let editorFontWidthRatio = 0.6 // works OK for Fira Code Mono
    let name = buttonText.ToLower()
    let domID = sprintf "info-button-%s-%d" name v
    let tooltip = ELEMENT "DIV" [sprintf "tooltip-%s" name] [toolTipDOM]
    let dom = 
        ELEMENT "BUTTON" [ sprintf "info-button-%s" name] [] 
        |> INNERHTML buttonText 
        |> ID domID
        |> STYLE ("margin-left",sprintf "%.0fpx" (editorFontWidthRatio * (float h+2.0) * float (int vSettings.EditorFontSize)))
    dom.addEventListener_click( fun _ ->
        Browser.console.log (sprintf "Clicking button %s" buttonText) |> ignore
        )
    deleteContentWidget domID // in some cases we may be updating an existing widget
    makeContentWidget domID dom <| Exact(0,v)
    makeTooltip (tippyTheme()) "bottom" true false domID tooltip



/// Add all the static tooltip information on the editor
let addFixedToolTips() =

    let makeTT domID tooltip = makeTooltip "dark" "right" false false domID tooltip
 
    let makeClickTT domID tooltip = makeTooltip "dark" "bottom" true false domID tooltip
    
    let makeTextTT placement htmlID cssClassLst text = makeTooltip "dark" placement false false htmlID (ELEMENT "p" cssClassLst [] |> INNERHTML text)
    let makeButtonTT placement htmlID cssClassLst text = makeTooltip "dark" placement false true htmlID (ELEMENT "p" cssClassLst [] |> INNERHTML text)

    makeTextTT "top" "flags" ["tootip-fixed"] "ARM Status bits (Flags) NZCV. <br> Blue indicates that Flag was written by <br> the most recently executed instruction."
    makeTextTT "bottom" "clock-symbol" ["tootip-fixed"] "Execution time"
    makeTextTT "bottom" "clock-time" ["tootip-fixed"] "Number of <br> Instructions"
    makeTextTT "bottom" "BR15" ["tootip-fixed"] "R15 (PC) is the Program Counter <br> It cannot be used as a data register"
    makeTextTT "right" "BR14" ["tootip-fixed"] "R14 (LR) is the Link Register <br> It can be used as a data register"
    makeTextTT "right" "BR13" ["tootip-fixed"] "R13 (SP) is the Stack Pointer. <br> It can be used as a data register"
    makeButtonTT "bottom" "tab-sym" ["tootip-fixed"] "Displays symbols (labels) <br> after execution has started"
    makeButtonTT "bottom" "tab-mem" ["tootip-fixed"] "Displays current data memory contents after execution has started <br> Words are added dynamically when they are written"
    makeButtonTT "bottom" "tab-reg" ["tooltip-fixed"] "Displays current register contents"
    makeButtonTT "bottom" "rep-hex" ["tooltip-fixed"] "Switch numeric displays to hexadecimal"
    makeButtonTT "bottom" "rep-bin" ["tooltip-fixed"] "Switch numeric displays to binary. <br> In binary '_' is separator <br> used to make bits more readable <br> optional in assembler literals"
    makeButtonTT "bottom" "rep-dec" ["tooltip-fixed"] "Switch numeric displays to two's complement signed decimal"
    makeButtonTT "bottom" "rep-udec" ["tooltip-fixed"] "Switch numeric displays to unsigned decimal"

    let makeRegTT regID = makeTextTT  "right" ("B"+regID) ["tootip-fixed"] (sprintf "%s is a data register" regID)
    List.iter (fun n -> makeRegTT  (sprintf "R%d" n)) [0..12]

   

let makeShiftTooltip (h,v) (dp:CommonData.DataPath) (rn:CommonData.RName) (shiftT:DP.ArmShiftType Option) (shiftAmt:uint32) (op2: DP.Op2) =
    let before = dp.Regs.[rn]|> uint64 |> int64 |> int32
    let (after,uf) = DP.evalOp2 op2 dp 
    let after' = after |> uint64 |> int64 |> int32
    printfn "Making shift tooltip"
    makeEditorInfoButton true h (v+1) "Shift" (displayShiftDiagram rn before (after',uf) shiftT (shiftAmt |> int))
    
    


