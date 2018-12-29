(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Views
    Description: Display registers, memory or symbol table in Views Panel
*)

/// implement views panel
module Views

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser
open Refs
open Fable
open ExecutionTop

let maxSymbolWidth = 30
let maxDataSymbolLength = 16


let nameSquash maxW name =
    let nameLen = String.length name
    if nameLen <= maxW then name
    else
        let fp = (float maxW) * 0.65 |> int
        let lp = maxW - (fp + 3)
        name.[0..fp - 1] + "..." + name.[nameLen - lp..nameLen - 1]

let calcDashboardWidth() =
    let w =
        match currentRep, currentView with
        | Bin, _ -> "--dashboard-width-binrep"
        | _, Registers -> "--dashboard-width-init-registers"
        | _ -> "--dashboard-width-init-other"
        |> getCustomCSS
    printf "Setting width to %s" w
    w |> setDashboardWidth


let setRepresentation rep =
    (// Disable the other button
    representation currentRep).classList.remove("btn-rep-enabled")
    |> ignore

    // Enable the newly pressed button
    let btnNew = representation rep
    btnNew.classList.add ("btn-rep-enabled");

    // Reassign currentRep, new mutability required
    // keep constants defining GUI sizes in CSS
    currentRep <- rep
    calcDashboardWidth()
    updateRegisters()

/// Toggle memory direction
let toggleReverseView() =
    reverseDirection <- not reverseDirection
    match reverseDirection with
    | true ->
        reverseViewBtn.classList.add ("btn-byte-active")
        reverseViewBtn.innerHTML <- "Disable Reverse Direction"
    | false ->
        reverseViewBtn.classList.remove ("btn-byte-active")
        reverseViewBtn.innerHTML <- "Enable Reverse Direction"


/// Toggle byte / word view
let toggleByteView() =
    byteView <- not byteView
    match byteView with
    | true ->
        byteViewBtn.classList.add ("btn-byte-active")
        byteViewBtn.innerHTML <- "Disable Byte View"
    | false ->
        byteViewBtn.classList.remove ("btn-byte-active")
        byteViewBtn.innerHTML <- "Enable Byte View"

/// Converts a memory map to a list of lists which are contiguous blocks of memory
let contiguousMemory reverse (mem : Map<uint32, uint32>) =
    Map.toList mem
    |> List.fold (fun state (addr, value) ->
        match state with
        | [] -> [ [ (addr, value) ] ]
        | hd :: tl ->
            match hd with
            | [] -> failwithf "Contiguous memory never starts a new list with no elements"
            | hd' :: _ when fst hd' = addr - 4u ->
                ((addr, value) :: hd) :: tl // Add to current contiguous block
                           | _ :: _ -> [ (addr, value) ] :: state // Non-contiguous, add to new block
    ) []
    |> List.map (if reverse then id else List.rev) // Reverse each list to go back to increasing
    |> if reverse then id else List.rev // Reverse the overall list

/// Converts a list of (uint32 * uint32) to a byte addressed
/// memory list of (uint32 * uint32) which is 4 times longer
/// LITTLE ENDIAN
let lstToBytes (lst : (uint32 * uint32) list) =
    let byteInfo (dat : uint32) =
        let b = dat &&& 0xFFu
        match b with
        | _ when b >= 32u && b <= 126u -> sprintf "'%c'" (char b), b
        | _ -> "", b
    lst
    |> List.collect (fun (addr, value) ->
        [
            addr, value |> byteInfo
            addr + 1u, (value >>> 8) |> byteInfo
            addr + 2u, (value >>> 16) |> byteInfo
            addr + 3u, (value >>> 24) |> byteInfo
        ]
    )

/// make an HTML element
/// id = element name
/// css = css class names to add to classlist
/// inner = inner HTML (typically text) for element
let makeElement (id : string) (css : string) (inner : string) =
        let el = document.createElement id
        el.classList.add css
        el.innerHTML <- inner
        el

/// make an HTML element
/// id = element name
/// css = css class names to add to classlist
let makeEl (id : string) (css : string) =
        let el = document.createElement id
        el.classList.add css
        el
/// appends child node after last child in parent node, returns parent
/// operator is left associative
/// child: child node
/// node: parent node.
let (&>>) (node : Node) child =
    node.appendChild child |> ignore
    node

let createDOM (parentID : string) (childList : Node list) =
    let parent = document.createElement parentID
    List.iter (fun ch -> parent &>> ch |> ignore) childList
    parent

let addToDOM (parent : Node) (childList : Node list) =
    List.iter (fun ch -> parent &>> ch |> ignore) childList
    parent

/// Update Memory view based on byteview, memoryMap, symbolMap
/// Creates the html to format the memory table in contiguous blocks
let updateMemoryIfChanged =

    let updateMemory' (currentRep, byteView, reverseView, symbolMap, mem, stkInf) =
        let chWidth = 13
        let memPanelShim = 50
        let onlyIfByte x = if byteView then [ x ] else []
        let invSymbolTypeMap symType =
            symbolMap
            |> Map.toList
            |> List.filter (fun (_, (_, typ)) -> typ = symType)
            |> List.distinctBy (fun (_, (addr, _)) -> addr)
            |> List.map (fun (sym, (addr, _)) -> (addr, sym))
            |> Map.ofList
        let invSymbolMap = invSymbolTypeMap ExecutionTop.DataSymbol
        let invCodeMap = invSymbolTypeMap ExecutionTop.CodeSymbol
        let invStackMap =
            match stkInf with
            | Some(si, sp) -> si |> List.map (fun { SP = sp; Target = target } ->
                                            sp - 4u, match Map.tryFind target invCodeMap with
                                                     | Some s -> "(" + s + ")"
                                                     | None -> sprintf "(%08x)" target)
                               |> Map.ofList, sp
            | _ -> Map.empty, 0u
            |> (fun (map, sp) -> // add SP legend
                    let lab =
                        match sp, Map.tryFind sp map with
                        | 0u, Some sym -> sym
                        | 0u, None -> ""
                        | _, None -> "SP ->"
                        | _, Some sym -> sym + " ->"
                    Map.add sp lab map)


        let lookupSym addr =
                match Map.tryFind addr invSymbolMap, Map.tryFind addr invStackMap with
                | Some sym, _ -> sym
                | option.None, Some sub -> sub
                | _ -> ""


        let makeRow (addr : uint32, (chRep : string, value : uint32)) =

            let tr = makeEl "tr" "tr-head-mem"

            let rowDat =
                [
                    lookupSym addr |> nameSquash maxDataSymbolLength
                    sprintf "0x%X" addr
                    (if byteView then
                        formatterWithWidth 8 currentRep value +
                        (chRep |> function | "" -> "" | chr -> sprintf " %s" chr)
                    else formatter currentRep value)
                ]

            let makeNode txt = makeElement "td" "selectable-text" txt :> Node

            addToDOM tr (List.map makeNode rowDat)

        let makeContig (lst : (uint32 * uint32) list) =

            let table = makeEl "table" "table-striped"

            let makeNode txt = makeElement "th" "th-mem" txt :> Node

            let tr = createDOM "tr" <| List.map makeNode ([ "Symbol"; "Address"; "Value" ])

            let byteSwitcher =
                match byteView with
                | true -> lstToBytes
                | false -> List.map (fun (addr, dat) -> (addr, ("", dat)))

            // Add each row to the table from lst
            let rows =
                lst
                |> byteSwitcher
                |> List.map makeRow

            addToDOM table <| [ tr ] @ rows
            |> ignore

            let li = makeEl "li" "list-group-item"
            li.style.padding <- "0px"

            addToDOM li [ table ]

        memList.innerHTML <- ""

        // Add the new memory list

        mem
        |> contiguousMemory reverseView
        |> List.map makeContig
        |> List.iter (fun html -> memList.appendChild (html) |> ignore)
    updateMemory'
    |> cacheLastWithActionIfChanged

let updateMemory() =
    let stackInfo =
        match runMode with
        | FinishedMode ri
        | RunErrorMode ri
        | ActiveMode(_, ri) ->
            let sp = (fst ri.dpCurrent).Regs.[CommonData.R13]
            Some(ri.StackInfo, sp)
        | _ -> Core.Option.None
    updateMemoryIfChanged (currentRep, byteView, reverseDirection, symbolMap, memoryMap, stackInfo)

/// Update symbol table View using currentRep and symbolMap
let updateSymTableIfChanged =
    let updateSymTable (symbolMap, currentRep) =
        let makeRow ((sym : string), (value, typ) : uint32 * ExecutionTop.SymbolType) =
            let tr = makeEl "tr" "tr-head-sym"
            addToDOM tr [
                makeElement "td" "selectable-text" sym
                makeElement "td" "selectable-text" (formatter currentRep value)
                ]

        let makeGroupHdr typ =
            let symName =
                match typ with
                | DataSymbol -> "Data Symbol"
                | CodeSymbol -> "Code Symbol"
                | CalculatedSymbol -> "EQU Symbol"

            createDOM "tr" [
                makeElement "th" "th-mem" symName
                makeElement "th" "th-mem" "Value"
                ]

        let symTabRows =
            let makeGroupRows (grpTyp, grpSyms) =
                grpSyms
                |> Array.map (fun (sym, addr) -> sym, addr)
                |> Array.sortBy snd
                |> Array.map (fun (sym, addr) -> nameSquash maxSymbolWidth sym, addr)
                |> Array.map makeRow
                |> Array.append [| makeGroupHdr grpTyp |]

            let groupOrder = function
                | (CodeSymbol, _) -> 1
                | (DataSymbol, _) -> 2
                | (CalculatedSymbol, _) -> 3

            symbolMap
            |> Map.toArray
            |> Array.groupBy (fun (_sym, (_addr, typ)) -> typ)
            |> Array.sortBy groupOrder
            |> Array.collect makeGroupRows
            |> Array.toList

        // Clear the old symbol table
        symTable.innerHTML <- ""
        // Add the new one
        addToDOM symTable (symTabRows) |> ignore
    updateSymTable
    |> cacheLastWithActionIfChanged

let updateSymTable() =
    updateSymTableIfChanged (symbolMap, currentRep)

/// Set View to view
let setView view =
    (// Change the active tab
    viewTab currentView).classList.remove("active")
    (viewTab view).classList.add("active")

    (// Change the visibility of the views
    viewView currentView).classList.add("invisible")
    (viewView view).classList.remove("invisible")

    // new mutability again, update the variable
    currentView <- view
    calcDashboardWidth()
    updateMemory()


