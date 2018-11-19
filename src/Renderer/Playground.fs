(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Playground
    Description: File for self-contained test code and checking FABLE compiler bugs
*)

module Playground

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open DP
open Refs
open EEExtensions
open Helpers
open CommonData

type TbSpec =
    | TbRegEquals of RName * uint32
    | TbRegPointsTo of RName * uint32 list

type TbInOut = TbIn | TbOut


let parseTbLine lNum (lin:string) =
    let (|RESOLVE|_|) lst =
        let ops = 
            String.concat " " lst
            |> String.splitString [|","|] 
            |> Array.map String.trim
            |> Array.toList
        let parseLiteral  = function | LITERALNUMB (lit,"") -> [lit] | _ -> []
        let parseL = List.map parseLiteral ops
        if List.exists ((=) []) parseL then None
        else
            List.concat parseL |> Some
    let (|Defs|_|) words =
        match words with
        | [RegMatch (Ok rn) ; "IS" ; LITERALNUMB (lit,"")] -> TbRegEquals(rn,lit) |> Some
        | RegMatch (Ok rn) :: "PTR" :: RESOLVE lits -> TbRegPointsTo(rn,lits) |> Some
        | _ -> None
    match lin.ToUpper() |> String.splitOnWhitespace |> Array.toList  with
    | "IN" :: Defs tbSpec -> Ok (TbIn, tbSpec)
    | "OUT" :: Defs tbSpec -> Ok (TbOut, tbSpec)
    | _ -> Error (lNum,"Parse Error in testbench")



let splitResult resL =
    List.fold (fun (rl,el) -> function | Error e -> rl, e :: el | Ok r -> r :: rl, el) ([],[]) resL

let decorateParseLine (i, pl) = ()

let setupTest tl = ()

let parseTestbench() =
    if currentFileTabId < 0 then [ Error (-1,"No current file") ]
    else
        let testLines, testErrors = 
            getCode currentFileTabId
            |> String.splitString [|"\n"|]
            |> Array.indexed
            |> Array.map (fun (i,lin) -> parseTbLine i lin)
            |> Array.toList
            |> splitResult
        if testErrors = [] then
            testLines |> List.map Ok
        else
            testErrors |> List.map Error




let check1() =
    parseTestbench()
    ()



