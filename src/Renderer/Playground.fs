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

let rand (s:int32) = (s*101001+1234759)

let rec rnds s n = match n with | 0 -> [] | n -> rand s :: rnds (rand s) (n-1)

let z = (10000001u*100000001u*1000000001u*100000001u &&& 0x1u)

let x = -1
let xl = -1L
let ul = 0xFFFFFFFFFFFFFFFFuL

let prop1 = x |> uint64
let prop2 = (0xFFFFFFFFu |> int64 = 0xFFFFFFFFL)
let prop3 = (xl |> uint64 = 0xFFFFFFFFFFFFFFFFuL)
let prop4 = (ul |> int64 = -1L)
let prop5 = (xl |> int32 = -1)
let prop6 = (xl |> uint32 = 0xFFFFFFFFu)
let prop7 = (xl |> int32 = -1)
let prop8 = ul |> int32
let prop9 = ul |> uint32

let check1() =
    printfn "Negative int32 sign extended to uint64=%x,%f" xl (prop1 |> double)
    printfn "Large uint32 zero extended to int64=%A" prop2
    printfn "Negative int64 unchanged as bits to uint64=%A" prop3
    printfn "Large uint64 unchanged as bits to int64=%A" prop4
    printfn "Negative int64 unchanged to int32 = %A" prop5
    printfn "Negative int64 unchanged as lower order bits to uint32 = %A" prop6
    printfn "Negative int64 unchanged to int32 = %A" prop7
    printfn "Large uint64 unchanged as lower order bits to int32 = %x,%f" prop8 (prop8 |> double)
    printfn "Large uint64 unchanged as lower order bits to uint32 = %x,%f" prop9 (prop9 |> double)
    printfn "large hex constant %x" 0xFFFFFFFFFFFFFFFFuL
    printfn "large uint32 hex constant: %x" 0xFFFFFFFFuL





