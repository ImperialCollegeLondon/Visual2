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

let z = -1L |> uint32

let prop1 = (-1 |> uint64 = 0xFFFFFFFFuL )
let prop2 = (0xFFFFFFFFu |> int64 = 0xFFFFFFFFL)
let prop3 = (-1L |> uint64 = 0xFFFFFFFFFFFFFFFFuL)
let prop4 = (0xFFFFFFFFFFFFFFFFuL |> int64 = -1L)
let prop5 = (-1L |> int32 = -1)
let prop6 = (-1L |> uint32 = 0xFFFFFFFFu)
let prop7 = (-1L |> int32 = -1)
let prop8' = 0xFFFFFFFFFFFFFFFFuL |> int32
let prop8 = prop8' = -1
let prop9' = 0xFFFFFFFFFFFFFFFFuL |> uint32
let prop9 = prop9' = 0xFFFFFFFFu
let prop10 = (0xFFFFFFFF + 0xFFFFFFFF = 0xFFFFFFFE)
let prop11 = -1L + -1L = -2L

let check1() =
    printfn "Negative int32 sign extended to uint64=%A" prop1
    printfn "Large uint32 zero extended to int64=%A" prop2
    printfn "Negative int64 unchanged as bits to uint64=%A" prop3
    printfn "Large uint64 unchanged as bits to int64=%A" prop4
    printfn "Negative int64 unchanged to int32 = %A" prop5
    printfn "Negative int64 unchanged as lower order bits to uint32 = %A" prop6
    printfn "Negative int64 unchanged to int32 = %A" prop7
    printfn "Large uint64 unchanged as lower order bits to int32 = %A %x" prop8 prop8'
    printfn "Large uint64 unchanged as lower order bits to uint32 = %A %x" prop9 prop9'
    printfn "Integer compare works with overflow"
    printfn "prop10=%A, prop11=%A" prop10 prop11

   

