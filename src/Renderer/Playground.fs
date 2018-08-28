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

let check1() =
    printfn "%A" (rnds 0 50)
    printfn "\n\nz=%d" z


