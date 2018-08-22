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

// Check uint32 vs int32

let intLst = [0..31] |> List.map (fun n -> 1 <<< n)
let getBit n = (1u <<< n)



let checkConvert () =
    let xs = (1 <<< 31) 
    let xu = (1u <<< 31)
    printfn "(1 <<< 31) signed: %x (%d); (1 <<< 31) unsigned: %x (%d)" xs xs xu xu



let check1() =
    printfn "Check 1 started"
    checkConvert()
    printfn "Check 1 finished"
