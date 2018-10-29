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

let check1() =
    let x = 0xFF000000u
    let y = uint32("-16777216")
    printfn "numbs are: %x,%x,%x" x y (x-y)
    match Helpers.parseNumberExpression Map.empty "-16777216" with
    | Ok n -> printfn "Parsed as:%x" n
    | _ -> printfn "no parse possible"


