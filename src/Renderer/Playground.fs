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
let prop12 = 
    let x = (0x80000000u) ||| 0u
    x = (x >>> 0)

let inline equals name a b =
    match a = b with
    | true -> printfn "%s: OK" name
    | false -> printfn "%s: expected %A, actual %A" name a b



let inline binOpCheck name op lh rh =
    equals (sprintf "%s: " name) ((op lh rh) >>> 0) (op lh rh) 

let x = -1
let xl = -1L
let ul = 0xFFFFFFFFFFFFFFFFuL



let check1() =

    binOpCheck "|||" (|||) 0x80000000u 0u
    binOpCheck "&&&" (&&&) 0x80000000u 0xffffffffu
    binOpCheck "^^^" (^^^) 0x80000000u 0u
    binOpCheck "<<<" (<<<) 0xf0000000u 2
    binOpCheck ">>>" (>>>) 0xf0000000u 0
    binOpCheck "~~~" (fun a b -> ~~~a) 0x70000000u ()
    binOpCheck "||| signed" (|||) 0x80000000 0
    binOpCheck "&&& signed" (&&&) 0x80000000 -1
    binOpCheck "^^^ signed" (^^^) 0x80000000 0
    binOpCheck "~~~ signed " (fun a b -> ~~~a) 0x70000000 ()
    binOpCheck "<<< signed " (<<<) 0xf0000000 2
    binOpCheck ">>> signed " (>>>) 0xf0000000 2
    equals "~~~ char" (~~~0x0fy) 0xf0y
    equals "hex unsigned: " 0x80000000u (0x80000000u >>> 0)
    equals "+ result equals"  true ((0x8000000u + 0x4000003u) = 0xc000003u)
    ()


   

