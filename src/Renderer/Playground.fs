(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Playground
    Description: File for self-contained test code and checking FABLE compiler bugs
*)

module Playground

// Check uint32 vs int32

let intLst = [0..31] |> List.map (fun n -> 1 <<< n)
let getBit n = (1u <<< n)



let checkConvert (n:uint32) =
    let x = n |> int32
    let n' = x |> uint32
    if n <> n' then printfn "Error for n = %x (%d), n |> uint32 = %x (%d), |> uint32 |> int32 = %x (%d)" n n x x n' n'
    

let check1() =
    printfn "Check 1 started"
    checkConvert (getBit 31)
    printfn "Check 1 finished"
