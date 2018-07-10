(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.ErrorDocs
    Description: create markdown for assembler hover message hints
*)
module ErrorDocs

open EEExtensions

type CodeType = DP3 | DP2 | CMP

let opCodeData = [
    "ADD","dest := op1 + op2", DP3
    "SUB","dest := op1 - op2", DP3
    "RSB","dest := op2 - op1", DP3
    "ADC","dest := op1 + op2 + C", DP3
    "SBC","dest := op1 - op2 + C", DP3
    "RSC","dest := op2 - op1 + C - 1", DP3
    "MOV","dest := op2", DP2
    "MVN","dest := NOT op2", DP2
    "CMP","set NZCV on op1 - op2", CMP
    "CMN","set NZCV on op1 + op2", CMP
    "TST","set NZ on op1 bitwise AND op2", CMP
    "TEQ","set NZ on op1 bitwise XOR op2", CMP
    ]

let makeDPHover2 opc func = 
    sprintf """
%s dest, op2;  %s

Examples:
```
%s R0, R1
%s R5, #101
%s R10, #0x5a
%s R7, #-1
```
"""    opc func  opc opc opc opc

let makeDPHover3 opc func =
    sprintf """
%s dest, op1, op2;  %s

Examples:
```
%s R0, R1, R2
%s R5, R5, #101
%s R10, R0, #0x5a
%s R7, R10, #-1
```
"""     opc func  opc opc opc opc

let makeHover mess opc =
    match List.tryFind (fun (op,_,_) -> op=opc) opCodeData with
    | None -> [|"Unknown opcode"|]
    | Some (_,key,typ) -> 
        match typ with
        | DP3 -> mess + makeDPHover3 opc key
        | DP2 | CMP -> mess + makeDPHover2 opc key
        |> String.split [|'\n'|]






    