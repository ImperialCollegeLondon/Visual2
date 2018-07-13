(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.ErrorDocs
    Description: create markdown for assembler hover message hints
*)
module ErrorDocs

open EEExtensions
open Refs
open Fable.PowerPack

type CodeType = DP3 | DP2 | CMP | LDRSTR | LDMSTM | MISC | EQU | UNIMPLEMENTED


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
    "LDR","load memory word or byte at address 'EA' into Rd.\nOptionally update Rb.", LDRSTR
    "STR","load memory word or byte at address 'EA' into Rd.\nOptionally update Rb.", LDRSTR
    "LDM","load multiple registers from memory", LDMSTM
    "STM","store multiple registers to memory", LDMSTM
    "FILL", "allocate op1 zero filled data bytes.\nop1 must be divisible by 4", MISC
    "DCD","allocate data words as specified by op1,...opn",MISC
    "DCB","allocate data bytes as specified by op1,...,opn.\nThe number of operand must be divisible by 4",MISC
    "EQU","define label on this line to equal op1\n op1 may contain: labels,numbers, +,-,*,/", EQU
    ]

    /// match opcode with opcode list returning root instruction (without suffixes or conditions)
    /// return "unimplemented" on no match.
let getRoot opc =
    opCodeData
    |> List.collect (fun (root,legend,opType) ->
        if String.startsWith root opc then 
            [root,opType]
        else [])
    |> function
        | [ spec] -> spec
        | _ -> "unimplemented", UNIMPLEMENTED




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

let makeCMPHover opc func = 
    sprintf """
%s op1, op2;  %s

Examples:
```
%s R0, R1
%s R5, #101
%s R10, #0x5a
%s R7, #-1
```
"""    opc func  opc opc opc opc

let makeLDRSTRHover opc func = 
    sprintf """
%s Rd, EA;  %s

Examples:
```
%s R0, [R10]
%s R5, [R9,#100]
%s R1, [R1], #0x5a
%s R8, [R13, #-32]!
%s R3, [R4, R5]
%s R3, [R4, R5, LSL #2]!
```
"""    opc func  opc opc opc opc opc opc

let makeLDMSTMHover opc func = 
    sprintf """
%s Rs[!], {register-list};  %s

Examples:
```
LDMFD R0!, {R1}
STMFD R10!, {R2,R14}
LDMIB R10, {R2-R9,R11}
```
"""    opc func

let makeMISCHover opc func = 
    let initLine =
        match opc with
        | "DCD" | "DCB" -> "op1, ..., opn"
        | "FILL"  -> "op1"
        | _ -> failwithf "%s is not a MISC opcode" opc        
    sprintf """
%s %s %s
Examples:
```
DCD op1, ..., opn
DCB op1, ..., opn
FILL N
```
"""  opc initLine func 

let makeEQUHover opc func =
    sprintf """
%s op1; %s
Examples:
```
X1       EQU 44
MULTDATA EQU DATA1 + 0x100
PTR      EQU (X1 - 12) * 4 + X2
```
"""  opc func 

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

let unimplementedHover =
    """
    This opcode is not recognised
"""


let getOpcHover mess opc line =
    let key,typ = getRoot opc
    let hoverText =
        match typ with
        | DP3 -> mess + makeDPHover3 opc key
        | DP2 -> mess + makeDPHover2 opc key
        | CMP -> mess + makeCMPHover opc key
        | MISC -> mess + makeMISCHover opc key
        | LDRSTR -> mess + makeLDRSTRHover opc key
        | LDMSTM -> mess + makeLDMSTMHover opc key
        | EQU -> mess + makeEQUHover opc key 
        | UNIMPLEMENTED -> unimplementedHover
        |> String.split [|'\n'|]
    let stripComment line = 
        match String.split [|';'|] line
        | | ins :: _ -> ins
        | failwithf "What? split should return at least one item!"
    let oLen = opc.Length
    let oStart = 
        match (" " + [|opc|] (stripComment line) + " ") String.splitString |> List.rev with
            | _afterPart :: before -> (before.Length-1)*oLen + before.sumBy (fun s -> s.Length) + 1
            | x -> failwithf "What? Unexpected split '%A' can't happen. line = '%s', opc = '%s'." x line opc
    hovertext, (oStart, oStart + oLen - 1)




    