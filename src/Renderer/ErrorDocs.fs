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
    "LDR","load memory word or byte at address 'EA' into Rd. Optionally update Rb.", LDRSTR
    "STR","load memory word or byte at address 'EA' into Rd. Optionally update Rb.", LDRSTR
    "LDM","load multiple registers from memory", LDMSTM
    "STM","store multiple registers to memory", LDMSTM
    "FILL", "allocate op1 data bytes. Op1 must be divisible by 4. Fill words with 0.", MISC
    "DCD","allocate data words as specified by op1,...opn",MISC
    "DCB","allocate data bytes as specified by op1,...,opn. The number of operand must be divisible by 4",MISC
    "EQU","define label on this line to equal op1. Op1 may contain: labels,numbers, +,-,*,/", EQU
    ]

    /// match opcode with opcode list returning root instruction (without suffixes or conditions)
    /// return "unimplemented" on no match.
let getRoot (opc:string) =
    opCodeData
    |> List.collect (fun (root,legend,opType) ->
        if String.startsWith (root.ToUpper()) (opc.ToUpper()) then 
            [root,legend, opType]
        else [])
    |> function
        | [ spec] -> spec
        | _ -> "unimplemented", "",UNIMPLEMENTED

/// <summary> Add extra diagnostic info for bad shift instructions <summary>
let makeShiftModes (isShift, isRRX, isBadShift) =
    match isShift,isRRX,isBadShift with
    | None, None, None -> " #-1"
    | Some sft,_, _ -> sprintf " R2, %s #5" sft
    | _, Some _, _ -> " R10, RRX"
    | _, _, Some bad  -> sprintf " R6, LSR #1**\n\n*Note: * **%s** *is not a valid shift, did you mean* **LSR** , **ASR** , **LSL** , **ROR** , **RRX** *?* **" (String.trim bad)



let makeDPHover2 (opc,sfts) func = 
    sprintf """
*%s dest, op2;  %s*

**%s R0, R1**

**%s R5, #101**

**%s R10, #0x5a**

**%s R7, %s**

"""    opc func  opc opc opc opc (makeShiftModes sfts)

let makeCMPHover (opc,sfts) func = 
    sprintf """
*%s op1, op2;  %s*

**%s R0, R1**

**%s R5, #101**

**%s R10, #0x5a**

**%s R7, %s**
"""    opc func  opc opc opc opc (makeShiftModes sfts)

let makeLDRSTRHover opc func = 
    sprintf """
*%s Rd, EA;  %s*

**%s R0, [R10]**

**%s R5, [R9,#100]**

**%s R1, [R1], #0x5a**

**%s R8, [R13, #-32]!**

**%s R3, [R4, R5]**

**%s R3, [R4, R5, LSL #2]!**
"""    opc func  opc opc opc opc opc opc

let makeLDMSTMHover opc func = 
    sprintf """
*%s Rs[!], {register-list};  %s*

**LDMFD R0!, {R1}**

**STMFD R10!, {R2,R14}**

**LDMIB R10, {R2-R9,R11}**
"""    opc func

let makeMISCHover opc func = 
    let initLine =
        match opc with
        | "DCD" | "DCB" | "dcd" | "dcb" -> "op1, ..., opn; "
        | "FILL" | "fill" -> "op1 ; "
        | _ -> failwithf "%s is not a MISC opcode" opc        
    sprintf """
*%s %s %s*

**DCD op1, ..., opn**

**DCB op1, ..., opn**

**FILL N**
"""  opc initLine func 

let makeEQUHover opc func =
    sprintf """
*%s op1; %s*

**X1       EQU 44**

**MULTDATA EQU DATA1 + 0x100**

**PTR      EQU (X1 - 12) * 4 + X2**
"""  opc func 

let makeDPHover3 (opc,sfts) func =
    sprintf """
*%s dest, op1, op2;  %s*

**%s R0, R1, R2**

**%s R5, R5, #101**

**%s R10, R0, #0x5a**

**%s R7, R10, %s**

"""     opc func  opc opc opc opc (makeShiftModes sfts)

let unimplementedHover opc =
    sprintf " '%s': There is a problem with this instruction" opc


let getOpcHover mess opc line =
    //printfn "getting hover: opc='%s' line='%s'" opc line
    let uLine = 
        String.toUpper (line + " ") 
        |> String.replaceChar ',' ' '
        |> String.replaceChar '#' ' '
    if opc = "" then failwithf "can't get hover for '' opcode"
    let lineContains lst = List.tryFind (fun sft -> String.contains sft uLine) lst
    let isShift = lineContains [" LSL";" LSR";" ASR";" ROR"]
    let isRRX = lineContains [" RRX"]
    let isBadShift = lineContains [" ASL ";" ROL "; " RRL "]
    let _, legend,typ = getRoot opc
    let opc' = opc,(isShift,isRRX,isBadShift)
    let hoverText =
        match typ with
        | DP3 -> mess + makeDPHover3 opc' legend
        | DP2 -> mess + makeDPHover2 opc' legend
        | CMP -> mess + makeCMPHover opc' legend
        | MISC -> mess + makeMISCHover opc legend
        | LDRSTR -> mess + makeLDRSTRHover opc legend
        | LDMSTM -> mess + makeLDMSTMHover opc legend
        | EQU -> mess + makeEQUHover opc legend 
        | UNIMPLEMENTED -> unimplementedHover opc
        |> (fun s -> [s])
    let stripComment line = 
        match String.split [|';'|] line |> Array.toList with
        |  ins :: _ -> ins
        | _ -> failwithf "What? split should return at least one item!"
    let oLen = opc.Length
    let splitLine =
        " " + (stripComment line) + " "
        |> String.splitString [|opc|]
        |> Array.toList 
        |>  List.rev
    let oStart = 
        match splitLine  with
            | _afterPart :: before -> List.sumBy String.length before + 1
            | x -> failwithf "What? Unexpected split '%A' can't happen. line = '%s', opc = '%s'." x line opc
    let lineText = sprintf "```\n%s\n```\n" (stripComment line)
    if oStart + oLen - 1 >= line.Length then 
        ([lineText] @ hoverText), (1, lineText.Length)
    else  ([lineText] @ hoverText), (oStart, oStart + oLen - 1)




    