(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.ErrorDocs
    Description: create markdown for assembler hover message hints
*)
module ErrorDocs

type CodeType = DP3 | DP2

let opCodeData = [
    "ADD","op1 + op2", DP3
    "SUB","op1 - op2", DP3
    "RSB","op2 - op1", DP3
    "ADC","op1 + op2 + C", DP3
    "SBC","op1 - op2 + C", DP3
    "RSC","op2 - op1 + C - 1", DP3
    "MOV","op2", DP2
    "MVN","NOT op2", DP2

    ]




    