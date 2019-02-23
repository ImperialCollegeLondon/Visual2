module RegisterNames

    // submodule for dependencies. In actual use these must come from CommonData module in Visual2


    //---------------------------ARM register names and operations---------------------------//

    /// ARM register names
    /// NB R15 is the program counter as read

    type RName = | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
                 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

    /// Map used to convert strings into RName values,
    /// includes register aliasses PC, LR, SP
    let regNames =
        Map.ofList [
            "R0", R0; "R1", R1; "R2", R2; "R3", R3; "R4", R4; "R5", R5
            "R6", R6; "R7", R7; "R8", R8; "R9", R9; "R10", R10; "R11", R11;
            "R12", R12; "R13", R13; "R14", R14; "R15", R15;
            "PC", R15; "LR", R14; "SP", R13
        ]