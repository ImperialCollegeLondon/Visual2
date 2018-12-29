(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.CommonData
    Description: Low-level data structures used throughout the emulator
*)

/// Common data types and code for emulator
module CommonData

    //////////////////////////////////////////////////////////////////////////////////////
    //                   Common types and code used by all modules
    //////////////////////////////////////////////////////////////////////////////////////

    /// ARM Status bits
    type Flags = { N : bool; C : bool; Z : bool; V : bool }


    ////////////////////////ARM register names and operations/////////////////////////////


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

    // various functions used to convert between string, RName, and register number

    /// Inverse of regNames, used to convert RName values to strings
    /// NB The string chosen will always be the register (not alias)
    let regStrings =
        regNames
        |> Map.toList
        |> List.map (fun (s, rn) -> (rn, s))
        |> List.filter (fun (_, s : string) -> s.StartsWith "R")
        |> Map.ofList

    /// Map converts RName into register number (no aliasses)
    let regNums = Map.map (fun _ (s : string) -> int (s.[1..])) regStrings

    /// Map converts register number into RName (no aliasses)
    let inverseRegNums =
        regNums |> Map.toList
        |> List.map (fun (rn, n) -> (n, rn)) |> Map.ofList

    /// Property on RName to return register number, for convenience
    /// Aliasses not included, since they are not RNames
    type RName with
        /// Return the number of a register as an integer
        member r.RegNum = regNums.[r]

    /// Return a register name from an integer
    let register n =
        if 0 <= n && n < 16
        then inverseRegNums.[n]
        else (failwithf "Register %d does not exist!" n)

    /// Type to represent the contents of one memory location
    /// 'INS is a parameter set to the type of an instruction
    /// needed because instruction type is only defined
    /// at top level.
    type MemLoc<'INS> =
        | DataLoc of uint32
        | Code of 'INS

    /// type to represent a (word) address
    /// there is some ambiguity. Does this contain the real address
    /// which is always divisible by 4
    /// or does it contain the word number (real address dvided by 4)
    /// either way multiply/divide by 4 will cause problems!
    /// document this well and be consistent.
    type WAddr = WA of uint32

    /// type to represent memory
    type CodeMemory<'INS> = Map<WAddr, 'INS>

    type Data = | Dat of uint32 | CodeSpace

    type DataMemory = Map<WAddr, Data>

    /// ARM state as values of all registers and status bits
    /// NB PC can be found as R15 - 8. (Pipelining)
    type DataPath = {
        Fl : Flags; // Flags
        Regs : Map<RName, uint32> // map representing registers.
                               // Must be correctly initialised
        MM : DataMemory // map showing the contents of all memory
        }
