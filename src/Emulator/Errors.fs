(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.Errors
    Description: Handle Errors
*)


// WARNING - this code is out of date and needs TLC when errors are properly defined

/// Error handling for parse and simulator
module Errors

    open CommonData


        /// Failure message for impossible match cases
    let alwaysMatchesFM = "Should never happen! Match statement always matches."

    let noErrorsFM = "Should never happen! No errors at this stage."

    // *************************************************************************
    // Error messages for instruction parsing
    // *************************************************************************

    /// Error message for `Invalid register`
    let notValidRegEM = " is not a valid register."

    /// Error message for `Invalid offset`
    let notValidOffsetEM = " is not a valid offset."

    /// Error message for `Invalid instruction`
    let notValidFormatEM = " is not a valid instruction format."

    /// Error message for `Invalid literal`
    let notValidLiteralEM = " is not a valid literal."

    /// Error message for `Invalid shift` or `Invalid second operand`
    let notValidRegLitEM = " is not a valid literal or register."

    /// Error message for `Invalid flexible second operand`
    let notValidFlexOp2EM = " is an invalid flexible second operand"

    /// Error message for `Invalid suffix`
    let notValidSuffixEM = " is not a valid suffix for this instruction."

    let notImplementedInsEM = " is not a recognised instruction."
    // *************************************************************************
    // Error messages for instruction execution
    // *************************************************************************

    type ErrCode =
        | ``Invalid syntax`` of wanted : string * found : string * page : string
        | ``Invalid format`` of error : string * found : string * page : string
        | ``Invalid instruction`` of reason : string
        | ``Label required`` of reason : string
        | ``Unimplemented parse``
        | ``Undefined symbol`` of symList : (string * string) list
        | ``Invalid opCode`` of root : string option * condition : string option * suffix : string
        | ``Unimplemented instruction`` of opCode : string
        | ``Duplicate symbol`` of sym : string * lines : int list
        | ``Literal more than 32 bits`` of literal : string
        | ``Literal is not a valid number`` of literal : string

    type ParseError = ErrCode

    /// The thing that stopped the simulation: EXIT means a normal program END.
    type ExecuteError =
        | NotInstrMem of uint32 // Trying to fetch from address where there is no instruction
        | ``Run time error`` of uint32 * string // a memory access error at given address (with error message)
        | ``Unknown symbol runtime error`` of string list // this should never happen, since symbols are resolved by parse
        | EXIT
        | TBEXIT

    let makeParseError wanted found page = ``Invalid syntax`` (wanted = wanted, found = found, page = page) |> Error

    let makeFormatError wanted found page = ``Invalid format`` (error = wanted, found = found, page = page) |> Error

    let makeInstructionError str = ``Invalid instruction`` str |> Error

    /// A function to combine results or forward errors.
    let combineError (res1 : Result<'T1, 'E>) (res2 : Result<'T2, 'E>) : Result<'T1 * 'T2, 'E> =
        match res1, res2 with
        | Error e1, _ -> Error e1
        | _, Error e2 -> Error e2
        | Ok rt1, Ok rt2 -> Ok(rt1, rt2)

    /// A function that combines two results by applying a function on them as a pair, or forwards errors.
    let combineErrorMapResult (res1 : Result<'T1, 'E>) (res2 : Result<'T2, 'E>) (mapf : 'T1 -> 'T2 -> 'T3) : Result<'T3, 'E> =
        combineError res1 res2
        |> Result.map (fun (r1, r2) -> mapf r1 r2)

    /// A function that applies a possibly erroneous function to a possibly erroneous argument, or forwards errors.
    let applyResultMapError (res : Result<'T1 -> 'T2, 'E>) (arg : Result<'T1, 'E>) =
        match arg, res with
        | Ok arg', Ok res' -> res' arg' |> Ok
        | _, Error e -> e |> Error
        | Error e, _ -> e |> Error

    let mapErrorApplyResult (arg : Result<'T1, 'E>) (res : Result<'T1 -> 'T2, 'E>) =
        match arg, res with
        | Ok arg', Ok res' -> res' arg' |> Ok
        | _, Error e -> e |> Error
        | Error e, _ -> e |> Error

    let condenseResultList transform (lst : Result<'a, 'b> list) : Result<'a list, 'b> =
        let rec condenser' inlst outlst =
            match inlst with
            | head :: tail ->
                match head with
                | Ok res -> condenser' tail ((transform res) :: outlst)
                | Error e -> e |> Error
            | [] -> List.rev outlst |> Ok
        condenser' lst []
