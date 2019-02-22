module HWSimulation
/// Emulating an AVR timer but with 32 bits
/// Counter register holds the count as a 32 bit number

/// TCNT = Timer Counter; TCCR = Timer Counter Control Register FOC0; WGM00; COM01; COM00; WGM01; CS02; CS01; CS00;
/// OCR = Output Compre Register; TIFR = Timer Counter Flag Register OCF2; TOV2; ICF1; OCF1A; OCF1B; TOV1; OCF0; TOV0;
type Clock = {TCNT: uint32; OCR: uint32; TCCR: uint8; TIFR: uint8;}

/// Type containing all hardware and number of cycles when last valid
type IntState = {Clock0: Clock; Clock1: Clock; Clock2: Clock; Cycles: int64;}

/// Type to indicate if interrupt raised, and which hardware block
type IntStatus = {Interrupt: bool; Hardware: string;}

/// Convert the TCCR register to an int for clock division
let scaler (TCCR: uint8) =
    let isolatedbits = TCCR &&& uint8(7)
    match int(isolatedbits) with
    | 0 -> 0
    | 1 -> 1
    | 2 -> 8
    | 3 -> 64
    | 4 -> 256
    | 5 -> 1024
    | _ ->  failwithf "Not implemented or invalid TCCR values"

let scaledIncrement (intState: IntState) (clock: Clock) (cycles: int64) =
    let clockScale = scaler clock.TCCR
    let cyclesPassed = cycles - intState.Cycles
    int(cyclesPassed)/clockScale

let clockUpdate (intState: IntState) (clock: Clock) (cycles: int64) =
    let TOV0FlagCheck TIFR = if (pown 2 32) - 1 - int(clock.TCNT) - (scaledIncrement intState clock cycles) < 0 then (TIFR ||| uint8(0b00000001)) else TIFR
    let OCF0FlagCheck TIFR = if (int(clock.OCR) - 1 - int(clock.TCNT) - (scaledIncrement intState clock cycles)) < 0 then (TIFR ||| uint8(0b00000010)) else TIFR
    let TIFRCheck = clock.TIFR |> TOV0FlagCheck |> OCF0FlagCheck
    let newCount = uint32(int64(clock.TCNT) + int64(scaledIncrement intState clock cycles))
    let newClock = {TCNT = newCount; OCR = clock.OCR; TCCR = clock.TCCR; TIFR = TIFRCheck}

    if (scaler clock.TCCR) = 0 then clock else newClock

let allClockUpdate (intState: IntState) (cycles: int64) =
    {Clock0 = clockUpdate intState intState.Clock0 cycles;
    Clock1 = clockUpdate intState intState.Clock1 cycles;
    Clock2 = clockUpdate intState intState.Clock2 cycles;
    Cycles = cycles}

let state (intstate: IntState) (cycles: int64) =
    allClockUpdate intstate cycles

let peripheralWrite (intState: IntState) (cycles: int64) (address: uint32) (data: uint32): IntState =
    intState

let peripheralRead (intState: IntState) (cycles: int64) (address: uint32): IntState * uint32 =
    (intState, address) 