

type OpCodeGen = {
    Base: (string*string*string) list
    Suffix: (string*string) list
}

type OpCodes = {
    DP: OpCodeGen
    MEM: OpCodeGen
    DATA: OpCodeGen
}

let makePage md =
    