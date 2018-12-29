(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Tests
    Description: run automated tests using data geenrated by VisualRandomTestGen
*)

/// Run automatic emulator testing

module Tests
open System
open ExecutionTop
open Integration
open Errors
open EEExtensions
open Fable.Core
open Fable.Import
open Node.Exports
open CommonData
open Helpers



let fNone = Core.Option.None

let runPipe mess s = printfn "%s: %A" mess s; s

let projectDir = Refs.appDirName + @"/../"

let sampleDir = projectDir + @"app/samples/"




type Flags = {
    FN : bool
    FZ : bool
    FC : bool
    FV : bool
 }


type DPath = {
    TRegs : uint32 list;
    TFlags : Flags
    }

type TestSetup = {
    Before : DPath;
    Asm : string;
    After : DPath option;
    Name : string
    }

type TestT = | OkTests | ErrorTests | BetterThanModelTests


let writeDirPath = Refs.appDirName + @"/test-results"

let readFileViaNode (path : string) : string =
    (fs.readFileSync path).toString("utf8")

let fnWithoutSuffix (f : string) = (f.Split [| '.' |]).[0]

let readAllowedTests() =
    fs.readdirSync (U2.Case1(Refs.appDirName + @"/test-data"))
    |> Seq.toList
    |> List.filter (String.contains "ALLOWED")
    |> List.map (fun s -> Refs.appDirName + @"/test-data/" + s)
    |> List.collect (fun path ->
        readFileViaNode path
        |> String.split [| '\n' |]
        |> Array.toList
        |> List.collect (fun s ->
            match s with
            | ParseRegex2 @"-------+([a-zA-Z0-9]+):([0-9]+)-----+.*" (name, LITERALNUMB(n, "")) ->
                [ name, n ]
            | _ -> []
        ))

let writeFileViaNode (path : string) (str : string) =
    try
        let errorHandler _err = // TODO: figure out how to handle errors which can occur
            ()
        if not (fs.existsSync (U2.Case1 writeDirPath)) then fs.mkdirSync writeDirPath
        fs.writeFileSync (path, str) |> ignore
        Ok()
    with
        | _ -> Error <| sprintf "File %s can't be written" path

let loadStateFile (fName : string) =
    let lines =
        readFileViaNode fName
        |> fun s -> s.Split('\n')
        |> Array.map (fun s -> s.Trim())
        |> Array.map (fun s -> s.Split([| ' '; '\t' |]) |> Array.filter ((<>) ""))
        |> Array.filter (Array.isEmpty >> not)
        |> Array.map Array.toList
        |> Array.toList

    let toDP rLst n z c v =
        let fl f = match f with
                    | "0" -> false
                    | "1" -> true
                    | _ -> failwithf "Parse error expecting '1' or '0' as flag value"
        {
            TRegs = rLst |> List.map (uint32);
            TFlags = { FN = fl n; FC = fl c; FZ = fl z; FV = fl v }
        }

    let (|GetASM|_|) lines =
        //printfn "GETASM:%A\n\n" lines
        let n = List.findIndex ((=) [ "..." ]) lines
        let toStr lst = String.concat " " lst
        (String.concat "\r\n" (lines.[0..n - 1] |> List.map toStr), lines.[n + 1..]) |> Some

    let (|Test1|_|) lines =
        //printfn "Test1\n%A\n------------\n" lines
        let (|DP|_|) lines =
            //printfn "DP: %A\n\n" lines
            match lines with
            | ("Regs" :: rLst) :: [ "NZCV"; n; z; c; v ] :: rst -> (toDP rLst n z c v |> Some, rst) |> Some
            | [ "ERROR" ] :: rst -> (fNone, rst) |> Some
            | _ -> failwithf "Parse error reading DP"
        match lines with
        | [ name ] :: DP(Some before, ([ "..." ] :: GetASM(asm, (DP(after, rst))))) ->
            Some(
                {
                    Before = before
                    After = after
                    Asm = asm
                    Name = name
                }, rst
            )
        | _ -> failwithf "Parse error reading file (1)"
    let rec testN tsts = function
        | [] -> tsts
        | Test1(tst, rst) -> testN (tst :: tsts) rst
        | _ -> failwithf "Parse error reading file (2)"


    testN [] lines

let handleTestRunError e (pInfo : RunInfo) (ts : TestSetup) =
    let matchMess actual model =
        List.zip actual model
        |> List.indexed
        |> List.filter (fun (_, (a, m)) -> a <> m)
        |> List.map (fun (r, (a, m)) -> sprintf "Bad output:R%d is 0x%x should be 0x%x" r (uint64 a) (uint64 m))
        |> String.concat "\n"

    let dp, uf = pInfo.dpCurrent

    let regs =
        dp.Regs
        |> Map.toList
        |> List.sortBy (fun (r, u) -> r.RegNum)
        |> List.map (fun (r, u) -> u)
        |> List.take 15

    let flags =
        match dp.Fl with
        | { N = n; C = c; V = v; Z = z } -> { FN = n; FC = c; FV = v; FZ = z }
    match e with
    | TBEXIT
    | EXIT ->
        match ts.After with
        | Core.Option.None -> BetterThanModelTests, ts, pInfo, "Visual2 runs when VisuAL gives an error?"
        | Some { TRegs = tr; TFlags = fl } when tr = regs && fl = flags ->
            OkTests, ts, pInfo, "Ok"
        | Some { TRegs = tr; TFlags = fl } when tr <> regs ->
            ErrorTests, ts, pInfo, matchMess regs tr
        | Some { TRegs = tr; TFlags = fl } when fl <> flags ->
            ErrorTests, ts, pInfo, sprintf "Flags:\n%A\ndo not match model flags\n%A" flags fl
        | _ -> failwithf "What? Can't happen!"

    | NotInstrMem x ->
        match ts.After with
        | Core.Option.None -> OkTests, ts, pInfo, "Both VisUAL and VisUAL2 give errors"
        | Some _ -> ErrorTests, ts, pInfo, "Error: trying to execute instruction memory"

    | ``Run time error`` (pos, msg) ->
        match ts.After with
        | Core.Option.None -> OkTests, ts, pInfo, "Both VisUAL and VisUAL2 give errors"
        | Some _ -> ErrorTests, ts, pInfo, sprintf "Error on line %d: %s" pos msg

    | ``Unknown symbol runtime error`` undefs ->
        ErrorTests, ts, pInfo, "Unknown symbol runtime error: should never happen!"

let writeResultsToFile fn rt resL =

    let nameOfCode =
        function | OkTests -> "OKs"
                 | ErrorTests -> "ERRORs"
                 | _ -> "BETTERs"

    let fName = Refs.appDirName + @"/test-results/" + (nameOfCode rt + fn)

    let displayState (ts : TestSetup) (outDp : DataPath) =

        let getFlags (a : Flags) = [ a.FN; a.FZ; a.FC; a.FV ]
        let getFlagsActual (a : CommonData.Flags) = [ a.N; a.Z; a.C; a.V ]

        let dispFlags (before : DPath) (after : DataPath) (model : DPath) =
            let dispFlag bf af mf n =
                let bToInt = function | true -> "1" | false -> "0"
                let bfv = bToInt (getFlags bf).[n]
                let afv = bToInt (getFlagsActual af).[n]
                let mfv = bToInt (getFlags mf).[n]
                sprintf "%-11c%11s%11s%11s" "NZCV".[n] bfv afv (if afv <> mfv then mfv else "")
            "Flag          Before        After        Model\n" +
            ([ 0..3 ]
            |> List.map (dispFlag before.TFlags after.Fl model.TFlags)
            |> String.concat "\n")

        let dispReg b model after n =
            let model =
                match model.TRegs.[n] = after.Regs.[register n] with
                | true -> ""
                | false -> sprintf "%d" model.TRegs.[n]
            sprintf "R%-8d%11d%11d%11s" n ts.Before.TRegs.[n] after.Regs.[register n] model

        let dispRegs b after model =
            "Register  Input      Actual Out    Model Out\n" +
            ([ 0..14 ]
            |> List.map (dispReg b model after)
            |> String.concat "\n")

        match ts.After with
        | Some a ->
            dispRegs ts.Before outDp a + "\n" +
            dispFlags ts.Before outDp a + "\n"
        | _ -> "Error in model\n"

    let displayTest (tt : TestT, ts : TestSetup, ri : RunInfo, mess : string) =
        sprintf "\n--------------%s----------------\n" ts.Name +
        mess + "\n\r\n" +
        displayState ts (fst ri.dpCurrent) + "\n" +
        "       ---------ASM----------\n" +
        ts.Asm +
        "\n----------------------------------\n\n"
    //printfn "Writing result file\n%s." fName
    //printfn "Resl =%A" resL
    match resL with
    | [] ->
        if fs.existsSync (U2.Case1 fName) then
            try
                fs.unlinkSync (U2.Case1 fName)
                Ok()
            with
                | _ -> Error <| sprintf "Can't delete read-only file %s" fName
        else
            Ok()
    | _ ->
        resL
        |> List.map displayTest
        |> String.concat "\n"
        |> (fun txt ->
                writeFileViaNode fName txt)

let processTestResults (fn : string) (res : Map<TestT, (TestT * TestSetup * RunInfo * string) list>) allowed =
    let getNum rt =
        let resL = Map.findWithDefault rt res []
        writeResultsToFile fn rt resL |> ignore
        resL.Length

    let badErrors =
        (Map.findWithDefault ErrorTests res [])
        |> List.map (fun (_, ts, _, _) ->
            match ts.Name.Split [| ':' |] with
            | [| name; num |] -> fnWithoutSuffix name, (uint32 (Int32.Parse num))
            | _ -> failwithf "Bad name: expected name:number. Number (%A) parse failed" ts.Name)
        |> Set.ofList
        |> Set.filter (fun x -> not (List.contains x allowed))
    if badErrors.Count <> 0 then
        Result.Error <| sprintf "Ok: %d ; Better: %d ; Errors: %d"
                  (getNum OkTests) (getNum BetterThanModelTests) (getNum ErrorTests)
    else Result.Ok "Passed"

/// on small test files print more info
let RunEmulatorTest allowed ts =
    let maxSteps = 1000L

    //let more = size < 4
    let more = false // disable printout

    let asm =
        ts.Asm.Split([| '\r'; '\n' |])
        |> Array.filter (fun s -> s <> "")
        |> Array.toList

    let lim = reLoadProgram asm

    if more then printfn "\n\nIndented ASM:\n%s\n" (lim.EditorText |> String.concat "\n")

    let ri = lim |> getRunInfoFromImage NoBreak

    if lim.Errors <> [] then
        match ts.After with
        | Some _ -> ErrorTests, ts, ri, "Visual2 cannot parse test assembler"
        | Core.None -> OkTests, ts, ri, "Visual2 cannot parse assembler: however this test returns an error in the model"
    else
        let dpBefore =
            { ri.dpInit with
                Regs =
                    ts.Before.TRegs
                    |> List.indexed
                    |> fun lis -> (15, 0u) :: lis
                    |> List.map (fun (n, u) -> inverseRegNums.[n], u)
                    |> Map.ofList
                Fl =
                    match ts.Before.TFlags with
                    | { FC = c; FV = v; FZ = z; FN = n } -> { C = c; V = v; N = n; Z = z }
            }

        let ri' = asmStep maxSteps { ri with dpInit = dpBefore }

        match ri' with
        | { State = PSExit } -> handleTestRunError EXIT ri' ts
        | { State = PSError e } -> handleTestRunError e ri' ts
        | _ ->
            ErrorTests, ts, ri', sprintf "Test code timed out after %d Visual2 instructions" maxSteps

let runEmulatorTestFile allowed fn =
    let testF = Refs.appDirName + @"/test-data/" + fn
    let results = loadStateFile testF
    let resultsBySuccess =
        results
        |> List.map (RunEmulatorTest allowed)
        |> List.groupBy (fun (rt, _, _, _) -> rt)
        |> Map.ofList
    let resultSummary = processTestResults fn resultsBySuccess allowed

    match resultSummary with
    | Result.Ok s
    | Result.Error s -> s
    |> printfn "%s with %d tests...%s" (fnWithoutSuffix fn) results.Length

let runAllEmulatorTests() =
    let allowed = readAllowedTests()
    printfn "Errors allowed in tests: %A" allowed
    let contents = electron.remote.getCurrentWebContents()
    if not (contents.isDevToolsOpened()) then contents.toggleDevTools()
    let files =
        try
            fs.readdirSync (U2.Case1(Refs.appDirName + "/test-data"))
        with
            | _ -> ResizeArray()
        |> Seq.toList
        |> (fun lis ->
                if List.contains "focus.txt" lis
                then [ "focus.txt" ]
                else lis)
        |> List.filter (String.startsWith "ALLOWED" >> not)

    List.iter (runEmulatorTestFile allowed) files
    printfn "Finished. See './test-results' for result files"

//*************************************************************************************
//                              CUSTOM TESTBENCH CODE
//*************************************************************************************

