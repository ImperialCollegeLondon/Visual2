(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Playground
    Description: File for self-contained test code and checking FABLE compiler bugs
*)

module Testbench

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open DP
open Refs
open EEExtensions
open Helpers
open CommonData
open ExecutionTop

let getTBWithTab() =
    Refs.fileTabList
    |> List.map (fun tab -> tab, getCode tab)
    |> List.filter (snd >> String.trim >> String.startsWith "##TESTBENCH")
    |> function | [tab, tb] -> Ok (tab, tb)
                | [] -> Error "No testbench is loaded"
                | _ -> Error "More than one testbench is loaded"

let getTB() = 
    getTBWithTab()
    |> Result.map snd

let parseTbLine lNum (lin:string) =
    let (|RESOLVE|_|) lst =
        let ops = 
            String.concat " " lst
            |> String.splitString [|","|] 
            |> Array.map String.trim
            |> Array.toList
        let parseLiteral  = function | LITERALNUMB (lit,"") -> [lit] | _ -> []
        let parseL = List.map parseLiteral ops
        if List.exists ((=) []) parseL then None
        else
            List.concat parseL |> Some
    let (|Defs|_|) words =
        match words with
        | [RegMatch (Ok rn) ; "IS" ; LITERALNUMB (lit,"")] -> TbRegEquals(lNum, rn,lit) |> Some
        | RegMatch (Ok rn) :: "PTR" :: RESOLVE lits -> TbRegPointsTo(lNum, rn, 0u, lits) |> Some
        | _ -> None
    match lin.ToUpper() |> String.splitOnWhitespace |> Array.toList  with
    | "IN" :: Defs tbSpec -> Ok (TbIn, tbSpec)
    | "OUT" :: Defs tbSpec -> Ok (TbOut, tbSpec)
    | _ -> Error (lNum,"Parse Error in testbench")

let splitResult resL =
    List.fold (fun (rl,el) -> function | Error e -> rl, e :: el | Ok r -> r :: rl, el) ([],[]) resL

let decorateParseLine (i, pl) = ()

let setupTest tl = ()

let linkSpecs start specs =
    let addSpec (start,linkedSpecs) (inOut,spec) =
        match spec with
        | TbRegEquals(_lNum, rn,u) -> start, (inOut,spec) :: linkedSpecs
        | TbRegPointsTo(lNum, rn, _start, uLst) ->
            let n = start + uint32(uLst.Length+4)
            n, (inOut, TbRegPointsTo(lNum, rn, n, uLst)) :: linkedSpecs
    List.fold addSpec (start,[]) specs
    |> snd

let parseOneTest dataStart testNum lines =

    let checkRes, tbLines =
        lines
        |> List.indexed
        |> List.partition (snd >> String.startsWith ">> ")
    
    let testLines,testErrors =
        tbLines
        |> List.map (fun (i,lin) -> parseTbLine i lin)
        |> splitResult

    let linkedLines = linkSpecs dataStart testLines 
    if testErrors = [] then
        { 
            TNum = testNum;
            Ins = List.collect (function | (TbIn,x) -> [x] | _ -> []) linkedLines
            Outs = List.collect (function | (TbOut,x) -> [x] | _ -> []) linkedLines
            CheckLines = (checkRes |> List.map snd)
        } |> Ok
    else
        testErrors |> Error

let parseTests dStart lines =
    lines
    |> List.map String.trim
    |> List.chunkAt (fun s -> (printfn "Pred"; let b = s.StartsWith "#TEST"; in  printfn "pred=%A" b; b))
    |> List.filter (fun x -> x <> [])
    |> List.map (fun c -> printfn "el=%A" c ; c)
    |> List.map (fun chunk -> 
                    printfn "parsing chunk %A\n\n" chunk
                    if chunk = [] then failwithf "What? chunk of testbench can never be []" else failwithf "Chunk=%A" chunk
                    List.head chunk
                    |> String.splitOnWhitespace 
                    |> Array.toList
                    |> (function | "#TEST" :: LITERALNUMB (n,"") :: _ -> parseOneTest dStart (int n) (List.tail chunk)
                                 | x -> Error [1,sprintf "Can't parse test header '%A'" (List.truncate 2 x)]))

let writeTest (test:Test) =
        getTBWithTab()
        |> Result.map ( fun (tabId, dat) ->
            dat
            |> String.splitString [|"\n"|]
            |> Array.toList
            |> List.chunkAt (String.trim >> String.startsWith "#TEST")
            |> List.collect (fun chunk -> 
                        let testLst = String.splitOnWhitespace (List.head chunk) |> Array.toList
                        let testData = List.filter (String.trim >> String.startsWith ">>" >> not) chunk
                        match testLst with
                        | "#TEST" :: LITERALNUMB (n,"") :: _ when int n = test.TNum -> testData @ test.CheckLines
                        | _ -> chunk) // no change
            |> String.concat "\n"
            |> fun r -> tabId, r)
        |> function | Ok (tabId, txt) -> 
                        let editor = editors.[tabId]
                        editor?setValue txt
                    | Error _-> showAlert "Error" "What? can't find testbench to write results!"

let initDP specs rMap =
    let addSpec dp spec =
        match spec with
        | TbRegEquals(_, rn,u) -> {dp with Regs = Map.add rn u dp.Regs}
        | TbRegPointsTo(_, rn, start, uLst) ->
            let mm' = ExecutionTop.addWordDataListToMem start dp.MM (uLst |> List.map Dat)
            let dp' = Map.add rn start dp.Regs
            {dp with Regs=dp'; MM=mm'}
    List.fold addSpec rMap specs
 
let checkTestResults (test:Test) (dp:DataPath) =
    let specs = test.Outs
    let checkSpec spec =
        let checkOneLoc (ma,u) =
            match dp.MM.[WA ma] with
            | Dat u' when u = u' -> []
            | Dat u' -> [u, TbMem (ma, Some u'), spec]
            | _ -> [u, TbMem (ma, None), spec]
        match spec with
        | TbRegEquals(lNum, rn, u) when dp.Regs.[rn] = u -> []
        | TbRegEquals(lNum, rn, u) -> [u, TbVal dp.Regs.[rn], spec]
        | TbRegPointsTo(_, rn, start, uLst) ->
            uLst
            |> List.indexed
            |> List.map (fun (n,u) ->  (start + uint32(4*n), u))
            |> List.collect checkOneLoc
    specs |> List.collect checkSpec
/// Add one Test of result messages to the testbench buffer.
/// test: test to add (one of those in the testbench).
/// dp: DataPath after test simulation ends.
/// Returns true if test has passed
let addResultsToTestbench (test:Test) (dp:DataPath) =
    let displayError (u: uint32, check: tbCheck, spec:TbSpec) =
        match check, spec with
        | TbVal act, TbRegEquals(n, reg, v) -> 
            sprintf ">> %A: Actual: %d, expected: %d\n" reg act v
        | TbMem(adr,act), TbRegPointsTo(n, ptr, start, uLst) -> 
            let actTxt = match act with None -> "None" | Some a -> sprintf "%d" a
            let offset = int (adr - start)
            sprintf ">> [%A,#%d] -> Actual: %s. expected: %d\n" ptr offset actTxt uLst.[offset/4] 
        | _ -> failwithf "What?: inconsistent specs and check results"
    let errorLines = 
        checkTestResults test dp
        |> List.map displayError
    let resultLines =
        errorLines
        |> function | [] -> [sprintf ">> Test %d PASSED.\n" test.TNum]
                    | errMess -> sprintf ">> Test %d FAILED.\n" test.TNum :: errMess
    writeTest {test with CheckLines = resultLines}
    errorLines = []
    
let processParseErrors (eLst: Result<Test,(int*string) list>list) =
    let highlightErrors tab = List.iter (fun (lNum, mess) -> Editors.highlightLine tab lNum "editor-line-error")  
    match getTBWithTab() with
    | Error mess -> Error mess
    | Ok (tab,_) ->
        List.iter (Result.mapError (highlightErrors tab) >> ignore) eLst
        match List.errorList eLst with
        | [] -> List.okList eLst |> Ok
        | _ -> 
            Tabs.selectFileTab tab
            Error "Parse errors in testbench"

let getParsedTests dStart =
    getTBWithTab()
    |> Result.bind (
            fun (tab, tb) -> 
                String.toUpper tb
                |> String.splitString [|"\n"|]
                |> Array.toList
                |> parseTests dStart
                |> processParseErrors
                |> Result.map (fun x -> tab,x))



