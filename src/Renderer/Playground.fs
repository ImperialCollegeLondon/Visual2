(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Playground
    Description: File for self-contained test code and checking FABLE compiler bugs
*)

module Playground

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

type Test = { TNum:int; Ins:TbSpec list; Outs:TbSpec list}


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
    let testLines, testErrors =
        lines
        |> List.indexed
        |> List.map (fun (i,lin) -> parseTbLine i lin)
        |> splitResult
    let linkedLines = linkSpecs dataStart testLines 
    if testErrors = [] then
        { 
            TNum = testNum;
            Ins = List.collect (function | (TbIn,x) -> [x] | _ -> []) linkedLines
            Outs = List.collect (function | (TbOut,x) -> [x] | _ -> []) linkedLines
        } |> Ok
    else
        testErrors |> Error


let rec parseTests dStart lines =
    let (|ISHEAD|_|) lin =
        lin
        |> String.splitOnWhitespace
        |> function | [| "TEST" ; LITERALNUMB(n,"") |] -> Some n | _ -> None
    let (|GETINDENT|_|) lines =
        let hasIndent (lin:string) = (lin = "") ||  (System.Char.IsWhiteSpace lin.[0])
        match List.takeWhile hasIndent lines |> List.filter ((<>) "") with
        | [] -> None
        | lins -> (lins, List.skipWhile hasIndent lines) |> Some
    match lines with
    | [] -> []
    | ISHEAD n :: GETINDENT(lines,txt) -> parseOneTest dStart (int n) lines :: parseTests dStart txt
    | _ -> [Error []]

let initDP specs rMap =
    let initFlags = {C=false;V=false;N=false;Z=false}
    let initRegs = initialRegMap
    let addSpec dp spec =
        match spec with
        | TbRegEquals(_, rn,u) -> {dp with Regs = Map.add rn u dp.Regs}
        | TbRegPointsTo(_, rn, start, uLst) ->
            let mm' = ExecutionTop.addWordDataListToMem start dp.MM (uLst |> List.map Dat)
            let dp' = Map.add rn start dp.Regs
            {dp with Regs=dp'; MM=mm'}
    List.fold addSpec rMap specs
 
let checkDP specs (dp:DataPath) =
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
    specs |> List.map checkSpec

let getTestbench dStart =
    let getTB id =
        let tests = getCode id
        if String.startsWith "##TESTBENCH" tests
        then
            tests
            |> String.toUpper
            |> String.splitString [|"\n"|]
            |> Array.toList
            |> parseTests dStart
            |> fun x -> [x]
        else    
           []
    Refs.fileTabList 
    |> List.collect (fun tabId -> (getTB tabId |> List.map (fun tb -> tabId,tb)))

let runEditorTab steps (test:Test) =
        Integration.prepareModeForExecution()
        match runMode with
        | ResetMode
        | ParseErrorMode _ ->
            let tId = Refs.currentFileTabId
            Editors.removeEditorDecorations tId
            match Integration.tryParseAndIndentCode tId with
            | Some (lim, _) -> 
                let dp = initDP test.Ins { Fl = {C=false;V=false;N=false;Z=false}; Regs=initialRegMap; MM= lim.Mem}
                Editors.disableEditors()
                let ri = 
                    lim 
                    |> fun lim -> Integration.getRunInfoFromImageWithInits lim dp.Regs dp.Fl Map.empty dp.MM
                Integration.setCurrentModeActiveFromInfo RunState.Running ri
                Integration.asmStepDisplay steps ri
            | _ -> ()
        | ActiveMode (RunState.Paused,ri) -> Integration.asmStepDisplay  (steps + ri.StepsDone) ri
        | ActiveMode _
        | RunErrorMode _ 
        | FinishedMode _ -> ()




let runTestbench() =
    match getTestbench 0x80000000u with
    | (tb1 :: tb2 ::_)-> showAlert "Too many testbenches" "There is more than one testbench currently loaded!"
    | [] -> showAlert "Can't find testbench" "Please load a testbench (file starting ##TESTBENCH) into an editor tab"
    | [tabId, tbL] when List.exists (function | Error _ -> true | Ok _ -> false) tbL -> 
        showAlert "Can't run testbench" "Errors found in testbench specification"
        Tabs.selectFileTab tabId
    | [tabId, tbL] when  Refs.currentFileTabId = tabId ->
        showAlert "Can't run testbench" "Please select the program tab which you want to test - not the testbench"
    | [tabId, tbL] -> ()
    

let check1() =
    ()



