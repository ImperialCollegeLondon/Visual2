(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Testbench
    Description: File for GUI interface and higher-level automation in testing of assembly programs with
    special testbench files containing sets of tests.
*)

module Testbench

open Fable.Core.JsInterop
open Refs
open EEExtensions
open Helpers
open CommonData
open ExecutionTop
open TestLib

let getTBWithTab() =
    Refs.fileTabList
    |> List.map (fun tab -> tab, getCode tab)
    |> List.filter (snd >> String.trim >> String.startsWith "##TESTBENCH")
    |> function | [ tab, tb ] -> Ok(tab, tb)
                | [] -> Error "No testbench is loaded"
                | _ -> Error "More than one testbench is loaded"

let getTB() =
    getTBWithTab()
    |> Result.map snd

let currentTabIsTB() =
    match Refs.currentFileTabId with
    | -1 -> false
    | tab -> getCode tab
             |> String.trim |> String.startsWith "##TESTBENCH"

/// Write test Checklines to the buffer containing the testbench file
let writeTest (test : Test) =
        getTBWithTab()
        |> Result.map (fun (tabId, dat) ->
            dat
            |> String.splitString [| "\n" |]
            |> Array.toList
            |> List.map String.trim
            |> List.chunkAt (String.trim >> String.startsWith "#TEST")
            |> List.collect (fun chunk ->
                        let testLst = String.splitOnWhitespace (List.head chunk) |> Array.toList
                        let testData = List.filter (String.trim >> String.startsWith ">>" >> not) (chunk |> List.tail)
                        match testLst with
                        | "#TEST" :: LITERALNUMB(n, "") :: _ when int n = test.TNum ->
                            (sprintf "#TEST %d" n) :: testData @ test.CheckLines
                        | _ -> chunk) // no change
            |> List.filter ((<>) "")
            |> String.concat "\n"
            |> fun r -> tabId, r)
        |> function | Ok(tabId, txt) ->
                        let editor = editors.[tabId]
                        editor?setValue txt
                    | Error _ -> showAlert "Error" "What? can't find testbench to write results!"

/// Generate one Test of result messages and add them to the testbench buffer.
/// If no errors mark the Test as Passed.
/// test: test to add (one of those in the testbench).
/// dp: DataPath after test simulation ends.
/// Returns true if test has passed.
let addResultsToTestbench (test : Test) (dp : DataPath) =
    let goodParse, resultLines = computeTestResults test dp
    writeTest { test with CheckLines = resultLines }
    goodParse

/// Top-level testbench parse. Locate loaded testbench, generate pair of testbench tab ID
/// and Test list, or Error message. If testbench lines contain errors these are highlighted in buffer.
/// Previous error highlights are removed from buffer.
let getParsedTests dStart =

    let processParseErrors (eLst : Result<Test, (int * string) list> list) =
        let highlightErrors tab =
            List.iter (fun (lNum, mess) ->
                printfn "Testbench error %d %s." lNum mess
                Editors.highlightLine tab lNum "editor-line-error")
        match getTBWithTab() with
        | Error mess -> Error mess
        | Ok(tab, _) ->
            Editors.removeEditorDecorations tab
            List.iter (Result.mapError (highlightErrors tab) >> ignore) eLst
            match List.errorList eLst with
            | [] -> List.okList eLst |> Ok
            | x ->
                Tabs.selectFileTab tab
                printfn "%A" x
                Error "Parse errors in testbench"

    let initStack = 0xFF000000u
    getTBWithTab()
    |> Result.bind (
            fun (tab, tb) ->
                String.toUpper tb
                |> String.splitString [| "\n" |]
                |> Array.toList
                |> parseTests initStack dStart
                |> processParseErrors
                |> Result.map (fun x -> tab, x))

let getTestList() =
    getParsedTests 0x10000000u
    |> function
        | Error e -> showVexAlert e; []
        | Ok(_, tests) ->
            tests
