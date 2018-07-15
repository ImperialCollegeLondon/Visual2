(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Stats
    Description: Collect locally and post to the web usage statistics. Also allow some control over updates etc.
*)

module Stats

open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Core
open EEExtensions
open Node.Exports

let time() = System.DateTime.Now.Ticks
let mutable activity: bool = true
let mutable sleeping: bool = false
let logName = os.homedir() + "/.visualLog"

let appendLog = fs.appendFile(logName, fun e -> ())

type ErrorT = string

type LogT = 
    | Wake 
    | Sleep 
    | Step 
    | ParseWithErrors of ErrorT list
    | RunOK 
    | Reset 

type LogMessage = {
        LogT: LogT
        Time: int64
    }

let logMessage (mess:LogMessage): Unit = failwithf "Not yet implemented"

let activityStats e = 
    if sleeping then
        logMessage {LogT=Wake;Time=time()}
        sleeping <- false
    activity <- true



let checkActivity() =
    if not activity then
        if not sleeping then
            logMessage {LogT=Sleep;Time=time()}
            sleeping <- true
    else 
        logMessage {LogT=Wake;Time=time()}
    activity <- false
    

document.addEventListener("mousemove", U2.Case1 activityStats)
document.addEventListener("keypress", U2.Case1 activityStats)




