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
open Refs

let logFileName = "Visual2eventLog.txt"

let time() = System.DateTime.Now.Ticks
let mutable activity: bool = true
let mutable sleeping: bool = false

 

let dirOfSettings() = 
    let settingsF = settings?file()
    printfn "SettingsF=%s" settingsF
    let m = String.regexMatchGroups @"(.*[\\\/])([^\\\/]*$)" settingsF
    match m with
    | [ dir ; _ ; _] -> dir
    | _ -> failwithf "Error finding directory of string: '%s'" settingsF



let appendLog item = 
    let logHeader = sprintf "%s %s %s\n" Refs.appVersion (os.homedir()) (os.hostname())
    let logName = dirOfSettings() + "//" + logFileName
    match fs.existsSync (U2.Case1 logName) with
    | true -> fs.writeFileSync (logHeader + logName, item)        
    | false ->  fs.appendFile(logName, item, fun _e -> ())

type ErrorT = (int * string)

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


let activityStats = { 
    new EventListenerObject with
    member x.handleEvent _event =
        if sleeping then
            logMessage {LogT=Wake;Time=time()}
            sleeping <- false
        activity <- true
    }

    
    

let pushLogFile() =
    ()

let checkActivity() =
    if not activity then
        if not sleeping then
            logMessage {LogT=Sleep;Time=time()}
            sleeping <- true
    else 
        logMessage {LogT=Wake;Time=time()}
    activity <- false
    pushLogFile()
    

document.addEventListener("mousemove", U2.Case2 activityStats)
document.addEventListener("keypress", U2.Case2 activityStats)




