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

open Fable.PowerPack
open Fable.PowerPack.Fetch

open System

let logFileName = "Visual2eventLog.txt"

let time() = System.DateTime.Now.Ticks
let mutable activity: bool = true
let mutable sleeping: bool = false


type VisualEvent =
    | Startup
    | RunningCode
    


let dirOfSettings() = 
    let settingsF = settings?file()
    printfn "SettingsF=%s" settingsF
    let m = String.regexMatchGroups @"(.*[\\\/])([^\\\/]*$)" settingsF
    match m with
    | Some [ dir ; _ ; _] -> dir
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
    

//document.addEventListener("mousemove", U2.Case2 activityStats)
//document.addEventListener("keypress", U2.Case2 activityStats)


let (|MatchDate|_|) txt =
   let parse = System.DateTime.TryParse
   match String.regexMatchGroups "([^\-]+)-([^\-]+)" txt with
   | Some [d1 ; d2; _] ->
//        if debugLevel > 1 then printfn "Parsed %A, %A" d1 d2
        match parse d1, parse d2 with
        | (true, dp1), (true, dp2)  -> 
            //printf "Parsedgood: %A %A" dp1 dp2
            Some (dp1.Date, dp1.TimeOfDay, dp2.TimeOfDay)
        | s -> None
   | Some s -> None
   | None -> None


let infoBox (mess:string) =
    showVexAlert ("<h4>Information</h4> <br> <p>" + mess + "</p>")
    ()
    

let (|MatchVersion|_|) txt =
       let parseVer txt = String.regexMatchGroups @"([0-9]+)\.([0-9]+)\.([0-9]+)" txt
       let appVer = parseVer Refs.appVersion
       let (|ToI|_|) txt = try Some (int txt) with | _exc -> (printfn "Can't convert %s to integer" txt ; None)
       match  appVer, parseVer txt with
       | None,_ -> failwithf "What? Can't parse app version= %s!" Refs.appVersion
       | Some [ToI aMajor; ToI aMinor; ToI aDebug;_], Some [ToI major ; ToI minor; ToI debug;_] ->
            printfn "Web latest version = %d.%d.%d" major minor debug
            match major > aMajor, minor > aMinor, debug > aDebug with
            | true, _, _ -> (infoBox <| sprintf "There is a new major release of Visual2 (%s) with additional features, you may wish to upgrade." txt) |> Some
            | _, true, _ when major = aMajor -> 
                (infoBox <| sprintf "There is a new minor release of Visual2 (%s) with new features, you should upgrade." txt) |> Some
            | _, false, true when major = aMajor && minor = aMinor -> 
                (infoBox <| sprintf "There is a new release of Visual2 (%s) with bug fixes, you may wish to upgrade." txt) |> Some
            | _ -> None
       | _, None -> None
       | _, x -> None


let remindNewVersion txt =
    printfn "Remind new version if needed!"
    txt 
    |> String.splitRemoveEmptyEntries [|'\n';'\r'|]
    |> Array.iter (
        function
            | MatchVersion _ -> ()
            | _ -> ()
    )

let mutable lastRemindTime: System.TimeSpan option = None

let remindInExams txt =
    printfn "Remind in exams every 5 min!"
    let remind() = 
        lastRemindTime <- Some DateTime.Now.TimeOfDay
        infoBox ("WARNING: an assessed Test is scheduled now." +
                "If you are currently doing this you are not allowed to use Visual2. " +
                "Please exit Visual2 immediately")
    txt 
    |> String.splitRemoveEmptyEntries [|'\n';'\r'|]
    |> Array.iter (
        function
            | MatchDate (date, t1, t2) -> 
                let tim = DateTime.Now
                let inExam = tim.TimeOfDay > t1 && tim.TimeOfDay < t2 && tim.Date = date.Date
                match inExam,lastRemindTime with
                | true, None -> remind()
                | true, Some tt when tt.Add (TimeSpan.FromMinutes 5.) < tim.TimeOfDay -> remind()                            
                | _ -> ()
            | _ -> ()
        )


let doIfHoursLaterThan hours (tim:System.DateTime) func =
    if System.DateTime.Now > tim.Add (System.TimeSpan.FromHours hours)
    then func()


let readOnlineInfo (ve: VisualEvent) =
    let checkActions txt =
        match ve with
            | Startup -> 
                remindNewVersion txt
                remindInExams txt
            | RunningCode -> 
                remindInExams txt

    let doFetch() =
        //new IHttpRequestHeaders
        //hdrs?append("pragma","no-cache") |> ignore
        //hdrs?append("cache-control","no-cache") |> ignore
        fetch ("http://intranet.ee.ic.ac.uk/t.clarke/tom/info.txt?" + DateTime.Now.Ticks.ToString())  [ 
            Cache RequestCache.Nostore
            ]
        |> Promise.map (
            fun res ->
                 if res.Ok 
                 then 
                     Ok res 
                 else 
                     if debugLevel > 0 then printfn "can't read internet data"
                     Refs.lastOnlineFetchTime <- Error System.DateTime.Now
                     checkActions vSettings.OnlineFetchText
                     Error "can't read internet data"
            )
        |> Promise.bindResult (fun res -> res.text())
        |> Promise.mapResult (fun txt -> 
            //if debugLevel > 0 then printf "----%s----%s----" txt vSettings.OnlineFetchText
            Refs.lastOnlineFetchTime <- Ok System.DateTime.Now
            checkActions txt
            if vSettings.OnlineFetchText <> txt
            then
                vSettings <- {vSettings with OnlineFetchText = txt}
                Refs.setJSONSettings()
                printfn "-----updating online text to-----\n%s\n------------------------" txt
        )
        |> ignore
        
    match Refs.lastOnlineFetchTime with
    | Error tim  -> doIfHoursLaterThan (if ve = Startup then 0. else 0.1) tim doFetch
    | Ok tim -> doIfHoursLaterThan (if debugLevel > 0 then 0.001 else 24.) tim doFetch
           
  





