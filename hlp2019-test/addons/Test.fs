module Test
    open Expecto
    open Helpers
    open VisualTest.VData
    open VisualTest.VCommon
    open VisualTest.Visual
    open VisualTest.VTest
    open CommonData

    // from given files
    let memReadBase = 0x1000u
    let expectoConfig = { Expecto.Tests.defaultConfig with 
                            parallel = testParas.Parallel
                            parallelWorkers = 6 // try increasing this if CPU use is less than 100%
                    }
    let fsConfig = {FsCheckConfig.defaultConfig with replay = Some (0,0); maxTest = 100}

    let visualTestProperty name test = testPropertyWithConfig fsConfig name test
    let runVisualTests () = 
        initCaches testParas
        let rc = runTestsInAssembly expectoConfig [||]
        finaliseCaches testParas
        rc // return an integer exit code - 0 if all tests pass    

    let visualToReg = function
        | R reg -> makeReg (makeRegFn reg)
        
    let visualToRegs vRegs = 
        List.map (fun (rOut, rInt) -> (visualToReg rOut, rInt |> uint32)) vRegs
        |> Map.ofList
    
    // make an address list from base address of data 0x100 
    // up to 0x200 by 0x4 each time
    let visualToMem vMem = 
        let alst = 
            [memReadBase..word..memReadBase + 0x30u] // need this to stop list complaints
            |> List.map WA
            |> List.rev
        List.zip alst (List.map DataLoc vMem)
        |> Map.ofList

    let returnData _ d =
        match d with
        | DataLoc dl -> dl
        | _ -> 0u

    let visualToDataPath visual =   
        let flags = {
                        N = visual.State.VFlags.FN; 
                        C = visual.State.VFlags.FC;
                        Z = visual.State.VFlags.FZ;
                        V = visual.State.VFlags.FV;
                    }
        let regs = visualToRegs visual.Regs
        let mem = visualToMem visual.State.VMemData
        {Fl = flags; Regs = regs; MM = mem}
    let returnVisualCpuData param src = 
        RunVisualWithFlagsOut param src 
        |> snd |> visualToDataPath

    let valList = 0u :: [2u..13u];
    let addrList = [4096u..4u..4144u];

    let storer = 
        List.zip valList addrList
        |> List.map (fun (a, b) -> STORELOC a b)
        |> List.fold (+) "\n"
    
    let resetR2R0 = "MOV R2, #0x1000\nMOV R0, #0x1000\n"
    let returnMemVisualCpuData paras src =
        let main, post = VisualTest.VData.GETWRAPPER paras.InitRegs paras.InitFlags paras.MemReadBase
        let res = RunVisual {paras with Prelude=main + storer + resetR2R0; Postlude=post} src
        let output = 
            match res with
            | Error e -> failwithf "Error reading Visual Log %A" e
            | Ok ({ Regs=_; State={VFlags=fl}} as vso) -> fl, vso
        snd output |> visualToDataPath

    let returnCpuDataMem (cpuData: DataPath<CommonTop.Instr>) = 
        Map.map returnData cpuData.MM

    let returnCpuDataRegs (cpuData: DataPath<CommonTop.Instr>) =
        cpuData.Regs
    
    let returnCpuDataFlags (cpuData: DataPath<CommonTop.Instr>) =
        cpuData.Fl