// Learn more about F# at http://fsharp.org

open System
open ParseHelpers

open Expecto
open FsCheck

module Gen =
    // custom random generators using FsCheck.Gen combinators
    // See https://github.com/haf/expecto/blob/master/README.md#property-based-tests
    // See https://fscheck.github.io/FsCheck/TestData.html for Gen.* info
    let alphaGen = Gen.elements ([ 'a'..'z'] @ ['A'..'Z']) // choose random element of list
    let digitGen = Gen.elements(['0'..'9'])
    let alphaNumGen = Gen.oneof [alphaGen ; digitGen] // uniform randomly select a generator from list
    let regNameGen = Gen.map (sprintf "R%d") (Gen.oneof [Gen.choose(0,15)])
    let manyAlphaNumGen = 
        // NB the simpler a |> Seq.ofArray |> string here does not work. Bug?
        let arrToStr a = a |> Array.map Char.ToString |> String.concat "" // convert array of Char to string
        Gen.arrayOf alphaNumGen // randomly sized array of alphaNumGen (length scales with 'size' parameter)
        |> Gen.map arrToStr // map result of generator to make new generator
    let idGen = Gen.map2 (fun (a:char) b -> 
        a.ToString() + b) alphaGen manyAlphaNumGen // combine initial letter and variable number of alphanum
    let opGen = Gen.elements tokenOpArray // random choice from list
    let numGen = Gen.map (sprintf "%d") (Gen.choose(0,9999999)) // random choice from integer range
    let tokListGen = 
        Gen.oneof [idGen ; idGen ; numGen; numGen; opGen] // randomly choose token type
        |> Gen.listOf // generate random length lists of tokens (similarly to arrayOf)
    let optSpaceGen = Gen.elements ["";" "]

    // 'partial active pattern' used in tokStringArb
    // see https://fsharpforfunandprofit.com/posts/convenience-active-patterns/
    /// match only on a string which is NOT an operator, returning the string
    let (|MatchNotOp|_|) s =
        if Array.contains s tokenOpArray then None else Some s

    /// Custom generator of even integers
    /// Every custom generator needs a separate D.U. type
    type EvenInt = EvenInt of int
    let evenIntArb = 
        Gen.choose(1,100) // random choice from 1..100
        |> Gen.filter (fun i -> i % 2 = 0) // filter random numbers
        // boilerplate to make 'Arbitrary' wrapped by D.U. for use in FsCheck
        |> Arb.fromGen
        |> Arb.convert EvenInt (fun (EvenInt l) -> l)

    /// Custom generator of valid strings for tokenisation
    /// Use gen {} comp expression for complex generation
    /// NB this will NOT normally be needed
    /// Every custom generator needs a separate D.U. type
    type TokString = TokString of string list
    let tokStringArb =
        gen { // create next value of a Gen type RNG: uses F# computation expression syntax
            let! tokList = tokListGen // let! gets one random sample from Gen on RHS
            let! sepList = Gen.listOfLength tokList.Length optSpaceGen // one random sample of list of opt spaces
            /// outputs string of next token with trailing space if needed
            /// operators before or after other tokens do NOT need a separating space
            let getNextTokWithSep (lst: (string * string) list) =
                // called with list of pairs (t,s): t is token string, s is randomly either "" or " "
                match lst with
                | [] | [_] -> ""
                | (MatchNotOp t1, s1) :: (MatchNotOp _, _) :: _ -> t1 + " " // in this case a space is needed
                | (t1, s1) :: (_, _) :: _ -> t1 + s1 // in this case an operator means no space is needed
                                                     // use sample from optSpaceGen to insert spaces randomly
            /// generate successive tails of input list
            let rec listTails  = function | [] -> [[]] | _ :: tl as lst  -> lst :: listTails tl
            // return a list of strings representing input tokens with spaces added
            // this outputs the next RNG sample from the Gen value defined by this computation expression
            // NB return is necessary because this is a computation expression
            return 
                List.zip tokList sepList
                |> listTails
                |> List.map getNextTokWithSep
        }
        // boilerplate to make Arbitrary wrapped in D.U.
        |> Arb.fromGen // wrap the custom Gen type as an Arbitrary for use with FsCheck
        |> Arb.convert TokString (fun (TokString l) -> l) // Wrap the string list in TokString case

    let addToConfig config =
        let addedTypes = [
                typeof<TokString>.DeclaringType
                typeof<EvenInt>.DeclaringType          
            ]
        { config with arbitrary = addedTypes @ config.arbitrary}

[<AutoOpen>]
module Auto =
    // this defines FsCheck tests that include the custom generators defined above
    // each generator is associated with a one case D.U. type used to wrap its data
    let private config = Gen.addToConfig FsCheckConfig.defaultConfig
    let testProp name = testPropertyWithConfig config name
    let ptestProp name = ptestPropertyWithConfig config name
    let ftestProp name = ftestPropertyWithConfig config name
    let etestProp stdgen name = etestPropertyWithConfig stdgen config name

/// show 10 samples from FsCheck custom Arbitrary
let printArbSamples mess (arb:Arbitrary<'a>) =
    printfn "%s" mess
    let lst = (Gen.sample 3 10 arb.Generator)
    lst |> List.map (printf "\t%A\n")
    |> ignore
    printfn ""

/// show 10 samples from FsCheck custom generator
let printGenSamples mess gen = 
    Arb.fromGen gen |> printArbSamples mess

let tokRes = tokenise tokenOpArray tokenEndStrings

let expectoConfig = {defaultConfig with verbosity = Logging.LogLevel.Debug}

[<Tests>]
let testCustomTokeniser = 
    testList "custom RNG tokeniser tests" [
        testProp "Tokeniser delivers the correct number of tokens" <| fun (Gen.TokString ct) ->
            let tokStr = String.concat "" ct
            let tokLst = 
                ct 
                |> List.takeWhile (fun s -> Array.contains (s.Trim()) tokenEndStrings |> not)
                |> List.filter (fun s -> String.IsNullOrWhiteSpace s |> not ) 
            match tokRes tokStr with
            | Ok lst -> Expect.equal lst.Length tokLst.Length 
                         (sprintf "Tokenise '%A' = %A" tokStr lst)
            | Error mess -> Expect.isTrue false (sprintf "Tokeniser failed with:%s" mess)

        testProp "Tokens are the same as input string" <| fun (Gen.TokString ct) ->
            let tokStr = String.concat "" ct
            let tokInStr (tok:Token) = 
                let posEnd = tok.Pos + tok.Text.Length
                if posEnd > tokStr.Length then failwithf "Token end is beyond string end %A" tok
                Expect.equal tokStr.[tok.Pos..posEnd-1] tok.Text "Token matches input string"
            tokRes tokStr
            |> Result.map (List.iter tokInStr)
            |> Result.mapError (fun mess -> Expect.isTrue false mess)
            |> ignore
        ]

/// Test the Expecto Test Framework!
[<Tests>]
let allTests = testList "Expecto tests" [
    testCase "A sample test" <| fun () ->
        let expected = 4
        Expect.equal expected (2+2) "2+2 = 4"

    testCase "Tokeniser sanity test" <| fun () ->
        let src = "ADD R1,R2"
        let expected = Ok [
            {Pos=0 ; Text="ADD" ; TokType=SymTok}
            {Pos=4 ; Text="R1" ; TokType=SymTok}
            {Pos=6 ; Text="," ; TokType=OpTok}
            {Pos=7 ; Text="R2" ; TokType=SymTok} ]
        Expect.equal expected (tokRes src) "Four tokens correct"

    testCase "Tokeniser insanity test" <| fun () ->
        let src = "MOV 0abc"
        Expect.isError (tokRes src) "Symbols starting with digit make error"
    ]

[<Tests>]
let allFsChecks =
    /// sample configuration for fscheck tests
    let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

    /// list of 3 properties to test: replace with real properties of your code
    testList "FsCheck samples" [
        testProperty "Addition is commutative" <| fun a b ->
          a + b = b + a

        testProperty "Reverse of reverse of a list is the original list" <|
          fun (xs:list<int>) -> List.rev (List.rev xs) = xs

        // you can also override the FsCheck config
        testPropertyWithConfig config "Product is distributive over addition" <|
          fun a b c ->
            a * (b + c) = a * b + a * c
      ]


// printable string equiv of tokens
let dispTok (tok:Token) = sprintf "<%A|'%s':%d>" tok.TokType tok.Text tok.Pos

// printable string equivalent of a list of tokens
let dispTLst lst = lst |> List.map dispTok |> String.concat " ; " |> sprintf "[%s]"



let tokeniseSomething() =
    match tokRes "LOOP MOV R0, R1, 123" with
    | Error e -> printfn "Tokenise error: %s" e
    | Ok tokLst -> printfn "%s" (dispTLst tokLst)

let parseSomething() =
    printfn "--------"
    "{R0,R1,R2,R3}"
    |> tokRes
    |> fun r -> match r with 
                | Ok toks -> printfn "Toks:%s" (dispTLst toks) 
                | Error e -> printfn "Tokenise Error: %A" e
                r
    |> Result.mapError (fun s -> s,([]:Token list))
    |> function 
        | TPRegListLRes (rl, x) -> 
            match x with
            | Ok toks -> printfn "parse OK with rl=%A and remaining toks=%s" rl (dispTLst toks)
            | Error e -> printfn "Parse failed with  %A" e
        | x -> failwithf "What? TPRegListLRes always matches! Can't match: %A" x
    printfn "---------"

[<EntryPoint>]
let main argv =
    // run all tests tagged with [<TESTS>] - can be in any file
    runTestsInAssembly expectoConfig [||] |> ignore // run all tests defined with [<Tests>]
    // print samples from the custom tokenise input generator
    // NB to get the input string concatenate the list of strings with empty separator
    //printGenSamples "alpha" Gen.alphaGen
    //printGenSamples "regName" Gen.regNameGen
    //printGenSamples "manyAlphaNum" Gen.manyAlphaNumGen
    //printGenSamples "id" Gen.idGen
    //printArbSamples "tokStringArb" Gen.tokStringArb
    //printGenSamples "tokListGen" Gen.tokListGen
    // run the tokeniser, printing the result
    //tokeniseSomething()
    // run the parser, printing the result
    //parseSomething()
    // keep command window open
    printfn "press any key to terminate"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
