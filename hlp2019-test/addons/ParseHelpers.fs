(*-------------------------------------------------------------------------------
                  Useful functions for text tokenisation and parsing
  -------------------------------------------------------------------------------

  This file contains a demonstration tokeniser and set of parse functions using active patterns.
  The tokeniser correctly tokenises numbers, identifiers, single character operators, 
  strings with escape sequences using '\'
  
  The parse functions demonstrate how to write parsers that can return errors using match statements*)

module ParseHelpers


    //--------------------------------------------------------------------------------------------//

    open System
    open RegisterNames // replace by open CommonData if used with Visual2 code base

    /// Token type determines how token Text field is interpreted
    type TType = 
        | SymTok // text identifier
        | NumTok of uint32// decimal, binary, or hex number
        | OpTok // operator

    /// Tokens contain the starting position in s of their text.
    /// Tokens are classified as operators, numbers, or symbols, with text string value in all cases
    type Token = {Pos: int ; Text: string ; TokType: TType}
    
    type ScanMode = 
        | Norm 
        | InString

    /// Parse a token as a 32 bit unsigned number.
    /// Works for decimal, hex, binary under FABLE as well as .NET.
    /// Returns Error if token is not a valid number.
    /// Correctly parses negative numbers '-123' into two's complement pattern.
    let to32BitUnsignedLiteral str =
        try
            str
            |> int64
            |> function
               | n when n > (1L <<< 32) || n < (-1L <<< 31) -> Error("Literal more than 32 bits", str)
               | n -> Ok(uint32 n)
        with
            | e -> Error("Literal is not a valid number", str)


    // characters interpreted as standalone
    let tokenOpArray = [|"/";"*";"+";"-";"/";"(";")";";";",";"{";"}"|]

    let tokenEndStrings = [|";"|]

    /// Tokenise a string returning a Result list of tokens or an error.
    /// Input s: string to tokenise.
    /// Input opArr: array of strings to be tokenised as operators: order determines priority on multiple matches.
    /// Input endstringArr: array of single character strings that end the tokenisation.
    let tokenise (opArr: string array) endStringArr (s:string) =
        let opInitArr = opArr  |> Array.map (fun s -> s.[0])     
        /// classify a character: depends on opList so must be subfunction
        let (|WS|LETTER|DIGIT|OPCHAR|OTHER|) (ch:char) =  
            if Char.IsWhiteSpace ch then WS
            elif Char.IsLetter ch then LETTER
            elif Char.IsDigit ch then DIGIT
            elif Array.contains ch opInitArr then OPCHAR
            else OTHER

        /// classify a token: depends on character classification so must be a subfunction
        let (|ISSYM|ISNUM|ISOP|NOTOK|) (s:string) =
            let base2Char (ch: char) = ch = '_' || ch = '0' || ch = '1'
            let base10Char (ch:char) = Char.IsDigit ch || ch = '_'
            let base16Char (ch:char) = Char.IsDigit ch || Array.contains (Char.ToUpper ch) [|'_';'A';'B';'C';'D';'F'|]
            match s |> Seq.toList with
            | LETTER :: rest when List.forall (fun ch -> Char.IsLetterOrDigit ch || ch = '_') rest -> ISSYM
            | DIGIT :: rest when List.forall (fun ch -> Char.IsDigit ch || ch = '_')  rest -> ISNUM
            | '0' :: ('x' | 'X') :: rest when List.forall base16Char rest -> ISNUM
            | '0' :: ('b' | 'B')  :: rest when List.forall base2Char rest -> ISNUM
            | OPCHAR :: rest -> ISOP
            | _ -> NOTOK

        /// scan line accumulating tokens.
        /// start: start of currently scanned token
        /// pos: end of currently scanned token
        /// state: state of scan (in or out of string etc)
        /// tokRes: accumulated list of tokens.
        let rec scan start pos state accToks =
            if pos < s.Length && Array.contains s.[start..start] endStringArr then
                accToks |> List.rev |> Ok
            else
                let nextCh = if pos >= s.Length then None else Some s.[pos]
                let nPos = pos + 1
                let nextScan = scan start nPos
                let newScan = scan nPos nPos
                let windowLeft = max (pos - 5) 0
                let windowRight = max (pos + 5) (s.Length - 1)
                /// add another token, calling scanner to process the rest of the line.
                /// return Error if the added token is invalid
                /// Use of scanner callback is needed so this function can terminate scan if needed
                let addNextTok scanner endPos toks = 
                    let makeTok typ text = Ok { Pos=start ; Text=text; TokType=typ}
                    let tokText = s.[start..endPos].Trim()
                    let tokenOf = function
                        | "" -> Error "What? No text found for added token"
                        | NOTOK as text -> Error (sprintf "Can't tokenise: '%s'" text)
                        | ISOP as text -> makeTok OpTok text
                        | ISNUM as text -> 
                            match to32BitUnsignedLiteral text with
                            | Ok u -> makeTok (NumTok u) text
                            | Error (mess, _) -> Error (sprintf "Can't tokenise: '%s': %s" text mess)
                        | ISSYM as text -> makeTok SymTok text
                    match tokText, start = nPos, tokenOf tokText with
                    | _,  true, _ 
                    | "", _   , _ -> toks |> scanner
                    | _, false, Ok tok -> tok :: toks |> scanner
                    | _, false, Error e -> Error e
             
                match nextCh, state with
                | None, InString ->
                    "Line ended with non-terminated string" |> Error                   
                | None, Norm when start = pos -> 
                    List.rev accToks |> Ok
                | None, Norm -> 
                    addNextTok (newScan Norm ) (pos - 1) accToks
                | Some '"', InString ->
                    addNextTok (nextScan Norm) pos accToks
                | Some '\\', InString ->
                    scan start (nPos + 1) InString accToks
                | Some '"', Norm ->
                    addNextTok (nextScan  InString) pos accToks
                | Some _, InString ->
                    nextScan InString accToks
                | Some LETTER, Norm 
                | Some DIGIT, Norm ->
                    nextScan Norm accToks
                | Some WS, Norm when start = pos -> 
                    newScan Norm accToks
                | Some WS, Norm -> 
                    addNextTok (newScan Norm ) (pos - 1) accToks
                | Some OPCHAR, Norm when start <> pos ->
                    addNextTok (scan pos pos Norm) (pos - 1) accToks
                | Some (OPCHAR as ch), Norm ->
                    let opMatch = opArr |> Array.tryFind (fun op -> s.[pos..s.Length - 1].StartsWith op)
                    match opMatch with
                    | None -> sprintf "Tokenise error at '%c' in '%s'" ch s.[windowLeft..windowRight] |> Error
                    | Some op -> 
                        let nPos' = pos + op.Length
                        scan nPos nPos Norm ({Pos=pos ; Text = op ; TokType = OpTok} :: accToks)
                | _ when start <> nPos ->
                   addNextTok ( newScan Norm) pos accToks
                | Some ch, Norm -> 
                    sprintf "Tokenise error at '%c' in '%s'" ch s.[windowLeft..windowRight] |> Error
        scan 0 0 Norm []

    /// matches a register name token
    let (|TPRName|_|) tok = Map.tryFind tok.Text regNames

    /// LRes version of TPRName.
    /// As TPRName but the match always succeed, returning an Error if a Register name is not matched 
    /// The pattern outputs a pair which is the correctly parsed name (if it exists) paired with a Result.
    /// This allows the pattern to be embedded in a chain of matches to parse a sequence of tokens.
    let (|TPRNameLRes|_|) tokList =
        match tokList with
        | Error e -> None, Error e
        | Ok (tok :: tL) -> 
                match Map.tryFind tok.Text regNames with
                | Some rn as rnOpt -> (rnOpt, Ok tL)
                | None -> None, Error(sprintf "%s found when %s expected" tok.Text "register name", tok :: tL )
        | Ok [] -> (None, Error (" '' found when register name expected",[]))
        |> Some

    /// matches a token with Text = tokStr (which must be a literal)
    let (|TPTok|_|) tokStr tok = 
        match tok with
        | {Text= s} when s = tokStr -> Some ()
        | _ -> None

    /// matches a token list with head token Text = tokStr (which must be a literal)
    /// Returns a Result Token list, with Error on no match
    let (|TPTokLRes|_|) tokStr tokLst = 
        match tokLst with
        | Ok ({Text= s} :: tL) when s = tokStr -> (Ok tL)
        | Ok ({Text= s} :: tL) -> Error (sprintf "%s found when %s expected" s tokStr, tL)
        | Ok [] -> Error( sprintf "End of line found when %s expected" tokStr, [])
        | Error e -> Error e
        |> Some

    /// matches a sequnce of tokens making a comma separated register list
    /// terminated by '}'
    /// Example: R1 , R3 , }
    let rec (|TPRegListTailL|_|) tokLst =
        match tokLst with
        | TPTok "}" () :: tL -> Some ([] , tL)
        | TPTok "," () :: TPRName rn :: TPRegListTailL (rnL, toks) -> Some( rn :: rnL, toks)
        | _ -> None

    /// matches a brace enclosed comma sepaprated list of register names
    let (|TPRegListL|_|) tokLst =
        match tokLst with
        | TPTok "{" () :: TPRName rn :: TPRegListTailL (rnL, toks') -> (rn :: rnL, toks') |> Some
        | _ -> None

    /// like TPRegs but returns a token list wrapped in a Result.
    /// Always matches, returns Error on parse failure.
    let rec (|TPRegListTailLRes|_|) tokLst =
        match tokLst with
        | Ok (TPTok "}" () :: tL) -> [], Ok tL
        | TPTokLRes "," (TPRNameLRes( Some rn,  TPRegListTailLRes (rnL, toks))) -> rn :: rnL, toks
        | Ok [] -> [], Error("looking for ', Rn}' or '}'", [])
        | _ -> failwithf "What? Can't happen"
        |> Some

    /// matches a brace enclosed comma separated list of register names
    /// Like TPRegsList: but returns a result Error on match failure
    let (|TPRegListLRes|_|) tokLst =
        match tokLst with
        | TPTokLRes "{" (TPRNameLRes (Some rn, TPRegListTailLRes (rnL, toks'))) -> rn :: rnL, toks'
        | _ -> failwithf "What? Can't happen"
        |> Some
