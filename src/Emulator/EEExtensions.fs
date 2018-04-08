namespace EEExtensions
/// Miscellaneous extensions to core F# library functions
/// Additions to Char, String, Map


/// various functions that exist in normal F# but cannot work in fable
module FableReplacements =
    let optionMap2 f v1 v2 = match v1, v2 with
                             | Some v1, Some v2 -> Some (f v1 v2)
                             | _ -> None

    let listChunkBySize chunkSize l =
        let rec listChunkBySize' state chunksLeft itemsRemaining =
            match chunksLeft, itemsRemaining with
            | _, [] -> state
            | 0, _ -> listChunkBySize' ([] :: state) chunkSize itemsRemaining
            | _, nextItem::itemsTail -> listChunkBySize' ((nextItem :: List.head state) :: (List.tail state)) (chunksLeft - 1) itemsTail

        match l with
        | [] -> []
        | _ -> listChunkBySize' [] 0 l |> List.map List.rev |> List.rev

    let hexToString (x : uint32) =
        let rec loop str =
            function
            | 0u -> str
            | num -> loop ((sprintf "%X" (num % 16u)) + str) (num / 16u)
        match x with
        | 0u -> "0"
        | _ -> loop "" x

// Following char method to convert to integer is partly based on code found on
// http://www.fssnip.net/25/title/Hex-encode-decode
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Char =

    [<CompiledName("ToInt")>]
    let inline toInt(ch: char): int = 
        match ch with
        | c when c >= '0' && c <= '9' -> int c - int '0' 
        | c when c >= 'a' && c <= 'f' -> (int c - int 'a') + 10
        | c when c >= 'A' && c <= 'F' -> (int c - int 'A') + 10
        | _ -> failwithf "What ? Error while converting character to digit"

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module String =

    open System

    [<CompiledName("OfChar")>]
    let inline ofChar( ch: char): string = [| ch |] |> Seq.ofArray |> System.String.Concat

    [<CompiledName("OfSeq")>]
    let inline ofSeq (chars: char seq) : string = System.String.Concat chars

    [<CompiledName("ToSeq")>]
    let inline toSeq (str: string) : char seq = str :> char seq
     
    [<CompiledName("OfList")>]
    let inline ofList (chars: char list) =  chars |> Seq.ofList |> System.String.Concat


    [<CompiledName("OfArray")>]
    let inline ofArray (chars: char array) = chars |> Seq.ofArray |> System.String.Concat

    [<CompiledName("ToList")>]
    let inline toList (str: string): char list = str |> List.ofSeq

    [<CompiledName("ToArray")>]
    let inline toArray (str: string): char array = str |> Array.ofSeq

    /// splits text into its array of non-whitepace strings separated by whitespace
    [<CompiledName("SplitOnWhitespace")>]
    let splitOnWhitespace (text:string): string array = 
        text.Split( (null : char array) , System.StringSplitOptions.RemoveEmptyEntries)

    let [<Literal>] DefaultComparison = StringComparison.Ordinal
    let inline emptyIfNull str = 
        match str with
        | null -> String.Empty
        | _ -> str
    /// Concatenate a sequence of strings
    /// Using sep as separator
    [<CompiledName("Concat")>]
    let concat sep (strings : seq<string>) =  
        String.Join(sep, strings)

    [<CompiledName("Length")>]
    let length (str:string) =
        let str = emptyIfNull str
        str.Length

    /// True if str contains value
    [<CompiledName("Contains")>]
    let contains (value:string) (str:string) =
        str.Contains(value)

    [<CompiledName("Compare")>]
    let compare (strB:string) (strA:string) =
        String.Compare(strA, strB, DefaultComparison)

    /// True if str ends with value
    [<CompiledName("EndsWith")>]
    let endsWith (value:string) (str:string) =
        str.EndsWith(value, DefaultComparison)

    [<CompiledName("Equals")>]
    /// See String.Equals
    let equals (comparisonType:StringComparison) (value:string) (str:string) =
        str.Equals(value, comparisonType)

    let inline checkIndex func (comparisonType:StringComparison) value =
        let index = func(value, comparisonType)
        if index = -1 then None
        else Some index

    /// Replace all occurences of oldChar by newchar
    [<CompiledName("ReplaceChar")>]
    let replaceChar (oldChar:char) (newChar:char) (str:string) =
        str.Replace(oldChar, newChar)

    /// Replace all occurences of oldValue by newValue
    [<CompiledName("Replace")>]
    let replace (oldValue:string) (newValue:string) (str:string) =
        str.Replace(oldValue, newValue)

    /// Split str at all of separator array elements
    /// Return array of strings
    /// Adjacent separators generate empty strings
    [<CompiledName("Split")>]
    let split (separator:char array) (str:string) =
        str.Split(separator, StringSplitOptions.None)

    /// Split str at all of separator array elements
    /// Return array of strings
    /// Adjacent separators do not generate strings   
    [<CompiledName("SplitRemoveEmptyEntries")>]
    let splitRemoveEmptyEntries (separator:char array) (str:string) =
        str.Split(separator, StringSplitOptions.RemoveEmptyEntries)

    /// Split str at all of separator string array elements
    /// Return array of strings
    /// Adjacent separators generate empty strings
    [<CompiledName("SplitString")>]
    let splitString (separator:string array) (str:string) =
        str.Split(separator, StringSplitOptions.None)
    /// Split str at all of separator string array elements
    /// Return array of strings
    /// Adjacent separators do not generate strings
    [<CompiledName("SplitStringRemoveEmptyEntries")>]
    let splitStringRemoveEmptyEntries (separator:string array) (str:string) =
        str.Split(separator, StringSplitOptions.RemoveEmptyEntries)

    /// Return true if str starts with value
    [<CompiledName("StartsWith")>]
    let startsWith (value:string) (str:string) = 
        str.StartsWith(value, DefaultComparison)

    /// Return substring of str at startIndex of length chars
    /// Throw ArgumentOutOfRange exception if any part of
    /// selected string lies outside str.
    [<CompiledName("SubstringLength")>]
    let substringLength (startIndex:int) (length: int) (str:string) =
        str.Substring(startIndex, length)
    /// Return str from startIndex till end
    /// Throw ArgumentOutOfRange exception if startWith
    /// lies outside str
    [<CompiledName("Substring")>]
    let substring (startIndex:int) (str:string) =
        str.Substring(startIndex)

    [<CompiledName("ToLower")>]
    let toLower(str:string) =
        str.ToLowerInvariant()

    [<CompiledName("ToUpper")>]
    let toUpper(str:string) =
        str.ToUpperInvariant()
    /// Remove all leading and training whitespace
    [<CompiledName("Trim")>]
    let trim(str:string) =
        str.Trim()
    /// Remove all leading and trailing chars in trimChars
    [<CompiledName("TrimChars")>]
    let trimChars (trimChars:char []) (str:string) =
        str.Trim(trimChars)
    /// Remove all leading whitespace
    [<CompiledName("TrimStart")>]
    let trimStart (trimChars:char []) (str:string) =
        str.TrimStart(trimChars)
    /// Remove all trailing whitespace    
    [<CompiledName("TrimEnd")>]
    let trimEnd(trimChars:char []) (str:string) =
        str.TrimEnd(trimChars)

    /// Match a regular expression
    /// Return Some (m,grps) where m is the match string,
    /// grps is the list of match groups (if any)
    /// return None on no match
    [<CompiledName("RegexMatch")>]
    let regexMatch (regex:string) (str:string) =
        let m = Text.RegularExpressions.Regex(regex).Match(str)
        if m.Success
        then
            Some m.Value // TODO workaround
            //let mLst = [ for x in m.Groups -> x.Value ]
            //Some (List.head mLst, List.tail mLst)
        else None




[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module List =
    [<CompiledName("ToString")>]
    let toString (chars: char list) =  chars |> Seq.ofList |> System.String.Concat

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Array =

    [<CompiledName("ToString")>]
    let toString (chars: char array) = chars |> Seq.ofArray |> System.String.Concat



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Map =
    /// Looks up key in table, returning defaultValue if 
    /// key is not in table
    [<CompiledName("FindWithDefault")>]
    let findWithDefault (key:'Key) (table:Map<'Key,'Value>) (defaultValue:'Value) =
        match table.TryFind key with | Some v -> v |None -> defaultValue

    /// Return array of all values in table
    [<CompiledName("Values")>]
    let values (table:Map<'Key,'Value>) =
        table |> Map.toArray |> Array.map snd

    /// Return array of all keys in table
    [<CompiledName("Keys")>]
    let keys (table:Map<'Key,'Value>) =
        table |> Map.toArray |> Array.map fst





