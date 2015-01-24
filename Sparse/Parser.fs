namespace Sparse

open System
open System.Collections.Generic
open System.Diagnostics.Contracts

type ParseResult<'TResult> =
    | Success of  result : 'TResult * next : int
    | Fail of atIndex : int

type Parser<'TResult> = CharStream -> ParseResult<'TResult>

[<AutoOpen>]
module Parser =
    [<CompiledName("Parse")>]
    let parse (p:Parser<_>) (input:String) =
        p (CharStream.Create input)  

[<AutoOpen>]
module Primitives = 
    [<CompiledName("Bind")>]
    let (>>=) (p:Parser<'a>) (f:'a->Parser<'b>) =
        let parse (input:CharStream) =
            match p input with
            | Fail i -> Fail i
            | Success (result1, next1) ->
                let p2 = f result1
                match input.SubStream(next1) |> p2 with
                | Fail next2 -> Fail (next1 + next2)
                | Success (result2, next2) -> Success (result2, next1 + next2)
        parse

    [<CompiledName("Map")>]
    let (|>>) (p:Parser<_>) f =
        let parse (input:CharStream) =
            let result = p input
            match result with
            | Fail i -> Fail i
            | Success (result, next) -> Success (f result, next)
        parse

    [<CompiledName("Sequence")>]
    let (.>>.) (p1:Parser<'T1>) (p2:Parser<'T2>) =
        let parse (input:CharStream) = 
            match p1 input with
            | Fail i -> Fail i
            | Success (result1, next1) -> 
                match input.SubStream(next1) |> p2 with
                | Fail next2 -> Fail (next1 + next2)
                | Success (result2, next2) -> Success ((result1, result2), next1 + next2)
        parse
     
    [<CompiledName("SequenceRight")>]
    let (>>.) (p1:Parser<_>) (p2:Parser<_>) = 
        p1 .>>. p2 |>> fun (r1,r2) -> r2

    [<CompiledName("SequenceLeft")>]
    let (.>>) (p1:Parser<_>) (p2:Parser<_>) = 
        p1 .>>. p2 |>> fun (r1,r2) -> r1

    // NOTE: Doesn't apper to be a corresponding FParsec combinator to this
    [<CompiledName("Or")>]
    let (<^>) (p1:Parser<_>) (p2:Parser<_>) =
        let parse (input:CharStream) = 
            match p1 input with 
            | Success (result, next) -> 
                Success (Choice1Of2 result, next)
            | _ -> 
                match p2 input with
                | Success (result, next) -> Success (Choice2Of2 result, next)
                | Fail i -> Fail i
        parse

    [<CompiledName("Or")>]
    let (<|>) (p1:Parser<_>) (p2:Parser<_>) = 
        let p = p1 <^> p2
        fun (input:CharStream) ->
            match p input with
            | Fail i -> Fail i
            | Success (Choice1Of2 result, next) -> Success (result, next)
            | Success (Choice2Of2 result, next) -> Success (result, next)

    [<CompiledName("Eof")>]
    let eof = 
        let parse (input:CharStream) =
            if input.Length = 0 
            then Success ((), 0)
            else Fail 0
        parse

    [<CompiledName("FollowedBy")>]
    let followedBy (pnext:Parser<_>) =
        let parse (input:CharStream) =
            match pnext input with
            | Success _ -> Success ((), 0)
            | Fail i -> Fail i
        parse

    [<CompiledName("NotFollowedBy")>]
    let notFollowedBy (pnext:Parser<_>) =
        let parse (input:CharStream) =
            match pnext input with
            | Success (_, i) -> Fail (i - 1)
            | Fail i -> Success ((), 0)
        parse

    [<CompiledName("CreateParserForwardedToRef")>]
    let createParserForwardedToRef () =
        let dummy (input:CharStream) = InvalidOperationException "a parser created with createParserForwardedToRef was not initialized" |> raise
        let r = ref dummy
        (fun input -> !r input), r : Parser<_> * Parser<_> ref

    [<CompiledName("Many")>]
    let many (p:Parser<_>) =
        let rec doParse input (acc, pos) =
            match p input with
            | Fail i -> 
                (acc, pos)
            | Success (result, next) ->
                let acc = result::acc
                let pos = next + pos
                doParse (input.SubStream(next)) (acc, pos)
        
        let parse input =
            let (result, next) = doParse input ([], 0)
            Success ((List.rev result) :> seq<_>,  next)
        parse

    [<CompiledName("Many1")>]
    let many1 (p:Parser<_>) =   
        let p = many p

        let parse input =
            match p input with
            | Fail i -> Fail i
            | Success (result, next) as success ->
                if Seq.isEmpty result
                then Fail 0
                else success
        parse

    [<CompiledName("Optional")>]
    let opt (p:Parser<'TResult>) =
        let parse input =
            match p input with
            | Success (result, next) -> Success(Some result, next)
            | _ -> Success(None, 0)
        parse

    [<CompiledName("OrElse")>]
    let (<|>%) (p:Parser<_>) alt =
        p |> opt |>> (function | Some x -> x | _ -> alt)
   
    [<CompiledName("SepBy1")>]
    let sepBy1 (p:Parser<_>) (sep:Parser<_>)  =
        p .>>. (many (sep >>. p)) |>> (fun (a, b) -> Seq.append [a] b) 

    [<CompiledName("SepBy")>]
    let sepBy (p:Parser<_>) (sep:Parser<_>) =
        (sepBy1 p sep) <|>% Seq.empty

    [<CompiledName("Return")>]
    let preturn result = 
        let parse (input:CharStream) = Success (result, 0)
        parse

    [<CompiledName("Fail")>]
    let pzero =
        let parse (input:CharStream) = 
            Fail 0
        parse

    [<CompiledName("ManyMinMax")>]
    let manyMinMax minCount maxCount (p:Parser<_>) =
        if minCount < 0 then ArgumentException ("minCount less than 0", "minCount") |> raise 
        if maxCount < minCount then ArgumentException ("maxCount less than minCount", "maxCount") |> raise
        Contract.EndContractBlock()

        let rec doParse input (acc, pos, cnt) =
            if cnt = maxCount 
                then (acc, pos, cnt)
            else 
                match p input with
                | Fail i -> 
                    (acc, pos, cnt)
                | Success (result, next) ->
                    let acc = result::acc
                    let pos = next + pos
                    doParse (input.SubStream(next)) (acc, pos, cnt + 1)

        let parse input =
            let (result, next, cnt) = doParse input ([], 0, 0)
            if cnt >= minCount then Success ((List.rev result) :> seq<_>,  next)
            else Fail (next)
        parse