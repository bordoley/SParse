namespace Sparse.Tests.CharParsers

open NUnit.Framework
open FsUnit
open Sparse

module ``test pstring parser`` =
    let successResult = "test"
    let p = pstring successResult

    [<Test>]
    let ``when the input is too short`` () =
        match parse p "tes" with
        | Fail i -> 
            i |> should equal 3
        | _ -> failwith "expected parsing to fail."

    [<Test>]
    let ``when the input is a partial match`` () =
        match parse p "tes_____" with
        | Fail i -> 
            i |> should equal 3
        | _ -> failwith "expected parsing to fail."

    [<Test>]
    let ``when the input is a match`` () =
        match parse p "test____" with
        | Success (result, next) -> 
            result |> should equal successResult
            next |> should equal successResult.Length
        | _ -> failwith "expected parsing to succeed."

module ``test regex parser`` =
    let p = regex "a+"

    [<Test>]
    let ``with match at the beginning of the string`` () =
        match parse p "aaabbb" with
        | Success (result, next) -> 
            result |> should equal "aaa"
            next |> should equal 3
        | _ -> failwith "expected parsing to succeed."

    [<Test>]
    let ``with match in the middle of the string`` () =
        let stream = CharStream.Create "bbbaaabbb"
        let stream = stream.SubStream 3
        match p stream with
        | Success (result, next) ->
            result |> should equal "aaa"
            next |> should equal 3
        | _ -> failwith "expected parsing to succeed."

    [<Test>]
    let ``with no match at the start of the string`` () =
        match parse p "bbbbbaaa" with
        | Fail i ->
            i |> should equal 0
        | _ -> failwith "expected parsing to fail."