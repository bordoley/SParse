namespace Sparse.Tests

open NUnit.Framework
open FsUnit
open Sparse

module CharParsers =
    module ``satisfy and pchar`` =
        let p = pchar 'c'

        [<Test>]
        let ``with empty input`` () =
            match parse p "" with
            | Fail i -> 
                i |> should equal 0
            | _ -> failwith "expected parsing to fail."

        [<Test>]
        let ``with matching input`` () =
            match parse p "ccc" with
            | Success (result, next) -> 
                result |> should equal 'c'
                next |> should equal 1
            | _ -> failwith "expected parsing to succeed."

        [<Test>]
        let ``without matching input`` () =
            match parse p "accc" with
            | Fail i -> 
                i |> should equal 0
            | _ -> failwith "expected parsing to fail."
    
    module ``manySatisfy and many1Satisfy`` = 
        let p = many1Satisfy (fun c -> c = 'a')

        [<Test>]
        let ``with empty input`` () =
            match parse p "" with 
            | Fail i -> 
                i |> should equal 0
            | _ -> failwith "expected parsing to fail."

        [<Test>]
        let ``with matching input`` () =
            match parse p "aaabbb" with 
             | Success (result, next) -> 
                result |> should equal "aaa"
                next |> should equal 3
             | _ -> failwith "expected parsing to succeed."

    module manyMinMaxSatisfy =
        let p = manyMinMaxSatisfy 3 5 (fun c -> c = 'a')

        [<Test>]
        let ``with too few chars matching`` () =
            match parse p "aabbb" with
            | Fail i ->
                i |> should equal 1
            | _ -> failwith "expected parsing to fail."
        
        [<Test>]
        let ``with more than max chars matching`` () =
            match parse p "aaaaaaaaaa" with
            | Success (result, next) -> 
                result |> should equal "aaaaa"
                next |> should equal 5
             | _ -> failwith "expected parsing to succeed."
        
        [<Test>]
        let ``within range chars matching`` () =
            match parse p "aaaa" with
            | Success (result, next) -> 
                result |> should equal "aaaa"
                next |> should equal 4
             | _ -> failwith "expected parsing to succeed."

    module pstring =
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

    module regex =
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