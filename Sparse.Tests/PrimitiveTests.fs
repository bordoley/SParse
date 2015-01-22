namespace Sparse.Tests

open NUnit.Framework
open FsUnit
open Sparse

module Primitives =
    module pzero =
        [<Test>]
        let ``assert always fails at position 0`` () =
            match parse pzero "test" with
            | Fail i -> i |> should equal 0
            | _ -> expectedParseFail ()
     
    module preturn =
        [<Test>]
        let ``assert always succeeds at position 0`` () =
            let successResult = "success"
            let parser = preturn successResult

            match parse parser "test" with
            | Success (result, next) ->
                result |> should equal successResult
                next |> should equal 0
            | _ -> expectedParseSuccess ()

    module eof =
        [<Test>]
        let ``when input stream is not empty`` () =
            match parse eof "not empty" with
            | Fail i -> 
                i |> should equal 0
            | _ -> expectedParseFail ()

        [<Test>]
        let ``when input stream is empty`` () =
            match parse eof "" with
            | Success (result, next) ->
                result |> should equal ()
                next |> should equal 0
            | _ -> expectedParseSuccess ()

    module ``>>=`` =
        let p = 
            let p1 = pstring "foo"
            let p2 = fun s -> pstring "bar"
            p1 >>= p2

        [<Test>]
        let ``when p1 fails`` () =
            match parse p "fobar" with
            | Fail i ->
                i |> should equal 2
            | _ -> expectedParseFail ()
           
        [<Test>]
        let ``when p1 succeeds and p2 fails`` () =
            match parse p "foobaz" with
            | Fail i ->
                i |> should equal 5
            | _ -> expectedParseFail ()

        [<Test>]
        let ``when p1 succeeds and p2 succeeds`` () =
            match parse p "foobaraaaaaa" with
            | Success (result, next) ->
                result |> should equal "bar"
                next |> should equal 6
            | _ -> expectedParseSuccess ()

    module ``|>>`` =
        let p = pstring "foo" |>> fun _ -> "bar"

        [<Test>]
        let ``when p succeeds`` () =
            match parse p "foobar" with
            | Success (result, next) ->
                result |> should equal "bar"
                next |> should equal 3
            | _ -> expectedParseSuccess ()

        [<Test>]
        let ``when p fails`` ()  =
            match parse p "fobar" with
            | Fail i ->
                i |> should equal 2
            | _ -> expectedParseFail ()

    module ``combining parsers pFoo pBar``  =
        let foo = (pstring "foo")
        let bar = (pstring "bar")

        let pFooBar = foo .>>. bar
        let pFoo = foo .>>  bar
        let pBar = foo  >>. bar

        [<Test>]
        let ``when pFoo fails`` () =
            let test = "fogbar"

            match parse pFooBar test with
            | Fail i ->
                i |> should equal 2
            | _ -> expectedParseFail ()

            match parse pFoo test with
            | Fail i ->
                i |> should equal 2
            | _ -> expectedParseFail ()

            match parse pBar test with
            | Fail i ->
                i |> should equal 2
            | _ -> expectedParseFail ()
 
        [<Test>]
        let ``when pBar fails`` () =
            let test = "foobazzzzz"

            match parse pFooBar test with
            | Fail i ->
                i |> should equal 5
            | _ -> expectedParseFail ()

            match parse pFoo test with
            | Fail i ->
                i |> should equal 5
            | _ -> expectedParseFail ()

            match parse pBar test with
            | Fail i ->
                i |> should equal 5
            | _ -> expectedParseFail ()

        [<Test>]
        let ``when parse succeed`` () =
            let test = "foobarzzzz"

            match parse pFooBar test with
            | Success (result, next) ->
                result |> should equal ("foo", "bar")
                next |> should equal 6
            | _ -> expectedParseSuccess ()

            match parse pFoo test with
            | Success (result, next) ->
                result |> should equal "foo"
                next |> should equal 6
            | _ -> expectedParseSuccess ()

            match parse pBar test with
            | Success (result, next) ->
                result |> should equal "bar"
                next |> should equal 6
            | _ -> expectedParseSuccess ()

    module ``choice parsers`` =
        let foo = (pstring "foo")
        let bar = (pstring "bar")
        let pFooOrBar = foo <|> bar
        let pChoiceFooOrBar = foo <^> bar

        [<Test>]
        let ``when input is neither foo or bar`` () =
            let test = "baz"

            match parse pFooOrBar test with
            | Fail i ->
                i |> should equal 2
            | _ -> expectedParseFail ()

            match parse pChoiceFooOrBar test with
            | Fail i ->
                i |> should equal 2
            | _ -> expectedParseFail ()
       
        [<Test>]
        let ``when input is foo`` () =
            let test = "foo"

            match parse pFooOrBar test with
            | Success (result, next) ->
                result |> should equal "foo"
                next |> should equal 3
            | _ -> expectedParseSuccess ()


            match parse pChoiceFooOrBar test with
            | Success (Choice1Of2 (result), next) ->
                result |> should equal "foo"
                next |> should equal 3
            | _ -> expectedParseSuccess ()

        [<Test>]
        let ``when input is bar`` () =
            let test = "bar"

            match parse pFooOrBar test with
            | Success (result, next) ->
                result |> should equal "bar"
                next |> should equal 3
            | _ -> expectedParseSuccess ()


            match parse pChoiceFooOrBar test with
            | Success (Choice2Of2 (result), next) ->
                result |> should equal "bar"
                next |> should equal 3
            | _ -> expectedParseSuccess ()

    module ``followedBy and notFollowedBy`` =
        let pFoo = pstring "foo"
        let pBar = pstring "bar"
        let pFooFollowedByBar = pFoo .>> followedBy pBar
        let pFooNotFollowedByBar = pFoo .>> notFollowedBy pBar

        [<Test>]
        let ``foo not followed by bar`` () =
            let test = "foobaz"

            match parse pFooNotFollowedByBar test with
            | Success (result, next) ->
                result |> should equal "foo"
                next |> should equal 3
            | _ -> expectedParseSuccess ()

            match parse pFooFollowedByBar test with
            | Fail i ->
                i |> should equal 5
            | _ -> expectedParseFail ()

        [<Test>]
        let ``foo followed by bar`` () =
            let test = "foobar"

            match parse pFooFollowedByBar test with
            | Success (result, next) ->
                result |> should equal "foo"
                next |> should equal 3
            | _ -> expectedParseSuccess ()

            match parse pFooNotFollowedByBar test with
            | Fail i ->
                i |> should equal 5
            | _ -> expectedParseFail ()

    module ``many and many1`` =
        let p =
            let p = pstring "test"
            many1 p

        [<Test>]
        let ``with no matches`` () =
            match parse p "failfail" with
            | Fail i ->
                i |> should equal 0
            | _ -> expectedParseFail ()

        [<Test>]
        let ``with more than one match`` () =
            match parse p "testtesttesttest" with
            | Success (result, next) ->
                (result |> List.ofSeq) |> should equal ["test"; "test"; "test";  "test"]
            | _ -> expectedParseSuccess ()

    module ``sepBy1 and sepBy`` =
        let p =
            let p = pstring "test"
            sepBy1 p (pchar ',')
        
        [<Test>]
        let ``with no matches`` () =
            match parse p "fail,fail" with
            | Fail i ->
                i |> should equal 0
            | _ -> expectedParseFail ()

        [<Test>]
        let ``with more than one match`` () =
            match parse p "test,test,test,test" with
            | Success (result, next) ->
                (result |> List.ofSeq) |> should equal ["test"; "test"; "test";  "test"]
            | _ -> expectedParseSuccess ()

    module ``<|>%`` =
        let p =
            let p = pstring "foo"
            p <|>% "bar"

        [<Test>]
        let ``when p succeeds`` () =
            match parse p "foo" with
            | Success (result, next) ->
                result |> should equal "foo"
                next |> should equal 3
            | _ -> expectedParseSuccess ()

        [<Test>]
        let ``when p fails`` () =
            match parse p "bar" with
            | Success (result, next) ->
                result |> should equal "bar"
                next |> should equal 0
            | _ -> expectedParseSuccess ()

    
    module createParserForwardedToRef =
        let x = 0

    module manyMinMax =
        let x = 0

    module opt =
        let p = pstring "foo" |> opt

        [<Test>]
        let ``when p succeeds`` () =
            match parse p "foo" with
            | Success (result, next) ->
                result |> should equal (Some "foo")
                next |> should equal 3
            | _ -> expectedParseSuccess ()

        [<Test>]
        let ``when p fails`` () =
            match parse p "bar" with
            | Success (result, next) ->
                result |> should equal None
                next |> should equal 0
            | _ -> expectedParseSuccess ()