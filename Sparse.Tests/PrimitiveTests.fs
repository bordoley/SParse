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