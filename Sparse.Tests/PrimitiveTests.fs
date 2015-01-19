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
            | _ -> failwith "expected parsing to fail."
     
    module preturn =
        [<Test>]
        let ``assert always succeeds at position 0`` () =
            let successResult = "success"
            let parser = preturn successResult

            match parse parser "test" with
            | Success (result, next) ->
                result |> should equal successResult
                next |> should equal 0
            | _ -> failwith "expected parsing to succeed."
     
    module ``>>=`` =
        let p1 = pstring "foo"
        let p2 = fun s -> pstring "bar"

        [<Test>]
        let ``when p1 fails`` () =
            match parse p1 "fobar" with
            | Fail i ->
                i |> should equal 2
            | _ -> failwith "expected parsing to fail."