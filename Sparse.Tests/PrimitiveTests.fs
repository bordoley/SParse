namespace Sparse.Tests

open NUnit.Framework
open FsUnit
open Sparse

module PrimitiveTests =
    [<Test>]
    let ``assert pzero always fails at position 0`` () =
        let input = CharStream.Create "test"
        match pzero input with
        | Fail i -> i |> should equal 0
        | _ -> failwith "expected parsing to fail."
      
    [<Test>]
    let ``assert preturn always succeeds at position 0`` () =
        let input = CharStream.Create "test"
        let successResult = "success"
        let parser = preturn successResult

        match parser input with
        | Success (result, next) ->
            result |> should equal successResult
            next |> should equal 0
        | _ -> failwith "expected parsing to succeed."