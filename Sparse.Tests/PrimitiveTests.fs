namespace Sparse.Tests

open NUnit.Framework
open FsUnit
open Sparse

module PrimitiveTests =
    [<Test>]
    let ``test pzero`` () =
        let input = CharStream.Create "test"
        match pzero input with
        | Fail i -> i |> should equal 0
        | Success _ -> failwith "expected parsing to fail."
       

