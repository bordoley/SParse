namespace Sparse.Tests.CharParsers

open NUnit.Framework
open FsUnit
open Sparse

module ``test pstring parser`` =
    let p = pstring "test"

    [<Test>]
    let ``when the input is too short`` () =
        match p (CharStream.Create "tes") with
        | Fail i -> 
            i |> should equal 3
        | _ -> failwith "expected parsing to fail."
    ()

    [<Test>]
    let ``when the input is a partial match`` () =
        match p (CharStream.Create "tes_____") with
        | Fail i -> 
            i |> should equal 3
        | _ -> failwith "expected parsing to fail."
    ()