namespace Sparse.Tests

open NUnit.Framework
open FsUnit
open Sparse
open System

module CharStream =
    module Item =
        [<Test>]
        let ``test preconditions`` () =
            (fun () -> CharStream.Empty.[-1] |> ignore) |> should throw typeof<ArgumentOutOfRangeException>
            (fun () -> CharStream.Empty.[1] |> ignore) |> should throw typeof<ArgumentOutOfRangeException>

        [<Test>]
        let ``with offset`` () =
            let stream = CharStream.Create("FooBar").SubStream(3)
            stream.[0] |> should equal 'B'

    module SubStream =
        [<Test>]
        let ``test preconditions`` () =
            (fun () -> CharStream.Empty.SubStream -1 |> ignore) |> should throw typeof<ArgumentOutOfRangeException>
            (fun () -> CharStream.Empty.SubStream 1  |> ignore) |> should throw typeof<ArgumentOutOfRangeException>

        [<Test>]
        let ``startIndex is 0`` () =
            let stream = CharStream.Create("FooBar")
            stream.SubStream 0 |> should equal stream

        [<Test>]
        let ``startIndex is input.Length`` () =
            let stream = CharStream.Create("FooBar")
            stream.SubStream 6 |> should equal CharStream.Empty