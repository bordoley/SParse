namespace Sparse.Tests
open System

[<AutoOpen>]
module Helpers =
    let expectedParseFail () = 
        failwith "expected parsing to fail."

    let expectedParseSuccess () =
        failwith "expected parsing to succeed."