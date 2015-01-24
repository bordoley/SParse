namespace Sparse

open System
open System.Collections
open System.Collections.Generic

[<Sealed>]
type CharStream private (str:string, offset:int) =
    member internal this.BackingString with get () = str

    member internal this.Offset with get () = offset

    member this.Length with get() = str.Length - offset

    member this.Item 
        with get(index:int) =
            if index < 0 || index >= this.Length then ArgumentOutOfRangeException "index" |> raise 

            str.Chars(offset + index)
    
    member this.SubStream (startIndex:int) =
        if startIndex < 0 || startIndex > this.Length then ArgumentOutOfRangeException "startIndex" |> raise 

        match startIndex with
        | 0 -> this
        | x when x = this.Length -> CharStream.Empty
        | _ -> CharStream(str, offset + startIndex)

    override this.ToString() = 
        str.Substring(offset)

    member this.ToString(startIndex, length) =
        str.Substring(offset + startIndex, length)

    static member val internal Empty = CharStream("", 0)

    static member internal Create (str:string) = 
        if str.Length = 0 then CharStream.Empty
        else CharStream(str, 0)