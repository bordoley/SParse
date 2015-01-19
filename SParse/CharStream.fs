namespace Sparse

open System
open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices

[<NoComparison;NoEquality>]
type CharStream =
    internal {
        str: string 
        offset:int
    }

    member this.Length with get() = this.str.Length - this.offset

    member this.Item 
        with get(index:int) =
            if index < 0 || index >= this.Length then ArgumentOutOfRangeException "index" |> raise 

            this.str.Chars(this.offset + index)
    
    member this.SubStream (startIndex:int) =
        if startIndex < 0 || startIndex > this.Length then ArgumentOutOfRangeException "startIndex" |> raise 

        match startIndex with
        | 0 -> this
        | x when x = this.Length -> CharStream.Empty
        | _ -> { str = this.str; offset = this.offset + startIndex }

    override this.ToString() = 
        this.str.Substring(this.offset)

    static member internal Empty = { str = ""; offset = 0 }

    static member internal Create (str:string) = 
        if str.Length = 0 then CharStream.Empty
        else { str = str; offset = 0 }

[<AutoOpen>] 
module CharStreamExtensions =
    type CharStream with
        [<Extension>]
        member this.ToString(startIndex, length) =
            this.str.Substring(this.offset + startIndex, length)