namespace SParse

open System

type CharStream private (str: string, offset:int, length:int) =   
    static let empty = new CharStream("", 0, 0)

    member this.Length with get() = length

    member this.Item 
        with get(index:int) =
            if index < 0 || index >= length then invalidArg "index" "index is out of bounds"

            str.Chars(offset + index)
                      
    member this.GetSlice(start, finish) =
        let start = defaultArg start 0
        let finish = defaultArg finish (length - 1)

        if start < 0 || finish < (start - 1) || finish >= length then ArgumentOutOfRangeException () |> raise 

        match (start, finish) with
        | (x, y) when x = 0 && y = (length - 1) -> this
        | (x, y) when y - x = -1 -> empty
        | _ -> new CharStream(str, offset + start, finish - start + 1)

    override this.ToString() = 
        str.Substring(offset,length)

    static member Create (str:string) = 
        if str.Length = 0 then empty
        else CharStream(str, 0, str.Length)

[<AutoOpen>] 
module CharStreamMixins =
    type CharStream with
        member this.ToString(startIndex, length) =
            this.[startIndex..(startIndex + length - 1)].ToString()