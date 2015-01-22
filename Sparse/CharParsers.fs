namespace Sparse

open System
open System.Diagnostics.Contracts
open System.Text.RegularExpressions

[<AutoOpen>]
module CharPredicates =
    [<CompiledName("IsAnyOf")>]
    let isAnyOf (chars:string) =
        // FIXME: Probably add some heuristics here based upon the number of chars in the string
        // and switch the implementation
        let chars = chars.ToCharArray()
        Array.sortInPlace chars
        fun c -> Array.BinarySearch(chars, c) > 0

    [<CompiledName("InRange")>]
    let inRange (start:char) (last:char) = 
        let isInRange (c:char) =
            c >= start && c <= last
        isInRange

    [<CompiledName("IsDigit")>]
    let isDigit = inRange '0' '9'

[<AutoOpen>]
module CharParsers =
    [<CompiledName("Satisfy")>]
    let satisfy f =
        let parse (input:CharStream) =
            if input.Length = 0 then Fail 0
            else 
                let result = input.Item 0
                if f result 
                then  Success(result, 1)
                else Fail 0
        parse

    [<CompiledName("Char")>]
    let pchar c  = satisfy (fun i -> i = c)

    [<CompiledName("Semicolon")>]
    let pSemicolon : Parser<char> = pchar ';'

    [<CompiledName("Comma")>]
    let pComma : Parser<char> = pchar ','

    [<CompiledName("Space")>]
    let pSpace : Parser<char> = pchar ' '

    [<CompiledName("Colon")>]
    let pColon : Parser<char> = pchar ':'

    [<CompiledName("Period")>]
    let pPeriod : Parser<char> = pchar '.' 

    [<CompiledName("Equals")>]
    let pEquals : Parser<char> = pchar '=' 

    [<CompiledName("ForwardSlash")>]
    let pForwardSlash : Parser<char> = pchar '/'

    [<CompiledName("Dash")>]
    let pDash : Parser<char> = pchar '-'

    [<CompiledName("OpenParen")>]
    let pOpenParen : Parser<char> = pchar '('

    [<CompiledName("CloseParen")>]
    let pCloseParen : Parser<char> = pchar ')'

    [<CompiledName("Quote")>]
    let pQuote : Parser<char> = pchar (char 34)

    [<CompiledName("Asterisk")>]
    let pAsterisk : Parser<char> = pchar '*'

    [<CompiledName("ManySatisfy")>]
    let manySatisfy f =    
        let parse (input:CharStream) =
            let rec findLast index =
                if index = input.Length then index
                else if f (input.Item index)
                    then findLast (index + 1)
                else index

            let result = findLast 0 

            Success (input.ToString(0, result), result)

        parse

    [<CompiledName("Many1Satisfy")>]
    let many1Satisfy f =
        let p = manySatisfy f

        let doParse input = 
            match p input with
            | Success (value, next) when value.Length = 0 -> Fail 0
            | result -> result

        doParse

    [<CompiledName("ManyMinMaxSatisfy")>]
    let manyMinMaxSatisfy minCount maxCount f =
        if minCount < 0 then ArgumentException ("minCount less than 0", "minCount") |> raise 
        if maxCount < minCount then ArgumentException ("maxCount less than minCount", "maxCount") |> raise
        Contract.EndContractBlock()

        let parse (input:CharStream) =
            let rec findLast index =
                if index = input.Length then index
                else if index = maxCount then index
                else if f input.[index]
                    then findLast (index + 1)
                else index

            let index = findLast 0

            if index >= minCount then Success (input.ToString(0, index), index)
            else Fail (index - 1)

        parse

    [<CompiledName("Regex")>]
    let regex pattern =
        let pattern = "\G" + pattern
        let regex = Regex(pattern, RegexOptions.Multiline ||| RegexOptions.ExplicitCapture)

        let parse (input:CharStream) =
            let result = regex.Match(input.BackingString, input.Offset)
            if not result.Success then Fail 0
            else Success (result.Value, result.Length)

        parse

    [<CompiledName("String")>]
    let pstring (str:string) =
        let parse (input:CharStream) =
            if input.Length < str.Length
                then Fail input.Length
            else
                let rec doParse i =
                    if i = str.Length
                        then Success (str, i)
                    else if (str.Chars i) = (input.Item i)
                        then doParse (i + 1)
                    else Fail i

                doParse 0
        parse