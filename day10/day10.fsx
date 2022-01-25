open System.IO

type Token =
    | OpenParen
    | OpenBracket
    | OpenBrace
    | OpenPointy
    | CloseParen
    | CloseBracket
    | CloseBrace
    | ClosePointy

type LineType =
    | Corrupted of Token
    | Incomplete of list<Token>

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let tokenize =
    function
    | '(' -> OpenParen
    | '[' -> OpenBracket
    | '{' -> OpenBrace
    | '<' -> OpenPointy
    | ')' -> CloseParen
    | ']' -> CloseBracket
    | '}' -> CloseBrace
    | '>' -> ClosePointy
    | c -> failwithf "bad parse: %c" c

let untokenize =
    function
    | OpenParen -> '('
    | OpenBracket -> '['
    | OpenBrace -> '{'
    | OpenPointy -> '<'
    | CloseParen -> ')'
    | CloseBracket -> ']'
    | CloseBrace -> '}'
    | ClosePointy -> '>'

let closes token1 token2 =
    match (token1, token2) with
    | OpenParen, CloseParen
    | OpenBracket, CloseBracket
    | OpenBrace, CloseBrace
    | OpenPointy, ClosePointy -> true
    | _ -> false

let flipToken =
    function
    | OpenParen -> CloseParen
    | OpenBracket -> CloseBracket
    | OpenBrace -> CloseBrace
    | OpenPointy -> ClosePointy
    | CloseParen -> OpenParen
    | CloseBracket -> OpenBracket
    | CloseBrace -> OpenBrace
    | ClosePointy -> OpenPointy

let rec autocomplete =
    function
    | token :: tokens -> flipToken token :: autocomplete tokens
    | tokens -> tokens

let processLine line =
    match List.ofSeq line with
    | c :: line ->
        let rec go tokens =
            function
            | c :: line ->
                let token = tokenize c

                match token with
                | OpenParen
                | OpenBracket
                | OpenBrace
                | OpenPointy -> go (token :: tokens) line
                | _ ->
                    match tokens with
                    | openingToken :: tokens when token |> closes openingToken -> go tokens line
                    | _ -> Corrupted token
            | [] -> Incomplete(autocomplete tokens)

        go [ tokenize c ] line
    | _ -> failwith "empty line"

let lines = File.ReadAllLines fileName

let scoreSyntaxError =
    function
    | CloseParen -> 3
    | CloseBracket -> 57
    | CloseBrace -> 1197
    | ClosePointy -> 25137
    | token -> failwithf "bad closing token: %c" (untokenize token)

let scoreAutocomplete tokens =
    let s =
        function
        | CloseParen -> 1I
        | CloseBracket -> 2I
        | CloseBrace -> 3I
        | ClosePointy -> 4I
        | token -> failwithf "bad closing token: %c" (untokenize token)

    let rec go score =
        function
        | token :: tokens ->
            let newScore = (score * 5I) + s token
            go newScore tokens
        | _ -> score

    go 0I tokens

let processedLines = lines |> Array.map processLine

processedLines
|> Seq.choose (function
    | Corrupted token -> Some token
    | _ -> None)
|> Seq.map scoreSyntaxError
|> Seq.reduce (+)
|> printfn "The total syntax error score is %d points"

let autocompleteScores =
    processedLines
    |> Seq.choose (function
        | Incomplete tokens -> Some tokens
        | _ -> None)
    |> Seq.map scoreAutocomplete
    |> Seq.sort

Seq.item (Seq.length autocompleteScores / 2) autocompleteScores
|> printfn "The middle autocomplete score is %A points"
