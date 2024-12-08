module JsParser

    open FParsec
    open System.Collections.Generic
    open System

    let whitespaces = many (skipAnyOf [' '; '\t']) |>> ignore
    let ws = spaces
    let str_ws s = pstring s .>> ws
    let float_ws = pfloat .>> ws
    let optionalSemicolon = whitespaces .>> (newline <|> pchar ';')
    //let optionalSemicolon = (lineFeed <|> skipChar ';')

    let private jsKeywords =
        [   "function"; "true"; "false"; "null"; "return"
        ] |> fun kws ->
            HashSet<string>(kws, StringComparer.Ordinal)

    let identifier =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'

        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws 
            // skips trailing whitespace
             >>=? fun ident ->
                    if jsKeywords.Contains(ident.ToString()) then
                        FParsec.Primitives.fail (sprintf "Cannot use %s as identifier" ident)
                    else
                        preturn ident

    let private stringEscape =
        choice
            [ stringReturn "\\\"" "\"" // \" -> "
              stringReturn """\\""" """\"""
              stringReturn """\/""" "/"
              stringReturn """\b""" "\b"
              stringReturn """\f""" "\f"
              stringReturn """\n""" "\n"
              stringReturn """\r""" "\r"
              stringReturn """\t""" "\t" ]

    let private stringUnicodeEscape =
        pstring """\u""" >>. tuple4 hex hex hex hex
        |>> (fun (h3, h2, h1, h0) -> Convert.ToUInt16($"{h3}{h2}{h1}{h0}", 16) |> char |> string)

    let private stringLiteral =
        (pchar '"')
        >>. manyStrings ((noneOf [ '"'; '\\' ] |>> string) <|> stringEscape <|> stringUnicodeEscape)
        .>> (pchar '"')

    let identifier_ws = identifier .>> ws

    let unwrap_identifier (part1, part2) =
        match part2 with
        | Some part2 -> ((Some part1), part2)
        | None -> (None, part1)

    let flatten v =
        match v with
        | ((a, b), c) -> (a, b, c) 
     //   | _ -> failwith "Invalid tuple to flatten"

    type JsExpression = 
        | JsNumberConstant of float
        | JsIdentifier    of string
        | JsString    of string
        | JsBool    of bool
        | JsNull
        | BinaryExpression of JsExpression * string * JsExpression
        | UnaryExpression  of string * JsExpression
        | Return of JsExpression option

    let strToBool v =
        v = "true"

    let JS_NUMBER_CONSTANT = float_ws |>> JsNumberConstant
    let JS_IDENTIFIER = identifier |>> JsIdentifier
    let JS_BOOL = (str_ws "true") <|> (str_ws "false") |>> strToBool |>> JsBool
    let JS_NULL = stringReturn "null" JsNull .>> ws
    let JS_STRING = stringLiteral |>> JsString
    let JS_PRIMITIVE_EXPRESSION = JS_NUMBER_CONSTANT <|> JS_IDENTIFIER <|> JS_BOOL <|> JS_NULL <|> JS_STRING

    let arithOpp = new OperatorPrecedenceParser<JsExpression,unit,unit>()
    let JS_MATH_EXPRESSION = arithOpp.ExpressionParser
    let arithExpressionTerm = (JS_PRIMITIVE_EXPRESSION) <|> between (str_ws "(") (str_ws ")") JS_MATH_EXPRESSION
    arithOpp.TermParser <- arithExpressionTerm

    arithOpp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, fun x y -> BinaryExpression(x, "+", y)))
    arithOpp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun x y -> BinaryExpression(x, "-", y)))
    arithOpp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, fun x y -> BinaryExpression(x, "*", y)))
    arithOpp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, fun x y -> BinaryExpression(x, "/", y)))
    arithOpp.AddOperator(PrefixOperator("-", ws, 3, false, fun x -> UnaryExpression("-", x)))
    arithOpp.AddOperator(PrefixOperator("+", ws, 3, false, fun x -> UnaryExpression("+", x)))
    arithOpp.AddOperator(PrefixOperator("++", ws, 3, false, fun x -> UnaryExpression("++", x)))
    arithOpp.AddOperator(PrefixOperator("--", ws, 3, false, fun x -> UnaryExpression("--", x)))

    let logicOpp = new OperatorPrecedenceParser<JsExpression,unit,unit>()
    let JS_LOGICAL_EXPRESSION = logicOpp.ExpressionParser
    let logicalOperator = str_ws "=" <|> str_ws "<=" <|> str_ws ">=" <|> str_ws "<>" <|> str_ws ">" <|> str_ws "<"
    let primitiveLogicalExpression =
        (JS_MATH_EXPRESSION .>>.? logicalOperator .>>. JS_MATH_EXPRESSION |>> flatten |>> BinaryExpression)
    let logicExpressionTerm = (primitiveLogicalExpression) <|> between (str_ws "(") (str_ws ")") JS_MATH_EXPRESSION
    logicOpp.TermParser <- logicExpressionTerm

    logicOpp.AddOperator(InfixOperator("&&", ws, 1, Associativity.Left, fun x y -> BinaryExpression(x, "&&", y)))
    logicOpp.AddOperator(InfixOperator("||", ws, 1, Associativity.Left, fun x y -> BinaryExpression(x, "||", y)))
    logicOpp.AddOperator(PrefixOperator("!", ws, 3, false, fun x -> UnaryExpression("!", x)))
    
    let x = opt(whitespaces >>. JS_LOGICAL_EXPRESSION)
    //let JS_RETURN = pstring "return" >>.  (opt(whitespaces >>. JS_MATH_EXPRESSION)) .>> optionalSemicolon |>> Return
    //let JS_RETURN = pstring "return" >>. optionalSemicolon |>> fun x -> Return None
    let JS_RETURN = pstring "return" >>. (opt(attempt(whitespaces >>. JS_LOGICAL_EXPRESSION))) .>> optionalSemicolon |>> Return
    //let JS_RETURN = pstring "return" >>. whitespaces >>. JS_LOGICAL_EXPRESSION .>> optionalSemicolon |>> fun x -> Return (Some x)
    let EXPRESSION_STATEMENT = JS_MATH_EXPRESSION .>> optionalSemicolon
    let JS_EXPRESSION = JS_RETURN <|> EXPRESSION_STATEMENT

    let JS_PROGRAM = many JS_EXPRESSION

    let test str =
        let p = run optionalSemicolon str 
        p

    let parseJs str =
        let str = str+";"
        match run JS_PROGRAM str with
        | Success(result, _, pos)   ->
            if pos.Index > str.Length then
                result
            else
                //failwithf "Did not consume all string.Current position %d. Leftover string '%s'" str.Length (str.Substring(int pos.Index - 1))
                result
        | Failure(errorMsg, err, _) -> failwithf "Failure: %s. \n Leftover string '%s'" errorMsg (str.Substring(int err.Position.Index - 1))