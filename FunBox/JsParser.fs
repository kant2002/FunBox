module JsParser

    open FParsec
    open System.Collections.Generic
    open System

    let ws = spaces
    let str_ws s = pstring s .>> ws
    let strCI_ws s = pstringCI s .>> ws
    let float_ws = pfloat .>> ws

    let private jsKeywords =
        [   "function"; "true"; "false"; "null"; 
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
        (JS_PRIMITIVE_EXPRESSION .>>.? logicalOperator .>>. JS_PRIMITIVE_EXPRESSION |>> flatten |>> BinaryExpression)
    let logicExpressionTerm = (primitiveLogicalExpression) <|> between (str_ws "(") (str_ws ")") JS_LOGICAL_EXPRESSION
    logicOpp.TermParser <- logicExpressionTerm

    logicOpp.AddOperator(InfixOperator("&&", ws, 1, Associativity.Left, fun x y -> BinaryExpression(x, "&&", y)))
    logicOpp.AddOperator(InfixOperator("||", ws, 1, Associativity.Left, fun x y -> BinaryExpression(x, "||", y)))
    logicOpp.AddOperator(PrefixOperator("!", ws, 3, false, fun x -> UnaryExpression("!", x)))

    let JS_EXPRESSION = JS_MATH_EXPRESSION 

    let JS_PROGRAM = JS_EXPRESSION

    let parseJs str =
        match run JS_PROGRAM str with
        | Success(result, _, _)   ->
            result
        | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg