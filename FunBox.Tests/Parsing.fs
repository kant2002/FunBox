module Tests

open System
open Xunit
open JsParser

[<Fact>]
let ``Number parsing`` () =
    let program = parseJs "3"
    match program with 
    | JsNumberConstant nc -> 
        Assert.Equal(3., nc)
    | _ -> Assert.True(false, "Expected number constant")

[<Theory>]
[<InlineData("true", true)>]
[<InlineData("false", false)>]
let ``True parsing`` code expected =
    let program = parseJs code
    match program with 
    | JsBool b -> 
        Assert.Equal(expected, b)
    | _ -> Assert.True(false, "Expected bool constant")

[<Fact>]
let ``Null parsing`` () =
    let program = parseJs "null"
    match program with 
    | JsNull -> 
        Assert.True(true)
    | _ -> Assert.True(false, sprintf "Expected null, but %O was given" program)

[<Theory>]
[<InlineData("\"true\"", "true")>]
[<InlineData("\"false\"", "false")>]
[<InlineData("\"3\"", "3")>]
[<InlineData("\"\\t\"", "\t")>]
let ``String literal parsing`` code expected =
    let program = parseJs code
    match program with 
    | JsString s -> 
        Assert.Equal(expected, s)
    | _ -> Assert.True(false, "Expected string constant")
    
    
[<Theory>]
[<InlineData("1 + 2", "+")>]
[<InlineData("1 - 2", "-")>]
[<InlineData("1 * 2", "*")>]
[<InlineData("1 / 2", "/")>]
let ``Binary math expression parsing`` code expected =
    let program = parseJs code
    match program with 
    | BinaryExpression (left, op, right) -> 
        Assert.Equal(expected, op)
    | _ -> Assert.True(false, "Expected binary expression")

[<Theory>]
[<InlineData("+ 2", "+")>]
[<InlineData("- 2", "-")>]
[<InlineData("++ 2", "++")>]
[<InlineData("-- 2", "--")>]
let ``Unary math expression parsing`` code expected =
    let program = parseJs code
    match program with 
    | UnaryExpression (op, expr) -> 
        Assert.Equal(expected, op)
    | _ -> Assert.True(false, "Expected unary expression")
