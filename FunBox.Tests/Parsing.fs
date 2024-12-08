module Tests

open System
open Xunit
open JsParser

[<Fact>]
let ``Number parsing`` () =
    let program = parseJs "3"
    match program with 
    | [expr] ->
        match expr with
        | JsNumberConstant nc -> 
            Assert.Equal(3., nc)
        | _ -> Assert.True(false, "Expected number constant")
    | _ -> Assert.Fail("Expected single expression")

[<Theory>]
[<InlineData("true", true)>]
[<InlineData("false", false)>]
let ``True parsing`` code expected =
    let program = parseJs code
    match program with 
    | [expr] ->
        match expr with
        | JsBool b -> 
            Assert.Equal(expected, b)
        | _ -> Assert.True(false, "Expected bool constant")
        | _ -> Assert.Fail("Expected single expression")
    | _ -> Assert.Fail("Expected single expression")

[<Fact>]
let ``Null parsing`` () =
    let program = parseJs "null"
    match program with 
    | [expr] ->
        match expr with
        | JsNull -> 
            Assert.True(true)
        | _ -> Assert.True(false, sprintf "Expected null, but %O was given" program)
    | _ -> Assert.Fail("Expected single expression")

[<Theory>]
[<InlineData("\"true\"", "true")>]
[<InlineData("\"false\"", "false")>]
[<InlineData("\"3\"", "3")>]
[<InlineData("\"\\t\"", "\t")>]
let ``String literal parsing`` code expected =
    let program = parseJs code
    match program with 
    | [expr] ->
        match expr with
        | JsString s -> 
            Assert.Equal(expected, s)
        | _ -> Assert.True(false, "Expected string constant")
    | _ -> Assert.Fail("Expected single expression")
    
    
[<Theory>]
[<InlineData("1 + 2", "+")>]
[<InlineData("1 - 2", "-")>]
[<InlineData("1 * 2", "*")>]
[<InlineData("1 / 2", "/")>]
let ``Binary math expression parsing`` code expected =
    let program = parseJs code
    match program with 
    | [expr] ->
        match expr with
        | BinaryExpression (left, op, right) -> 
            Assert.Equal(expected, op)
        | _ -> Assert.True(false, "Expected binary expression")
    | _ -> Assert.Fail("Expected single expression")

[<Theory>]
[<InlineData("+ 2", "+")>]
[<InlineData("- 2", "-")>]
[<InlineData("++ 2", "++")>]
[<InlineData("-- 2", "--")>]
let ``Unary math expression parsing`` code expected =
    let program = parseJs code
    match program with 
    | [expr] ->
        match expr with
        | UnaryExpression (op, expr) -> 
            Assert.Equal(expected, op)
        | _ -> Assert.True(false, "Expected unary expression")
    | _ -> Assert.Fail("Expected single expression")

[<Theory>]
[<InlineData("return", true)>]
[<InlineData("return   ", true)>]
[<InlineData("return 5", false)>]
[<InlineData("return 5   ", false)>]
[<InlineData("return
5", true)>]
[<InlineData("5", false)>]
let ``First return without expression`` code isNone =
    let x = test "5\n5\t"
    let program = parseJs code
    match program |> List.head with 
    | Return (expr) -> 
        Assert.Equal(isNone, expr.IsNone)
    | expr -> Assert.True(false, sprintf "Expected return expression %O given" expr)
