module Tests

open System
open Xunit
open JsParser

[<Fact>]
let ``Number parsing`` () =
    let program = parseJs "3"
    match program with 
    | JsMathExpression me -> 
        match me with
        | Primitive p -> 
            match p with
            | JsNumberConstant nc -> 
                Assert.Equal(3., nc)
            | _ -> Assert.True(false, "Expected number constant")
        | _ -> Assert.True(false, "Expected primitive expression")
    | _ -> Assert.True(false, "Expected math expression")

[<Theory>]
[<InlineData("true", true)>]
[<InlineData("false", false)>]
let ``True parsing`` code expected =
    let program = parseJs code
    match program with 
    | JsMathExpression me -> 
        match me with
        | Primitive p -> 
            match p with
            | JsBool b -> 
                Assert.Equal(expected, b)
            | _ -> Assert.True(false, "Expected bool constant")
        | _ -> Assert.True(false, "Expected primitive expression")
    | _ -> Assert.True(false, "Expected math expression")

[<Fact>]
let ``Null parsing`` () =
    let program = parseJs "null"
    match program with 
    | JsMathExpression me -> 
        match me with
        | Primitive p -> 
            match p with
            | JsNull -> 
                Assert.True(true)
            | _ -> Assert.True(false, sprintf "Expected null, but %O was given" p)
        | _ -> Assert.True(false, "Expected primitive expression")
    | _ -> Assert.True(false, "Expected math expression")

[<Theory>]
[<InlineData("\"true\"", "true")>]
[<InlineData("\"false\"", "false")>]
[<InlineData("\"3\"", "3")>]
[<InlineData("\"\\t\"", "\t")>]
let ``String literal parsing`` code expected =
    let program = parseJs code
    match program with 
    | JsMathExpression me -> 
        match me with
        | Primitive p -> 
            match p with
            | JsString s -> 
                Assert.Equal(expected, s)
            | _ -> Assert.True(false, "Expected string constant")
        | _ -> Assert.True(false, "Expected primitive expression")
    | _ -> Assert.True(false, "Expected math expression")
