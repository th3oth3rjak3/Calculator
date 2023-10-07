module Tests

open Xunit

open Swensen.Unquote

open Calculator.Lib.Application
open Calculator.Lib.Domain

[<Fact>]
let ``It should remove empty spaces from simple inputs`` () =
    removeSpaces " " |> fun result -> result =! ""


[<Fact>]
let ``It should remove spaces from complex inputs`` () =
    removeSpaces " 1 + 47 / (2 + 4) " |> fun result -> result =! "1+47/(2+4)"

[<Fact>]
let ``It should return true when operator is at index position`` () =
    "1+2" |> hasOperatorAtIndex 1 |> (fun result -> result =! true)

[<Fact>]
let ``It should return false when operator is not at index position`` () =
    "1+2" |> hasOperatorAtIndex 0 |> (fun result -> result =! false)

[<Fact>]
let ``It should apply negation to operands at the beginning of the input`` () =
    "-1+2" |> applyNegationSymbols |> (fun result -> result =! "!1+2")

[<Fact>]
let ``It should negate complex inputs`` () =
    "1+-1+2-(123)"
    |> applyNegationSymbols
    |> fun result -> result =! "1+!1+2-(123)"

[<Fact>]
let ``It should negate complex inputs with parenthesis`` () =
    "1+-1+2/-(123)"
    |> applyNegationSymbols
    |> fun result -> result =! "1+!1+2/!(123)"

[<Fact>]
let ``It should create a parenthesis string when empty`` () =
    accumulateParentheses "" '(' |> fun result -> result =! "("

[<Fact>]
let ``It should add additional lefts to the string`` () =
    accumulateParentheses "(" '(' |> fun result -> result =! "(("

[<Fact>]
let ``It should remove a left parend when a right is found`` () =
    accumulateParentheses "(" ')' |> fun result -> result =! ""

[<Fact>]
let ``It should remove a left brace when a right is found`` () =
    accumulateParentheses "[" ']' |> fun result -> result =! ""

[<Fact>]
let ``It should remove a left bracket when a right is found`` () =
    accumulateParentheses "{" '}' |> fun result -> result =! ""

[<Fact>]
let ``It should be true when parentheses are valid`` () =
    parenthesesAreValid "{[()]}" |> fun result -> result =! true

[<Fact>]
let ``It should return true when multiple sets of parends are valid`` () =
    parenthesesAreValid "()()[()]" |> fun result -> result =! true

[<Fact>]
let ``It should be false when parentheses are invalid`` () =
    parenthesesAreValid "([)" |> fun result -> result =! false

[<Fact>]
let ``Valid characters should be valid`` () =
    "1234567890+-*/!()[]{}" |> charactersAreValid |> (fun result -> result =! true)

[<Fact>]
let ``Invalid characters should be invalid`` () =
    " @#$%^&_=;:',<>?`~".ToCharArray()
    |> Array.forall (fun ch -> characterIsValid ch |> not)
    |> fun result -> result =! true

[<Fact>]
let ``Valid inputs should be Ok result`` () =
    "1+1/2" |> validateInput |> (fun result -> result =! Ok "1+1/2")

[<Fact>]
let ``Invalid parentheses inputs should be a domain error`` () =
    "1/2)" |> validateInput |> (fun result -> result =! Error MalformedInput)

[<Fact>]
let ``It should tokenize basic inputs`` () =
    "1+1"
    |> tryTokenizeInput
    |> fun result ->
        match result with
        | Ok tokens ->
            tokens
            =! [ MathOperand(Positive "1"); MathOperator Addition; MathOperand(Positive "1") ]
        | Error _ -> true =! false


[<Fact>]
let ``It should tokenize negative inputs`` () =
    "!1+!1"
    |> tryTokenizeInput
    |> fun result ->
        match result with
        | Ok tokens ->
            tokens
            =! [ MathOperand(Negative "1"); MathOperator Addition; MathOperand(Negative "1") ]
        | Error _ -> true =! false

[<Fact>]
let ``It should tokenize negative inputs with parentheses`` () =
    "!1+!(1+1)"
    |> tryTokenizeInput
    |> function
        | Ok tokens ->
            tokens
            =! [ MathOperand(Negative "1")
                 MathOperator Addition
                 MathSymbol Negation
                 MathSymbol LeftParend
                 MathOperand(Positive "1")
                 MathOperator Addition
                 MathOperand(Positive "1")
                 MathSymbol RightParend ]
        | Error _ -> true =! false

[<Fact>]
let ``It should not tokenize malformed input`` () =
    "abcd"
    |> tryTokenizeInput
    |> function
        | Ok _ -> true =! false
        | Error error -> error =! MalformedInput


[<Fact>]
let ``It should convert infix to postfix for simple expressions`` () =
    [ MathOperand(Positive "1"); MathOperator Addition; MathOperand(Positive "1") ]
    |> fun input ->
        toInfixState input (Ok []) []
        |> tryInfixToPostfix
        |> fun state -> state.output
        |> function
            | Ok output ->
                output
                =! [ MathOperand(Positive "1"); MathOperand(Positive "1"); MathOperator Addition ]
            | Error _ -> true =! false

[<Fact>]
let ``It should convert infix to postfix for complex expressions`` () =
    [ MathOperand(Positive "1"); 
      MathOperator Addition; 
      MathOperand(Positive "2");
      MathOperator Multiplication;
      MathOperand(Negative "3")]
    |> fun input ->
        toInfixState input (Ok []) []
        |> tryInfixToPostfix
        |> fun state -> state.output
        |> function
            | Ok output ->
                output
                =! [ MathOperand(Positive "1"); 
                     MathOperand(Positive "2");
                     MathOperand(Negative "3");
                     MathOperator Multiplication;
                     MathOperator Addition;]
            | Error _ -> true =! false


[<Fact>]
let ``It should calculate basic inputs`` () =
    "1+1"
    |> tryCalculate
    |> fun result -> result =! "2"


[<Fact>]
let ``It should calculate chained inputs`` () =
    "1+1-2*4/1"
    |> tryCalculate
    |> fun result -> result =! "-6"

[<Fact>]
let ``It should calculate chained inputs with symbols`` () =
    "1+(1-2)*4/1"
    |> tryCalculate
    |> fun result -> result =! "-3"

[<Fact>]
let ``It should calculate chained inputs with negatives and symbols`` () =
    "1+-(1-2)*4/1"
    |> tryCalculate
    |> fun result -> result =! "5"

[<Fact>]
let ``it should handle decimals`` () =
    "1.1 + 2.2"
    |> tryCalculate
    |> fun result -> result =! "3.3"

[<Fact>]
let ``It should do decimal division`` () =
    "2/.5"
    |> tryCalculate
    |> fun result -> result =! "4"

[<Fact>]
let ``It should handle div/0 error `` () =
    "1/0"
    |> tryCalculate
    |> fun result -> result =! "Err: Div/0"

[<Fact>]
let ``It should handle single operator`` () =
    "*"
    |> tryCalculate
    |> fun result -> result =! "Err: Operators"

[<Fact>]
let ``It should handle negative numbers in parentheses`` () =
    "4+(-5*2)"
    |> tryCalculate
    |> fun result -> result =! "-6"

[<Fact>]
let ``Too many operators should be an error`` () =
    "4+*5"
    |> tryCalculate
    |> fun result -> result =! "Err: Operators"
