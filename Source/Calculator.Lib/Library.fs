namespace Calculator.Lib

open System



module Domain =

    [<RequireQualifiedAccess>]
    module String =

        let tryItem index (input: string) =
            match input.Length with
            | 0 -> None
            | _ -> Some input[index]

        let item index (input: string) =
            input[index]
            
    type Operator =
        | Addition
        | Subtraction
        | Multiplication
        | Division
        with
            override this.ToString() =
                match this with
                | Addition -> "+"
                | Subtraction -> "-"
                | Multiplication -> "*"
                | Division -> "/"

            member this.Precedence fromStack =
                match fromStack with
                | true -> 
                    match this with 
                    | Addition | Subtraction -> 2
                    | Multiplication | Division -> 4
                | false -> 
                    match this with
                    | Addition | Subtraction -> 1
                    | Multiplication | Division -> 3

    let isOperator input =
        match input with
        | str when str = Operator.Addition.ToString() -> true
        | str when str = Operator.Subtraction.ToString() -> true
        | str when str = Operator.Multiplication.ToString() -> true
        | str when str = Operator.Division.ToString() -> true
        | _ -> false

    let isOperatorChar character =
        match character with
        | '+' | '-' | '*' | '/' -> true
        | _ -> false

    let hasOperatorAtIndex index (input: string) =
        input
        |> String.tryItem index
        |> Option.map (fun char -> isOperatorChar char)
        |> function
        | Some bool -> bool
        | None -> false

    let hasCharAtIndex index char (input:string) =
        input
        |> String.tryItem index
        |> Option.map (fun ch -> ch = char)
        |> function
        | Some bool -> bool
        | None -> false

    let hasLeftParendAtIndex index input =
        input
        |> hasCharAtIndex index '('

    let hasLeftSquareBracketAtIndex index input =
        input
        |> hasCharAtIndex index '['

    let hasLeftCurlyBraceAtIndex index input =
        input
        |> hasCharAtIndex index '{'

    let tryGetOperator input =
        match input with
        | str when str = Operator.Addition.ToString() -> Some Addition
        | str when str = Operator.Subtraction.ToString() -> Some Subtraction
        | str when str = Operator.Multiplication.ToString() -> Some Multiplication
        | str when str = Operator.Division.ToString() -> Some Division
        | _ -> None


    type Operand =
        | Positive of string
        | Negative of string
        with 
            override this.ToString() =
                match this with
                | Positive positive -> positive
                | Negative negative -> "-" + negative


    let isOperandCharacter character =
        match character with
        | ch when '0' <= ch && ch <= '9' -> true
        | ch when ch = '.' -> true
        | _ -> false

    module Operand = 

        let toPositive operand =
            match operand with
            | Positive positive -> Positive positive
            | Negative negative -> Positive negative

        let toNegative operand =
            match operand with
            | Positive positive -> Negative positive
            | Negative negative -> Negative negative

        let tryParseFloat (input: string) : float option =
            match System.Double.TryParse input with
            | true, float -> Some float
            | _ -> None

        let toDouble operand =
            match operand with 
            | Positive positive -> positive |> tryParseFloat
            | Negative negative -> negative |> tryParseFloat |> Option.map (fun flt -> flt * -1.0)

        let fromFloat value =
            match value with
            | flt when flt >= 0.0 -> value.ToString() |> Positive
            | _ -> (value * -1.0) |> fun flt -> flt.ToString() |> Negative

        let tryAddChar ch operand =
            match isOperandCharacter ch with 
            | true ->
                match operand with
                | Positive positive -> 
                    positive + ch.ToString()
                    |> Positive
                    |> Some
                | Negative negative -> 
                    negative + ch.ToString()
                    |> Negative
                    |> Some
            | false -> None

    let tryGetOperand input =
        input
        |> Operand.tryParseFloat
        |> Option.map (fun flt -> 
            match flt with 
            | float when float >= 0.0 -> flt.ToString() |> Positive
            | _ -> flt * -1.0 |> fun rslt -> rslt.ToString() |> Negative) 


    type DomainError =
        | DivByZero
        | MalformedInput
        | OperatorError
        | OperandError
        | SymbolError
        | CalculationError

    type Symbol =
        | LeftParend
        | RightParend
        | Negation
        with
            override this.ToString() =
                match this with
                | LeftParend -> "("
                | RightParend -> ")"
                | Negation -> "!"

            member this.Precedence fromStack =
                match fromStack with
                | true -> 
                    match this with
                    | LeftParend -> 0
                    | RightParend -> -1
                    | Negation -> 6
                | false -> 
                    match this with
                    | LeftParend -> 7
                    | RightParend -> 0
                    | Negation -> 5

    let tryParseSymbol character =
        match character with 
        | '(' | '[' | '{' -> Some LeftParend
        | ')' | ']' | '}' -> Some RightParend
        | '!' -> Some Negation
        | _ -> None

    type Token =
        | MathOperator of Operator
        | MathOperand of Operand
        | MathSymbol of Symbol


    let isSymbol character =
        match character with
        | '!' -> true
        | '(' | ')' -> true
        | '[' | ']' -> true
        | '{' | '}' -> true
        | _ -> false


module Application =

    open Domain

    /// <summary>
    /// Remove all spaces from the input string.
    /// </summary>
    /// <param name="inputString">The user input to remove spaces from.</param>
    let removeSpaces (inputString: string) =
        inputString
        |> String.filter (fun character -> character <> ' ')
    
    /// <summary>
    /// Apply a negation symbol ('!') instead of ('-') for negative numbers
    /// to simplify tokenization.
    /// </summary>
    /// <param name="inputString"></param>
    /// <param name="outputString"></param>
    let applyNegationSymbols (inputString: string) : string= 
        inputString.ToCharArray()
        |> Array.indexed
        |> Array.map (fun (i, char) ->
            match char with 
            | '-' when i = 0 -> '!'
            | '-' when i > 0 && ((inputString |> hasOperatorAtIndex (i - 1)) || (inputString |> hasLeftParendAtIndex(i - 1))) -> '!'
            | _ -> char)
        |> System.String
        
    /// <summary>
    /// Collect all parentheses in a string to ensure that all
    /// lefts have a matching right in the correct order.
    /// </summary>
    /// <param name="outputString">The accumulated parentheses.</param>
    /// <param name="char">The current character to validate.</param>
    let accumulateParentheses (outputString: string) (char: char) =
        match char with
        | '(' | '[' | '{' -> outputString + char.ToString()
        | ')' when outputString |> hasLeftParendAtIndex (outputString.Length - 1) -> outputString[..outputString.Length - 2]
        | ']' when outputString |> hasLeftSquareBracketAtIndex (outputString.Length - 1) -> outputString[..outputString.Length - 2]
        | '}' when outputString |> hasLeftCurlyBraceAtIndex (outputString.Length - 1) -> outputString[..outputString.Length - 2]
        | ')' | ']' | '}' -> outputString + char.ToString()
        | _ -> outputString

    /// <summary>
    /// Decide if the parentheses in the input string are valid.
    /// </summary>
    /// <param name="inputString">The user input to validate.</param>
    let parenthesesAreValid (inputString:string) =
        inputString.ToCharArray()
        |> Array.fold accumulateParentheses ""
        |> System.String
        |> fun str -> str.Length = 0
        
    /// <summary>
    /// Determine if a character is valid input.
    /// </summary>
    /// <param name="ch">The character to check.</param>
    let characterIsValid ch =
        isSymbol ch
        || isOperandCharacter ch
        || isOperatorChar ch

    /// <summary>
    /// Determine if all characters are valid.
    /// </summary>
    /// <param name="userInput">The user input to validate.</param>
    let charactersAreValid (userInput: string) =
        userInput.ToCharArray()
        |> Array.forall characterIsValid
        

    type TokenCount = {
        operators: int
        operands: int
    }

    let countTokens tokenCount token =
        match token with
        | MathOperand _ -> { tokenCount with operands = tokenCount.operands + 1 }
        | MathOperator _ -> { tokenCount with operators = tokenCount.operators + 1 }
        | _ -> tokenCount
        
    let validateOperatorCount (tokens: Token list) =
        tokens
        |> List.fold countTokens { operands = 0; operators = 0; }
        |> fun count -> count.operands = (count.operators + 1)
        |> function
        | true -> Ok tokens
        | false -> Error OperatorError
    
    /// <summary>
    /// Validate the user input.
    /// </summary>
    /// <param name="userInput">The user input string to validate.</param>
    let validateInput userInput =
        (parenthesesAreValid userInput && charactersAreValid userInput)
        |> function
            | true -> Ok userInput
            | false -> Error MalformedInput

    /// <summary>
    /// Try to get the item from the index position in the list.
    /// </summary>
    /// <param name="index">The index position to use to find the item.</param>
    /// <param name="list">The list to search through.</param>
    let tryItem index (list: 'a list)=
        match list.Length with
        | 0 -> None
        | len when len <= (list.Length - 1) -> None
        | _ -> Some list[index]


    let tryHandleOperators character =
        character.ToString()
        |> tryGetOperator
        |> Option.map MathOperator
        |> function
        | Some operator -> Ok operator
        | None -> Error OperatorError

    let addNewOperand character (tokens: Token list) =
        match character with
        | '.' -> "0."
        | _ -> character.ToString()
        |> fun str -> 
            tokens
            |> List.append <| [ MathOperand <| Positive str ]
            |> Ok

    let tryHandleOperands character (tokens: Token list): Result<Token list, DomainError> = 
        tokens
        |> tryItem (tokens.Length - 1)
        |> function
        | Some previousToken -> 
            match previousToken with
            | MathOperator _ -> 
                tokens
                |> addNewOperand character
            | MathOperand operand -> 
                operand 
                |> Operand.tryAddChar character
                |> function
                | Some updatedOperand -> 
                    tokens[..tokens.Length - 2]
                    |> List.append <| [ MathOperand updatedOperand ]
                    |> Ok
                | None -> Error OperandError  
            | MathSymbol symbol -> 
                match symbol with
                | LeftParend | RightParend -> 
                    tokens
                    |> addNewOperand character
                | Negation ->
                    Negative ""
                    |> Operand.tryAddChar character
                    |> function
                    | Some updatedOperand -> 
                        tokens[..tokens.Length - 2]
                        |> List.append <| [ MathOperand updatedOperand ]
                        |> Ok
                    | None -> Error OperandError
         | None -> 
            tokens
            |> addNewOperand character

    let tryHandleSymbol character =
        tryParseSymbol character 
        |> function
        | Some symbol -> MathSymbol symbol |> Ok
        | None -> Error SymbolError


    let charToToken (tokens: Result<Token list, DomainError>) character =
        match character with
        | ch when isOperatorChar ch -> 
            tryHandleOperators ch
            |> Result.bind (fun op -> 
                tokens 
                |> Result.map (fun tokens -> 
                    tokens
                    |> List.append <| [ op ]))
        | ch when isOperandCharacter ch -> 
            tokens |> Result.bind (tryHandleOperands character)
        | ch when isSymbol ch -> 
            tryHandleSymbol ch
            |> Result.bind (fun sym -> 
                tokens 
                |> Result.map (fun tokens -> 
                    tokens 
                    |> List.append <| [ sym ]))
        | _ -> Error MalformedInput

    type InfixToPostfixState = {
        input: Token list
        output: Result<Token list, DomainError>
        stack: Token list
    }

    let tryTokenizeInput (userInput: string): Result<Token list, DomainError> =
        userInput.ToCharArray()
        |> Array.fold charToToken (Ok [])


    let addOperandToOutput operand output =
        output
        |> Result.map (fun tokens -> 
            tokens 
            |> List.append <| [MathOperand operand])

    let addOperatorToList operator =
        fun list -> 
            list
            |> List.append <| [MathOperator operator]

    let addSymbolToList symbol =
        fun list -> 
            list
            |> List.append <| [MathSymbol symbol]

    let addSymbolToStack symbol stack =
        stack
        |> List.append <| [MathSymbol symbol]

    let addOperatorToOutput operator output =
        output
        |> Result.map (addOperatorToList operator)

    let addSymbolToOutput symbol output =
        output
        |> Result.map (addSymbolToList symbol)

    let toInfixState input output stack =
        { input = input; output = output; stack = stack }


    let handleInfixOperand operand state =
        state.output
        |> addOperandToOutput operand
        |> fun updatedOutputs -> 
            toInfixState state.input updatedOutputs state.stack

    let handleInfixOperator (operator: Operator) state =
        state.stack
        |> tryItem (state.stack.Length - 1)
        |> function
        | Some previousToken -> 
            match previousToken with
            | MathOperator previousOperator ->
                match (previousOperator.Precedence true >= operator.Precedence false) with
                | true -> 
                    state.output
                    |> addOperatorToOutput previousOperator
                    |> fun updatedOutputs -> 
                        (addOperatorToList operator state.stack[..state.stack.Length - 2])
                        |> toInfixState state.input updatedOutputs 
                | false -> 
                    (addOperatorToList operator state.stack)
                    |> toInfixState state.input state.output
            | MathSymbol previousSymbol ->
                match (previousSymbol.Precedence true >= operator.Precedence false) with
                | true -> 
                    match previousSymbol with
                    | LeftParend ->
                        toInfixState state.input state.output state.stack[..state.stack.Length - 2]
                    | RightParend |  Negation -> 
                        state.output
                        |> addSymbolToOutput previousSymbol
                        |> fun updatedOutputs -> 
                            (addOperatorToList operator state.stack[..state.stack.Length - 2])
                            |> toInfixState state.input updatedOutputs 
                | false -> 
                    (addOperatorToList operator state.stack)
                    |> toInfixState state.input state.output
            | MathOperand _ -> 
                (addOperatorToList operator state.stack)
                |> toInfixState state.input state.output
        | None -> 
            (addOperatorToList operator state.stack)
            |> toInfixState state.input state.output

    let handleInfixSymbol (symbol: Symbol) state =
        state.stack
        |> tryItem (state.stack.Length - 1)
        |> function
        | Some previousToken -> 
            match previousToken with
            | MathOperator previousOperator ->
                match (previousOperator.Precedence true >= symbol.Precedence false) with
                | true -> 
                    state.output
                    |> Result.map (addOperatorToList previousOperator)
                    |> fun updatedOutputs -> 
                        (state.stack[..state.stack.Length - 2])
                        |> addSymbolToList symbol
                        |> toInfixState state.input updatedOutputs 
                | false -> 
                    state.stack
                    |> addSymbolToList symbol
                    |> toInfixState state.input state.output 
            | MathSymbol previousSymbol ->
                match (previousSymbol.Precedence true >= symbol.Precedence false) with
                | true -> 
                    state.output
                    |> Result.map (addSymbolToList previousSymbol)
                    |> fun updatedOutputs -> 
                        (state.stack[..state.stack.Length - 2])
                        |> addSymbolToList symbol
                        |> toInfixState state.input updatedOutputs 
                | false -> 
                    state.stack
                    |> addSymbolToList symbol
                    |> toInfixState state.input state.output 
            | MathOperand _ -> 
                state.stack
                |> addSymbolToStack symbol
                |> toInfixState state.input state.output 
        | None -> 
            state.stack
            |> addSymbolToStack symbol
            |> toInfixState state.input state.output

    let popTokenToOutput state =
        state.stack
        |> tryItem (state.stack.Length - 1)
        |> function
        | Some token ->
            state.output
            |> Result.map (fun tokens -> tokens |> List.append <| [ token ])
            |> fun updatedOutput -> 
                (state.stack[..state.stack.Length - 2])
                |> toInfixState state.input updatedOutput
        | None -> state

    let rec tryInfixToPostfix state =

        match state.input with
        | [] -> 
            match state.stack with
            | [] -> state
            | _ -> 
                popTokenToOutput state
                |> tryInfixToPostfix
        | head::xs -> 
            match head with
            | MathOperand operand -> 
                toInfixState xs state.output state.stack
                |> handleInfixOperand operand
            | MathOperator operator -> 
                toInfixState xs state.output state.stack
                |> handleInfixOperator operator
            | MathSymbol symbol -> 
                toInfixState xs state.output state.stack
                |> handleInfixSymbol symbol
            |> tryInfixToPostfix 

    type SolvePostfixState = {
        input: Token list;
        stack: Result<Token list, DomainError>;
    }

    let toPostfixState input stack : SolvePostfixState =
        { input = input; stack = stack; }
   
    let addTokenToStack token stack =
        stack |> List.append <| [ token ]

    let toDoubles (token1, token2) =
        match (token1, token2) with
        | (MathOperand val1, MathOperand val2) -> 
            val1 
            |> Operand.toDouble 
            |> function 
            | Some dbl -> Ok dbl 
            | None -> Error CalculationError
            |> Result.bind (fun val1 -> 
                val2 
                |> Operand.toDouble
                |> function
                | Some dbl -> Ok (val1, dbl)
                | None -> Error CalculationError) 
        | (_ , _) -> Error CalculationError

    let doMath operator state =
        state.stack
        |> Result.bind (fun stack -> 
            (stack[stack.Length - 2], stack[stack.Length - 1])
            |> toDoubles
            |> Result.bind (fun (val1, val2) -> 
                match operator with
                | Addition -> val1 + val2 |> Ok
                | Subtraction -> val1 - val2 |> Ok
                | Multiplication -> val1 * val2 |> Ok
                | Division -> 
                    match val2 with
                    | 0.0 -> Error DivByZero
                    | _ -> Ok (val1 / val2))
            |> Result.map (fun flt -> flt |> Operand.fromFloat |> MathOperand))
        |> Result.bind (fun mathResult -> 
            state.stack
            |> Result.map (fun stack -> 
                { state with stack = Ok (stack[..stack.Length - 3] |> List.append <| [ mathResult ])}))


    let handleMathSymbols symbol state =
        match symbol with
        | Negation ->
            state.stack
            |> Result.map (fun stack -> 
                stack
                |> tryItem (stack.Length - 1)
                |> function
                | Some token -> 
                    match token with
                    | MathOperand operand -> 
                        match operand with
                        | Positive _ -> operand |> Operand.toNegative
                        | Negative _ -> operand |> Operand.toPositive
                        |> MathOperand
                    | _ -> token
                    |> fun updatedToken -> 
                        { state with stack = Ok (stack[..stack.Length - 2] |> List.append <| [ updatedToken ] ) }
                | None -> { state with stack = Error CalculationError } )
            |> function
            | Ok state -> state
            | Error err -> { state with stack = Error err }
        | _ -> state

    let rec trySolvePostfix state =
        match state.input with
        | [] -> 
            state.stack
            |> Result.map (fun stack -> 
                match stack.Length with 
                | 1 -> state
                | _ -> { state with stack = Error CalculationError })
        | head::xs -> 
            match head with
            | MathOperand _ -> 
                toPostfixState xs state.stack
                |> fun state -> 
                    state.stack
                    |> Result.map (fun stack -> addTokenToStack head stack)
                    |> Result.map (fun stack -> toPostfixState xs (Ok stack))
                    |> function
                    | Ok state -> state
                    | Error err -> {state with stack = Error err }
            | MathOperator operator -> 
                toPostfixState xs state.stack
                |> doMath operator
                |> function
                | Ok state -> state
                | Error err -> { state with stack = Error err; input = [] }
            | MathSymbol symbol -> 
                toPostfixState xs state.stack
                |> handleMathSymbols symbol
            |> trySolvePostfix 


    let toScientificNotation (input: string) =
        input.ToCharArray()
        |> fun chars -> 
            chars
            |> Array.tryFindIndex (fun ch -> ch = '.')
            |> function
            | Some decimalIndex -> 
                chars 
                |> Array.removeAt decimalIndex
                |> Array.insertAt 1 '.'
                |> fun chars -> chars[..6]
                |> System.String
                |> fun str -> str + " * 10^" + (decimalIndex - 1).ToString()
            | None -> 
                chars
                |> Array.insertAt 1 '.'
                |> fun chars -> chars[..6]
                |> System.String
                |> fun str -> str + " * 10^" + (input.Length - 1).ToString()
    
    let tryCalculate(userInput: string): string =
        match userInput with 
        | "" -> "0"
        | _ -> 
        removeSpaces userInput
        |> applyNegationSymbols
        |> validateInput
        |> Result.bind tryTokenizeInput
        |> Result.bind validateOperatorCount
        |> Result.bind (fun inputs -> 
            toInfixState inputs (Ok []) []
            |> tryInfixToPostfix
            |> fun state -> state.output)
        |> Result.bind (fun inputs -> 
            toPostfixState inputs (Ok [])
            |> trySolvePostfix)
        |> Result.bind (fun postfixState -> 
            postfixState.stack
            |> Result.bind(fun stack -> 
                stack 
                |> tryItem 0 
                |> function 
                | Some token -> 
                    match token with
                    | MathOperand op -> 
                        op 
                        |> Operand.toDouble 
                        |> function
                        | Some db -> db |> Ok
                        | None -> Error CalculationError
                    | _ -> Error CalculationError
                | None -> Error CalculationError ))
        |> fun result -> 
            match result with
            | Ok flt -> 
                Math.Round(flt, 10)
                |> string
                |> fun str -> 
                    match str.Length with
                    | len when len <= 16 -> str
                    | _ -> str |> toScientificNotation |> string
            | Error err -> 
                match err with
                | CalculationError -> "Err: Calculation"
                | DivByZero -> "Err: Div/0"
                | MalformedInput -> "Err: Bad Input"
                | OperandError -> "Err: Operands"
                | OperatorError -> "Err: Operators"
                | SymbolError -> "Err: Symbols"
