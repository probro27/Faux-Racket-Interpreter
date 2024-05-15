# Faux Racket Interpreter

## Overview

This is a simple interpreter for a subset of the Racket programming language. It supports basic arithmetic operations, variable binding, function definition, and function application.

## Features

- Arithmetic Operations: Addition, subtraction, multiplication, and division of numbers.
- Variable Binding: Ability to bind variables to values using the `With` construct.
- Function Definition: Define functions using the `Fun` construct.
- Function Application: Apply functions to arguments using the `App` construct.
- Error Handling: Detects division by zero errors and reports them appropriately.

## Usage

To use the interpreter, you can define your Racket expressions as Abstract Syntax Trees (ASTs) and then use the provided `run` and `runDebug` functions to interpret them.

### Example

```haskell
import RacketInterpreter

main :: IO ()
main = do
    let ast = BinOp Add (Number 5) (Number 3)
    let result = run ast
    putStrLn $ "Result: " ++ show result
```
