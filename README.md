# Faux Racket Interpreter in Haskell

## Overview

This is a simple interpreter for a subset of the Racket programming language. It supports basic arithmetic operations, variable binding, function definition, and function application.

## Features

- Arithmetic Operations: Addition, subtraction, multiplication, and division of numbers.
- Variable Binding: Ability to bind variables to values using the `With` construct.
- Function Definition: Define functions using the `Fun` construct.
- Function Application: Apply functions to arguments using the `App` construct.
- Error Handling: Detects division by zero errors and reports them appropriately.

## Installation

Before building the project, make sure you have Cabal and GHC (Glasgow Haskell Compiler) installed on your system.

- Cabal: Cabal is a system for building and packaging Haskell libraries and programs. You can download and install Cabal from [here](https://www.haskell.org/cabal/download.html).

- GHC: GHC is the standard Haskell compiler. You can download and install GHC from [here](https://www.haskell.org/ghc/download.html).

## Building the Project

To build the project, navigate to the project directory in your terminal and run the following command:

```bash
cabal build
```

This will compile the project and generate the executable file in the `dist-newstyle` directory.

## Running the Code

To run the code, you can use the following command:

```bash
cabal run
```

## Usage

To use the interpreter for different ASTs, you can define your Racket expressions as Abstract Syntax Trees (ASTs) and then use the provided `run` and `runDebug` functions to interpret them.

### Example

```haskell
import RacketInterpreter

main :: IO ()
main = do
    let ast = BinOp Add (Number 5) (Number 3)
    let result = run ast
    putStrLn $ "Result: " ++ show result
```
