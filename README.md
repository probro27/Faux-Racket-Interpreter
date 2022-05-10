# Faux-Racket-Interpreter
A Faux-Racket-Interpreter in Haskell.

This will evaluate the expressions of Racket and compute their result. 

We support the following operators:

- numbers
- variables
- functions
- applications
- with {with (var, args) body}
- seq
- set

We are using Monads in order to abstract away the state and the output stream. 

interp :: Ast -> Env -> State Val
interp (ADD 1 2) Empty --> Empty (Numb 3)

Feel free to use!!
