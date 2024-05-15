module Data where

import Data.Maybe (fromMaybe)

data Op = Add | Sub | Mul | Div deriving (Eq, Show)

data Ast = Number Int | 
            BinOp Op Ast Ast | 
            Fun String Ast | 
            App Ast Ast | 
            Var String |
            With (String, Ast) Ast |
            Set String Ast |
            Seq Ast Ast
            deriving (Eq, Show)

type Loc = Int
type Env = [(String, Loc)]
type Store = [(Loc, Int)]

data Val = Numb Int | Closure String Ast Env | Void | Error String deriving (Eq, Show)
