module Main where

import Data
import Interpreter
import Data.Maybe (isJust)
-- import Control.Monad.State

run :: Ast -> Val
run ast = let (val, _) = app (interp ast []) [] in val

runDebug :: Ast -> (Val, Store)
runDebug ast = app (interp ast []) []

-- Function to test division by zero error handling
testDivisionByZero :: Bool
testDivisionByZero = case fst $ runDebug (BinOp Div (Number 10) (Number 0)) of
                        Error _ -> True
                        _       -> False

-- Function to test invalid application error handling
testInvalidApplication :: Bool
testInvalidApplication = case fst $ runDebug (App (Number 5) (Number 3)) of
                            Error _ -> True
                            _       -> False

-- Function to run all tests
runTests :: IO ()
runTests = do
    putStrLn "Running tests..."
    let tests = [ ("Addition", BinOp Add (Number 5) (Number 3), Numb 8)
                , ("Subtraction", BinOp Sub (Number 10) (Number 7), Numb 3)
                , ("Multiplication", BinOp Mul (Number 4) (Number 6), Numb 24)
                , ("Division", BinOp Div (Number 20) (Number 5), Numb 4)
                , ("Function Application", App (Fun "x" (BinOp Add (Var "x") (Number 3))) (Number 7), Numb 10)
                , ("Division by Zero Error", BinOp Div (Number 10) (Number 0), Error "Division by zero")
                , ("Invalid Application Error", App (Number 5) (Number 3), Error "Invalid application")
                ]
    mapM_ (\(desc, ast, expected) -> do
                let (val, _) = runDebug ast
                if val == expected
                    then putStrLn $ "Test '" ++ desc ++ "' passed"
                    else putStrLn $ "Test '" ++ desc ++ "' failed: Expected " ++ show expected ++ ", got " ++ show val
            ) tests
    putStrLn "All tests completed."

-- Main function
main :: IO ()
main = runTests
