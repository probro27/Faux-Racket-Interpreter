module Interpreter where

import Data
import Data.Maybe (fromMaybe)
-- import Control.Monad.State

-- Helper function to translate arithmetic operations
opTrans :: Op -> (Int -> Int -> Maybe Int)
opTrans Add = \x y -> Just (x + y)
opTrans Sub = \x y -> Just (x - y)
opTrans Mul = \x y -> Just (x * y)
opTrans Div = \x y -> if y == 0 then Nothing else Just (div x y)

newtype State a = S (Store -> (a, Store))

app :: State a -> Store -> (a, Store)
app (S st) str = st str

instance Functor State where
    -- fmap :: (a -> b) -> State a -> State b
    fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

instance Applicative State where
    -- pure :: a -> State a
    pure x = S (\s -> (x, s))
    
    -- (<*>) :: State (a -> b) -> State a -> State b
    stf <*> stx = S (\s -> 
        let (f, s0) = app stf s in 
            let (x, s1) = app stx s0 in (f x, s1))

instance Monad State where
    -- (>>=) :: State a -> (a -> State b) -> State b
    st >>= f = S (\s -> let (x, s0) = app st s in app (f x) s0)

-- | Get a variable value from store
getVar :: Loc -> State Val
getVar loc = S (\str -> case lookup loc str of
                          Just v -> (Numb v, str)
                          Nothing -> (Error "Variable not found", str))

fromVal :: Val -> Int
fromVal (Numb v) = v
fromVal _        = error "Trying to set non-numeric value to variable"

-- -- | Set a variable value in store
setVar :: Loc -> Val -> State Val
setVar loc nv = S (\str -> (Void, (loc, fromVal nv) : filter (\(l, _) -> l /= loc) str))


-- | Generate a new location in store
newloc :: State Loc
newloc = S (\str -> (length str, str))

-- | Interpret an abstract syntax tree within an environment
interp :: Ast -> Env -> State Val
interp (Number v) _ = return $ Numb v
interp (Fun p b) e = return $ Closure p b e
interp (With (var, exp) bdy) e = interp (App (Fun var bdy) exp) e
interp (BinOp op x y) e = 
    interp x e >>= \xv ->
    interp y e >>= \yv ->
    case (xv, yv) of
        (Numb v, Numb w) ->
            case opTrans op v w of
                Just result -> return $ Numb result
                Nothing     -> return $ Error "Division by zero"
        _ -> return $ Error "Invalid operands"
interp (Seq x y) e = interp x e >> interp y e
interp (Var x) e = return (fromMaybe undefined (lookup x e)) >>= getVar
interp (App f x) e =
    interp f e >>= \fVal ->
    case fVal of
        Closure fp fb fe ->
            interp x e >>= \x' ->
            newloc >>= \nl ->
            setVar nl x' >>= \_ ->
            interp fb $ (fp, nl) : fe
        _ -> return $ Error "Invalid application"
interp (Set x y) e = do let lx = fromMaybe undefined (lookup x e)
                        nv <- interp y e
                        setVar lx nv
                        return Void
