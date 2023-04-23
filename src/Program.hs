module Program where

import Grammar.Abs
import Data.Map
import Types

-- data Program a = Program a [ProgComp a]

-- data ProgComp a
--     = FunDecl a (Type a) Ident [Arg a] (Block a)
--     | VarDecl a (Type a) [Item a]

-- data Arg a = ValArg a (Type a) Ident | RefArg a (Type a) Ident

-- data Item a = NoInit a Ident | Init a Ident (Expr a)

execProgComps :: [ProgComp] -> InterpreterMonad MyEnv
execProgComps [] = do
    ask

execProgComps (comp : comps) = do
    env <- ask
    (store, last) <- get
    env' <- local (const env) $ execProgComp comp
    local (const env') $ execProgComps comps


execProgram :: Program -> InterpreterMonad Int
execProgram (Program _ components) = do
    env <- ask
    (store, last) <- get
    env' <- local (const env) $ execProgComps components
    return 0

-- TODO
execProgComp :: ProgComp -> InterpreterMonad MyEnv
execProgComp (FunDecl _ typ ident args block) = do
    env <- ask
    return env

execProgComp (VarDecl _ typ items) = do
    env <- ask
    return env

evalItem :: Item -> InterpreterMonad MyEnv
evalItem (NoInit _ ident) defaultValue = do
    env <- ask
    (store, loc) <- get
    let newEnv = Data.Map.insert ident loc env
    let newStore = (Data.Map.insert defaultValue loc store, loc + 1)
    put newStore
    return newEnv

evalItem (Init a Ident (Expr a)) _ = do
    value <- evalExpr expr
    env <- ask
    (store, loc) <- get
    let newEnv = Data.Map.insert ident loc env
    let newStore = (Data.Map.insert value loc store, loc + 1)
    put newStore
    return newEnv
