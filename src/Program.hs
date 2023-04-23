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
    env' <- local (const newEnv) $ execProgComp comp
    local (const env') $ execProgComps comps


execProgram :: Program -> InterpreterMonad Int
execProgram (Program _ components) = do
    env <- ask
    (store, last) <- get
    env' <- local (const newEnv) $ execProgComps components
    return 0

-- TODO
execProgComp :: ProgComp -> InterpreterMonad MyEnv
execProgComp (FunDecl _ typ ident args block) = do
    env <- ask
    return env

execProgComp (VarDecl _ typ items) = do
    env <- ask
    return env