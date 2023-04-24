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
    -- TODO -  run all porgram components and then run Expr Application of main
    -- and return that value
    -- if hasReturn env' then do
    --     val <- getValueFromMemory env' store "return"
    --     case val of
    --         VInt i -> return $ fromInteger i
    -- else
    --     return 0 --TODO ->return main() return 
    return env'

-- TODO
execProgComp :: ProgComp -> InterpreterMonad MyEnv
execProgComp (FunDecl _ retType ident args block) = do
    env <- ask
    (store, loc) <- get
    let newEnv = Data.Map.insert ident loc env
    let newStore = (Data.Map.insert (VFun args retType block newEnv) loc store, loc + 1)
    put newStore
    return newEnv

execProgComp (VarDecl _ varType items) = do
    env <- ask
    case varType of
        Int _ -> local (const env) $ evalItems items $ VInt 0
        Str _ -> local (const env) $ evalItems items $ VStr ""
        Bool _ -> local (const env) $ evalItems items $ VBool False
        Void _ -> local (const env) $ evalItems items VVoid
        Fun _ [Type' _] (Type' _) -> local (const env) $ evalItems items $ VFun [] (Type' _) (Block _ [])
    

evalItems :: [Item] -> InterpreterMonad MyEnv
evalItems [] = do
    ask

evalItems (item : items) = do
    env <- ask
    (store, loc) <- get
    env' <- local (const env) $ evalItem item
    local (const env') $ evalItems items

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
