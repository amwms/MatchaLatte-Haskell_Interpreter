module Stmt where

import Grammar.Abs
import Data.Map
import Types
import Evaluator (MyEnv)
import Utils
import Main (getValueFromMemory)
import Control.Monad.Trans.RWS (ask)


----- STMTS -----
-- data Block a = Block a [Stmt' a]
-- data Stmt a
--     = Empty a
--     | StmtBlock a (Block a)
--     | StmtComp a (ProgComp a)
--     | Assign a Ident (Expr a)
--     | Incr a Ident
--     | Decr a Ident
--     | StmtExp a (Expr a)
--     | Ret a (Expr a)
--     | VRet a
--     | If a (Expr a) (Block a)
--     | IfElse a (Expr a) (Block a) (Block a)
--     | While a (Expr a) (Block a)

execStmts :: [Stmt] -> InterpreterMonad MyEnv
execStmts [] = do
    ask

execStmts (stmt : stmts) = do
    env' <- evalStmt stmt
    local (const env') (evalStmts stmts)


execBlock :: Block -> InterpreterMonad MyEnv -- Todo: Maybe change to MyEnv
execBlock (Block _ stmts) = do
    env <- ask
    (store, last) <- get
    env' <- local (const env) $ execStmts stmts
    return env'


execStmt :: Stmt -> InterpreterMonad MyEnv
execStmt (Empty _) = do
    ask

execStmt (StmtBlock _ block) = do
    execBlock block
    ask

execStmt (StmtComp _ component) = do
    execProgComp component

execStmt (Assign _ ident expr) = do
    val <- evalExpr expr
    env <- ask
    (store, last) <- get
    loc <- case Data.Map.lookup ident env of
        Just loc -> return loc
        Nothing -> throwError $ "Variable " ++ show ident ++ " not found"
    let newStore = Data.Map.insert loc val store 
    put (newStore, last)
    return env

execStmt (Incr _ ident) = do
    env <- ask
    (store, last) <- get
    let val = getValueFromMemory env store ident
    return execStmt (Assign _ ident (EInt _ (val + 1)))

execStmt (Decr _ ident) = do
    env <- ask
    (store, last) <- get
    let val = getValueFromMemory env store ident
    return execStmt (Assign _ ident (EInt _ (val - 1)))

execStmt (StmtExp _ expr) = do
    evalExpr expr
    ask

-- TODO
execStmt (Ret _ expr) = do
    value <- evalExpr expr
    env <- ask
    (store, last) <- get
    let newEnv = Data.Map.insert "return" last env
    let newStore = (Data.Map.insert value last store, last + 1)
    put newStore
    return newEnv

-- TODO
execStmt (VRet _) = do
    env <- ask
    (store, last) <- get
    let newEnv = Data.Map.insert "return" last env
    let newStore = (Data.Map.insert VVoid last store, last + 1)
    put newStore
    return newEnv


execStmt (If _ expr block) = do
    val <- evalExpr expr
    case val of
        VBool True -> execBlock block
        VBool False -> ask
        _ -> throwError $ "If error - " ++ val ++ " is not a boolean value"

execStmt (IfElse _ expr block1 block2) = do
    val <- evalExpr expr
    case val of
        VBool True -> execBlock block1
        VBool False -> execBlock block2
        _ -> throwError $ "If error - " ++ val ++ " is not a boolean value"

execStmt (While _ expr block) = do
    val <- evalExpr expr
    case val of
        VBool True -> do
            execBlock block
            execStmt (While _ expr block)
        VBool False -> ask
        _ -> throwError $ "While error - " ++ val ++ " is not a boolean value"  
