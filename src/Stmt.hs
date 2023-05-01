module Stmt where

import Grammar.Abs
import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

-- import Expr
-- import Program
import Types
import Utils

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
    env' <- execStmt stmt
    
    if hasReturn env' then
        return env'
    else
        local (const env') (execStmts stmts)

execBlock :: Block -> InterpreterMonad MyEnv -- Todo: Maybe change to MyEnv
execBlock (Block _ stmts) = do
    env <- ask
    env' <- local (const env) $ execStmts stmts
    if hasReturn env' then do
        val <- getValueFromMemory (Ident "return")
        (store, loc) <- get
        let newEnv = Data.Map.insert (Ident "return") loc env
        let newStore = (Data.Map.insert loc val store, loc + 1)
        put newStore
        return newEnv
    else
        return env

execStmt :: Stmt -> InterpreterMonad MyEnv
execStmt (Empty _) = do
    ask

execStmt (StmtBlock _ block) = do
    execBlock block

execStmt (StmtComp _ component) = do
    execProgComp component

execStmt (Assign _ ident expr) = do
    val <- evalExpr expr
    env <- ask
    (store, last) <- get
    loc <- getVariableLocation ident
    let newStore = Data.Map.insert loc val store
    put (newStore, last)
    return env

execStmt (Incr pos ident) = do
    env <- ask
    (store, last) <- get
    val <- getValueFromMemory ident
    case val of
        VInt intVal -> execStmt (Assign pos ident (EInt pos (intVal + 1)))
        _ -> throwError "Incr error - value is not an integer"

execStmt (Decr pos ident) = do
    env <- ask
    (store, last) <- get
    val <- getValueFromMemory ident
    case val of
        VInt intVal -> execStmt (Assign pos ident (EInt pos (intVal - 1)))
        _ -> throwError "Incr error - value is not an integer"

execStmt (StmtExp _ expr) = do
    evalExpr expr
    ask

execStmt (Ret _ expr) = do
    env <- ask
    if not(hasReturn env) then do
        value <- evalExpr expr
        (store, last) <- get
        let newEnv = Data.Map.insert (Ident "return") last env
        let newStore = (Data.Map.insert last value store, last + 1)
        put newStore
        return newEnv
    else
        ask

execStmt (VRet _) = do
    env <- ask
    if not(hasReturn env) then do
        (store, last) <- get
        let newEnv = Data.Map.insert (Ident "return") last env
        let newStore = (Data.Map.insert last VVoid store, last + 1)
        put newStore
        return newEnv
    else
        return env

execStmt (If _ expr block) = do
    val <- evalExpr expr
    case val of
        VBool True -> execBlock block
        VBool False -> ask
        _ -> throwError $ "If error - not a boolean value"

execStmt (IfElse _ expr block1 block2) = do
    val <- evalExpr expr
    case val of
        VBool True -> execBlock block1
        VBool False -> execBlock block2
        _ -> throwError $ "If error - not a boolean value"

execStmt while@(While _ expr block) = do
    val <- evalExpr expr
    case val of
        VBool True -> do
            env' <- execBlock block
            local (const env') $ execStmt while
        VBool False -> ask
        _ -> throwError $ "While error - not a boolean value"
