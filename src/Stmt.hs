module Stmt where

import Grammar.Abs
import Data.Map
import Types
import Evaluator (MyEnv)
import Utils
import Main (getValueFromMemory)


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

evalStmts :: [Stmt] -> InterpreterMonad MyEnv
evalStmts [] = do
    ask

evalStmts (stmt:stmts) = do
    env' <- evalStmt stmt
    local (const env') (evalStmts stmts)


execBlock :: Block -> InterpreterMonad MyEnv -- Todo: Maybe change to MyEnv
execBlock (Block _ stmts) = do
    env <- ask
    (store, last) <- get
    env' <- local (const newEnv) $ execStmts stmts
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
    ask

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
