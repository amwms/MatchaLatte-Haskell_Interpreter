module Expr where

import Grammar.Abs
import Types
import Data.Map

------ EXPRESSIONS -----

-- data Expr a
--     = EVar a Ident
--     | EInt a Integer
--     | ETrue a
--     | EFalse a
--     | EString a String
--     | ELambda a [Arg a] (Type a) (Block a)
--     | EApplic a Ident [Expr a]
--     | ENeg a (Expr a)
--     | ENot a (Expr a)
--     | EMul a (Expr a) (MulOp a) (Expr a)
--     | EAdd a (Expr a) (AddOp a) (Expr a)
--     | ERel a (Expr a) (RelOp a) (Expr a)
--     | EAnd a (Expr a) (Expr a)
--     | EOr a (Expr a) (Expr a)

evalExpr :: Expr -> InterpreterMonad Value
evalExpr (EVar _ ident) = do
    env <- ask
    (store, last) <- get
    let loc = Data.Map.lookup ident env
    case loc of
        Nothing -> throwError "Variable " ++ ident ++ " does not exist"
        Just n ->  return (findWithDefault 0 n store)

----- VALUE EXPRESSIONS ----
evalExpr (EInt _ int) = return $ VInt int
evalExpr (ETrue _) = return $ VBool True
evalExpr (EFalse _) = return $ VBool False
evalExpr (EString _ str) = return $ VString str
evalExpr (ELambda _ args retType block) = do
    env <- ask
    return $ VFun args retType block env

------ NOT -------
evalExpr (ENot _ expr) = do
    val <- evalExpr expr
    case val of
        VBool b -> return $ VBool $ not b
        _ -> throwError $ "Not error - not a boolean value"

------- NEG -------
evalExpr (ENeg _ expr) = do
    val <- evalExpr expr
    case val of
        VInt i -> return $ VInt $ -i
        _ -> throwError $ "Negation error - not an integer value"

------- MUL -------
evalExpr (EMul _ expr1 mulOp expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case (val1, val2) of
        (VInt i1, VInt i2) -> return $ VInt $ evalMulOp mulOp i1 i2
        _ -> throwError $ "Multiplication error - not an integer value"

------- ADD -------
evalExpr (EAdd _ expr1 addOp expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case (val1, val2, addOp) of
        (VInt i1, VInt i2, _) -> return $ VInt $ evalAddOp addOp i1 i2
        (VString s1, VString s2, Plus _) -> return $ VString $ s1 ++ s2
        _ -> throwError $ "Addition error - not an integer or string value"

------- REL -------
-- TODO can compare bool with == but not with <, <=, >=, > 
evalExpr (ERel _ expr1 relOp expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case (val1, val2) of
        (VInt i1, VInt i2) -> return $ VBool $ evalRelOp relOp i1 i2
        (VBool b1, VBool b2) -> return $ VBool $ evalRelOp relOp b1 b2
        (VString s1, VString s2) -> return $ VBool $ evalRelOp relOp s1 s2
        _ -> throwError $ "Comparison error - not an integer, boolean or string value"

------- AND -------
evalExpr (EAnd _ expr1 expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case (val1, val2) of
        (VBool b1, VBool b2) -> return $ VBool $ b1 && b2
        _ -> throwError $ "And error -not a boolean value"

------- OR -------
evalExpr (EOr _ expr1 expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case (val1, val2) of
        (VBool b1, VBool b2) -> return $ VBool $ b1 || b2
        _ -> throwError $ "Or error - not a boolean value"

------- APPLICATION -------
--- TODO 
evalExpr (EApplic _ ident exprs) = do
    -- env <- ask
    -- (store, last) <- get
    -- let loc = Data.Map.lookup ident env
    -- (VFun args retType block env) <- evalExpr (EVar _ ident)

    -- return fun