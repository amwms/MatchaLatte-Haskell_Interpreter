module Expr where

import Grammar.Abs
import Data.Map 
import Control.Monad.Reader      
import Control.Monad.Except
import Control.Monad.State

import Types
import Utils
-- import Stmt

----- OPERATIONS -----
-- data AddOp a = Plus a | Minus a
-- data MulOp a = Times a | Div a | Mod a
-- data RelOp a = Less a | LEQ a | Greater a | GEQ a | EQU a | NEQ a

evalAddOp :: Integral a1 => AddOp -> a1 -> a1 -> a1
evalAddOp (Plus _) = (+)
evalAddOp (Minus _) = (-)

evalMulOp :: Integral a1 => MulOp -> a1 -> a1 -> a1
evalMulOp (Times _) = (*)
evalMulOp (Div _) = div
evalMulOp (Mod _) = mod


evalRelOp :: Ord a1 => RelOp -> a1 -> a1 -> Bool
evalRelOp (Less _) = (<)
evalRelOp (LEQ _) = (<=)
evalRelOp (Greater _) = (>)
evalRelOp (GEQ _) = (>=)
evalRelOp (EQU _) = (==)
evalRelOp (NEQ _) = (/=)

-- ------ EXPRESSIONS -----

-- -- data Expr a
-- --     = EVar a Ident
-- --     | EInt a Integer
-- --     | ETrue a
-- --     | EFalse a
-- --     | EString a String
-- --     | ELambda a [Arg a] (Type a) (Block a)
-- --     | EApplic a Ident [Expr a]
-- --     | ENeg a (Expr a)
-- --     | ENot a (Expr a)
-- --     | EMul a (Expr a) (MulOp a) (Expr a)
-- --     | EAdd a (Expr a) (AddOp a) (Expr a)
-- --     | ERel a (Expr a) (RelOp a) (Expr a)
-- --     | EAnd a (Expr a) (Expr a)
-- --     | EOr a (Expr a) (Expr a)

-- evalExpr :: Expr -> InterpreterMonad Value
-- evalExpr (EVar _ ident) = 
--     -- env <- ask
--     -- (store, last) <- get
--     -- let loc = Data.Map.lookup ident env
--     -- case loc of
--     --     Nothing -> throwError "Variable " ++ ident ++ " does not exist"
--     --     Just n ->  return (findWithDefault 0 n store)
--     getValueFromMemory ident

-- ----- VALUE EXPRESSIONS ----
-- evalExpr (EInt _ int) = return $ VInt int
-- evalExpr (ETrue _) = return $ VBool True
-- evalExpr (EFalse _) = return $ VBool False
-- evalExpr (EString _ str) = return $ VString str
-- evalExpr (ELambda _ args retType block) = do
--     env <- ask
--     return $ VFun args retType block env

-- ------ NOT -------
-- evalExpr (ENot _ expr) = do
--     val <- evalExpr expr
--     case val of
--         VBool b -> return $ VBool $ not b
--         _ -> throwError $ "Not error - not a boolean value"

-- ------- NEG -------
-- evalExpr (ENeg _ expr) = do
--     val <- evalExpr expr
--     case val of
--         VInt i -> return $ VInt $ -i
--         _ -> throwError $ "Negation error - not an integer value"

-- ------- MUL -------
-- evalExpr (EMul _ expr1 mulOp expr2) = do
--     val1 <- evalExpr expr1
--     val2 <- evalExpr expr2
--     case (val1, val2) of
--         (VInt i1, VInt i2) -> return $ VInt $ evalMulOp mulOp i1 i2
--         _ -> throwError $ "Multiplication error - not an integer value"

-- ------- ADD -------
-- evalExpr (EAdd _ expr1 addOp expr2) = do
--     val1 <- evalExpr expr1
--     val2 <- evalExpr expr2
--     case (val1, val2, addOp) of
--         (VInt i1, VInt i2, _) -> return $ VInt $ evalAddOp addOp i1 i2
--         (VString s1, VString s2, Plus _) -> return $ VString $ s1 ++ s2
--         _ -> throwError $ "Addition error - not an integer or string value"

-- ------- REL -------
-- -- TODO can compare bool with == but not with <, <=, >=, > 
-- evalExpr (ERel _ expr1 relOp expr2) = do
--     val1 <- evalExpr expr1
--     val2 <- evalExpr expr2
--     case (val1, val2) of
--         (VInt i1, VInt i2) -> return $ VBool $ evalRelOp relOp i1 i2
--         (VBool b1, VBool b2) -> return $ VBool $ evalRelOp relOp b1 b2
--         (VString s1, VString s2) -> return $ VBool $ evalRelOp relOp s1 s2
--         _ -> throwError $ "Comparison error - not an integer, boolean or string value"

-- ------- AND -------
-- evalExpr (EAnd _ expr1 expr2) = do
--     val1 <- evalExpr expr1
--     val2 <- evalExpr expr2
--     case (val1, val2) of
--         (VBool b1, VBool b2) -> return $ VBool $ b1 && b2
--         _ -> throwError $ "And error -not a boolean value"

-- ------- OR -------
-- evalExpr (EOr _ expr1 expr2) = do
--     val1 <- evalExpr expr1
--     val2 <- evalExpr expr2
--     case (val1, val2) of
--         (VBool b1, VBool b2) -> return $ VBool $ b1 || b2
--         _ -> throwError $ "Or error - not a boolean value"

-- evalExpr (EApplic pos ident exprs) = do
--     (VFun args retType block env) <- evalExpr (EVar pos ident)
--     env' <- local (const env) evalArgs args exprs
--     env'' <- local (const env') execBlock block

--     if retType == Void then 
--         return VVoid
--     else 
--         getReturnValue env''

-- ------- APPLICATION -------
-- evalArg :: Arg -> Expr -> InterpreterMonad MyEnv
-- evalArg (ValArg _ _ ident) expr = do
--     val <- evalExpr expr
--     env <- ask
--     (store, loc) <- get
--     let newEnv = Data.Map.insert ident loc env
--     let newStore = (Data.Map.insert loc val store, loc + 1)
--     put newStore
--     return newEnv

-- evalArg (RefArg _ _ ident) expr = do
--     ident <- case expr of
--         EVar _ ident -> return ident
--         _ -> throwError "Reference error - not a variable"
--     env <- ask
--     (store, loc) <- get
--     identLoc <- getVariableLocation ident
--     let newEnv = Data.Map.insert ident identLoc env
--     return newEnv

-- evalArgs :: [Arg] -> [Expr] -> InterpreterMonad MyEnv
-- evalArgs [] [] = do
--     ask

-- evalArgs (arg:args) (expr:exprs) = do
--     env <- evalArg arg expr
--     local (const env) (evalArgs args exprs)
