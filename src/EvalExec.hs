module EvalExec where

import Grammar.Abs
import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

import Expr
import Types
import Utils
import Errors (divisionByZeroError)
import TypeChecker.CheckerUtils (showPosition)

---------------------------------
---------- STATEMENTS -----------
---------------------------------

execStmts :: [Stmt] -> InterpreterMonad MyEnv
execStmts [] = do
    ask

execStmts (stmt : stmts) = do
    env' <- execStmt stmt
    if hasReturn env' then
        return env'
    else
        local (const env') (execStmts stmts)

execBlock :: Block -> InterpreterMonad MyEnv
execBlock (Block _ stmts) = do
    env <- ask
    env' <- local (const env) $ execStmts stmts
    if hasReturn env' then do
        val <- local (const env') $ getValueFromMemory (Ident "return")
        (store, last) <- get

        let newEnv = Data.Map.insert (Ident "return") last env
        let newStore = (Data.Map.insert last val store, last + 1)

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
    val <- getValueFromMemory ident
    case val of
        VInt intVal -> execStmt (Assign pos ident (EInt pos (intVal - 1)))
        _ -> throwError "Decr error - value is not an integer"

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
        _ -> throwError "If error - not a boolean value"

execStmt (IfElse _ expr block1 block2) = do
    val <- evalExpr expr
    case val of
        VBool True -> execBlock block1
        VBool False -> execBlock block2
        _ -> throwError "If error - not a boolean value"

execStmt while@(While _ expr block) = do
    val <- evalExpr expr
    case val of
        VBool True -> do
            env' <- execBlock block
            if hasReturn env' then
                return env'
            else
                local (const env') $ execStmt while
        VBool False -> ask
        _ -> throwError "While error - not a boolean value"

execStmt (Print _ expr) = do
    val <- evalExpr expr
    liftIO $ putStrLn $ showValue val
    ask


---------------------------------
----------- EXPRESSIONS ---------
---------------------------------

evalExpr :: Expr -> InterpreterMonad Value
evalExpr (EVar _ ident) = 
    getValueFromMemory ident

evalExpr (EInt _ int) = return $ VInt int
evalExpr (ETrue _) = return $ VBool True
evalExpr (EFalse _) = return $ VBool False
evalExpr (EString _ str) = return $ VString str
evalExpr (ELambda _ args retType block) = do
    env <- ask
    return $ VFun args retType block env

evalExpr (ENot _ expr) = do
    val <- evalExpr expr
    case val of
        VBool b -> return $ VBool $ not b
        _ -> throwError "Not error - not a boolean value"

evalExpr (ENeg _ expr) = do
    val <- evalExpr expr
    case val of
        VInt i -> return $ VInt $ -i
        _ -> throwError $ "Negation error - not an integer value"

evalExpr (EMul pos expr1 mulOp expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case (mulOp, val2) of
        (Div _, VInt 0) -> throwError $ divisionByZeroError pos
        (Mod _, VInt 0) -> throwError $ divisionByZeroError pos
        _ -> case (val1, val2) of
                (VInt i1, VInt i2) -> return $ VInt $ evalMulOp mulOp i1 i2
                _ -> throwError $ "Multiplication error - not an integer value"

evalExpr (EAdd _ expr1 addOp expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case (val1, val2, addOp) of
        (VInt i1, VInt i2, _) -> return $ VInt $ evalAddOp addOp i1 i2
        (VString s1, VString s2, Plus _) -> return $ VString $ s1 ++ s2
        _ -> throwError "Addition error - not an integer or string value"

evalExpr (ERel _ expr1 relOp expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case (val1, val2, relOp) of
        (VInt i1, VInt i2, _) -> return $ VBool $ evalRelOp relOp i1 i2
        (VBool b1, VBool b2, EQU _) -> return $ VBool $ evalRelOp relOp b1 b2
        (VString s1, VString s2, _) -> return $ VBool $ evalRelOp relOp s1 s2
        _ -> throwError "Comparison error - not an integer, boolean or string value"

evalExpr (EAnd _ expr1 expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case (val1, val2) of
        (VBool b1, VBool b2) -> return $ VBool $ b1 && b2
        _ -> throwError "And error -not a boolean value"

evalExpr (EOr _ expr1 expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case (val1, val2) of
        (VBool b1, VBool b2) -> return $ VBool $ b1 || b2
        _ -> throwError "Or error - not a boolean value"

------- APPLICATION -------
evalExpr (EApplic pos ident exprs) = do
    (VFun args retType block funEnv) <- getValueFromMemory ident
    outsideEnv <- ask
    env' <- local (const funEnv) $ evalArgs pos args exprs outsideEnv
    env'' <- local (const env') $ execBlock block

    if hasReturn env'' then 
        getReturnValue env''
    else 
        case retType of
            (Void _) -> return VVoid
            _ -> throwError $ "No return value for function " ++ show ident
        
evalArg :: Arg -> Expr -> MyEnv -> InterpreterMonad MyEnv
evalArg (ValArg _ _ ident) expr outsideEnv = do
    val <- local (const outsideEnv) $ evalExpr expr
    env <- ask
    (store, loc) <- get

    let newEnv = Data.Map.insert ident loc env
    let newStore = (Data.Map.insert loc val store, loc + 1)
    put newStore
    return newEnv

evalArg (RefArg _ _ ident) expr outsideEnv = do
    outsideIdent <- case expr of
        EVar _ ident -> return ident
        _ -> throwError "Reference error - not a variable"

    env <- ask
    (store, loc) <- get
    identLoc <- local (const outsideEnv) $ getVariableLocation outsideIdent
    let newEnv = Data.Map.insert ident identLoc env
    return newEnv

evalArgs :: BNFC'Position -> [Arg] -> [Expr] -> MyEnv -> InterpreterMonad MyEnv
evalArgs _ [] [] outsideEnv = do
    ask

evalArgs pos (arg : args) (expr : exprs) outsideEnv = do
    env <- evalArg arg expr outsideEnv
    local (const env) $ evalArgs pos args exprs outsideEnv

evalArgs pos _ _ _ = throwError $ "Application error - wrong number of arguments at " ++ showPosition pos


---------------------------------
------------ PROGRAM ------------
---------------------------------

execProgram :: Program -> InterpreterMonad Integer
execProgram (Program pos components) = do
    env <- ask
    env' <- local (const env) $ execProgComps components
    (store, _) <- get

    intVal <- local (const env') $ evalExpr (EApplic pos (Ident "main") [])
    case intVal of
        VInt i -> return i
        _ -> throwError "Main function must return an integer value"

execProgComps :: [ProgComp] -> InterpreterMonad MyEnv
execProgComps [] = do
    ask

execProgComps (comp : comps) = do
    env <- ask
    (store, _) <- get
    env' <- local (const env) $ execProgComp comp
    local (const env') $ execProgComps comps

execProgComp :: ProgComp -> InterpreterMonad MyEnv
execProgComp (FunDecl _ retType ident args block) = do
    env <- ask
    (store, loc) <- get
    let newEnv = Data.Map.insert ident loc env
    let newStore = (Data.Map.insert loc (VFun args retType block newEnv) store, loc + 1)
    put newStore
    return newEnv

execProgComp (VarDecl _ varType items) = do
    env <- ask
    case varType of
        Int _ -> local (const env) $ evalItems items $ VInt 0
        Str _ -> local (const env) $ evalItems items $ VString ""
        Bool _ -> local (const env) $ evalItems items $ VBool False
        Void _ -> local (const env) $ evalItems items VVoid 
        (Fun x argTypes retType) -> local (const env) $ evalItems items $ VFun [] retType (Block x [Empty x]) env

evalItems :: [Item] -> Value -> InterpreterMonad MyEnv
evalItems [] _ = do
    ask

evalItems (item : items) defaultValue = do
    env <- ask
    (store, _) <- get
    env' <- local (const env) $ evalItem item defaultValue
    local (const env') $ evalItems items defaultValue

evalItem :: Item -> Value -> InterpreterMonad MyEnv
evalItem (NoInit _ ident) defaultValue = do
    env <- ask
    (store, loc) <- get
    let newEnv = Data.Map.insert ident loc env
    let newStore = (Data.Map.insert loc defaultValue store, loc + 1)
    put newStore
    return newEnv

evalItem (Init _ ident expr) _ = do
    value <- evalExpr expr
    env <- ask
    (store, loc) <- get
    let newEnv = Data.Map.insert ident loc env
    let newStore = (Data.Map.insert loc value store, loc + 1)
    put newStore
    return newEnv
