module GrammarChecker where

import Grammar.Abs
import Data.Map
import Control.Monad.Reader
import Control.Monad.Except

import Expr
import CheckerTypes
import CheckerUtils

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

checkStmts :: [Stmt] -> TypeCheckerMonad TypeEnv
checkStmts [] = do
    ask

checkStmts (stmt : stmts) = do
    env' <- checkStmt stmt
    local (const env') (checkStmts stmts)

checkBlock :: Block -> TypeCheckerMonad TypeEnv 
checkBlock (Block _ stmts) = do
    env <- ask
    env' <- local (const env) $ checkStmts stmts
    if hasReturn env' then do
        valType <- local (const env') $ getVariableLocation (Ident "return")
        let newEnv = Data.Map.insert (Ident "return") valType last env
        return newEnv
    else
        return env

checkStmt :: Stmt -> TypeCheckerMonad TypeEnv
checkStmt (Empty _) = do
    ask

checkStmt (StmtBlock _ block) = do
    checkBlock block

checkStmt (StmtComp _ component) = do
    checkProgComp component

checkStmt (Assign pos ident expr) = do
    env <- ask
    exprType <- evalTypeExpr expr
    varType <- getVariableType pos ident

    if varType == exprType then
        return env
    else
        throwError $ "Assign error (" ++ show pos ++ ") - variable " ++ show ident ++ " is of type " ++ show varType ++ " but expression is of type " ++ show exprType

checkStmt (Incr pos ident) = do
    env <- ask

    valType <- getVariableType pos ident
    case valType of
        Int _ -> return env
        _ -> throwError "Increment error (" ++ show pos ++ ") - variable " ++ show ident ++ " is of type " ++ show  valType ++ " and not an integer"

checkStmt (Decr pos ident) = do
    env <- ask

    valType <- getVariableType pos ident
    case valType of
        Int _ -> return env
        _ -> throwError "Decrement error (" ++ show pos ++ ") - variable " ++ show ident ++ " is of type " ++ show  valType ++ " and not an integer"

checkStmt (StmtExp _ expr) = do
    checkExpr expr
    ask

checkStmt (Ret pos expr) = do
    env <- ask
    valType <- checkExpr expr
    if not(hasReturn env) then do
        let newEnv = Data.Map.insert (Ident "return") valType env
        return newEnv
    else
        if valType == getReturnType env then
            return env
        else
            throwError $ "Return error - different return types in function (" ++ show pos ++ ")"

checkStmt (VRet pos) = do
    env <- ask
    if not(hasReturn env) then do
        let newEnv = Data.Map.insert (Ident "return") (Void pos) env
        return newEnv
    else
        if valType == getReturnType env then
            return env
        else
            throwError $ "Return error - different return types in function (" ++ show pos ++ ")"

checkStmt (If pos expr block) = do
    valType <- checkExpr expr
    case valType of
        Bool _ -> checkBlock block
        _ -> throwError $ "If error - expression in pos " ++ show pos ++ " is not a boolean value"

checkStmt (IfElse _ expr block1 block2) = do
    valType <- checkExpr expr
    case valType of
        Bool _ -> checkBlock block1 >> checkBlock block2
        _ -> throwError $ "If error - expression in pos " ++ show pos ++ " is not a boolean value"

checkStmt (While _ expr block) = do
    val <- checkExpr expr
    case val of
        Bool _ -> checkBlock block
        _ -> throwError $ "While error - expression in pos " ++ show pos ++ " is not a boolean value"

checkStmt (Print _ expr) = do
    valType <- checkExpr expr
    case valType of
        Int _ -> ask
        Bool _ -> ask
        String _ -> ask
        Void _ -> ask
        _ -> throwError "Print error - expression in pos " ++ show pos ++ " is not a printable value"


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

checkExpr :: Expr -> TypeCheckerMonad Type
checkExpr (EVar pos ident) = 
    getVariableType pos ident

----- VALUE EXPRESSIONS ----
checkExpr (EInt pos _) = return $ Int pos
checkExpr (ETrue pos) = return $ Bool pos
checkExpr (EFalse pos) = return $ Bool pos
checkExpr (EString pos _) = return $ String pos

checkExpr (ELambda pos args retType block) = do
    checkFunction pos args retType block 
    return $ Fun pos args retType

------ NOT -------
checkExpr (ENot _ expr) = do
    valType <- checkExpr expr
    case (valType) of
        (Bool _) -> return $ Bool pos
        _ -> throwError $ "Not error in position (" ++ show pos ++ ") - " ++ show valType ++ " is not an boolean value"

------- NEG -------
checkExpr (ENeg pos expr) = do
    valType <- checkExpr expr
    case (valType) of
        (Int _) -> return $ Int pos
        _ -> throwError $ "Negation error in position (" ++ show pos ++ ") - " ++ show valType ++ " is not an integer value"

------- MUL -------
checkExpr (EMul _ expr1 mulOp expr2) = do
    valType1 <- checkExpr expr1
    valType2 <- checkExpr expr2
    case (valType1, valType2) of
        (Int _, Int _, _) -> return $ Int pos
        _ -> throwError $ "Multiplication error - cannot multiply/divide types " ++ show valType1 ++ " and " ++ show valType2 ++ " in position (" ++ show pos ++ ")"

------- ADD -------
checkExpr (EAdd pos expr1 addOp expr2) = do
    valType1 <- checkExpr expr1
    valType2 <- checkExpr expr2
    case (valType1, valType2, addOp) of
        (Int _, Int _, _) -> return $ Int pos
        (String _, String _, Plus _) -> return $ String pos
        _ -> throwError $ "Addition error - cannot add/subtract types " ++ show valType1 ++ " and " ++ show valType2 ++ " in position (" ++ show pos ++ ")"

------- REL -------
-- TODO can compare bool with == but not with <, <=, >=, > 
checkExpr (ERel pos expr1 relOp expr2) = do
    valType1 <- checkExpr expr1
    valType2 <- checkExpr expr2
    case (valType1, valType2, relOp) of
        (Int _, Int _, _) -> return $ Bool pos
        (Bool _, Bool _, EQU _) -> return $ Bool pos
        (String _, String _, _) -> return $ Bool pos
        _ -> throwError $ "Comparison error in position (" ++ show pos ++ ") - error with types " ++ show valType1 ++ " and " ++ show valType2

------- AND -------
checkExpr (EAnd pos expr1 expr2) = do
    valType1 <- checkExpr expr1
    valType2 <- checkExpr expr2
    case (valType1, valType2) of
        (Bool _, Bool _) -> return $ Bool pos
        _ -> throwError $ "And error in position (" ++ show pos ++ ") - " ++ show valType1 ++ " and " ++ show valType2 ++ " are not both boolean values"

------- OR -------
checkExpr (EOr _ expr1 expr2) = do
    valType1 <- checkExpr expr1
    valType2 <- checkExpr expr2
    case (valType1, valType2) of
        (Bool _, Bool _) -> return $ Bool pos
        _ -> throwError $ "Or error in position (" ++ show pos ++ ") - " ++ show valType1 ++ " and " ++ show valType2 ++ " are not both boolean values"

checkExpr (EApplic pos ident exprs) = do
    (Fun args retType) <- checkExpr (EVar pos ident)
    checkArgs args exprs
    return retType

checkArg :: Arg -> Expr -> MyEnv -> TypeCheckerMonad ()
checkArg (ValArg pos argType _) expr outsideEnv = do
    exprType <- local (const outsideEnv) $ checkExpr expr
    if exprType == argType then
        return ()
    else
        throwError $ "Argument error - expected type " ++ show argType ++ " but got " ++ show exprType ++ " in position (" ++ show pos ++ ")"

checkArg (RefArg pos argType _) expr outsideEnv = do
    outsideIdent <- case expr of
        EVar _ ident -> return ident
        _ -> throwError "Reference error - argument " ++ show ident " is not a variable in position (" ++ show pos ++ ")"
    exprType <- local (const outsideEnv) $ checkExpr expr
    if exprType == argType then
        return ()
    else
        throwError $ "Argument error - expected type " ++ show argType ++ " but got " ++ show exprType ++ " in position (" ++ show pos ++ ")"

checkArgs :: [Arg] -> [Expr] -> MyEnv -> TypeCheckerMonad ()
checkArgs [] [] outsideEnv = do
    ask

checkArgs (arg : args) (expr : exprs) outsideEnv = do
    checkArg arg expr outsideEnv
    checkArgs args exprs outsideEnv

---------- PROGRAM ------------

execProgComps :: [ProgComp] -> TypeCheckerMonad TypeEnv
execProgComps [] = do
    ask

execProgComps (comp : comps) = do
    env <- ask
    (store, last) <- get
    env' <- local (const env) $ execProgComp comp
    local (const env') $ execProgComps comps


execProgram :: Program -> InterpreterMonad Integer
execProgram (Program pos components) = do
    env <- ask
    env' <- local (const env) $ execProgComps components
    (store, last) <- get

    -- DEBUG
    -- liftIO (putStrLn $ "ENV :  " ++ show env')
    -- liftIO (putStrLn $ "STORE : " ++ show store)


    -- TODO -  run all porgram components and then run Expr Application of main
    -- and return that value
    loc <- local (const env') $ getVariableLocation (Ident "main")
    
    intVal <- local (const env') $ checkExpr (EApplic pos (Ident "main") [])
    case intVal of
        VInt i -> return i
        _ -> throwError "Main function must return an integer value"
    -- return 0

-- TODO
execProgComp :: ProgComp -> TypeCheckerMonad TypeEnv
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

evalItems :: [Item] -> Value -> TypeCheckerMonad TypeEnv
evalItems [] _ = do
    ask

evalItems (item : items) defaultValue = do
    env <- ask
    (store, loc) <- get
    env' <- local (const env) $ evalItem item defaultValue
    local (const env') $ evalItems items defaultValue

evalItem :: Item -> Value -> TypeCheckerMonad TypeEnv
evalItem (NoInit _ ident) defaultValue = do
    env <- ask
    (store, loc) <- get
    let newEnv = Data.Map.insert ident loc env
    let newStore = (Data.Map.insert loc defaultValue store, loc + 1)
    put newStore
    return newEnv

evalItem (Init _ ident expr) _ = do
    value <- checkExpr expr
    env <- ask
    (store, loc) <- get
    let newEnv = Data.Map.insert ident loc env
    let newStore = (Data.Map.insert loc value store, loc + 1)
    put newStore
    return newEnv


-- FUNCTION 

checkFunction :: Pos -> [Arg] -> Type -> Block -> TypeCheckerMonad ()
        (Fun args retType) <- checkExpr (EVar pos ident)
    checkArgs args exprs
    return retType
    -- outsideEnv <- ask
    -- env' <- local (const funEnv) $ evalArgs args exprs outsideEnv
    -- env'' <- local (const env') $ execBlock block
    -- if hasReturn env'' then 
    --     getReturnValue env''
    -- else 
    --     return VVoid
------- APPLICATION -------
evalArg :: Arg -> Expr -> MyEnv -> TypeCheckerMonad TypeEnv
evalArg (ValArg _ _ ident) expr outsideEnv = do
    val <- local (const outsideEnv) $ checkExpr expr
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

evalArgs :: [Arg] -> [Expr] -> MyEnv -> TypeCheckerMonad TypeEnv
evalArgs [] [] outsideEnv = do
    ask

evalArgs (arg : args) (expr : exprs) outsideEnv = do
    env <- evalArg arg expr outsideEnv
    local (const env) $ evalArgs args exprs outsideEnv

