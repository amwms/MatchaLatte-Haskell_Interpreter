module TypeChecker.GrammarChecker where

import Grammar.Abs
import Data.Map
import Control.Monad.Reader
import Control.Monad.Except

import Expr
import TypeChecker.CheckerTypes
import TypeChecker.CheckerUtils

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
checkBlock (Block pos stmts) = do
    env <- ask
    env' <- local (const env) $ checkStmts stmts
    if hasReturn env' then do
        valType <- local (const env') $ getVariableType pos (Ident "return")
        let newEnv = Data.Map.insert (Ident "return") valType env
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
    exprType <- checkExpr expr
    varType <- getVariableType pos ident

    if compareTypes varType exprType then
        return env
    else
        throwError $ "Assign error (" ++ show pos ++ ") - variable " ++ show ident ++ " is of type " ++ show varType ++ " but expression is of type " ++ show exprType

checkStmt (Incr pos ident) = do
    env <- ask

    valType <- getVariableType pos ident
    case valType of
        Int _ -> return env
        _ -> throwError $ "Increment error (" ++ show pos ++ ") - variable " ++ show ident ++ " is of type " ++ show  valType ++ " and not an integer"

checkStmt (Decr pos ident) = do
    env <- ask

    valType <- getVariableType pos ident
    case valType of
        Int _ -> return env
        _ -> throwError $ "Decrement error (" ++ show pos ++ ") - variable " ++ show ident ++ " is of type " ++ show  valType ++ " and not an integer"

checkStmt (StmtExp _ expr) = do
    checkExpr expr
    ask

checkStmt (Ret pos expr) = do
    env <- ask
    exprType <- checkExpr expr

    functionRetType <- case Data.Map.lookup (Ident "@return") env of
        Just funRetType -> return funRetType
        Nothing -> throwError $ "Return outside of function in position " ++ show pos

    if not(compareTypes functionRetType exprType) then
        throwError $ "Return type error - expected type " ++ show functionRetType ++ " but return value was type " ++ show exprType ++ " in position (" ++ show pos ++ ")"
    else do
        if not(hasReturn env) then do
            let newEnv = Data.Map.insert (Ident "return") exprType env
            return newEnv
        else
            return env
    -- else do
    --     retType <- getReturnType pos env
    --     if compareTypes exprType retType then
    --         return env
    --     else
    --         throwError $ "Return error - different return types in function (" ++ show pos ++ ")"

checkStmt (VRet pos) = do
    env <- ask
    
    functionRetType <- case Data.Map.lookup (Ident "@return") env of
        Just funRetType -> return funRetType
        Nothing -> throwError $ "Return outside of function in position " ++ show pos

    if not(compareTypes functionRetType (Void pos)) then
        throwError $ "Return type error - expected type " ++ show functionRetType ++ " but return value was type Void in position (" ++ show pos ++ ")"    
    else do
        if not(hasReturn env) then do
            let newEnv = Data.Map.insert (Ident "return") (Void pos) env
            return newEnv
        else   
            return env
    -- else do
    --     retType <- getReturnType pos env
    --     case retType of
    --        (Void _) -> return env
    --        _ -> throwError $ "Return error - different return types in function (" ++ show pos ++ ")"

checkStmt (If pos expr block) = do
    valType <- checkExpr expr
    case valType of
        Bool _ -> checkBlock block
        _ -> throwError $ "If error - expression in pos " ++ show pos ++ " is not a boolean value"

checkStmt (IfElse pos expr block1 block2) = do
    valType <- checkExpr expr
    case valType of
        Bool _ -> checkBlock block1 >> checkBlock block2
        _ -> throwError $ "If error - expression in pos " ++ show pos ++ " is not a boolean value"

checkStmt (While pos expr block) = do
    val <- checkExpr expr
    case val of
        Bool _ -> checkBlock block
        _ -> throwError $ "While error - expression in pos " ++ show pos ++ " is not a boolean value"

checkStmt (Print pos expr) = do
    valType <- checkExpr expr
    case valType of
        Int _ -> ask
        Bool _ -> ask
        Str _ -> ask
        Void _ -> ask
        _ -> throwError $ "Print error - expression in pos " ++ show pos ++ " is not a printable value"


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
checkExpr (EString pos _) = return $ Str pos

checkExpr (ELambda pos args retType block) = do
    checkFunction pos args retType block 
    argTypes <- getArgTypes args
    return $ Fun pos argTypes retType

------ NOT -------
checkExpr (ENot pos expr) = do
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
checkExpr (EMul pos expr1 mulOp expr2) = do
    valType1 <- checkExpr expr1
    valType2 <- checkExpr expr2
    case (valType1, valType2) of
        (Int _, Int _) -> return $ Int pos
        _ -> throwError $ "Multiplication error - cannot multiply/divide types " ++ show valType1 ++ " and " ++ show valType2 ++ " in position (" ++ show pos ++ ")"

------- ADD -------
checkExpr (EAdd pos expr1 addOp expr2) = do
    valType1 <- checkExpr expr1
    valType2 <- checkExpr expr2
    case (valType1, valType2, addOp) of
        (Int _, Int _, _) -> return $ Int pos
        (Str _, Str _, Plus _) -> return $ Str pos
        _ -> throwError $ "Addition error - cannot add/subtract types " ++ show valType1 ++ " and " ++ show valType2 ++ " in position (" ++ show pos ++ ")"

------- REL ------- 
checkExpr (ERel pos expr1 relOp expr2) = do
    valType1 <- checkExpr expr1
    valType2 <- checkExpr expr2
    case (valType1, valType2, relOp) of
        (Int _, Int _, _) -> return $ Bool pos
        (Bool _, Bool _, EQU _) -> return $ Bool pos
        (Str _, Str _, _) -> return $ Bool pos
        _ -> throwError $ "Comparison error in position (" ++ show pos ++ ") - error with types " ++ show valType1 ++ " and " ++ show valType2

------- AND -------
checkExpr (EAnd pos expr1 expr2) = do
    valType1 <- checkExpr expr1
    valType2 <- checkExpr expr2
    case (valType1, valType2) of
        (Bool _, Bool _) -> return $ Bool pos
        _ -> throwError $ "And error in position (" ++ show pos ++ ") - " ++ show valType1 ++ " and " ++ show valType2 ++ " are not both boolean values"

------- OR -------
checkExpr (EOr pos expr1 expr2) = do
    valType1 <- checkExpr expr1
    valType2 <- checkExpr expr2
    case (valType1, valType2) of
        (Bool _, Bool _) -> return $ Bool pos
        _ -> throwError $ "Or error in position (" ++ show pos ++ ") - " ++ show valType1 ++ " and " ++ show valType2 ++ " are not both boolean values"

checkExpr (EApplic pos ident exprs) = do
    (Fun _ argTypes retType) <- checkExpr (EVar pos ident)
    checkArgs pos argTypes exprs 
    return retType

checkArg :: ArgType -> Expr -> TypeCheckerMonad ()
checkArg (ValArgType pos argType) expr = do
    exprType <- checkExpr expr
    if compareTypes exprType argType then
        return ()
    else
        throwError $ "Argument error - expected type " ++ show argType ++ " but got " ++ show exprType ++ " in position (" ++ show pos ++ ")"

checkArg (RefArgType pos argType) expr = do
    outsideIdent <- case expr of
        EVar _ id -> return id
        _ -> throwError $ "Reference error - argument is not a variable but regular expression in position (" ++ show pos ++ ")"
    exprType <- checkExpr expr
    if compareTypes exprType argType then
        return ()
    else
        throwError $ "Argument error - expected type " ++ show argType ++ " but got " ++ show exprType ++ " in position (" ++ show pos ++ ")"

checkArgs :: BNFC'Position -> [ArgType] -> [Expr] -> TypeCheckerMonad ()
checkArgs pos [] [] = do
    return ()

checkArgs pos (arg : args) (expr : exprs) = do
    checkArg arg expr
    checkArgs pos args exprs

checkArgs pos _ _ = throwError $ "Application error - wrong number of arguments at " ++ printPosition pos

-- ---------- PROGRAM ------------
checkProgComps :: [ProgComp] -> TypeCheckerMonad TypeEnv
checkProgComps [] = do
    ask

checkProgComps (comp : comps) = do
    env' <- checkProgComp comp
    local (const env') $ checkProgComps comps


checkProgram :: Program -> TypeCheckerMonad ()
checkProgram (Program pos components) = do
    env' <- checkProgComps components
    mainType <- local (const env') $ getVariableType pos (Ident "main")
    case mainType of
        (Fun _ _ (Int _)) -> return ()
        _ -> throwError "Main function must return an integer value"

checkProgComp :: ProgComp -> TypeCheckerMonad TypeEnv
checkProgComp (FunDecl pos retType ident args block) = do
    checkFunction pos args retType block
    env <- ask
    argTypes <- getArgTypes args
    let newEnv = Data.Map.insert ident (Fun pos argTypes retType) env
    return newEnv

checkProgComp (VarDecl _ varType items) = do
    evalItems items varType

evalItems :: [Item] -> Type -> TypeCheckerMonad TypeEnv
evalItems [] _ = do
    ask

evalItems (item : items) varType = do
    env' <- evalItem item varType
    local (const env') $ evalItems items varType

evalItem :: Item -> Type -> TypeCheckerMonad TypeEnv
evalItem (NoInit _ ident) varType = do
    env <- ask
    let newEnv = Data.Map.insert ident varType env
    return newEnv

evalItem (Init pos ident expr) varType = do
    exprType <- checkExpr expr
    if compareTypes exprType varType then do
        env <- ask
        let newEnv = Data.Map.insert ident varType env
        return newEnv
    else
        throwError $ "Initialization error - expected type " ++ show varType ++ " but got " ++ show exprType ++ " in position (" ++ show pos ++ ")"


-- -- FUNCTION 

checkFunction :: BNFC'Position -> [Arg] -> Type -> Block -> TypeCheckerMonad ()
checkFunction pos args retType block = do
    env' <- evalArgs args 
    envWithReturn <- local (const env') $ addReturn retType
    env'' <- local (const envWithReturn) $ checkBlock block
    if hasReturn env'' then do
        return ()
        -- evalRetType <- getReturnType pos env''
        -- if compareTypes retType evalRetType then
        --     return ()
        -- else
        --     throwError $ "Return type error - expected type " ++ show retType ++ " but return value was type " ++ show evalRetType ++ " in position (" ++ show pos ++ ")"
    else 
        case retType of
            (Void _) -> return ()
            _ -> throwError $ "No return value for function in position" ++ show pos ++ " but expected type " ++ show retType

addReturn :: Type -> TypeCheckerMonad TypeEnv
addReturn retType = do
    env <- ask
    let newEnv = Data.Map.insert (Ident "@return") retType env
    return newEnv

----- APPLICATION -------
evalArg :: Arg -> TypeCheckerMonad TypeEnv
evalArg (ValArg pos argType ident) = do
    env <- ask
    let newEnv = Data.Map.insert ident argType env
    return newEnv

evalArg (RefArg pos argType ident) = do
    env <- ask
    let newEnv = Data.Map.insert ident argType env
    return newEnv

evalArgs :: [Arg] -> TypeCheckerMonad TypeEnv
evalArgs [] = do
    ask

evalArgs (arg : args) = do
    env <- evalArg arg
    local (const env) $ evalArgs args 

