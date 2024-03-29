module TypeChecker.GrammarChecker where

import Grammar.Abs
import Data.Map
import Control.Monad.Reader
import Control.Monad.Except

import Expr
import TypeChecker.CheckerTypes
import TypeChecker.CheckerUtils
import TypeChecker.TypeErrors


---------------------------------
---------- STATEMENTS -----------
---------------------------------

checkStmts :: [Stmt] -> TypeCheckerMonad TypeEnv
checkStmts [] = do
    ask

checkStmts (stmt : stmts) = do
    env' <- checkStmt stmt
    local (const env') (checkStmts stmts)

checkBlock :: Block -> TypeCheckerMonad TypeEnv 
checkBlock (Block pos stmts) = do
    (env, scope) <- ask
    env' <- local (const (env, scope)) $ checkStmts stmts
    if hasReturn env' then do
        valType <- local (const env') $ getVariableType pos (Ident "return")
        let newEnv = Data.Map.insert (Ident "return") (valType, scope) env
        return (newEnv, scope)
    else
        return (env, scope)

checkStmt :: Stmt -> TypeCheckerMonad TypeEnv
checkStmt (Empty _) = do
    ask

checkStmt (StmtBlock _ block) = do
    (env, scope) <- ask
    local (const (env, scope + 1)) $ checkBlock block

checkStmt (StmtComp _ component) = do
    checkProgComp component

checkStmt (Assign pos ident expr) = do
    env <- ask
    exprType <- checkExpr expr
    varType <- getVariableType pos ident

    if compareTypes varType exprType then
        return env
    else
        throwError $ assignWrongTypesError pos ident varType exprType

-- checkStmt (Incr pos ident) = do
--     env <- ask
--     valType <- getVariableType pos ident
--     case valType of
--         Int _ -> return env
--         _ -> throwError $ genericVariableTypeInPositionError pos "Increment" ident valType (Int Nothing)

-- checkStmt (Decr pos ident) = do
--     env <- ask
--     valType <- getVariableType pos ident
--     case valType of
--         Int _ -> return env
--         _ -> throwError $ genericVariableTypeInPositionError pos "Decrement" ident valType (Int Nothing)

checkStmt (StmtExp _ expr) = do
    checkExpr expr
    ask

checkStmt (Ret pos expr) = do
    (env, scope) <- ask
    exprType <- checkExpr expr
    
    functionRetType <- case Data.Map.lookup (Ident "@return") env of
        Just (funRetType, _) -> return funRetType
        Nothing -> throwError $ returnOutsideOfFunctionError pos

    catchWrongReturnTypeError pos functionRetType exprType
    if not(hasReturn (env, scope)) then do
        let newEnv = Data.Map.insert (Ident "return") (exprType, scope) env
        return (newEnv, scope)
    else
        return (env, scope)

checkStmt (VRet pos) = do
    (env, scope) <- ask
    functionRetType <- case Data.Map.lookup (Ident "@return") env of
        Just (funRetType, _) -> return funRetType
        Nothing -> throwError $ returnOutsideOfFunctionError pos

    catchWrongReturnTypeError pos functionRetType (Void pos)
    if not(hasReturn (env, scope)) then do
        let newEnv = Data.Map.insert (Ident "return") ((Void pos), scope) env
        return (newEnv, scope)
    else   
        return (env, scope)

checkStmt (If pos expr block) = do
    valType <- checkExpr expr
    case valType of
        Bool _ -> checkBlock block
        _ -> throwError $ genericExpressionTypeInPositionError pos "If" "boolean" valType

checkStmt (IfElse pos expr block1 block2) = do
    valType <- checkExpr expr
    case valType of
        Bool _ -> checkBlock block1 >> checkBlock block2
        _ -> throwError $ genericExpressionTypeInPositionError pos "If-else" "boolean" valType

checkStmt (While pos expr block) = do
    valType <- checkExpr expr
    case valType of
        Bool _ -> checkBlock block
        _ -> throwError $ genericExpressionTypeInPositionError pos "While" "boolean" valType

checkStmt (Print pos expr) = do
    valType <- checkExpr expr
    case valType of
        Int _ -> ask
        Bool _ -> ask
        Str _ -> ask
        Fun _ _ _ -> ask
        _ -> throwError $ genericExpressionTypeInPositionError pos "Print" "printable" valType


---------------------------------
----------- EXPRESSIONS ---------
---------------------------------

checkExpr :: Expr -> TypeCheckerMonad Type
checkExpr (EVar pos ident) = 
    getVariableType pos ident

checkExpr (EInt pos _) = return $ Int pos
checkExpr (ETrue pos) = return $ Bool pos
checkExpr (EFalse pos) = return $ Bool pos
checkExpr (EString pos _) = return $ Str pos

checkExpr (ELambda pos args retType block) = do
    checkFunction pos args retType block 
    argTypes <- getArgTypes args
    return $ Fun pos argTypes retType

checkExpr (Incr pos ident) = do
    env <- ask
    valType <- getVariableType pos ident
    case valType of
        Int pos -> return $ Int pos
        _ -> throwError $ genericVariableTypeInPositionError pos "Increment" ident valType (Int Nothing)

checkExpr (Decr pos ident) = do
    env <- ask
    valType <- getVariableType pos ident
    case valType of
        Int _ -> return $ Int pos
        _ -> throwError $ genericVariableTypeInPositionError pos "Decrement" ident valType (Int Nothing)

checkExpr (ENot pos expr) = do
    valType <- checkExpr expr
    case (valType) of
        (Bool _) -> return $ Bool pos
        _ -> throwError $ genericExpressionTypeInPositionError pos "Not" "boolean" valType

checkExpr (ENeg pos expr) = do
    valType <- checkExpr expr
    case (valType) of
        (Int _) -> return $ Int pos
        _ -> throwError $ genericExpressionTypeInPositionError pos "Negation" "integer" valType

checkExpr (EMul pos expr1 mulOp expr2) = do
    valType1 <- checkExpr expr1
    valType2 <- checkExpr expr2
    case (valType1, valType2) of
        (Int _, Int _) -> return $ Int pos
        _ -> throwError $ mathOperationTypeError pos "multiply/divide" valType1 valType2

checkExpr (EAdd pos expr1 addOp expr2) = do
    valType1 <- checkExpr expr1
    valType2 <- checkExpr expr2
    case (valType1, valType2, addOp) of
        (Int _, Int _, _) -> return $ Int pos
        (Str _, Str _, Plus _) -> return $ Str pos
        _ -> throwError $ mathOperationTypeError pos "add/subtract" valType1 valType2

checkExpr (ERel pos expr1 relOp expr2) = do
    valType1 <- checkExpr expr1
    valType2 <- checkExpr expr2
    case (valType1, valType2, relOp) of
        (Int _, Int _, _) -> return $ Bool pos
        (Bool _, Bool _, EQU _) -> return $ Bool pos
        (Bool _, Bool _, NEQ _) -> return $ Bool pos
        (Str _, Str _, _) -> return $ Bool pos
        (Bool _, Bool _, _) -> throwError $ boolComparisonTypeError pos relOp
        _ -> throwError $ comparisonTypesError pos valType1 valType2

checkExpr (EAnd pos expr1 expr2) = do
    valType1 <- checkExpr expr1
    valType2 <- checkExpr expr2
    case (valType1, valType2) of
        (Bool _, Bool _) -> return $ Bool pos
        _ -> throwError $ boolOperationTypeError pos "AND" valType1 valType2

checkExpr (EOr pos expr1 expr2) = do
    valType1 <- checkExpr expr1
    valType2 <- checkExpr expr2
    case (valType1, valType2) of
        (Bool _, Bool _) -> return $ Bool pos
        _ -> throwError $ boolOperationTypeError pos "OR" valType1 valType2

------- APPLICATION -------
checkExpr (EApplic pos ident exprs) = do
    (Fun _ argTypes retType) <- checkExpr (EVar pos ident)
    checkArgs pos argTypes exprs 
    return retType

checkArg :: ArgType -> Expr -> TypeCheckerMonad ()
checkArg (ValArgType pos argType) expr = do
    exprType <- checkExpr expr
    catchWrongArgumentTypeError pos argType exprType

checkArg (RefArgType pos argType) expr = do
    outsideIdent <- case expr of
        EVar _ id -> return id
        _ -> throwError $ referenceAssignedNonReferenceTypeError pos
    exprType <- checkExpr expr
    catchWrongArgumentTypeError pos argType exprType

checkArgs :: BNFC'Position -> [ArgType] -> [Expr] -> TypeCheckerMonad ()
checkArgs pos [] [] = do
    return ()

checkArgs pos (arg : args) (expr : exprs) = do
    checkArg arg expr
    checkArgs pos args exprs

checkArgs pos _ _ = throwError $ wrongNumberOfArgumentsInApplicationError pos


---------------------------------
------------ PROGRAM ------------
---------------------------------

checkProgram :: Program -> TypeCheckerMonad ()
checkProgram (Program pos components) = do
    env' <- checkProgComps components
    
    unless (doesMainFunctionExist env') $
        throwError $ noMainFunctionFoundError

    mainType <- local (const env') $ getVariableType pos (Ident "main")
    case mainType of
        (Fun _ _ (Int _)) -> return ()
        (Fun pos _ _) -> throwError $ mainFunctionTypeError pos mainType
        _ -> throwError $ noMainFunctionFoundError

checkProgComps :: [ProgComp] -> TypeCheckerMonad TypeEnv
checkProgComps [] = do
    ask

checkProgComps (comp : comps) = do
    env' <- checkProgComp comp
    local (const env') $ checkProgComps comps

checkProgComp :: ProgComp -> TypeCheckerMonad TypeEnv
checkProgComp (FunDecl pos retType ident args block) = do
    (env, scope) <- ask
    argTypes <- getArgTypes args
    let newEnv = (Data.Map.insert ident ((Fun pos argTypes retType), scope) env, scope + 1)
    
    local (const newEnv) $ checkFunction pos args retType block

    return newEnv

checkProgComp (VarDecl pos varType items) = do
    catchVoidVariable pos varType
    evalItems items varType

evalItems :: [Item] -> Type -> TypeCheckerMonad TypeEnv
evalItems [] _ = do
    ask

evalItems (item : items) varType = do
    env' <- evalItem item varType
    local (const env') $ evalItems items varType

evalItem :: Item -> Type -> TypeCheckerMonad TypeEnv
evalItem (NoInit pos ident) varType = do
    catchIsVariableAlreadyInScope pos ident
    (env, scope) <- ask
    let newEnv = (Data.Map.insert ident (varType, scope) env, scope)
    return newEnv

evalItem (Init pos ident expr) varType = do
    catchIsVariableAlreadyInScope pos ident
    exprType <- checkExpr expr
    if compareTypes exprType varType then do
        (env, scope) <- ask
        let newEnv = (Data.Map.insert ident (varType, scope) env, scope)
        return newEnv
    else
        throwError $ initializationTypesError pos varType exprType

---- FUNCTION CHECK -----

checkFunction :: BNFC'Position -> [Arg] -> Type -> Block -> TypeCheckerMonad ()
checkFunction pos args retType block = do
    (env, scope) <- ask
    env' <- local (const (env, scope + 1)) $ evalArgs args
    envWithReturn <- local (const env') $ addReturn retType
    env'' <- local (const envWithReturn) $ checkBlock block
    if hasReturn env'' then do
        return ()
    else
        case retType of
            (Void _) -> return ()
            _ -> throwError $ noReturnValueForNonVoidFunctionError pos retType

addReturn :: Type -> TypeCheckerMonad TypeEnv
addReturn retType = do
    (env, scope) <- ask
    let newEnv = (Data.Map.insert (Ident "@return") (retType, scope) env, scope)
    return newEnv

evalArg :: Arg -> TypeCheckerMonad TypeEnv
evalArg (ValArg pos argType ident) = do
    catchIsVariableAlreadyInScope pos ident
    catchVoidVariable pos argType
    (env, scope) <- ask
    let newEnv = (Data.Map.insert ident (argType, scope) env, scope)
    return newEnv

evalArg (RefArg pos argType ident) = do
    catchIsVariableAlreadyInScope pos ident
    catchVoidVariable pos argType
    (env, scope) <- ask
    let newEnv = (Data.Map.insert ident (argType, scope) env, scope)
    return newEnv

evalArgs :: [Arg] -> TypeCheckerMonad TypeEnv
evalArgs [] = do
    ask

evalArgs (arg : args) = do
    env <- evalArg arg
    local (const env) $ evalArgs args

