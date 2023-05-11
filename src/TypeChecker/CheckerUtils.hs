module TypeChecker.CheckerUtils where

import Data.Map
import Control.Monad.Reader      
import Control.Monad.Except
import Control.Monad      ( when )

import Grammar.Abs
import TypeChecker.CheckerTypes

getVariableType :: BNFC'Position -> Ident -> TypeCheckerMonad Type
getVariableType pos (Ident varName) = do
    (env, _) <- ask
    let ident = Ident varName
    case Data.Map.lookup ident env of
        Just (identType, _) -> return identType
        Nothing -> throwError $ "Error - variable \"" ++  varName ++ "\" in position (" ++ showPosition pos ++ ") not found"

doesMainFunctionExist :: TypeEnv -> Bool
doesMainFunctionExist (env, _) = 
    case Data.Map.lookup (Ident "main") env of
    Just (Fun _ _ (Int _), _) -> True
    _ -> False

hasReturn :: TypeEnv -> Bool
hasReturn (env, _) = case Data.Map.lookup (Ident "return") env of
    Just _ -> True
    Nothing -> False

-- getReturnType :: BNFC'Position -> TypeEnv -> TypeCheckerMonad Type
-- getReturnType pos env = 

--     case Data.Map.lookup (Ident "return") env of
--         Just (retType, _) -> return retType
--         Nothing -> return (Void pos)

getArgTypes :: [Arg] -> TypeCheckerMonad [ArgType]
getArgTypes [] = return []
getArgTypes ((ValArg pos argType ident):args) = do
    argTypes <- getArgTypes args
    return $ (ValArgType pos argType):argTypes
getArgTypes ((RefArg pos argType ident):args) = do
    argTypes <- getArgTypes args
    return $ (RefArgType pos argType):argTypes

compareTypes :: Type -> Type -> Bool
compareTypes (Int _) (Int _) = True
compareTypes (Str _) (Str _) = True
compareTypes (Bool _) (Bool _) = True
compareTypes (Fun _ argTypes1 retType1) (Fun _ argTypes2 retType2) = 
    compareArgTypes argTypes1 argTypes2 && compareTypes retType1 retType2
compareTypes (Void _) (Void _) = True
compareTypes _ _ = False

compareArgTypes :: [ArgType] -> [ArgType] -> Bool
compareArgTypes [] [] = True
compareArgTypes (argType1:argTypes1) (argType2:argTypes2) = 
    compareArgTypes argTypes1 argTypes2 && compareArgType argType1 argType2
compareArgTypes _ _ = False

compareArgType :: ArgType -> ArgType -> Bool
compareArgType (ValArgType _ type1) (ValArgType _ type2) = compareTypes type1 type2
compareArgType (RefArgType _ type1) (RefArgType _ type2) = compareTypes type1 type2
compareArgType _ _ = False

showType :: Type -> String
showType (Int _) = "int"
showType (Str _) = "string"
showType (Bool _) = "bool"
showType (Fun _ argTypes retType) = "function" ++ "(" ++ showArgTypes argTypes ++ ")" ++ " -> " ++ showType retType
showType (Void _) = "void"

showArgTypes :: [ArgType] -> String
showArgTypes [] = ""
showArgTypes ((ValArgType _ argType):argTypes) = showType argType ++ ", " ++ showArgTypes argTypes
showArgTypes ((RefArgType _ argType):argTypes) = showType argType ++ "@" ++ ", " ++ showArgTypes argTypes

showPosition :: BNFC'Position -> String
showPosition (Just (line, column)) = "line " ++ show line ++ ", column " ++ show column
showPosition Nothing = "unknown position"

catchVoidVariable :: BNFC'Position -> Type -> TypeCheckerMonad ()
catchVoidVariable pos varType = do
    Control.Monad.when (compareTypes varType (Void pos)) $
        throwError $ "Variable type error in position " ++ showPosition pos ++ " - variable cannot be of type void"

catchIsVariableAlreadyInScope :: BNFC'Position -> Ident -> TypeCheckerMonad ()
catchIsVariableAlreadyInScope pos (Ident ident) = do
    (env, scope) <- ask
    case Data.Map.lookup (Ident ident) env of
        Just (_, scopeId) -> 
            Control.Monad.when (scopeId == scope) $ 
            throwError $ "Declaration error - variable \"" ++ ident ++
            "\" in position (" ++ showPosition pos ++ ") already declared"
        Nothing -> return ()
