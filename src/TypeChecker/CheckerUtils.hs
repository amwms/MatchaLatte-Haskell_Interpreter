module TypeChecker.CheckerUtils where

import Data.Map
import Control.Monad.Reader      
import Control.Monad.Except

import Grammar.Abs
import TypeChecker.CheckerTypes

getVariableType :: BNFC'Position -> Ident -> TypeCheckerMonad Type
getVariableType pos ident = do
    env <- ask
    case Data.Map.lookup ident env of
        Just identType -> return identType
        Nothing -> throwError $ show ident ++ " in position " ++ show pos ++ " not found"

hasReturn :: TypeEnv -> Bool
hasReturn env = case Data.Map.lookup (Ident "return") env of
    Just _ -> True
    Nothing -> False

getReturnType :: BNFC'Position -> TypeEnv -> TypeCheckerMonad Type
getReturnType pos env = 
    case Data.Map.lookup (Ident "return") env of
        Just (retType) -> return retType
        Nothing -> return (Void pos)

getArgTypes :: [Arg] -> TypeCheckerMonad [ArgType]
getArgTypes [] = return []
getArgTypes ((ValArg pos argType ident):args) = do
    argTypes <- getArgTypes args
    return $ (ValArgType pos argType):argTypes
getArgTypes ((RefArg pos argType ident):args) = do
    argTypes <- getArgTypes args
    return $ (RefArgType pos argType):argTypes