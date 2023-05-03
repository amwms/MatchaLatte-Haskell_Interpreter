module TypeChecker.CheckerUtils where

import Data.Map
import Control.Monad.Reader      
import Control.Monad.Except

import Grammar.Abs
import TypeChecker.CheckerTypes

getVariableType :: Ident -> TypeCheckerMonad Type
getVariableType pos ident = do
    env <- ask
    case Data.Map.lookup ident env of
        Just identType -> return identType
        Nothing -> throwError $ show ident ++ " in position " ++ show pos ++ " not found"

hasReturn :: TypeEnv -> Bool
hasReturn env = case Data.Map.lookup (Ident "return") env of
    Just _ -> True
    Nothing -> False

getReturnType :: TypeEnv -> TypeCheckerMonad Type
getReturnType pos env = 
    case Data.Map.lookup (Ident "return") env of
        Just (retType _) -> return (retType pos)
        Nothing -> return (Void pos)