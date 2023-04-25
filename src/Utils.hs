module Utils where

import Grammar.Abs
import Data.Map
import Types

import Control.Monad.Reader      
import Control.Monad.Except
import Control.Monad.State

getValueFromMemory :: Ident -> InterpreterMonad Value
getValueFromMemory ident = do
    env <- ask
    (store, _) <- get
    case Data.Map.lookup ident env of
        Just loc -> return (findWithDefault VVoid loc store)
        Nothing -> throwError ("Variable " ++ show ident ++ " not found")

hasReturn :: MyEnv -> Bool
hasReturn env = case Data.Map.lookup (Ident "return") env of
    Just _ -> True
    Nothing -> False

getVariableLocation :: Ident -> InterpreterMonad Int
getVariableLocation ident = do
    env <- ask
    case Data.Map.lookup ident env of
        Just loc -> return loc
        Nothing -> throwError $ "Variable " ++ show ident ++ " not found"
