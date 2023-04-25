module Utils where

import Grammar.Abs
import Data.Map
import Types

getValueFromMemory env store ident = do
    loc <- case Data.Map.lookup ident env of
        Just loc -> return loc
        Nothing -> throwError $ "Variable " ++ show ident ++ " not found"
    return $ findWithDefault (VInt 0) loc store

hasReturn :: MyEnv -> Bool
hasReturn env = case Data.Map.lookup "return" env of
    Just _ -> True
    Nothing -> False

getVariableLocation :: Ident -> MyEnv -> Int
getVariableLocation ident env = case Data.Map.lookup ident env of
    Just loc -> loc
    Nothing -> throwError $ "Variable " ++ show ident ++ " not found"
