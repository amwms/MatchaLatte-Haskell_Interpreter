module Utils where

import Grammar.Abs
import Data.Map
import Types
import TypeChecker.CheckerUtils ( showType )

import Control.Monad.Reader      
import Control.Monad.Except
import Control.Monad.State

getValueFromMemory :: Ident -> InterpreterMonad Value
getValueFromMemory ident = do
    env <- ask
    (store, _) <- get
    case Data.Map.lookup ident env of
        Just loc -> return (findWithDefault VVoid loc store)
        Nothing -> throwError (show ident ++ " not found")

getVariableLocation :: Ident -> InterpreterMonad Integer
getVariableLocation ident = do
    env <- ask
    case Data.Map.lookup ident env of
        Just loc -> return loc
        Nothing -> throwError $ show ident ++ " not found"

hasReturn :: MyEnv -> Bool
hasReturn env = case Data.Map.lookup (Ident "return") env of
    Just _ -> True
    Nothing -> False

getReturnValue :: MyEnv -> InterpreterMonad Value
getReturnValue env = 
    case Data.Map.lookup (Ident "return") env of
        Just loc -> local (const env) $ getValueFromMemory (Ident "return")
        Nothing -> throwError "Return value not found"

showValue :: Value -> String
showValue (VInt intVal) = show intVal
showValue (VString str) = str
showValue (VBool boolVal) = show boolVal
showValue (VFun args retType block _) = "function" ++ "(" ++ showArgs args ++ ")" ++ " -> " ++ showType retType
showValue (VVoid) = "void"

showArgs :: [Arg] -> String
showArgs [] = ""
showArgs ((ValArg _ argType (Ident name)):args) = showType argType ++ " " ++ name ++ ", " ++ showArgs args
showArgs ((RefArg _ argType (Ident name)):args) = showType argType ++ " @" ++ name ++ ", " ++ showArgs args
