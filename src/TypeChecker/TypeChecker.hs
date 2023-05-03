module TypeChecker.TypeChecker where

import Grammar.Abs
import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

import TypeChecker.CheckerTypes
import TypeChecker.GrammarChecker

run :: Program -> IO ()
run p = 
    do
        x <- runExceptT (runReaderT (checkProgram p) empty) 
        case x of
            Left err -> print err
            Right (code, store) -> print code

typeCheckProgram p =
    runExceptT (runReaderT (checkProgram p) empty) 

