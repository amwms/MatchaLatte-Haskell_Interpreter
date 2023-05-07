module Evaluator where

import Grammar.Abs
import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

import Types
import EvalExec


run :: Program -> IO ()
run p = 
    do
        x <- runExceptT (runStateT (runReaderT (execProgram p) empty) (empty, 0))
        case x of
            Left err -> print err
            Right (code, store) -> print code

runProgram :: Program -> IO (Either ProgramException (Integer, MyStore))
runProgram p =
    runExceptT (runStateT (runReaderT (execProgram p) empty) (empty, 0))
