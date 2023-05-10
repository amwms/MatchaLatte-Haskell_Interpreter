module TypeChecker.TypeChecker where

import Grammar.Abs
import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

import TypeChecker.CheckerTypes
import TypeChecker.GrammarChecker ( checkProgram )

typeCheckProgram p =
    runExceptT (runReaderT (checkProgram p) empty) 

