module Types where

import Grammar.Abs
import Data.Map (Map)
import Control.Monad.Reader      
import Control.Monad.Except

type TypeEnv = Map Ident Type
type ProgramException = String

type TypeCheckerMonad =  (ReaderT MyEnv (ExceptT ProgramException IO))