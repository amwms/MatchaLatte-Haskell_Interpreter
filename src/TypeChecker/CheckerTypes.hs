module TypeChecker.CheckerTypes where

import Grammar.Abs
import Data.Map (Map)
import Control.Monad.Reader      
import Control.Monad.Except

type Scope = Integer
type TypeEnv = (Map Ident (Type, Scope), Scope)
type ProgramException = String

type TypeCheckerMonad =  (ReaderT TypeEnv (ExceptT ProgramException IO))