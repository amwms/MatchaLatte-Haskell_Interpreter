module Types where

import Grammar.Abs
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Map

type MyEnv = Map Ident Int
data Value = VInt Integer | VBool Bool | VString String | VVoid | VFun [Arg] Type Block MyEnv
type MyStore = (Map Int Value, Int)
type ProgramException = String

type InterpreterMonad =  (ReaderT MyEnv (StateT MyStore (ExceptT ProgramException IO)))