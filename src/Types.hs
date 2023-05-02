module Types where

import Grammar.Abs
import Data.Map (Map)
import Control.Monad.Reader      
import Control.Monad.Except
import Control.Monad.State

type MyEnv = Map Ident Integer
data Value 
    = VInt Integer
    | VBool Bool 
    | VString String 
    | VVoid 
    | VFun [Arg] Type Block MyEnv
    deriving (Show, Eq)

type MyStore = (Map Integer Value, Integer)
type ProgramException = String

type InterpreterMonad =  (ReaderT MyEnv (StateT MyStore (ExceptT ProgramException IO))) 