module Expr where

import Grammar.Abs
import Data.Map 
import Control.Monad.Reader      
import Control.Monad.Except
import Control.Monad.State


evalAddOp :: Integral a1 => AddOp -> a1 -> a1 -> a1
evalAddOp (Plus _) = (+)
evalAddOp (Minus _) = (-)

evalMulOp :: Integral a1 => MulOp -> a1 -> a1 -> a1
evalMulOp (Times _) = (*)
evalMulOp (Div _) = div
evalMulOp (Mod _) = mod


evalRelOp :: Ord a1 => RelOp -> a1 -> a1 -> Bool
evalRelOp (Less _) = (<)
evalRelOp (LEQ _) = (<=)
evalRelOp (Greater _) = (>)
evalRelOp (GEQ _) = (>=)
evalRelOp (EQU _) = (==)
evalRelOp (NEQ _) = (/=)
