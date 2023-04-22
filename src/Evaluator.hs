module Evaluator where

import Grammar.Abs
import Data.Map
import Types


----- OPERATIONS -----
-- data AddOp a = Plus a | Minus a
-- data MulOp a = Times a | Div a | Mod a
-- data RelOp a = Less a | LEQ a | Greater a | GEQ a | EQU a | NEQ a

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