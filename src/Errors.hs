module Errors where
import Grammar.Abs (BNFC'Position)
import TypeChecker.CheckerUtils (showPosition)

divisionByZeroError :: BNFC'Position -> String
divisionByZeroError pos = 
    "Division error - attempt to divide by 0 in position (" ++ showPosition pos ++ ")"