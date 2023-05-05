module Evaluator where

import Grammar.Abs
import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

-- import Expr
-- import Program
-- import Types
-- import Utils
import EvalExec

-- run :: Program -> IO ()
run p = 
    do
        -- x :: Either ProgramException IO (Int, MyStore)
        x <- runExceptT (runStateT (runReaderT (execProgram p) empty) (empty, 0))
        case x of
            Left err -> print err
            Right (code, store) -> print code

runProgram p =
    runExceptT (runStateT (runReaderT (execProgram p) empty) (empty, 0))

-- x p = runExcept (runStateT (runReaderT (execProgram p) empty) (empty, 0))

-- test1 = Program [
--     FunDecl Int (Ident "main") [] (
--         Block [
--             StmtComp (VarDecl Int [NoInit (Ident "example_var")]),
--             Assign (Ident "example_var") (EMul (EAdd (EInt 5) Plus (EInt 3)) Div (EInt 4)),
--             Ret (EInt 0)
--         ]
--     )]

-- printEnv :: InterpreterMonad MyEnv -> IO ()
-- printEnv m =
--     let 
--         x = runExcept (runStateT (runReaderT m empty) (empty, 0))
--     in case x of
--         Left err -> print err
--         Right r -> print r

-- test1 = Program (Just (1,1)) [FunDecl (Just (1,1)) (Int (Just (1,1))) (Ident "main") [] (Block (Just (1,12)) [])] 
-- test1 = Program (Just (1,1)) [FunDecl (Just (1,1)) (Int (Just (1,1))) (Ident "main") [] (Block (Just (1,12)) [StmtComp (Just (2,5)) (VarDecl (Just (2,5)) (Int (Just (2,5))) [NoInit (Just (2,9)) (Ident "example_var")]),Assign (Just (3,5)) (Ident "example_var") (EMul (Just (3,19)) (EAdd (Just (3,20)) (EInt (Just (3,20)) 5) (Plus (Just (3,22))) (EInt (Just (3,24)) 3)) (Div (Just (3,27))) (EInt (Just (3,29)) 4)),Ret (Just (5,5)) (EInt (Just (5,12)) 0)])] 
-- test1 = Program (Just (1, 1)) [VarDecl (Just(1, 1)) (Int (Just(1, 1))) [NoInit (Just(1, 2)) (Ident "main")]]
-- test1 = Program (Just (1,1)) [FunDecl (Just (1,1)) (Int (Just (1,1))) (Ident "main") [] (Block (Just (1,12)) []), VarDecl (Just(1, 1)) (Int (Just(1, 1))) [NoInit (Just(1, 2)) (Ident "main")],FunDecl (Just (1,1)) (Int (Just (1,1))) (Ident "m") [] (Block (Just (1,12)) [])] 
test1 = Program (Just (1,1)) [FunDecl (Just (1,1)) (Int (Just (1,1))) (Ident "main") [] (Block (Just (1,12)) [StmtComp (Just (2,5)) (VarDecl (Just (2,5)) (Int (Just (2,5))) [NoInit (Just (2,9)) (Ident "example_var")]),Assign (Just (3,5)) (Ident "example_var") (EMul (Just (3,19)) (EAdd (Just (3,20)) (EInt (Just (3,20)) 5) (Plus (Just (3,22))) (EInt (Just (3,24)) 3)) (Div (Just (3,27))) (EInt (Just (3,29)) 4)),Ret (Just (5,5)) (EInt (Just (5,12)) 0)])]
t1 = run test1

test2 = Program (Just (1,1)) [FunDecl (Just (1,1)) (Int (Just (1,1))) (Ident "main") [] (Block (Just (1,12)) [StmtComp (Just (2,5)) (VarDecl (Just (2,5)) (Int (Just (2,5))) [NoInit (Just (2,9)) (Ident "example_var")]),Assign (Just (3,5)) (Ident "example_var") (EMul (Just (3,19)) (EAdd (Just (3,20)) (EInt (Just (3,20)) 5) (Plus (Just (3,22))) (EInt (Just (3,24)) 3)) (Div (Just (3,27))) (EInt (Just (3,29)) 4)),Print (Just (4,5)) (EVar (Just (4,11)) (Ident "example_var")),Ret (Just (6,5)) (EInt (Just (6,12)) 0)])]
t2 = run test2
