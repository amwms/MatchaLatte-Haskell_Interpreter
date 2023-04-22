-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Grammar.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Grammar.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: Grammar.Abs.Ident -> Result
transIdent x = case x of
  Grammar.Abs.Ident string -> failure x

transProgram :: Show a => Grammar.Abs.Program' a -> Result
transProgram x = case x of
  Grammar.Abs.Program _ progcomps -> failure x

transProgComp :: Show a => Grammar.Abs.ProgComp' a -> Result
transProgComp x = case x of
  Grammar.Abs.FunDecl _ type_ ident args block -> failure x
  Grammar.Abs.VarDecl _ type_ items -> failure x

transArg :: Show a => Grammar.Abs.Arg' a -> Result
transArg x = case x of
  Grammar.Abs.ValArg _ type_ ident -> failure x
  Grammar.Abs.RefArg _ type_ ident -> failure x

transItem :: Show a => Grammar.Abs.Item' a -> Result
transItem x = case x of
  Grammar.Abs.NoInit _ ident -> failure x
  Grammar.Abs.Init _ ident expr -> failure x

transBlock :: Show a => Grammar.Abs.Block' a -> Result
transBlock x = case x of
  Grammar.Abs.Block _ stmts -> failure x

transStmt :: Show a => Grammar.Abs.Stmt' a -> Result
transStmt x = case x of
  Grammar.Abs.Empty _ -> failure x
  Grammar.Abs.StmtBlock _ block -> failure x
  Grammar.Abs.StmtComp _ progcomp -> failure x
  Grammar.Abs.Assign _ ident expr -> failure x
  Grammar.Abs.Incr _ ident -> failure x
  Grammar.Abs.Decr _ ident -> failure x
  Grammar.Abs.StmtExp _ expr -> failure x
  Grammar.Abs.Ret _ expr -> failure x
  Grammar.Abs.VRet _ -> failure x
  Grammar.Abs.If _ expr block -> failure x
  Grammar.Abs.IfElse _ expr block1 block2 -> failure x
  Grammar.Abs.While _ expr block -> failure x

transType :: Show a => Grammar.Abs.Type' a -> Result
transType x = case x of
  Grammar.Abs.Int _ -> failure x
  Grammar.Abs.Str _ -> failure x
  Grammar.Abs.Bool _ -> failure x
  Grammar.Abs.Void _ -> failure x
  Grammar.Abs.Fun _ types type_ -> failure x

transExpr :: Show a => Grammar.Abs.Expr' a -> Result
transExpr x = case x of
  Grammar.Abs.EVar _ ident -> failure x
  Grammar.Abs.EInt _ integer -> failure x
  Grammar.Abs.ETrue _ -> failure x
  Grammar.Abs.EFalse _ -> failure x
  Grammar.Abs.EString _ string -> failure x
  Grammar.Abs.ELambda _ args type_ block -> failure x
  Grammar.Abs.EApplic _ ident exprs -> failure x
  Grammar.Abs.ENeg _ expr -> failure x
  Grammar.Abs.ENot _ expr -> failure x
  Grammar.Abs.EMul _ expr1 mulop expr2 -> failure x
  Grammar.Abs.EAdd _ expr1 addop expr2 -> failure x
  Grammar.Abs.ERel _ expr1 relop expr2 -> failure x
  Grammar.Abs.EAnd _ expr1 expr2 -> failure x
  Grammar.Abs.EOr _ expr1 expr2 -> failure x

transAddOp :: Show a => Grammar.Abs.AddOp' a -> Result
transAddOp x = case x of
  Grammar.Abs.Plus _ -> failure x
  Grammar.Abs.Minus _ -> failure x

transMulOp :: Show a => Grammar.Abs.MulOp' a -> Result
transMulOp x = case x of
  Grammar.Abs.Times _ -> failure x
  Grammar.Abs.Div _ -> failure x
  Grammar.Abs.Mod _ -> failure x

transRelOp :: Show a => Grammar.Abs.RelOp' a -> Result
transRelOp x = case x of
  Grammar.Abs.Less _ -> failure x
  Grammar.Abs.LEQ _ -> failure x
  Grammar.Abs.Greater _ -> failure x
  Grammar.Abs.GEQ _ -> failure x
  Grammar.Abs.EQU _ -> failure x
  Grammar.Abs.NEQ _ -> failure x