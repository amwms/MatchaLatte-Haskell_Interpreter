module Program where

import Grammar.Abs
import Data.Map
import Control.Monad.Reader      
import Control.Monad.Except
import Control.Monad.State

import Expr
import Types
import Utils
-- import Stmt

-- data Program a = Program a [ProgComp a]

-- data ProgComp a
--     = FunDecl a (Type a) Ident [Arg a] (Block a)
--     | VarDecl a (Type a) [Item a]

-- data Arg a = ValArg a (Type a) Ident | RefArg a (Type a) Ident

-- data Item a = NoInit a Ident | Init a Ident (Expr a)

-- execProgComps :: [ProgComp] -> InterpreterMonad MyEnv
-- execProgComps [] = do
--     ask

-- execProgComps (comp : comps) = do
--     env <- ask
--     (store, last) <- get
--     env' <- local (const env) $ execProgComp comp
--     local (const env') $ execProgComps comps


-- execProgram :: Program -> InterpreterMonad Int
-- execProgram (Program pos components) = do
--     env <- ask
--     env' <- local (const env) $ execProgComps components
--     (store, last) <- get

--     liftIO (putStrLn $ show env')
--     liftIO (putStrLn $ show store)


--     -- TODO -  run all porgram components and then run Expr Application of main
--     -- and return that value
--     loc <- getVariableLocation (Ident "main")
--     evalExpr (EApplic pos (Ident "main") [])
--     -- return 0

-- -- TODO
-- execProgComp :: ProgComp -> InterpreterMonad MyEnv
-- execProgComp (FunDecl _ retType ident args block) = do
--     env <- ask
--     (store, loc) <- get
--     let newEnv = Data.Map.insert ident loc env
--     let newStore = (Data.Map.insert loc (VFun args retType block newEnv) store, loc + 1)
--     put newStore
--     return newEnv

-- execProgComp (VarDecl _ varType items) = do
--     env <- ask
--     case varType of
--         Int _ -> local (const env) $ evalItems items $ VInt 0
--         Str _ -> local (const env) $ evalItems items $ VString ""
--         Bool _ -> local (const env) $ evalItems items $ VBool False
--         Void _ -> local (const env) $ evalItems items VVoid
--         (Fun x argTypes retType) -> local (const env) $ evalItems items $ VFun [] retType (Block x [Empty x]) env

-- evalItems :: [Item] -> Value -> InterpreterMonad MyEnv
-- evalItems [] _ = do
--     ask

-- evalItems (item : items) defaultValue = do
--     env <- ask
--     (store, loc) <- get
--     env' <- local (const env) $ evalItem item defaultValue
--     local (const env') $ evalItems items defaultValue

-- evalItem :: Item -> Value -> InterpreterMonad MyEnv
-- evalItem (NoInit _ ident) defaultValue = do
--     env <- ask
--     (store, loc) <- get
--     let newEnv = Data.Map.insert ident loc env
--     let newStore = (Data.Map.insert loc defaultValue store, loc + 1)
--     put newStore
--     return newEnv

-- evalItem (Init _ ident expr) _ = do
--     value <- evalExpr expr
--     env <- ask
--     (store, loc) <- get
--     let newEnv = Data.Map.insert ident loc env
--     let newStore = (Data.Map.insert loc value store, loc + 1)
--     put newStore
--     return newEnv
