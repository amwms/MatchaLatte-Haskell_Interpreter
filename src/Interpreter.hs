module Interpreter where

import Prelude
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )
import System.IO (hPutStrLn, stderr)

import Grammar.Abs ( Program )
import Grammar.Lex ( Token )
import Grammar.Par ( pProgram, myLexer )

import Evaluator
import TypeChecker.TypeChecker

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int


runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = readFile f >>= interpret v p

interpret :: Verbosity -> ParseFun Program -> String -> IO ()
interpret v parser input =
    case parser tokens of
        Left err -> do
            hPutStrLn stderr "Parse Failed..."
            hPutStrLn stderr err
            exitFailure
        Right tree -> do
            typeCheckResult <- typeCheckProgram tree
            case typeCheckResult of
                Left err -> do
                    hPutStrLn stderr "Error..."
                    hPutStrLn stderr err
                    exitFailure
                Right _ -> do
                    runProgramResult <- runProgram tree
                    case runProgramResult of
                        Left err -> do
                            hPutStrLn stderr "Runtime Error..."
                            hPutStrLn stderr err
                            exitFailure
                        Right (code, store) -> do
                            putStrLn $ "Program successful with exit code " ++ show code
    where
    tokens = myLexer input

