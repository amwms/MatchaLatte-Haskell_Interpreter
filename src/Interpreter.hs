module Interpreter where

import Prelude
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )

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
            putStrLn "Parse Failed..."
            putStrLn err
            exitFailure
        Right tree -> do
            --TODO - run type checking here and have the case below as Right case of result
            typeCheckResult <- typeCheckProgram tree
            case typeCheckResult of
                Left err -> do
                    putStrLn "Type Error..."
                    putStrLn err
                    exitFailure
                Right _ -> do
                    runProgramResult <- runProgram tree
                    case runProgramResult of
                        Left err -> do
                            putStrLn "Runtime Error..."
                            putStrLn err
                            exitFailure
                        Right (code, store) -> do
                            putStrLn $ "Program successful with exit code " ++ show code
    where
    tokens = myLexer input
    
usage :: IO ()
usage = do
    putStrLn $ unlines
        [ "usage: Call with one of the following argument combinations:"
        , "  --help          Display this help message."
        , "  (no arguments)  Parse stdin verbosely."
        , "  (files)         Parse content of files verbosely."
        , "  -s (files)      Silent mode. Parse content of files silently."
        ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage
        []         -> getContents >>= interpret 2 pProgram
        "-s":fs    -> mapM_ (runFile 0 pProgram) fs
        fs         -> mapM_ (runFile 2 pProgram) fs

