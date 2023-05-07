module Main where

import Interpreter
import System.Environment ( getArgs )
import Grammar.Par ( pProgram )

usage :: IO ()
usage = do
    putStrLn $ unlines
        [ "usage: Call with one of the following argument combinations:"
        , "  --help          Display this help message."
        , "  (no arguments)  Parse stdin verbosely."
        , "  (files)         Parse content of files verbosely."
        ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage
        []         -> getContents >>= interpret 2 pProgram
        fs         -> mapM_ (runFile 2 pProgram) fs
