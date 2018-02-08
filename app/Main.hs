module Main (main) where

import System.Environment
import Control.Monad (liftM)

import SchemeCompiler.REPL
import SchemeCompiler.Lisp

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne args
