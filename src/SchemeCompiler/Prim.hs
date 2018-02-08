{-# LANGUAGE ExistentialQuantification #-}

module SchemeCompiler.Prim
  ( primitives
  , eqv
  , equal
  ) where

import           Control.Monad
import           Data.Monoid                ((<>))
import           SchemeCompiler.Lisp
import           SchemeCompiler.Prim.Base
import           SchemeCompiler.Prim.Bool
import           SchemeCompiler.Prim.Number
import           SchemeCompiler.Prim.String

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("list?", unaryOp isList)
  , ("null?", unaryOp null')
  , ("char?", unaryOp isChar)
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  ] <>
  basePrimitives <>
  symbolPrimitives <>
  stringPrimitives <>
  boolPrimitives <>
  numericPrimitives

isList :: LispVal -> ThrowsError LispVal
isList (List _) = return $ Bool True
isList _        = return $ Bool False

isPair :: LispVal -> ThrowsError LispVal
isPair (DottedList [a] b) = return $ Bool True
isPair _                  = return $ Bool False

null' :: LispVal -> ThrowsError LispVal
null' (List []) = return $ Bool True
null' (List _)  = return $ Bool False
null' x         = throwError $ TypeMismatch "list" x

isChar :: LispVal -> ThrowsError LispVal
isChar (Character _) = return $ Bool True
isChar _             = return $ Bool False

-- | List primitives
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [xs] x]   = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return . List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList
