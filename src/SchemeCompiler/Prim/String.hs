module SchemeCompiler.Prim.String
  ( stringPrimitives
  ) where

import           Control.Monad
import           Data.Char                (toUpper)
import           SchemeCompiler.Lisp
import           SchemeCompiler.Prim.Base
import Data.Monoid ((<>))

stringPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
stringPrimitives =
  [ ("string?", unaryOp isString)
  , ("string=?", strBoolBinop (==))
  , ("string>?", strBoolBinop (>))
  , ("string<?", strBoolBinop (<))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("string-ci=?", strBoolBinop $ caseInsensitiveBinop (==))
  , ("string-ci>?", strBoolBinop $ caseInsensitiveBinop (>))
  , ("string-ci<?", strBoolBinop $ caseInsensitiveBinop (<))
  , ("string-ci<=?", strBoolBinop $ caseInsensitiveBinop (<=))
  , ("string-ci>=?", strBoolBinop $ caseInsensitiveBinop (>=))
  , ("make-string", makeString)
  , ("string", string)
  , ("string-length", stringLength)
  , ("string-ref", stringRef)
  , ("substring", substring)
  , ("string-append", stringAppend)
  , ("string->list", stringToList)
  , ("list->string", listToString)
  , ("string-fill!", stringFill)
  ]

strBoolBinop = boolBinop unpackStr

isString :: LispVal -> ThrowsError LispVal
isString (String _) = return $ Bool True
isString _          = return $ Bool False

makeString :: [LispVal] -> ThrowsError LispVal
makeString [Number k, Character char] = return . String $ replicate (fromInteger k) char
makeString [Number _, x] = throwError $ TypeMismatch "char" x
makeString [x, Character _] = throwError $ TypeMismatch "number" x
makeString badArgs = throwError $ NumArgs 2 badArgs

string :: [LispVal] -> ThrowsError LispVal
string val@(Character c:args) = String <$> createString val
string (x:args)               = throwError $ TypeMismatch "char" x
string []                     = throwError $ NumArgs 1 []

createString :: [LispVal] -> ThrowsError String
createString (Character x:xs) = return $ x : extractValue (createString xs)
createString (x:xs)           = throwError $ TypeMismatch "char" x
createString []               = return ""

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [String x] = return . Number . toInteger $ length x
stringLength [x]        = throwError $ TypeMismatch "string" x
stringLength xs         = throwError $ NumArgs 1 xs

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [String s, Number k] = return . Character $ s !! fromInteger k
stringRef [String _, x]        = throwError $ TypeMismatch "number" x
stringRef [x, Number _]        = throwError $ TypeMismatch "string" x
stringRef badForm              = throwError $ NumArgs 2 badForm

substring :: [LispVal] -> ThrowsError LispVal
substring [String s, Number start, Number end]
  | start >= 0 && end < toInteger (length s) = return . String
                                             $ map ((s !!) . fromInteger) [start..end]
  | otherwise = throwError $ ValueError "bad slice" (List [Number start, Number end])
substring badArgs
  | length badArgs /= 3 = throwError $ NumArgs 3 badArgs
  | otherwise = throwError $ TypeMismatch "string number" (List badArgs)

capitalize :: String -> String
capitalize = map toUpper

caseInsensitiveBinop :: (String -> String -> a) -> String -> String -> a
caseInsensitiveBinop op a b = capitalize a `op` capitalize b

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend args@(String s:ss) = String <$> _stringAppend args
  where
    _stringAppend :: [LispVal] -> ThrowsError String
    _stringAppend (String x:xs) = (x <>) <$> _stringAppend xs
    _stringAppend (x:xs) = throwError $ TypeMismatch "string" x
    _stringAppend [] = return ""
stringAppend badArgs = throwError $ TypeMismatch "string" (List badArgs)

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [String s] = return . List $ map Character s
stringToList [x] = throwError $ TypeMismatch "string" x
stringToList badArgs = throwError $ NumArgs 1 badArgs

listToString :: [LispVal] -> ThrowsError LispVal
listToString [List chars@(Character _:_)] = String <$> _listToString chars
  where
    _listToString :: [LispVal] -> ThrowsError String
    _listToString (Character c:cs) = (c:) <$> _listToString cs
    _listToString (x:_) = throwError $ TypeMismatch "string" x
    _listToString [] = return ""
listToString [x] = throwError $ TypeMismatch "list of chars" x
listToString badArgs = throwError $ NumArgs 1 badArgs

stringFill :: [LispVal] -> ThrowsError LispVal
stringFill [String s, Character c] = return . String $ replicate (length s) c
stringFill [String _, x] = throwError $ TypeMismatch "character" x
stringFill [x, Character _] = throwError $ TypeMismatch "string" x
stringFill badArgs = throwError $ NumArgs 2 badArgs
