module SchemeCompiler.Prim.Number
  ( numericPrimitives
  ) where

import           SchemeCompiler.Lisp
import           SchemeCompiler.Prim.Base

numericPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
numericPrimitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("=", numBoolBinop (==))
  , (">", numBoolBinop (>))
  , ("<", numBoolBinop (<))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("modulo", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("max", numericBinop max)
  , ("min", numericBinop min)
  , ("gcd", numericBinop gcd)
  , ("lcm", numericBinop lcm)
  , ("abs", unaryOp abs')
  , ("number?", unaryOp isNumber)
  , ("zero?", unaryOp isZero)
  , ("positive?", unaryOp isPositive)
  , ("negative?", unaryOp isNegative)
  , ("odd?", unaryOp isOdd)
  , ("even?", unaryOp isEven)
  , ("number->string", unaryOp numberToString)
  , ("string->number", unaryOp stringToNumber)
  ]

numBoolBinop = boolBinop unpackNum

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

isNumber :: LispVal -> ThrowsError LispVal
isNumber (Number _) = return $ Bool True
isNumber _          = return $ Bool False

isZero :: LispVal -> ThrowsError LispVal
isZero (Number x) = return $ Bool (x == 0)
isZero nonNumber = throwError $ TypeMismatch "number" nonNumber

isPositive :: LispVal -> ThrowsError LispVal
isPositive (Number x) = return $ Bool (x > 0)
isPositive nonNumber  = throwError $ TypeMismatch "number" nonNumber

isNegative :: LispVal -> ThrowsError LispVal
isNegative (Number x) = return $ Bool (x < 0)
isNegative nonNumber  = throwError $ TypeMismatch "number" nonNumber

isOdd :: LispVal -> ThrowsError LispVal
isOdd (Number x) = return $ Bool (odd x)
isOdd nonNumber  = throwError $ TypeMismatch "number" nonNumber

isEven :: LispVal -> ThrowsError LispVal
isEven (Number x) = return $ Bool (even x)
isEven nonNumber  = throwError $ TypeMismatch "number" nonNumber

abs' :: LispVal -> ThrowsError LispVal
abs' (Number x) = return $ Number (abs x)
abs' nonNumber  = throwError $ TypeMismatch "number" nonNumber


numberToString :: LispVal -> ThrowsError LispVal
numberToString (Number x) = return $ String (show x)
numberToString nonNumber  = throwError $ TypeMismatch "number" nonNumber

stringToNumber :: LispVal -> ThrowsError LispVal
stringToNumber (String x) =
  let parsed = reads x in
    if null parsed
      then throwError . TypeMismatch "number" $ String x
      else return . Number . fst . head $ parsed
stringToNumber nonString = throwError $ TypeMismatch "string" nonString

