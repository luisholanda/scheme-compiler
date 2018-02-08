module SchemeCompiler.Prim.Bool
  ( boolPrimitives
  ) where

import SchemeCompiler.Lisp
import SchemeCompiler.Prim.Base


boolPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
boolPrimitives =
  [ ("boolean?", unaryOp isBoolean)
  , ("not", unaryOp not')
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  ]


boolBoolBinop = boolBinop unpackBool

not' :: LispVal -> ThrowsError LispVal
not' (Bool b) = return . Bool $ not b
not' notBool  = throwError $ TypeMismatch "boolean" notBool

isBoolean :: LispVal -> ThrowsError LispVal
isBoolean (Bool _) = return $ Bool True
isBoolean _ = return $ Bool False
