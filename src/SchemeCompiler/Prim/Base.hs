{-# LANGUAGE ExistentialQuantification #-}

module SchemeCompiler.Prim.Base
  ( unaryOp
  , unpackNum
  , unpackStr
  , unpackBool
  , boolBinop
  , eqv
  , equal
  , basePrimitives
  , symbolPrimitives
  ) where

import SchemeCompiler.Lisp


basePrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
basePrimitives =
  [ ("eqv?", eqv)
  , ("eq?", eqv)
  , ("equal?", equal)
  ]


-- | Function base for creating unary operations
unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [arg] = op arg
unaryOp op as@(x:y:xs) = throwError $ NumArgs 1 as


-- | Function base for creating binary boolean operations
boolBinop :: (LispVal -> ThrowsError a)
          -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ head args
      right <- unpacker . head $ tail args
      return . Bool $ left `op` right

-- | Equivalent (eqv) function
eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool a1, Bool a2] = return . Bool $ a1 == a2
eqv [Number a1, Number a2] = return . Bool $ a1 == a2
eqv [String a1, String a2] = return . Bool $ a1 == a2
eqv [Atom a1, Atom a2] = return . Bool $ a1 == a2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List a1, List a2] = return . Bool $ sameList
  where
    sameList = length a1 == length a2 && all eqvPair (zip a1 a2)
    eqvPair (x1, x2) =
      case eqv [x1, x2] of
        Left err -> False
        Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


-- | Equal Function
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a1 a2 (AnyUnpacker unpacker) =
  areEqual `catchError` const (return False)
  where
    areEqual = do
      unpacked1 <- unpacker a1
      unpacked2 <- unpacker a2
      return $ unpacked1 == unpacked2

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                          if null parsed
                            then throwError . TypeMismatch "number" $ String n
                            else return . fst . head $ parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- Equal function definition
equal :: [LispVal] -> ThrowsError LispVal
equal [List a1, List a2] = do
  areEquals <- and <$> mapM pureEqv pairsList
  return $ Bool areEquals
  where
    pairsList = map (\(x, y) -> [x, y]) $ zip a1 a2
    pureEqv [x, y] =
      let areEqual = equal [x, y] in
      case areEqual of
        Right (Bool x) -> Right x
        Right notBool  -> throwError $ TypeMismatch "boolean" notBool
        Left err -> throwError err
equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2)
                                 [ AnyUnpacker unpackNum
                                 , AnyUnpacker unpackStr
                                 , AnyUnpacker unpackBool
                                 ]
  eqvEquals <- eqv [arg1, arg2]
  return . Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


-- | Symbol functions

symbolPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
symbolPrimitives =
  [ ("symbol?", unaryOp isSymbol)
  , ("symbol->string", unaryOp symbolToString)
  , ("string->symbol", unaryOp stringToSymbol)
  ]

isSymbol :: LispVal -> ThrowsError LispVal
isSymbol (Atom _) = return $ Bool True
isSymbol _        = return $ Bool False

symbolToString :: LispVal -> ThrowsError LispVal
symbolToString (Atom x)  = return $ String x
symbolToString nonSymbol = throwError $ TypeMismatch "symbol" nonSymbol

stringToSymbol :: LispVal -> ThrowsError LispVal
stringToSymbol (String s) = return $ Atom s
stringToSymbol nonString  = throwError $ TypeMismatch "string" nonString
