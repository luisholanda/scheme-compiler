module SchemeCompiler.Lisp
  ( symbol
  , spaces
  , Env(..)
  , LispVal(..)
  , LispError(..)
  , ThrowsError(..)
  , IOThrowsError(..)
  , trapError
  , throwError
  , catchError
  , extractValue
  , returnOrThrow
  ) where

import           Control.Monad.Error
import           Data.IORef
import           System.IO (Handle)
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Character Char
  | Float Float
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle
  | Func { params :: [String]
         , vararg :: Maybe String
         , body :: [LispVal]
         , closure :: Env
         }

type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ErrorT LispError IO

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/<:=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | ValueError String LispVal
               | Default String

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

returnOrThrow :: ThrowsError a -> ThrowsError a
returnOrThrow = either throwError return