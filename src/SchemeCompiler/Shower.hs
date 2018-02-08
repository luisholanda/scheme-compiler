module SchemeCompiler.Shower where

import           SchemeCompiler.Lisp
import Data.Monoid ((<>))

showVal :: LispVal -> String
showVal (String contents) = show contents
showVal (Character c)     = "#\\" ++ [c]
showVal (Atom name)       = name
showVal (Number contents) = show contents
showVal (Bool True)       = "#t"
showVal (Bool False)      = "#f"
showVal (List l)          = "(" ++ unwordsList l ++ ")"
showVal (DottedList h t)  = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {params = args, vararg = varargs, body = _, closure = _} =
  "(lambda (" <>  unwords (map show args) <>
    (case varargs of
      Nothing -> ""
      Just arg -> " . " <> arg) <> ") ...)"

instance Show LispVal where
  show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ showVal form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args : found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type:expected " ++ expected ++ " , found " ++ showVal found
showError (Parser parseErr) = " Parse error at " ++ show parseErr

instance Show LispError where
  show = showError
