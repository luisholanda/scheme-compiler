module SchemeCompiler.Evaluator where

import           Data.Either
import           Data.Maybe
import           SchemeCompiler.Env
import           SchemeCompiler.Lisp
import           SchemeCompiler.Prim
import SchemeCompiler.Parser (readExprList)
import Control.Monad.Error (liftIO)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Character _) = return val
eval env (Atom id) = getVar env id
-- if block
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    Bool True  -> eval env conseq
    nonBoolean -> throwError $ TypeMismatch "boolean" nonBoolean
-- cond block
eval env (List (Atom "cond":clauses)) = evaluateCondBlock env clauses
-- case block
eval env (List (Atom "case":expressions)) = evaluateCaseBlock env key clauses
  where
    key = head expressions
    clauses = tail expressions
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "load", String filename]) = load filename
                                             >>= fmap last . mapM (eval env)
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var: params) varargs : body)) =
  makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList  params varargs : body)) =
  makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarargs varargs env [] body
eval env (List (func@(PrimitiveFunc _):args)) = mapM (eval env) args >>= apply func
eval env (List (func@Func {}:args)) = mapM (eval env) args >>= apply func
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm = throwError $ BadSpecialForm "unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else (liftIO . bindVars closure $ zip params args)
         >>= bindVarsArgs varargs
         >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = last <$> mapM (eval env) body
    bindVarsArgs arg env =
      case arg of
        Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
        Nothing -> return env


-- | Functions for `cond` block
evaluateCondBlock :: Env -> [LispVal] -> IOThrowsError LispVal
evaluateCondBlock env (c:cs) = do
  clauseValue <- evaluateCondClause env c
  fromMaybe (evaluateCondBlock env cs) clauseValue

evaluateCondClause :: Env -> LispVal -> IOThrowsError (Maybe (IOThrowsError LispVal))
evaluateCondClause env (List (Atom "else":clauses)) = evaluateExpressions env clauses
evaluateCondClause env (List (test:Atom "=>":procedure)) = do
  testValue <- eval env test
  case testValue of
    Bool False -> return Nothing
    nonFalse   -> return . Just $ eval env nonFalse
evaluateCondClause env (List (test:expressions)) = do
  testValue <- eval env test
  case testValue of
    Bool False -> return Nothing
    Bool True  -> evaluateExpressions env expressions
    nonBool    -> return . Just $ return nonBool
evaluateCondClause env badForm = throwError $ BadSpecialForm "bad cond clause form" badForm

evaluateExpressions env = return . Just . last . map (eval env)

-- | Functions for `case` block
evaluateCaseBlock :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evaluateCaseBlock env key [] = throwError $ BadSpecialForm "bad case block" key
evaluateCaseBlock env key (c:cs) = do
  keyValue <- eval env key
  clauseValue <- evaluateCaseClause env keyValue c
  fromMaybe (evaluateCaseBlock env key cs) clauseValue

evaluateCaseClause :: Env -> LispVal -> LispVal -> IOThrowsError (Maybe (IOThrowsError LispVal))
evaluateCaseClause env key (List (Atom "else":expressions)) =
  evaluateExpressions env expressions
evaluateCaseClause env key (List datumsAndExpression) =
  if checkKey key datums
    then return . Just $ eval env expression
    else return Nothing
  where
    datums = init datumsAndExpression
    expression = last datumsAndExpression
evaluateCaseClause env key badForm = throwError
                                  $ BadSpecialForm "bad case clause form" badForm

checkKey :: LispVal -> [LispVal] -> Bool
checkKey key = foldr (\(Bool b) acc -> acc && b) True . rights . map (\d -> equal [key, d])


-- | Functions fro function definitions
makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map show params) varargs body env

makeNormalFunc :: Monad m => Env -> [LispVal] -> [LispVal] -> m LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: Monad m => LispVal -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeVarargs = makeFunc . Just . show

-- | IO Things
load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList