module SchemeCompiler.REPL where

import Control.Monad
import System.IO
import SchemeCompiler.Env
import SchemeCompiler.Evaluator hiding (makeFunc)
import SchemeCompiler.Parser
import SchemeCompiler.Lisp
import SchemeCompiler.Prim (primitives)
import SchemeCompiler.Prim.IO (ioPrimitives)
import Data.Monoid ((<>))

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows . fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a ->  Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars primitiveList
  where
    primitiveList = map (makeFunc PrimitiveFunc) primitives
               <> map (makeFunc IOFunc) ioPrimitives
    makeFunc constructor (var, func) = (var, constructor func)

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (show <$> eval env (List [Atom "load", String (head args)]))
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt  "Lisp>>> ") . evalAndPrint
