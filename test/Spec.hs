import SchemeCompiler.Parser (readExpr)
import SchemeCompiler.Evaluator (eval)


main :: IO ()
main = testExpr


testExpr :: IO ()
testExpr = do
  let test = print . eval . readExpr
  test "(a test)"
  test "(a (nested) test)"
  test "(a (dotted . list) test)"
  test "(a '(quoted (dotted . list)) test)"
  test "(a '(imbalanced parens)"
