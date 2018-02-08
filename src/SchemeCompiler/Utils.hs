module SchemeCompiler.Utils where

import Numeric


hex2dig = fst . head . readHex
oct2dig = fst . head . readOct
bin2dig = bin2dig' 0
  where
    bin2dig' digint "" = digint
    bin2dig' digint (x:xs) =
      let old = 2 * digint + (if x == '0' then 0 else 1)
      in bin2dig' old xs

