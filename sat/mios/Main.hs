module Main where

import           SAT.Mios
import           Text.Printf

clauses :: [[Int]]
clauses =
  [
    [ 1,  2]
  , [ 1,  3]
  , [-1, -2]
  , [ 1, -2, 3]
  , [-3]
  ]

desc :: CNFDescription
desc = CNFDescription
       (maximum . map abs . concat $ clauses)
       (length clauses)
       ""

main :: IO ()
main = do
  asg <- solveSAT desc clauses
  putStrLn $
    if null asg
      then "==UNSAT=="
      else printf "==SAT==\n%s" (show asg)
