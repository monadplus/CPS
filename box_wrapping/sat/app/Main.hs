module Main where

import           Data.Foldable (traverse_)
import           Data.List     (intercalate)
import           SAT

main :: IO ()
main = do

  -- Read input and translate it to the BWP statement
  statement <- readStatement

  -- Translate the BWP problem into a CNF formula
  let sat = runSAT buildSAT statement
      cnf = sat ^. clauses . to coerceCNF
      description = getDescription sat

  -- printCNF cnf

  -- Execute MIOS SAT solver with the given CNF formula
  sol <- solveSAT description cnf

  ---- Translate SAT solution back to BWP and print the BWP solution
  let bws = translateSolution sat sol
  printBoxWrappingSolution bws







printCNF :: [[Int]] -> IO ()
printCNF = traverse_ (print . intercalate " " . fmap show)
