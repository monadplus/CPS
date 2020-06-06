module Main where

import           Data.Foldable (traverse_)
import           Data.List     (intercalate)
import           SAT

main :: IO ()
main = do

  -- Read input and translate it to the BWP statement
  (rawStatement, statement) <- readStatement

  -- Translate the BWP problem into a CNF formula
  let sat = runSAT buildSAT statement
      cnf = sat ^. clauses . to coerceCNF
      description = getDescription sat

  putStrLn $ "Variables = " ++ sat^.nVars.to show
  putStrLn $ "Clauses = " ++ sat^.clauses.to (show . length)

  -- We print the statement before the solution
  traverse_ print rawStatement

  -- sat: Data.Vector.Mutable: uninitialised element. If you are trying to compact a vector, use the 'force' function to remove uninitialised elements from the underlying array.

  -- Execute MIOS SAT solver with the given CNF formula
  sol <- description `seq` cnf `seq` solveSAT description cnf

  ---- Translate SAT solution back to BWP and print the BWP solution
  let bws = translateSolution sat sol
  printBoxWrappingSolution bws

--printCNF :: [[Int]] -> IO ()
--printCNF = traverse_ (print . intercalate " " . fmap show)
