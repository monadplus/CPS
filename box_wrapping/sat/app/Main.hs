module Main where

import           Data.Foldable (traverse_)
import           Data.List     (intercalate)
import           SAT
import           System.IO
import           Data.Coerce

main :: IO ()
main = do

  -- Read input and translate it to the BWP statement
  (rawStatement, statement) <- readStatement

  -- Translate the BWP problem into a CNF formula
  let sat = runSAT buildSAT statement
      cnf = sat ^. clauses . to coerceCNF
      description = getDescription sat

  writeSAT sat

  -- We print the statement before the solution
  traverse_ putStrLn rawStatement

  -- Execute MIOS SAT solver with the given CNF formula
  sol <- solveSAT description cnf
  --print sol

  -- Translate SAT solution back to BWP and print the BWP solution
  let bws = translateSolution sat sol
  printBoxWrappingSolution bws

--------------------

updateStatement :: Int -> S -> S
updateStatement newMaxLen s =
  s & maxLength .~ newMaxLen
    & clauses .~ []

writeSAT :: S -> IO ()
writeSAT s =
  withFile "statement.cnf" WriteMode $ \h -> do
    hPutStrLn h $ "p cnf " ++ nvars ! nclauses
    traverse_ (hPutStrLn h . show)
              (s^.clauses)
  where
    nclauses = lengthOf (clauses.traverse) s
    nvars = maximum . fmap abs . concat $ (coerce (s^.clauses) :: [[Int]])
