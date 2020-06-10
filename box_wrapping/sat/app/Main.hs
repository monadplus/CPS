module Main where

import           Data.Foldable (traverse_)
import           Data.List     (intercalate)
import           SAT
import           System.IO
import           Data.Coerce
import           Text.Printf

main :: IO ()
main = do

  -- Read input and translate it to the BWP statement
  (rawStatement, statement) <- readStatement

  -- We print the statement before the solution
  traverse_ putStrLn rawStatement

  let sat = runSAT buildSAT statement
      cnf = sat ^. clauses . to coerceCNF
      description = getDescription sat
  sol <- solveSAT description cnf
  --if checkSolution
  let bws = translateSolution sat sol

  writeSAT sat

  printBoxWrappingSolution bws

--------------------

--loop :: S -> Maybe (BWPSolution, S)
--loop initial = do
  --foldM

-- TODO better checks
checkSolution :: MiosSolution -> Bool
checkSolution = not . null

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
