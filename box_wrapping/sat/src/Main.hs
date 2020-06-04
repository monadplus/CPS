{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Data.List    as List
import           Data.Maybe   (mapMaybe)
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           SAT.Mios
--import           Text.Printf
import           Text.Read    (readMaybe)
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import           Control.Monad.State.Strict
import           Control.Lens
import           Data.Coerce
import           Data.Foldable(traverse_)

----------------------------------------------
-- Parsing
----------------------------------------------

data Box = Box
    { _width  :: Int
    , _height :: Int
    }
  deriving (Show)

makeLenses ''Box

readBoxes :: Int -> IO ([Text], [Box])
readBoxes = go [] []
  where
    go acc bs 0 = pure (acc, bs)
    go acc bs n = do
      line <- T.getLine
      let [m, w, h] = mapMaybe readInt (T.splitOn " " line)
      let b   = Box w h
          bs' = replicate m b
      go (acc ++ [line]) (bs ++ bs') (n - m)

readInt :: Text -> Maybe Int
readInt = readMaybe . T.unpack

getMaxLength :: [Box] -> Int
getMaxLength = maximum . fmap getLength
  where
    getLength Box{..} = max _width _height

----------------------------------------------
-- Printing
----------------------------------------------

--printBoxes :: [Box] -> IO ()
--printBoxes boxes = putStrLn $ List.intercalate "\n" (show <$> boxes)

-- Output Example

--10 10
--3   2 4
--6   1 2
--1   2 2
--4
--0 0   3 1
--5 2   8 3
--6 0   9 1
--2 2   3 3
--4 0   4 1
--1 2   1 3
--4 2   4 3
--0 2   0 3
--9 2   9 3
--5 0   5 1

printSolution :: [Int] -> IO ()
printSolution _ = putStrLn "TODO"

----------------------------------------------
-- Main
----------------------------------------------

newtype Variable = Variable { variable :: Int }

-- | Disjunctive clause
newtype Clause = Clause { clause :: [Variable] }

singleClause :: Variable -> Clause
singleClause variable = Clause [variable]

-- | Conjunction of disjunction of variables.
type CNF = [Clause]

data S = S
  { _boxes     :: [Box]
  , _maxWidth  :: Int        -- ^ w
  , _maxLength :: Int        -- ^ maxLength
  , _nVars     :: Int        -- ^ Number of variables
  , _rotations :: Vector Int -- ^ Mapping between ith box and rotation variable
  , _clauses   :: CNF
  }

makeLenses ''S

newS :: [Box] -> Int -> Int -> S
newS boxes w l = S
  { _boxes = boxes
  , _maxWidth = w
  , _maxLength = l
  , _nVars = 0
  , _rotations = Vector.empty
  , _clauses = []
  }

-- | The SAT Monad.
type SAT = State S ()

addClause :: Clause -> SAT
addClause clause = clauses %= (clause:)

(\/) :: Variable -> Clause -> Clause
v \/ Clause{..} = Clause { clause = v:clause }
infixr 4 \/

desc :: S -> CNFDescription
desc s = CNFDescription
   (s ^. nVars)         -- # variables
   (lengthOf clauses s) -- # clauses
   mempty               -- FilePath

prepareSAT :: [Box] -> Int -> Int -> SAT
prepareSAT boxes w maxLength =
  undefined

runSAT :: SAT -> S -> S
runSAT = execState

coerceCNF :: CNF -> [[Int]]
coerceCNF = coerce

main :: IO ()
main = do
  str <- T.getLine
  let [w, n] = mapMaybe readInt (T.splitOn " " str)
  (strs, boxes)  <- readBoxes n
  traverse_ T.putStrLn  (str:strs)
  let maxLength = getMaxLength boxes
      sat = runSAT (prepareSAT boxes w maxLength) (newS boxes w maxLength)
      cnf = sat ^. clauses
  sol <- solveSAT (desc sat) (coerceCNF cnf)
  printSolution sol
