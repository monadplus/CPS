{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module SAT (
    module Control.Lens
  , module SAT.Mios
  , module SAT
  ) where

----------------------------------------------
-- Imports
----------------------------------------------

import           Control.Arrow              ((&&&))
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Coerce
import           Data.Foldable              (traverse_)
import           Data.Maybe                 (mapMaybe, catMaybes)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Tuple                 (swap)
import           SAT.Mios
import           Text.Read                  (readMaybe)

----------------------------------------------
-- Data Types
----------------------------------------------

data Box = Box
    { _width  :: Int
    , _height :: Int
    }
  deriving stock (Show)
makeLenses ''Box

getMaxLength :: [Box] -> Int
getMaxLength = maximum . fmap getLength
  where
    getLength Box{..} = max _width _height

type Coord = (Int, Int)

----------------------------------------------
-- BWP -> SAT
----------------------------------------------

-- The problem will be translated to rectangular box where each position is a variable.
--
-- It will additionally require:
--  - A vector of rotations variables, one for each box.
--       A negated rotation variable means that the box has been rotated.
--  - A vector of coordinates vector for the overlapping.
--
--      +--------------+
--     /|             /| |
--    / |            / | |
--   *--+-----------*  | |
--   |  |           |  | |  maxLength
--   |  |           |  | |
--   |  |           |  | |
--   |  |           |  | |
--   |  +-----------|--+
--   | /            | / /
--   |/             |/ /  #boxes
--   *--------------* /
--    -------------
--        width

newtype Variable = Variable { variable :: Int }
  deriving newtype (Show)

-- | Disjunctive clause
newtype Clause = Clause { variables :: [Variable] }
  deriving newtype (Show)

-- | Conjunction of disjunction of variables.
type CNF = [Clause]

-- | State S for the building of the BWP.
data S = S
    { _boxes     :: [Box]
    , _w         :: Int             -- ^ w
    , _maxLength :: Int             -- ^ maxLength
    , _nVars     :: Int             -- ^ Number of variables
    , _rotations :: [Variable]      -- ^ Mapping between ith box and rotation variable
    , _clauses   :: CNF
    }
  deriving stock (Show)
makeLenses ''S

-- | The SAT Monad
type StateS a = State S a
type SAT = StateS ()

neg :: Variable -> Variable
neg = Variable . negate . variable

emptyClause :: Clause
emptyClause = Clause []

addClause :: Clause -> SAT
addClause c = clauses %= (c :)

addClauses :: [Clause] -> SAT
addClauses = traverse_ addClause

increaseNVars :: Int -> SAT
increaseNVars x = nVars += x

class Disjunctive a b where
  (\/) :: a -> b -> Clause
  infixr 8 \/

instance Disjunctive Variable Variable where
  var1 \/ var2 = Clause [var1, var2]

instance Disjunctive Variable Clause where
  var \/ Clause{..} = Clause (var:variables)

instance Disjunctive Clause Variable where
  Clause{..} \/ var = Clause (var:variables)

instance Disjunctive Clause Clause where
  (Clause vars1) \/ (Clause vars2) = Clause (vars1 ++ vars2)

newS :: [Box] -> Int -> Int -> S
newS boxes width maxLen = S
    { _boxes     = boxes
    , _w         = width
    , _maxLength = maxLen
    , _nVars     = nvars + length rots
    , _rotations = rots
    , _clauses   = []
    }
  where
    boxesSize = length boxes
    nvars = width * maxLen * boxesSize
    rots = coerce [nvars+1 .. nvars+1+boxesSize]



-- | Returns the variable associated with the coordinate and the ith box.
bVar :: S -> Coord -> Int -> Variable
bVar S{..} (x, y) b =
  Variable (x + y*_w + b*_w*_maxLength + 1)
  --  ^^^^^^^ + 1 because variables start from 1 instead of 0

-- | Conjunction of variables.
conjunctionOf :: [Variable] -> Clause
conjunctionOf = foldr (\/) emptyClause

-- | At least one,
alo :: [Variable] -> Clause
alo = conjunctionOf

-- At most one constraint: quadractic encoding
amoQ :: [Variable] -> [Clause]
amoQ variables =
  [ neg (variables !! i) \/ neg (variables !! j)
  | i <- [0  ..(length variables - 1)]
  , j <- [i+1..(length variables - 1)]
  ]

-- At most one constraint: logarithmic encoding
amoL :: [Variable] -> [Clause]
amoL variables = undefined

-- At most one constraint: heule encoding
amoH :: [Variable] -> [Clause]
amoH variables = undefined

-- | For each box, executes for all x,y coordinates the given function 'f'.
forEachBox :: ([Variable] -> r) -> StateS [r]
forEachBox f = do
  s@S{..} <- get
  return $ [ f [ bVar s (x,y) b | x <- [0..(_w - 1)]
                                , y <- [0..(_maxLength - 1)]]
           | b <- [0..(length _boxes - 1)]]

-- | Boxes must be placed exactly once in the paper roll.
exactlyOne :: SAT
exactlyOne =
  traverse_ (\(c,cs) -> addClauses (c:cs))
      =<< forEachBox (alo &&& amoQ)

-- | Boxes must be placed inside the paper roll.
insideTheBounds :: SAT
insideTheBounds = do
  s@S{..} <- get
  let boundsClauses =
        concat [ let box = _boxes !! b
                     var x y = bVar s (x,y) b
                     rot = s ^?! rotations . ix b

                     -- When width = height, there is no need to test rotation.
                     fixRotation =
                       if box^.width == box^.height then
                         \clauses -> (neg rot \/ emptyClause) : clauses
                       else
                         id

                     cs = catMaybes [ computeClause (x,y) box _w _maxLength rot (var x y)
                                    | x <- [0..(_w - 1)]
                                    , y <- [0..(_maxLength - 1)]
                                    ]

                  in fixRotation cs

               | b <- [0..(length _boxes - 1)]
               ]
  addClauses boundsClauses

  where
    computeClause :: Coord -> Box -> Int -> Int -> Variable -> Variable -> Maybe Clause
    computeClause (x,y) box w maxLength rot var =
       -- No rotation
       if box^.width == box^.height then
         if x > w - box^.width || y > maxLength - box^.height
             then Just $ neg var \/ emptyClause
         else Nothing

       -- May rotate
       else
         -- Not rotated
         if x > w - box^.width || y > maxLength - box^.height
           then Just $ rot \/ neg var
         -- Rotated
         else if x > w - box^.height || y > maxLength - box^.width
           then Just $ neg rot \/ neg var
         else Nothing

-- TODO
buildSAT :: SAT
buildSAT = do
  -- Symmetry: first box on (0,0) [the biggest one]
  exactlyOne
  insideTheBounds
  -- Overlapping

----------------------------------------------
-- SAT -> BWP
----------------------------------------------

(!) :: (Show a, Show b) => a -> b -> String
(!) x y = show x <> " " <> show y
infixr 8 !

type MiosSolution = [Int]

data BoxSol = BoxSol
    { tl :: Coord
    , br :: Coord
    }

instance Show BoxSol where
  show (BoxSol (tl_x, tl_y) (br_x, br_y)) =
    tl_x ! tl_y <> "  " <> br_x ! br_y

data BWPSolution = BWPSolution
    { len          :: Int
    , boxSolutions :: [BoxSol]
    }

type RawBoxesSol = [Int] -- ^ Length = w * maxLength * #boxes
type RawBoxSol = [Int]   -- ^ Length = w * maxLength

type RawRotsSol = [Int]  -- ^ Length = #boxes
type RawRotSol = Int

getLen :: [BoxSol] -> Int
getLen =
  (+) 1 . foldr (\BoxSol{..} -> max (snd br)) 0

getSections :: S -> MiosSolution -> (RawBoxesSol, RawRotsSol)
getSections S{..} sol = (boxSec, rotSec)
  where
    (boxSec, rem) = splitAt (length _boxes*_w*_maxLength) sol
    rotSec = take (length _rotations) rem

-- Get the top-left coordinates
getCoord :: S -> RawBoxSol -> Coord
getCoord S{..} raw =
  let pos = length $ takeWhile (< 0) raw
   in (pos `mod` _w,  pos `div` _w)

translateBox :: S -> Box -> RawBoxSol -> RawRotSol -> BoxSol
translateBox s b rawBox rawRot =
    BoxSol{ tl = (tl_x, tl_y)
          , br = (tl_x + w, tl_y + h)
          }
  where
    (tl_x,tl_y) = getCoord s rawBox

    rot = if rawRot > 0 then id else swap

    (w, h) = rot (b^.width, b^.height)


translateSolution :: S -> MiosSolution -> BWPSolution
translateSolution s@S{..} sol = BWPSolution{..}
  where
    (rawBoxes, rawRots) = getSections s sol

    boxSolutions =
      let (_, _, bxs) = foldr go (rawBoxes, rawRots, []) _boxes
        in reverse bxs

    go box (bs, rs, acc) =
      let (b, bs') = splitAt (_w * _maxLength) bs
          ([r], rs') = splitAt 1 rs
       in (bs', rs', (translateBox s box b r):acc)

    len = getLen boxSolutions

----------------------------------------------
-- Parsing & Pretty Printing
----------------------------------------------

-- | Input Example
--
--   10 10
--   3  2 4
--   6  1 2
--   1  2 2


-- | Output Example
--
--   10 10
--   3   2 4
--   6   1 2
--   1   2 2
--   4
--   0 0   3 1
--   5 2   8 3
--   6 0   9 1
--   2 2   3 3
--   4 0   4 1
--   1 2   1 3
--   4 2   4 3
--   0 2   0 3
--   9 2   9 3
--   5 0   5 1

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

readStatement :: IO S
readStatement = do
  -- Read input and translate it to the BWP
  str <- T.getLine
  let [w, n] = mapMaybe readInt (T.splitOn " " str)
  (strs, bxs) <- readBoxes n
  let maxLen = getMaxLength bxs

  -- Print the BWP statement
  traverse_ T.putStrLn  (str:strs)

  return $ newS bxs w maxLen

printBoxWrappingSolution :: BWPSolution -> IO ()
printBoxWrappingSolution BWPSolution{..} =
  print len >> traverse_ print boxSolutions

----------------------------------------------
-- Utils
----------------------------------------------

getDescription :: S -> CNFDescription
getDescription s = CNFDescription
   (s ^. nVars)         -- # variables
   (lengthOf clauses s) -- # clauses
   mempty               -- FilePath

runSAT :: SAT -> S -> S
runSAT = execState

coerceCNF :: CNF -> [[Int]]
coerceCNF = coerce
