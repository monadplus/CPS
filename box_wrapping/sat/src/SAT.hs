{-# LANGUAGE BangPatterns           #-}
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
import           Data.List                  (intersperse, sortOn)
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Tuple                 (swap)
import           SAT.Mios
import           Text.Read                  (readMaybe)
import qualified Data.Ord as Ord

----------------------------------------------
-- Data Types
----------------------------------------------

data Box = Box
    { _width  :: Int
    , _height :: Int
    }
  deriving stock (Show)
makeLenses ''Box

instance Eq Box where
  b1 == b2 =
    b1^.width == b2^.width
      && b1^.height == b2^.height

-- By total area
instance Ord Box where
  b1 `compare` b2 =
    let area b = b^.width * b^.height
     in area b1 `compare` area b2

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

instance Show Clause where
  show (Clause vars) =
    concat $ intersperse " " $ fmap show (vars ++ [Variable 0])

-- | Conjunction of disjunction of variables.
type CNF = [Clause]

-- | State S for the building of the BWP.
data S = S
    { _boxes     :: [Box]
    , _w         :: Int        -- ^ w
    , _maxLength :: Int        -- ^ maxLength
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

maybeClause :: Maybe Variable -> Clause
maybeClause Nothing         = emptyClause
maybeClause (Just variable) = Clause [variable]

addClause :: Clause -> SAT
addClause c = clauses %= (c :)

addClauses :: [Clause] -> SAT
addClauses = traverse_ addClause

singleClause :: Variable -> Clause
singleClause var = Clause [var]

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
    , _clauses   = []
    }

-- | Returns the variable associated with the coordinate and the ith box.
bVar :: S -> Coord -> Int -> Variable
bVar S{..} (x, y) b =
  Variable (x + y*_w + b*_w*_maxLength + 1)
  --  ^^^^^^^ + 1 because variables start from 1 instead of 0

bVarS :: Coord -> Int -> StateS Variable
bVarS coord b = (\s -> bVar s coord b) <$> get

getRot :: S -> Int -> Variable
getRot S{..} b =
  Variable ((length _boxes * _w * _maxLength) + 1 + b)

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

-- | Boxes must be placed exactly once in the paper roll.
exactlyOne :: SAT
exactlyOne =
    forEachBox (alo &&& amoQ) >>=
      traverse_ (\(c,cs) -> addClauses (c:cs))
  where
    forEachBox :: ([Variable] -> r) -> StateS [r]
    forEachBox f = do
      s@S{..} <- get
      return $ [ f [ bVar s (x,y) b | x <- [0..(_w - 1)]
                                    , y <- [0..(_maxLength - 1)]]
               | b <- [0..(length _boxes - 1)]]
               -- ^^^^^ Notice we start from second box.

-- | Boxes must be placed inside the paper roll.
insideTheBounds :: SAT
insideTheBounds = do
  s@S{..} <- get
  sequence_  [ addAll >> fixRotation
             | b <- [0..(length _boxes - 1)]
             , let box = _boxes !! b
                   var x y = bVar s (x,y) b
                   rot = getRot s b

                   -- 1) When width = height, there is no need to test rotation.
                   --    We can set it to rotated and just add the negation of each position (x,y)
                   fixRotation =
                     when (box^.width == box^.height) $
                       addClause (neg rot \/ emptyClause)

                   addAll = sequence  [ computeClause (x,y) box _w _maxLength rot (var x y)
                                      | x <- [0..(_w - 1)]
                                      , y <- [0..(_maxLength - 1)]
                                      ]
             ]
         -- ^^^^ Notice we start from second box.
  -- Add all clauses to the CNF.

  where

    computeClause :: Coord -> Box -> Int -> Int -> Variable -> Variable -> SAT
    computeClause (x,y) box w maxLength rot var
       -- See 1)
       | box^.width == box^.height =
           when (x > w - box^.width || y > maxLength - box^.height) $
             addClause (neg var \/ emptyClause)

       -- May rotate: we need to add clauses for both positions.
       | otherwise = do
           -- 1.- Not rotated
           when (x > w - box^.width || y > maxLength - box^.height) $
             addClause (rot \/ neg var)
           -- 2.- Rotated
           when (x > w - box^.height || y > maxLength - box^.width) $
             addClause (neg rot \/ neg var)


-- | For all boxes b1, b2 (b1 < b2) computes the overlapping region form the following implication:
--
--    var(b,(x,y)) ----> neg var(b2,(i,j))
--
-- (x,y) is the traversal of the whole roll paper (except for the margins)
-- (i,j) is the area where the two boxes would collide.
--
-- We also take into account all possible rotations of b1 and box2.
--
--
--  xtl_b1_xy....

--  AND

--  xtl_b1_xy -> c_b1_x+withd and c_b1_y+height
--  AND
--  xtl_b1_xy -> c_b1_x+withd and c_b1_y+height
--  xtl_b1_xy -> c_b1_x+withd and c_b1_y+height
--  xtl_b1_xy -> c_b1_x+withd and c_b1_y+height

--  AND

--  xtl_b2_xy -> c_b2_x+withd and c_b2_y+height
--  AND

--  xtl_b2_xy -> c_b2_x+withd and c_b2_y+height
--  xtl_b2_xy -> c_b2_x+withd and c_b2_y+height
--  xtl_b2_xy -> c_b2_x+withd and c_b2_y+height

--  AND

--  ....
noOverlapping :: SAT
noOverlapping = do
  s@S{..} <- get
  let clauses =
        concat [ computeClauses s b1 w1 h1 rot1 ++ computeClauses s b1 h1 w1 (neg <$> rot1)

                  | b1 <- [0..length _boxes - 1]
                  , let w1 = s ^?! boxes . ix b1 . width
                  , let h1 = s ^?! boxes . ix b1 . height
                  , let rot1 | w1 == h1  = Nothing
                             | otherwise = Just (getRot s b1)
               ]
  addClauses clauses

  where

    computeClauses :: S -> Int -> Int -> Int -> Maybe Variable -> [Clause]
    computeClauses s@S{..} b1 w1 h1 rot1 =
      concat $
        [ computeClauses' (x,y) b2 w2 h2 rot2 ++ computeClauses' (x,y) b2 h2 w2 (neg <$> rot2)

            | b2 <- [b1+1..(length _boxes - 1)]
            , let w2 = s ^?! boxes . ix b2 . width
            , let h2 = s ^?! boxes . ix b2 . height
            , let rot2 | w2 == h2  = Nothing
                       | otherwise = Just (getRot s b2)

            , x  <- [0.._w  - w1]
            , y  <- [0.._maxLength - h1]
        ]

      where

        computeClauses' :: Coord -> Int -> Int -> Int -> Maybe Variable -> [Clause]
        computeClauses' (x,y) b2 w2 h2 rot2 =
          [ neg boxVar1 \/ neg boxVar2 \/ maybeClause rot1 \/ maybeClause rot2

              | i <- [max 0 (x - w2 + 1)  ..  min _w (x + w1) - 1]
              , j <- [max 0 (y - h2 + 1)  ..  min _maxLength (y + h1) - 1]
              , let boxVar1 = bVar s (x,y) b1
              , let boxVar2 = bVar s (i,j) b2
          ]

-- By symmetry, place the biggest box on the coordinate (0,0).
biggestBoxTopLeft :: SAT
biggestBoxTopLeft = do
    s@S{..} <- use id

    -- Set the first box top-left coordinate to true
    --   and the rest to false.
    --
    -- Boxes are sorted in decreasing order.
    addClause =<< singleClause <$> bVarS (0,0) 0
    addClauses $
      fmap (singleClause . neg)
        [ bVar s (x,y) 0 | x  <- [0.._w         - 1]
                         , y  <- [0.._maxLength - 1]
                         , x > 0 || y > 0]

    -- Set the rest of boxes to false on the coordinates used by the first box.
    -- To simplify, we will consider only the smallest area.
    let k = min (s^?!boxes . ix 0 . width) (s^?!boxes . ix 0 . height)
    addClauses $
      fmap (singleClause . neg)
        [ bVar s (x,y) b | b <- [1..(length _boxes - 1)]
                         , x <- [0..k - 1]
                         , y <- [0..k - 1]]

buildSAT :: SAT
buildSAT = do
  biggestBoxTopLeft
  exactlyOne
  insideTheBounds
  noOverlapping

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
    rotSec = take (length _boxes) rem

-- Get the top-left coordinates
getCoord :: S -> RawBoxSol -> Coord
getCoord S{..} raw =
  let pos = length $ takeWhile (< 0) raw
   in (pos `mod` _w,  pos `div` _w)

translateBox :: S -> Box -> RawBoxSol -> RawRotSol -> BoxSol
translateBox s b rawBox rawRot =
    BoxSol{ tl = (tl_x, tl_y)
          , br = (tl_x + w - 1, tl_y + h - 1)
          }
  where
    (tl_x,tl_y) = getCoord s rawBox

    rot = if rawRot > 0 then swap else id

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

readBoxes :: Int -> Int -> IO ([Text], [Box], Int)
readBoxes w = go [] [] 0
  where
    go acc bs !maxLen 0 = pure (acc, bs, maxLen)
    go acc bs !maxLen n = do
      line <- T.getLine
      let [m, width, height] = mapMaybe readInt (T.splitOn " " line)
      let b   = Box width height
          bs' = replicate m b
          l   = min (upperBoundLength m width height)
                    (upperBoundLength m height width)
      go (acc ++ [line]) (bs ++ bs') (maxLen + l) (n - m)

    upperBoundLength :: Int -> Int -> Int -> Int
    upperBoundLength m width height =
      let boxesPerRow = w `div` min width height
          minRows = ceiling (fromIntegral m / fromIntegral boxesPerRow :: Double)
       in minRows * max width height


readInt :: Text -> Maybe Int
readInt = readMaybe . T.unpack

-- | Read input and translate it to the BWP
readStatement :: IO ([String], S)
readStatement = do
  str <- T.getLine
  let [w, n] = mapMaybe readInt (T.splitOn " " str)
  (strs, bxs, maxLen) <- readBoxes w n
  let rawStatement = T.unpack <$> (str:strs)
      statement = newS (sortOn Ord.Down bxs) w maxLen
  return (rawStatement, statement)

-- | Prints the BWP solution to the expected format
printBoxWrappingSolution :: BWPSolution -> IO ()
printBoxWrappingSolution BWPSolution{..} =
  print len >> traverse_ print boxSolutions

----------------------------------------------
-- Utils
----------------------------------------------

getDescription :: S -> CNFDescription
getDescription s = CNFDescription
   nVars                           -- # variables
   (lengthOf (clauses.traverse) s) -- # clauses
   mempty                          -- FilePath
  where
    -- Get the number of variables
    nVars = maximum . fmap abs . concat $ (coerce (s^.clauses) :: [[Int]])

runSAT :: SAT -> S -> S
runSAT = execState

coerceCNF :: CNF -> [[Int]]
coerceCNF = coerce
