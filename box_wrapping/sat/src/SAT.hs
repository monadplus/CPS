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
import           Control.Lens               hiding(inside)
import           Control.Monad.State.Strict
import           Data.Coerce
import           Data.Foldable              (traverse_)
import           Data.List                  (sortOn)
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

isSquare :: Box -> Bool
isSquare b = b^.width == b^.height

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
    unwords $ fmap show (vars ++ [Variable 0])

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

instance Disjunctive (Maybe Variable) (Maybe Variable) where
  Just var1 \/ Just var2 = var1 \/ var2
  Just var \/ Nothing    = var \/ emptyClause
  Nothing \/ Just var    = var \/ emptyClause
  Nothing \/ Nothing     = emptyClause

newS :: [Box] -> Int -> Int -> S
newS boxes width maxLen = S
    { _boxes     = boxes
    , _w         = width
    , _maxLength = maxLen
    , _clauses   = []
    }

getBox :: Coord -> Int -> StateS Variable
getBox coord b = (\s -> getBoxS s coord b) <$> get

getBoxS :: S -> Coord -> Int -> Variable
getBoxS S{..} (x, y) b =
  Variable (x + y*_w + b*_w*_maxLength + 1)

getRot :: Int -> StateS Variable
getRot b = (`getRotS` b) <$> get

getRotS :: S -> Int -> Variable
getRotS S{..} b =
  Variable ((length _boxes * _w * _maxLength) + 1 + b)

getCell :: Coord -> Int -> StateS Variable
getCell coords c = (\s -> getCellS s coords c) <$> get

getCellS :: S -> Coord -> Int -> Variable
getCellS s@S{..} coord c =
  Variable (cellIndexStart + cellOffset)
  where
    nboxes = length _boxes
    cellIndexStart      = (nboxes*_w*_maxLength) + nboxes
    Variable cellOffset = getBoxS s coord c

----------------------------------------------
-- Encoding
----------------------------------------------


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

----------------------------------------------
-- Constraints
----------------------------------------------

-- By symmetry, place the biggest box on the coordinate (0,0).
--
-- Boxes are sorted in decreasing order so pick the first one.
biggestBoxTopLeft :: SAT
biggestBoxTopLeft = do
    s@S{..} <- use id

    -- Set the first box top-left coordinate to true
    --   and the rest to false.
    --
    addClause =<< singleClause <$> getBox (0,0) 0
    addClauses $
      fmap (singleClause . neg)
        [ getBoxS s (x,y) 0 | x  <- [0.._w         - 1]
                         , y  <- [0.._maxLength - 1]
                         , x > 0 || y > 0]

    -- Set the rest of boxes to false on the coordinates used by the first box.
    -- To simplify, we will consider only the smallest area.
    let k = min (s^?!boxes . ix 0 . width) (s^?!boxes . ix 0 . height)
    addClauses $
      fmap (singleClause . neg)
        [ getBoxS s (x,y) b | b <- [1..(length _boxes - 1)]
                         , x <- [0..k - 1]
                         , y <- [0..k - 1]]

-- | Boxes must be placed exactly once in the paper roll.
exactlyOne :: SAT
exactlyOne =
    forEachBox (alo &&& amoQ) >>=
      traverse_ (\(c,cs) -> addClauses (c:cs))
  where
    forEachBox :: ([Variable] -> r) -> StateS [r]
    forEachBox f = do
      s@S{..} <- get
      return $ [ f [ getBoxS s (x,y) b | x <- [0..(_w - 1)]
                                    , y <- [0..(_maxLength - 1)]]
               | b <- [1..(length _boxes - 1)]]
               -- ^^^^^ Notice we start from second box.

-- | Boxes must be placed inside the paper roll.
insideTheBounds :: SAT
insideTheBounds = do
  s@S{..} <- get
  sequence_  [ sequence  [ addBoundingClause (x,y) box _w _maxLength rot (var x y)
                           | x <- [0..(_w - 1)]
                           , y <- [0..(_maxLength - 1)]]
             | b <- [1..(length _boxes - 1)]
             , let box = _boxes !! b
                   var x y = getBoxS s (x,y) b
                   rot = getRotS s b
             ]
  where
    addBoundingClause :: Coord -> Box -> Int -> Int -> Variable -> Variable -> SAT
    addBoundingClause coords box w maxLength rot var
      | isSquare box =
        whenM (not <$> inside coords box) $ addClause (neg var \/ emptyClause)
      | otherwise = do
         whenM (not <$> inside coords box)        $ addClause (rot \/ neg var)
         whenM (not <$> insideRotated coords box) $ addClause (neg rot \/ neg var)

overlappingVariables :: SAT
overlappingVariables = do
  S{..} <- get
  sequence_ [ addCellsForEachBoxAndCoordinates (x,y) (b, box)
                 | b <- [0..(length _boxes - 1)]
                 , let box = _boxes !! b
                 , x <- [0.._w - 1]
                 , y <- [0.._maxLength -1]]
  where
    addCellsForEachBoxAndCoordinates (x,y) (b, box)
      | isSquare box = do
          addCellsNoRotation (x,y) (b,box)
          rot <- getRot b
          addClause (neg rot \/ emptyClause)

      | otherwise = do
          addCellsNoRotated (x,y) (b,box)
          addCellsRotated (x,y) (b,box)

addCellsNoRotation :: Coord -> (Int, Box) -> SAT
addCellsNoRotation (x,y) (b, box@Box{..}) =
  forM_ (area box (x,y)) $ \(i,j) ->
    whenM (inside (i,j) box) $ do
      tl  <- getBox (x,y) b
      cl  <- getCell (i,j) b
      addClause $ neg tl \/ cl

addCellsNoRotated :: Coord -> (Int, Box) -> SAT
addCellsNoRotated (x,y) (b, box@Box{..}) =
  forM_ (area box (x,y)) $ \(i,j) ->
    whenM (inside (i,j) box) $ do
      tl  <- getBox (x,y) b
      cl  <- getCell (i,j) b
      rot <- getRot b
      addClause $ neg tl \/ cl \/ rot

addCellsRotated :: Coord -> (Int, Box) -> SAT
addCellsRotated (x,y) (b, box@Box{..}) =
  forM_ (areaRotated box (x,y)) $ \(i,j) ->
    whenM (insideRotated (i,j) box) $ do
      tl  <- getBox (x,y) b
      cl  <- getCell (i,j) b
      rot <- getRot b
      addClause $ neg tl \/ cl \/ neg rot

amoOverlapping :: SAT
amoOverlapping = do
  coords <- paperRollCoords
  nboxes <- uses boxes length
  forM_ coords $ \(x,y) -> do
    bxsVars <- traverse (getCell (x,y)) [0..nboxes-1]
    addClauses $ amoQ bxsVars

-- | Boxes can't overlap.
noOverlapping :: SAT
noOverlapping =
  overlappingVariables >> amoOverlapping

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
       in (bs', rs', translateBox s box b r : acc)

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

-- | Like 'when', but where the test can be monadic.
whenM :: Monad m => m Bool -> m () -> m ()
whenM b t = ifM b t (pure ())

-- | Like 'unless', but where the test can be monadic.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b = ifM b (pure ())

-- | Like @if@, but where the test can be monadic.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f

runSAT :: SAT -> S -> S
runSAT = execState

coerceCNF :: CNF -> [[Int]]
coerceCNF = coerce

getDescription :: S -> CNFDescription
getDescription s = CNFDescription
   nVars                           -- # variables
   (lengthOf (clauses.traverse) s) -- # clauses
   mempty                          -- FilePath
  where
    -- Get the number of variables
    nVars = maximum . fmap abs . concat $ (coerce (s^.clauses) :: [[Int]])

--forAllBoxesAllCoordinates :: StateS [(b, Box, Coord)]
--forAllBoxesAllCoordinates = do
  --bxs <- allBoxes
  --liftA2 (
  --(\(b, box) -> (b, box, paperRollCoords)) <$> bxs

paperRollCoords :: StateS [Coord]
paperRollCoords = do
  S{..} <- get
  return [ (x,y) | x <- [0..       _w - 1]
                 , y <- [0.._maxLength -1]]

allBoxes :: StateS [(Int, Box)]
allBoxes = do
  s <- get
  return $ zip [0..] (s^.boxes)

area :: Box -> Coord -> [Coord]
area Box{..} (x,y) =
  [ (i,j)
    | i <- [x..x + _width  - 1]
    , j <- [y..y + _height - 1]]

areaRotated :: Box -> Coord -> [Coord]
areaRotated Box{..} (x,y) =
  [ (i,j)
    | i <- [x..x + _height - 1]
    , j <- [y..y +  _width - 1]]

inside :: Coord -> Box -> StateS Bool
inside (x,y) Box{..} = do
  S{..} <- use id
  return (x + _width - 1 < _w && y + _height - 1 < _maxLength)

insideRotated :: Coord -> Box -> StateS Bool
insideRotated (x,y) Box{..} = do
  S{..} <- use id
  return (x + _height - 1 < _w && y + _width - 1 < _maxLength)
