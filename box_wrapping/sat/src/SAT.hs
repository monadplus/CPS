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

instance Eq Box where
  b1 == b2 =
    b1^.width == b2^.width
      && b1^.height == b2^.height

-- By total area
instance Ord Box where
  b1 `compare` b2 =
    let area b = b^.width * b^.height
     in area b1 `compare` area b2


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

negMaybe :: Maybe Variable -> Maybe Variable
negMaybe Nothing = Nothing
negMaybe (Just variable) = Just (neg variable)

emptyClause :: Clause
emptyClause = Clause []

maybeClause :: Maybe Variable -> Clause
maybeClause Nothing         = emptyClause
maybeClause (Just variable) = Clause [variable]

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

                     -- 1) When width = height, there is no need to test rotation.
                     --    We can set it to rotated and just add the negation of each position (x,y)
                     fixRotation
                       | box^.width == box^.height =
                          \clauses -> (neg rot \/ emptyClause) : clauses
                       | otherwise = id

                     cs = catMaybes [ computeClause (x,y) box _w _maxLength rot (var x y)
                                    | x <- [0..        (_w - 1)]
                                    , y <- [0..(_maxLength - 1)]
                                    ]

                  in fixRotation cs

               | b <- [0..(length _boxes - 1)]
               ]
  -- Add all clauses to the CNF.
  addClauses boundsClauses

  where
    computeClause :: Coord -> Box -> Int -> Int -> Variable -> Variable -> Maybe Clause
    computeClause (x,y) box w maxLength rot var
       -- See 1)
       | box^.width == box^.height =
           if x > w - box^.width || y > maxLength - box^.height
               then Just $ neg var \/ emptyClause
           else Nothing

       -- May rotate: we need to add clauses for both positions.
       | otherwise =
           -- 1.- Not rotated
           if x > w - box^.width || y > maxLength - box^.height
             then Just $ rot \/ neg var
           -- 2.- Rotated
           else if x > w - box^.height || y > maxLength - box^.width
             then Just $ neg rot \/ neg var
           else Nothing

-- | For all boxes b1, b2 (b1 < b2) computes the overlapping region form the following implication:
--
--    var(b,(x,y)) ----> neg var(b2,(i,j))
--
-- (x,y) is the traversal of the whole roll paper (except for the margins)
-- (i,j) is the area where the two boxes would collide.
--
-- We also take into account all possible rotations of b1 and box2.
--
noOverlapping :: SAT
noOverlapping = do
  s@S{..} <- get
  let clauses =
        concat [ computeClauses s b1 w1 h1 rot1 ++ computeClauses s b1 h1 w1 (negMaybe rot1)
                  | b1 <- [0..length _boxes - 1]
                  , let w1 = s ^?! boxes . ix b1 . width
                  , let h1 = s ^?! boxes . ix b1 . height
                  , let rot1 | w1 == h1  = Nothing
                             | otherwise = Just (s ^?! rotations . ix b1)
               ]
  addClauses clauses

  where

    computeClauses :: S -> Int -> Int -> Int -> Maybe Variable -> [Clause]
    computeClauses s@S{..} b1 w1 h1 rot1 =
      concat $
        [ computeClauses' (x,y) b2 w2 h2 rot2 ++ computeClauses' (x,y) b2 h2 w2 (negMaybe rot2)
            | x  <- [0.._w  - w1]
            , y  <- [0.._maxLength - h1]
            , b2 <- [b1+1..(length _boxes - 1)]
            , let w2 = s ^?! boxes . ix b2 . width
            , let h2 = s ^?! boxes . ix b2 . height
            , let rot2 | w2 == h2  = Nothing
                       | otherwise = Just (s ^?! rotations . ix b2)
        ]

      where

        computeClauses' :: Coord -> Int -> Int -> Int -> Maybe Variable -> [Clause]
        computeClauses' (x,y) b2 w2 h2 rot2 =
          [ maybeClause rot1 \/ maybeClause rot2 \/ neg boxVar1 \/ neg boxVar2
              | i <- [max 0 (x - w2 + 1)  ..  min _w (x + w1 - 1)]
              , j <- [max 0 (y - h2 + 1)  ..  min _w (y + h1 - 1)]
              , let boxVar1 = bVar s (x,y) b1
              , let boxVar2 = bVar s (i,j) b2
          ]


-- TODO
-- TODO
-- TODO
-- TODO
-- TODO
buildSAT :: SAT
buildSAT = do
  -- Symmetry: first box on (0,0) [the biggest one]
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

--     --- TODO
--
--     The upper bound on the length of the roll can also be improved. The
--     one you have now corresponds to placing the boxes one on top of the
--     other. But you can exploit that there may be many identical boxes. For
--     example, you can put identical rectangles one *next* to each other
--     until they fill the whole width, and only then begin a new line. In
--     this way you will get an upper bound which is always as good as the
--     one you have now, and usually strictly better.
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
