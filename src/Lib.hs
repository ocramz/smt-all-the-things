-- inspired by https://buttondown.com/hillelwayne/archive/many-hard-leetcode-problems-are-easy-constraint/
{-# language DataKinds #-}
{-# language QuasiQuotes #-}
module Lib where

import Control.Applicative (Alternative(..))
import Control.Monad (guard, foldM)
import Control.Monad.IO.Class (liftIO)

import Data.Int (Int8, Int16, Int64)
import Data.Function (on, (&))
import Data.List (groupBy)
import Data.Maybe (fromMaybe)
import Data.Foldable (for_)

import Data.SBV (QuantifiedBool(..), SBool, Forall(..), ForallN(..), Exists(..), ExistsN(..), quantifiedBool, SMTConfig(..), Logic(..), setLogic, Timing(..), z3, prove, sat, satWith, allSat, allSatWith, AllSatResult(..), ite, SBV, sFromIntegral, sAll, (.&&), (.==), (./=), (.>), (.<), (.<=), (.>=), constrain, optimize, optimizeWith, OptimizeStyle(..), minimize, maximize, sInt, SInt, sInteger, SInteger, sTuples, SInt16, sInt16, sInt64, literal, unliteral, OrdSymbolic(..), Symbolic, SList, SymVal(..))
import qualified Data.SBV.List as SL ((!!), implode, length, foldr)
import qualified Data.SBV.Tuple as ST (untuple)

-- import Prelude hiding ((!!), minimum)


import Control.Monad.Logic (observeAll, observe)


{-| https://leetcode.com/problems/maximal-rectangle/description/

Given a rows x cols binary matrix filled with 0's and 1's, find the largest rectangle containing only 1's and return its area.

Input: matrix = [["1","0","1","0","0"],["1","0","1","1","1"],["1","1","1","1","1"],["1","0","0","1","0"]]
Output: 6
Explanation: The maximal rectangle is shown in the above picture.

Input: matrix = [["0"]]
Output: 0

Input: matrix = [["1"]]
Output: 1

-}

type SMatrix a = SList [a]

(@@) :: (SymVal a) => SMatrix a -> (SInteger, SInteger) -> SBV a
mtx @@ (i, j) = (mtx SL.!! i) SL.!! j

-- qe = quantifiedBool

solveMaxRectangle mtx = optimizeWith opts Lexicographic $ rectangle mtx

opts = z3 {
  verbose = True,
  timing = PrintTiming
  }

solveRectangle :: [[String]] -> IO AllSatResult
solveRectangle = allSatWith opts . rectangle

rectangle :: [[String]] -> Symbolic ()
rectangle mtx = do
  -- setLogic QF_UFLIA
  let
    nrows = literal $ fromIntegral $ length mtx
    ncols = literal $ fromIntegral $ length (mtx !! 0)
    smtx = literal mtx
  -- constrain $ quantifiedBool $ \(ExistsN [i0, i1, j0, j1]) (ForallN [i, j]) ->
  -- constrain $ qe $ \(Exists i0) (Exists i1) (Exists j0) (Exists j1) (Forall i) (Forall j) ->
  i0 <- sInteger "i0"
  j0 <- sInteger "j0"
  i1 <- sInteger "i1"
  j1 <- sInteger "j1"
  i <- sInteger "i"
  j <- sInteger "j"
  let
    inRows ii = ii .>= 0 .&& ii .< nrows
    inCols jj = jj .>= 0 .&& jj .< ncols
    rectInBounds =
      inRows i0 .&&
      inCols j0 .&&
      inRows i1 .&&
      inCols j1
      -- (i0 .>= 0 .&& i0 .< nrows) .&&
      -- (j0 .>= 0 .&& j0 .< ncols) .&&
      -- (i1 .>= 0 .&& i1 .< nrows) .&&
      -- (j1 .>= 0 .&& j1 .< ncols)
    rectOk =
      (i1 .> i0 .&& j1 .> j0)
    allOnesInRect =
      inRows i .&&
      inCols j .&&
      (i .>= i0 .&& i .<= i1) .&&
      (j .>= j0 .&& j .<= j1) .&&
      smtx @@ (i, j) .== literal "1"
  constrain $
    rectInBounds .&&
    -- rectOk .&&
    allOnesInRect
  maximize "area" $ (i1 - i0) * (j1 - j0)

-- rectangle mtx (ExistsN [i0, i1, j0, j1]) = do
--   constrain (i1 .>= i0 .&& j1 .>= j0)
--   constrain $ qe (rect mtx (i0, j0) (i1, j1))
--   maximize "area" $ (i1 - i0) * (j1 - j0)

-- rect ::
--   SMatrix String
--   -> (SBV Integer, SBV Integer)
--   -> (SBV Integer, SBV Integer)
--   -> ForallN 2 nm Integer
--   -> SBool
-- rect mtx (i0, j0) (i1, j1) (ForallN [i, j]) =
--     (i .>= i0 .&& i .<= i1) .&&
--     (j .>= j0 .&& j .<= j1) .&&
--     mtx `ix2` (i, j) .== literal "1"



-- -- --

-- 1d version of the rectangle-in-matrix problem above

-- longestSegment :: []
longestSegment ::
  (SymVal a, Num a) =>
  [a] -> Symbolic ()
longestSegment vs = do
  let
    svs = literal vs
    len = SL.length svs
    inBounds i = i .>= 0 .&& i .< len
    isValueOK i = svs SL.!! i .== literal 1
  a <- sInteger "a"
  b <- sInteger "b"
  constrain $
    inBounds a .&&
    inBounds b .&&
    quantifiedBool (\(Forall i) ->
                       (i .>= a .&& i .<= b) .&&
                       isValueOK i
                       )
  -- maximize "length" (b - a)




-- -- -- -- ---





choose :: (Foldable t, Alternative f) => t a -> f a
choose = foldr ((<|>) . pure) empty

chooseA :: (Foldable t, Alternative f) => t (f a) -> f a
chooseA = foldr (<|>) empty

-- coinsA value = do
--   a <- choose [0 .. ]
--   b <- choose [0 .. ]
--   c <- choose [0 .. ]
--   let
--     va = 25
--     vb = 10
--     vc = 1
--   guard $ (a * va) + (b * vb) + (c * vc) == value
--   pure (a + b + c)

-- Given a set of coin denominations, find the minimum number of coins required to make change for a given number.  for USA coinage and 37 cents, the minimum number is four (quarter, dime, 2 pennies).

-- int: total;
-- array[int] of int: values = [10, 9, 1];
-- array[index_set(values)] of var 0..: coins;
--
-- constraint sum (c in index_set(coins)) (coins[c] * values[c]) == total;
-- solve minimize sum(coins);

coins :: Integer -> Symbolic ()
coins value = do
  a <- sInteger "a"
  b <- sInteger "b"
  c <- sInteger "c"
  let
    va = 25
    vb = 10
    vc = 1
  constrain (a .> 0)
  constrain (b .> 0)
  constrain (c .> 0)
  constrain $ (a * va) + (b * vb) + (c * vc) .== literal value
  minimize "change" (a + b + c)


-- λ> optimize Lexicographic (coins 37)
-- Optimal model:
--   a      = 1 :: Integer
--   b      = 1 :: Integer
--   c      = 2 :: Integer
--   change = 4 :: Integer


-- -- --

-- Given a list of stock prices through the day, find maximum profit you can get by buying one stock and selling one stock later.

-- array[int] of int: prices = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8];
-- var int: buy;
-- var int: sell;
-- var int: profit = prices[sell] - prices[buy];

-- constraint sell > buy;
-- constraint profit > 0;
-- solve maximize profit;


maxProfit :: Symbolic ()
maxProfit = do
  let
    prices :: SList Integer
    prices = literal [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8]
    -- prices = implode [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8]
  -- buy <- sInteger "buy"
  -- sell <- sInteger "sell"
  -- constrain (sell .> buy)
  -- constrain (sell .> 0 .&& buy .> 0)
  -- constrain (sell .< 12 .&& buy .< 12)
  buy <- pick prices "buy"
  sell <- pick prices "sell"
  let
    profit = prices SL.!! sell - prices SL.!! buy
  constrain (profit .> 0)
  maximize "profit" profit

-- λ> optimize Lexicographic maxProfit
-- Optimal model:
--   buy    = 3 :: Integer
--   sell   = 5 :: Integer
--   profit = 8 :: Integer

chooseIx :: (Alternative f, Foldable t) => t a -> f Int
chooseIx vs = choose [0 .. length vs - 1]

maxProfitA :: [(Int, Int, Int)]
maxProfitA = observeAll $ do
  let
    prices = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8]
  buy <- chooseIx prices
  sell <- choose prices
  let
    profit = prices !! sell - prices !! buy
  guard (sell > buy)
  guard (profit > 0)
  pure (buy, sell, profit)

-- λ> :t groupBy ((==) `on` (\(_, _, z) -> z))
-- groupBy ((==) `on` (\(_, _, z) -> z))
--   :: Eq b1 => [(a, b2, b1)] -> [[(a, b2, b1)]]

-- -- --

-- | nondeterministic choice in a list
pick :: (SymVal a) => SList a -> String -> Symbolic (SBV a)
pick xs name = do
  i <- sInteger name
  let
    l = SL.length xs
  constrain (i .>= 0 .&& i .< l)
  pure $ xs SL.!! i

-- Given a list, determine if three numbers in that list can be added or subtracted to give 0?

zeroSum3 :: (SymVal a, Num a, Ord a) => [a] -> Symbolic ()
zeroSum3 xs = do
  let
    lxs = literal xs
  a <- pick lxs "a"
  b <- pick lxs "b"
  c <- pick lxs "c"
  -- constrain (a ./= b .&& b ./= c .&& a ./= c) -- all distinct
  constrain (a + b + c .== 0)


-- -- --

-- Given an array of integers heights representing the histogram's bar height where the width of each bar is 1, return the area of the largest rectangle that fits under the histogram.
--
-- ns = [2,1,5,6,2,3]
-- answer = 10

-- minimum ::
minimum :: (SymVal a, Ord a, Num a) => SList a -> SBV a
minimum = SL.foldr (\x acc -> ite (x .< acc) x acc) 0

-- -- largestHistRec :: (SymVal a, Ord a, Show a, Bounded a, Integral a) => [a] -> Symbolic ()
-- largestHistRec :: [Integer] -> Symbolic ()
-- largestHistRec vs = do
--   let
--     lvs = literal vs
--   v0 <- pick lvs "v0"
--   v1 <- pick lvs "v1"
--   let
--     xs = [v0 .. v1]
--   for_ xs $ \v -> do
--     constrain (v .>= v0)
--   -- let
--   --   mi = minimum lvs
--   -- maximize "area" (mi * (SL.length $ SL.implode xs))

  -- let
  --   lvs = literal vs
  --   ns :: SList Int8
  --   ns = literal [0 .. fromIntegral (length vs - 1)]
  -- i0 <- pick ns "a"
  -- i1 <- pick ns "b"
  -- constrain (i0 .< i1)
  -- for_ [i0 .. i1] $ \i -> do
  --   constrain (lvs SL.!! i .<= lvs SL.!! i0)
--   let
--     area = sum [vs !! i | <- [i0 .. i1]]


-- -- --

{-| trapping rain water - leetcode (hard)

https://leetcode.com/problems/trapping-rain-water/description/ 

Given n non-negative integers representing an elevation map where the width of each bar is 1, compute how much water it can trap after raining.

Input: height = [0,1,0,2,1,0,1,3,2,1,2,1]
Output: 6
Explanation: The above elevation map (black section) is represented by array [0,1,0,2,1,0,1,3,2,1,2,1]. In this case, 6 units of rain water (blue section) are being trapped.

Input: height = [4,2,0,3,2,5]
Output: 9

-}

rainwater :: [Int64] -> Symbolic ()
rainwater heights = do
  let
    shs = literal heights
    nheigths = fromIntegral $ length heights
  n <- sInt64 "n"
  constrain (n .>= 0 .&& n .< literal nheigths)
  let
    nConcrete = fromMaybe 0 $ unliteral n
    intervalNames = map show [0 .. nConcrete-1]
  intervalBounds <- sTuples intervalNames
  -- liftIO $ print intervalBounds
  area <- foldM (\acc bounds -> (acc +) <$> pocket shs (ST.untuple bounds) ) 0 intervalBounds
  maximize "area" area

cfg :: SMTConfig
cfg = z3 {verbose = False , validateModel = False }

-- > optimizeWith cfg Lexicographic (rainwater [0,1,0,2,1,0,1,3,2,1,2,1])



pocket :: (SymVal b, Num b, Ord b) =>
           SBV [b] -> (SBV Int64, SBV Int64) -> Symbolic (SBV b)
pocket heights (ia, ib) = do
  constrain (ia .>= 0 .&& ia .< sFromIntegral (SL.length heights))
  constrain (ib .>= 0 .&& ib .< sFromIntegral (SL.length heights))
  constrain (ia .< ib)
  let
    height i = heights SL.!! sFromIntegral i
    ha = height ia
    hb = height ib
    h = ha `smin` hb
  constrain (height (ia - 1) .<= ha)
  constrain (height (ib + 1) .<= hb)
  let
    hs = map (\i -> abs (h - height i)) [ia .. ib]
  -- liftIO $ print hs
  pure $ sum hs




-- -- logict version

-- rainwaterA :: (Monad m, Alternative m, Ord b, Num b) => [b] -> m b
-- rainwaterA heights = do
--   let
--     nheights = fromIntegral $ length heights
--   n <- choose [0 .. nheights]
--   -- guard (n >= 0 && n < nheights)
--   intervalBounds <- do
--   foldM (\acc bounds -> (acc +) <$> pocketA heights bounds) 0 _

-- isPocket hs = foldr f 0 [0 .. n-1]
--   where
--     n = length hs
--     f i
--       | hs (i-1) > hs i 

-- pocketA :: (Integral a, Num b, Ord b) =>
--            [b] -> (a, a) -> Logic b
pocketA heights (ia, ib) = do
  -- guard (ia < ib)
  let
    height i = heights !! fromIntegral i
    ha = height ia
    hb = height ib
    h = min ha hb
  guard (height (ia - 1) <= ha)
  guard (height (ib + 1) <= hb)
  let
    hs = map (\i -> h - height i) [ia .. ib]
  pure $ sum hs


-- --

-- λ> rainwater2 [0,1,0,2,1,0,1,3,2,1,2,1]
-- (should be 6)

rainwater2 :: [Integer] -> Integer
rainwater2 = unpack . foldr f z
  where
    unpack (_, _, z) = z
    z = (0, 0, 0) -- (current max, prev max, running total)
    f h (cmax, pmax, ctot)
      | h > cmax  = (h, cmax, ctot)
      | h == cmax = (cmax, cmax, ctot)

    -- f h (cmax, pmax, ctot)
    --   | h >= cmax = (h, ctot)
    --   | h < cmax  = (cmax, ctot + cmax - h)


-- -- -- -- --


