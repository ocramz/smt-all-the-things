-- inspired by https://buttondown.com/hillelwayne/archive/many-hard-leetcode-problems-are-easy-constraint/

{-# language QuasiQuotes #-}
module Lib where

import Data.Int (Int8)
import Data.Foldable (for_)

import Data.SBV (sat, ite, SBV, sAll, (.&&), (.==), (./=), (.>), (.<), (.<=), (.>=), constrain, optimize, OptimizeStyle(..), minimize, maximize, sInteger, SInteger, literal, Symbolic, SList, SymVal(..))
import qualified Data.SBV.List as SL ((!!), implode, length, foldr)

import Prelude hiding ((!!), minimum)

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
  

