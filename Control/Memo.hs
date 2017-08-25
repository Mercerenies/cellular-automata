
module Control.Memo(MemoTable(), tableRule, memoTable, positionOfMemo) where

-- This module handles memoization using the ST monad.

import Control.Arrow
import Control.Automaton
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Map(Map)
import qualified Data.Map as Map

-- A memoization table for our function consists of a reference to the
-- rule and a Haskell associative map.
newtype MemoTable s = MemoTable {
      unMemoTable :: STRef s (Rule, Map (Int, Int) Bool)
    }

-- Extracts the rule from a memoization table.
tableRule :: MemoTable s -> ST s Rule
tableRule = fmap fst . readSTRef . unMemoTable

-- Constructs a new memoization table.
memoTable :: Rule -> ST s (MemoTable s)
memoTable r = MemoTable <$> newSTRef (r, Map.empty)

-- This is the function that actually implements memoization. It
-- behaves like `positionOf` except that it remembers answers from
-- previous computations.
positionOfMemo :: MemoTable s -> Rule -> Int -> Int -> ST s Bool
positionOfMemo memo rule row col = do
  rule' <- tableRule memo
  -- If the rule has changed since the last time we called this
  -- routine, we need to reset the cache.
  when (rule /= rule')
       reloadCache
  -- Load the stored value from the cache.
  storedValue
    where MemoTable ref = memo
          reloadCache =
              -- Reloading the cache resets its map to an empty data
              -- structure and marks it as being intended for a new
              -- automaton rule.
              writeSTRef ref (rule, Map.empty)
          storedValue = do
            (_, table) <- readSTRef ref
            -- If the map contains the row and column we're looking
            -- for then return that. Otherwise, we have a cache miss.
            case Map.lookup (row, col) table of
              Nothing -> cacheMiss
              Just result -> pure result
          cacheMiss =
              -- We have a cache miss. We need to actually compute the
              -- value, then store it and return it.
              let result = positionOf rule row col
              in result <$ modifySTRef' ref (second $ Map.insert (row, col) result)
