
module Control.Memo(MemoTable(), tableRule, memoTable, positionOfMemo) where

import Control.Arrow
import Control.Automaton
import Control.Monad.ST
import Data.STRef
import Data.Map(Map)
import qualified Data.Map as Map

newtype MemoTable s = MemoTable {
      unMemoTable :: STRef s (Rule, Map (Int, Int) Bool)
    }

tableRule :: MemoTable s -> ST s Rule
tableRule = fmap fst . readSTRef . unMemoTable

memoTable :: Rule -> ST s (MemoTable s)
memoTable r = MemoTable <$> newSTRef (r, Map.empty)

positionOfMemo :: MemoTable s -> Rule -> Int -> Int -> ST s Bool
positionOfMemo memo rule row col = tableRule memo >>= \rule' ->
                                   if rule == rule' then
                                       stored
                                   else
                                       reloadCache
    where MemoTable ref = memo
          reloadCache = do
            writeSTRef ref (rule, Map.empty)
            stored
          stored = do
            (_, table) <- readSTRef ref
            case Map.lookup (row, col) table of
              Nothing -> cacheMiss
              Just result -> pure result
          cacheMiss =
              let result = positionOf rule row col
              in result <$ modifySTRef' ref (second $ Map.insert (row, col) result)
