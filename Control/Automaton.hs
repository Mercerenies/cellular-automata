{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Automaton(Automaton(), Rule(..), nextCell, prevCell, getCell,
                         initial, autoRule, positionOf) where

-- This module contains the functionally pure cellular automata
-- implementation.

import Control.Comonad
import Data.Bits
import Data.Stream(Stream(Cons))
import qualified Data.Stream as Stream

-- An automaton is a data structure containing an infinite stream of
-- data in both directions. Cardinally, there is no difference between
-- a one-sided infinity and a two-sided infinity, but comonadically
-- they are distinct concepts.
data Automaton a = U (Stream a) a (Stream a)
                   deriving Show

-- Simple newtype wrapper to avoid confusion. Rules should be wrapped
-- in such to avoid mixing them up with row or column indices.
newtype Rule = Rule { unRule :: Int }
    deriving (Read, Show, Eq, Ord, Enum, Num)

-- Advances the automaton to consider the next cell to be its
-- "center".
nextCell :: Automaton a -> Automaton a
nextCell (U bs x (a `Cons` as)) = U (x `Cons` bs) a as

-- Advances the automaton to consider the previous cell to be its
-- "center".
prevCell :: Automaton a -> Automaton a
prevCell (U (b `Cons` bs) x as) = U bs b (x `Cons` as)

-- Given a numerical index (which may be negative), extracts the given
-- value from the automaton. It is impossible for the index to be out
-- of bounds, as the comonad extends infinitely in both the positive
-- and negative directions.
getCell :: Int -> Automaton a -> a
getCell n = case n `compare` 0 of
              EQ -> extract
              GT -> getCell (n - 1) . nextCell
              LT -> getCell (n + 1) . prevCell

-- Automata are functors.
instance Functor Automaton where
    fmap f (U bs x as) = U (fmap f bs) (f x) (fmap f as)

-- Automata are comonads. By implementing `extract` and `duplicate`,
-- we get `extend` for for free.
instance Comonad Automaton where
    extract (U _ x _) = x
    duplicate auto = let prev = Stream.tail $ Stream.iterate prevCell auto
                         next = Stream.tail $ Stream.iterate nextCell auto
                     in U prev auto next

-- The starting conditions for an automaton: a single live cell in the
-- center with all other cells being dead.
initial :: Automaton Bool
initial = U (Stream.repeat False) True (Stream.repeat False)

-- Produces a function which applies the given cellular automaton
-- rule. Note that `extend (autoRule rule)` has type `Automaton Bool
-- -> Automaton Bool` and advances one step in the automaton
-- computation.
autoRule :: Rule -> (Automaton Bool -> Bool)
autoRule (Rule n) (U (b `Cons` _) x (a `Cons` _)) =
    let matchBit m i y = if m then setBit y i else y
        k :: Int
        k = matchBit b 2 . matchBit x 1 . matchBit a 0 $ zeroBits
    in testBit n k

-- Given a "row" and a column (where the row is simply how many steps
-- to advance), returns whether the given cell is alive or dead.
positionOf :: Rule -> Int -> Int -> Bool
positionOf rule row col =
    let step = extend $ autoRule rule
        currRow = iterate step initial !! row
    in getCell col currRow
