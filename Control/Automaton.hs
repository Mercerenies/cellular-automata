{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Automaton(Automaton(), Rule(..), nextCell, prevCell, getCell,
                         initial, autoRule, positionOf) where

import Control.Comonad
import Data.Bits
import Data.Stream(Stream(Cons))
import qualified Data.Stream as Stream

data Automaton a = U (Stream a) a (Stream a)
                   deriving Show

newtype Rule = Rule { unRule :: Int }
    deriving (Read, Show, Eq, Ord, Enum, Num)

nextCell :: Automaton a -> Automaton a
nextCell (U bs x (a `Cons` as)) = U (x `Cons` bs) a as

prevCell :: Automaton a -> Automaton a
prevCell (U (b `Cons` bs) x as) = U bs b (x `Cons` as)

getCell :: Int -> Automaton a -> a
getCell n = case n `compare` 0 of
              EQ -> extract
              GT -> getCell (n - 1) . nextCell
              LT -> getCell (n + 1) . prevCell

instance Functor Automaton where
    fmap f (U bs x as) = U (fmap f bs) (f x) (fmap f as)

instance Comonad Automaton where
    extract (U _ x _) = x
    duplicate auto = let prev = Stream.tail $ Stream.iterate prevCell auto
                         next = Stream.tail $ Stream.iterate nextCell auto
                     in U prev auto next

initial :: Automaton Bool
initial = U (Stream.repeat False) True (Stream.repeat False)

autoRule :: Rule -> (Automaton Bool -> Bool)
autoRule (Rule n) (U (b `Cons` _) x (a `Cons` _)) =
    let matchBit m i y = if m then setBit y i else y
        k :: Int
        k = matchBit b 2 . matchBit x 1 . matchBit a 0 $ zeroBits
    in testBit n k

positionOf :: Rule -> Int -> Int -> Bool
positionOf rule row col =
    let step = extend $ autoRule rule
        currRow = iterate step initial !! row
    in getCell col currRow
