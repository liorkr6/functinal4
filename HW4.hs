{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall -Werror #-}
-- Needed for adding instances outside the data definition module.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW4 where

import Data.Either
import Data.List (foldl', uncons)
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))
import Data.Ord (Down(..))
import Data.Semigroup (Arg (..), Max (..), Min (..))
import Data.Set (Set)
import qualified Data.Set as S
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Foldable (foldMap, foldl, foldr), Functor (fmap), Int, Maybe (..), Monoid (..), Num (..), Ord (..), Ordering (..), Semigroup (..), Show (..), String, all, and, any, concat, concatMap, const, curry, drop, dropWhile, error, filter, flip, fst, id, init, map, not, or, replicate, reverse, snd, take, takeWhile, uncurry, undefined, zip, zipWith, (!!), ($), (&&), (++), (.), (||))

import Deque (Deque)
import qualified Deque as DQ
import PersistentArray (PersistentArray)
import qualified PersistentArray as PA

-- Section 1: Foldable functions
fold :: (Foldable t, Monoid a) => t a -> a
fold = foldMap id
toList :: Foldable t => t a -> [a]
toList = foldMap (: [])
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\y acc -> acc || y == x) False
find :: (Foldable t, Eq a) => (a -> Bool) -> t a -> Maybe a
find p = foldr (\x acc -> if p x then Just x else acc) Nothing
length :: Foldable t => t a -> Int
length = foldr (\_ l -> l+1) 0
null :: Foldable t => t a -> Bool
null foldable = length foldable == 0
-- maximum :: (Foldable t, Ord a) => t a -> Maybe a
-- maximum = foldMap (Max . Just)
-- maxBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
-- minimum :: (Foldable t, Ord a) => t a -> Maybe a
-- minBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
-- sum :: (Foldable t, Num a) => t a -> a
-- product :: (Foldable t, Num a) => t a -> a

-- -- Section 2: Functor functions
fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)
fmapToFst f1 = fmap (\x -> (f1 x, x))
fmapToSnd :: Functor f => (a -> b) -> f a -> f (a, b)
fmapToSnd f1 = fmap (\x -> (x, f1 x))
strengthenL :: Functor f => b -> f a -> f (b, a)
strengthenL y = fmap (y, )
strengthenR :: Functor f => b -> f a -> f (a, b)
strengthenR y = fmap (, y)
unzip :: Functor f => f (a, b) -> (f a, f b)
unzip ftupple = (fmap fst ftupple, fmap snd ftupple)
coUnzip :: Functor f => Either (f a) (f b) -> f (Either a b)
coUnzip (Left fa) = fmap Left fa
coUnzip (Right fb) = fmap Right fb


-- -- Section 3: Unfodlable
-- class Unfoldable t where
--     fromList :: [a] -> t a
--     unfoldr :: (b -> Maybe (a, b)) -> b -> t a
--     {-# MINIMAL fromList | unfoldr #-}

-- instance Unfoldable []
-- instance Unfoldable Deque
-- instance Unfoldable PersistentArray

-- -- Section 4: Data structure instances
-- instance Foldable Deque
-- instance Functor Deque
-- instance Semigroup (Deque a)
-- instance Monoid (Deque a)

-- instance Foldable PersistentArray
-- instance Functor PersistentArray
-- instance Semigroup (PersistentArray a)
-- instance Monoid (PersistentArray a)

-- -- Bonus section
-- newtype ZipList a = ZipList {getZipList :: [a]} deriving (Show, Eq)

-- instance Semigroup a => Semigroup (ZipList a)
-- instance Monoid a => Monoid (ZipList a)
