module Util.Set ( module Data.HashSet
                , Set
                , notMember
                , ordNub
                , toAscList
                , (\\)
                ) where

import Data.List (sort)
import Data.Hashable
import Data.HashSet

type Set = HashSet

infixl 9 \\

(\\) :: (Eq a, Hashable a) => Set a -> Set a -> Set a
(\\) = difference

notMember :: (Eq a, Hashable a) => a -> Set a -> Bool
notMember k = not . member k

ordNub :: (Ord a, Hashable a) => [a] -> [a]
ordNub = toAscList . fromList

toAscList :: (Ord a) => Set a -> [a]
toAscList = sort . toList
