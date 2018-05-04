module Util.Map ( module M
                , Map
                , keysSet
                , findWithDefault
                , notMember
                , restrictKeys
                , toAscList
                , unionsWith
                ) where

import Data.List as L
import Data.Hashable
import Data.HashMap.Strict as M
import qualified Util.Set as S

type Map = M.HashMap

keysSet :: (Eq k, Hashable k) => Map k v -> S.Set k
keysSet = S.fromList . M.keys

findWithDefault :: (Eq k, Hashable k) => v -> k -> Map k v -> v
findWithDefault d n m = case M.lookup n m of
      Just x -> x
      Nothing -> d

restrictKeys :: (Eq k, Hashable k) => Map k v -> S.Set k -> Map k v
restrictKeys m s = M.filterWithKey (\k _ -> k `S.member` s) m

unionsWith :: (Eq k, Hashable k) => (v -> v -> v) -> [Map k v] -> Map k v
unionsWith f = L.foldl' (M.unionWith f) M.empty

notMember :: (Eq k, Hashable k) => k -> Map k v -> Bool
notMember k = not . M.member k

toAscList :: (Ord k, Ord v) => Map k v -> [(k,v)]
toAscList = sortOn fst . M.toList
