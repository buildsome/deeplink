module OrderedSet
    ( OrderedSet
    , empty
    , maybeAppend, tryAppend
    , toList
    , isMember
    ) where

import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Prelude.Compat

data OrderedSet a = OrderedSet
  { _oset :: Set a
  , _olist :: [a] -- held in reverse order to support fast append rather than fast prepend
  }

empty :: OrderedSet a
empty = OrderedSet Set.empty []

-- | Append if item is not already in the set
maybeAppend :: Ord a => a -> OrderedSet a -> Maybe (OrderedSet a)
maybeAppend x (OrderedSet s l)
    | x `Set.member` s = Nothing
    | otherwise = Just (OrderedSet (Set.insert x s) (x:l))

tryAppend :: Ord a => a -> OrderedSet a -> OrderedSet a
tryAppend x s = fromMaybe s (maybeAppend x s)

toList :: OrderedSet a -> [a]
toList (OrderedSet _ l) = reverse l

isMember :: Ord a => a -> OrderedSet a -> Bool
isMember x (OrderedSet s _) = Set.member x s
