module MapLike
 ( MapLike(..)
 , ListMap(..)
 , ArrowMap(..)
 ) where

import qualified Data.List as L
import           Prelude   hiding (lookup)

class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k, v)] -> m k v

  fromList []          = empty
  fromList ((k, v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k, v)] } deriving (Eq, Show)

instance MapLike ListMap where
    empty                     = ListMap []
    lookup key (ListMap list) = L.lookup key list
    delete key (ListMap list) = ListMap $ filter (\ (x, _) -> x /= key) list
    insert key value map      = ListMap $ (key, value) : (getListMap $ delete key map)

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
  empty                   = ArrowMap (\x -> Nothing)
  lookup k (ArrowMap f)   = f k
  insert k v (ArrowMap f) = ArrowMap (\x -> if k == x then Just v else f x)
  delete k (ArrowMap f)   = ArrowMap (\x -> if k == x then Nothing else f x)
