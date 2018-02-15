module NewMaybe where

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' getMaybe mempty
    (Maybe' Nothing)  `mappend` m = m
    m `mappend` (Maybe' Nothing) =  m
    Maybe' (Just x1) `mappend` Maybe' (Just x2) = Maybe' (Just (mappend x1 x2))
