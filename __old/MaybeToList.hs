module MaybeToList where

maybeToList :: Maybe a -> [a]
maybeToList m = case m of
    Just a  -> [a]
    Nothing -> []

{- or

maybeToList (Just x) = [x]
maybeToList _  = []

-}

listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (a : _) = Just a
