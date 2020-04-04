module Prs where

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
     -- fmap :: (a -> b) -> Prs a -> Prs b
        fmap f p = Prs fun where
                fun s = case runPrs p s of
                    Nothing      -> Nothing
                    Just (a, s') -> Just (f a, s')

anyChr :: Prs Char
anyChr = Prs f where
        f []     = Nothing
        f (c:cs) = Just (c, cs)
