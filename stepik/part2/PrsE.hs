module PrsE where

newtype PrsE a =
  PrsE
    { runPrsE :: String -> Either String (a, String)
    }

instance Functor PrsE where
  fmap f p = PrsE func
    where
      func s =
        case runPrsE p s of
          Left err -> Left err
          Right (a, s') -> Right (f a, s')

instance Applicative PrsE where
  pure a = PrsE $ \s -> Right (a, s)
  pf <*> pv = PrsE func
    where
      func s = do
        (g, s') <- runPrsE pf s
        (a, s'') <- runPrsE pv s'
        return (g a, s'')

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE func
  where
    func [] = Left "unexpected end of input"
    func (c:cs)
      | p c = Right (c, cs)
      | otherwise = Left ("unexpected " ++ [c])

charE :: Char -> PrsE Char
charE c = satisfyE (== c)
