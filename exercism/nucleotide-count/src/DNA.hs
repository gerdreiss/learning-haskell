module DNA (nucleotideCounts) where

import           Data.Map (Map)
import qualified Data.Map as Map


nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts cs
  | all (`elem` "ACGT") cs = Right $ Map.fromList $ map (\c -> (c, length . filter (== c) $ cs)) "ACGT"
  | otherwise              = Left "Invalid strand"

