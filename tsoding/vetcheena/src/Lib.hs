module Lib where

import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import           Data.Char                      ( isAlphaNum )
import           Data.Foldable                  ( Foldable(fold) )
import           Data.Maybe                     ( fromMaybe )
import           System.Directory               ( listDirectory )

import           Prelude                 hiding ( Word )

newtype Word =
  Word T.Text
  deriving (Show, Read, Eq, Ord)

newtype Bow =
  Bow
    { bowToMap :: M.Map Word Int
    }
  deriving (Show)

instance Semigroup Bow where
  Bow bow1 <> Bow bow2 = Bow $ M.unionWith (+) bow1 bow2

instance Monoid Bow where
  mempty = Bow M.empty

mkWord :: T.Text -> Word
mkWord = Word . T.toUpper

normalizeTextToWords :: T.Text -> [Word]
normalizeTextToWords =
  map mkWord . T.words . T.map (\c -> if isAlphaNum c then c else ' ')

wordToBow :: Word -> Bow
wordToBow w = Bow $ M.fromList [(w, 1)]

textToBow :: T.Text -> Bow
textToBow = foldMap wordToBow . normalizeTextToWords

wordsCount :: Bow -> Int
wordsCount (Bow bow) = sum . map snd . M.toList $ bow

wordProbability :: Word -> Bow -> Float
wordProbability word bow = wordOccurance / numOfWords
 where
  wordOccurance = fromIntegral . fromMaybe 0 . M.lookup word . bowToMap $ bow
  numOfWords    = fromIntegral . wordsCount $ bow

bowFromFile :: FilePath -> IO Bow
bowFromFile path = textToBow <$> T.readFile path

bowFromFolder :: FilePath -> IO Bow
bowFromFolder folderPath =
  fold <$> (listDirectory folderPath >>= mapM (bowFromFile . (folderPath <>)))

spamBow :: IO Bow
spamBow = bowFromFolder "./.data/train/spam/"

hamBow :: IO Bow
hamBow = bowFromFolder "./.data/train/ham/"

spamProbabilityWord :: Word -> IO Float
spamProbabilityWord w = do
  pws <- wordProbability w <$> spamBow
  phs <- wordProbability w <$> hamBow
  let p = if pws + phs == 0.0 then 0.0 else pws / (pws + phs)
  return p

spamProbabilityText :: T.Text -> IO Float
spamProbabilityText text = do
  ps <- mapM spamProbabilityWord (normalizeTextToWords text)
  let pp  = product ps
      ipp = product $ map (1.0 -) ps
      p   = if pp + ipp == 0.0 then 0.0 else pp / (pp + ipp)
  return p
