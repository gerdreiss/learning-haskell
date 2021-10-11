module Lib where

import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import           Data.Char                      ( isAlphaNum )
import           Data.Foldable                  ( Foldable(fold) )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           System.Directory               ( listDirectory )

import           Prelude                 hiding ( Word )

newtype Word = Word T.Text deriving (Show, Read, Eq, Ord)
newtype Bow  = Bow { bowToMap :: M.Map Word Int } deriving (Show)

instance Semigroup Bow where
  Bow bow1 <> Bow bow2 = Bow $ M.unionWith (+) bow1 bow2

instance Monoid Bow where
  mempty = Bow M.empty

data SpamClassifier = SpamClassifier
  { spam :: Bow
  , ham  :: Bow
  }
  deriving Show

loadSpamClassifier :: IO SpamClassifier
loadSpamClassifier = do
  spam <- bowFromFolder "./.data/train/spam/"
  ham  <- bowFromFolder "./.data/train/ham/"
  return $ SpamClassifier spam ham

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

wordSeen :: SpamClassifier -> Word -> Bool
wordSeen sc w = isJust sm || isJust hm
 where
  sm = M.lookup w . bowToMap . spam $ sc
  hm = M.lookup w . bowToMap . ham $ sc

spamProbabilityWord :: SpamClassifier -> Word -> Float
spamProbabilityWord sc w = if pws + phs == 0.0 then 0.0 else pws / (pws + phs)
 where
  pws = wordProbability w . spam $ sc
  phs = wordProbability w . ham $ sc

spamProbabilityText :: SpamClassifier -> T.Text -> Float
spamProbabilityText sc text = if pp + ipp == 0.0 then 0.0 else pp / (pp + ipp)
 where
  ps  = map (spamProbabilityWord sc) (normalizeTextToWords text)
  pp  = product ps
  ipp = product $ map (1.0 -) ps
