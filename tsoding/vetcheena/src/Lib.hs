module Lib where

import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import           Data.Char                      ( isAlphaNum )
import           Data.Foldable                  ( Foldable(fold) )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                , mapMaybe
                                                )
import           System.Directory               ( listDirectory )

import           Prelude                 hiding ( Word )


newtype Word = Word T.Text deriving (Show, Read, Eq, Ord)
newtype Bow  = Bow { bowToMap :: M.Map Word Int } deriving (Show)

instance Semigroup Bow where
  Bow bow1 <> Bow bow2 = Bow $ M.unionWith (+) bow1 bow2

instance Monoid Bow where
  mempty = Bow M.empty


data SpamModel = SpamModel
  { spam :: Bow
  , ham  :: Bow
  }
  deriving Show

mkWord :: T.Text -> Word
mkWord = Word . T.toUpper

normalizeTextToWords :: T.Text -> [Word]
normalizeTextToWords = map mkWord . T.words . T.map alphaNumOnly
  where alphaNumOnly c = if isAlphaNum c then c else ' '

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

spamProbabilityWord :: Word -> SpamModel -> Maybe Float
spamProbabilityWord word model | wordSeen word model = Just $ pws / (pws + phs)
                               | otherwise           = Nothing
 where
  pws = wordProbability word (spam model)
  phs = wordProbability word (ham model)

spamProbabilityText :: T.Text -> SpamModel -> Maybe Float
spamProbabilityText text model = if null ps
  then Nothing
  else Just $ pp / (pp + ipp)
 where
  ps  = mapMaybe (`spamProbabilityWord` model) (normalizeTextToWords text)
  pp  = product ps
  ipp = product $ map (1.0 -) ps

wordSeen :: Word -> SpamModel -> Bool
wordSeen word (SpamModel (Bow spamBow) (Bow hamBow)) =
  M.member word spamBow || M.member word hamBow

bowFromFile :: FilePath -> IO Bow
bowFromFile path = textToBow <$> T.readFile path

bowFromFolder :: FilePath -> IO Bow
bowFromFolder folderPath =
  fold <$> (listDirectory folderPath >>= mapM (bowFromFile . (folderPath <>)))

loadSpamModel :: IO SpamModel
loadSpamModel = do
  spamBow <- bowFromFolder "./.data/train/spam/"
  hamBow  <- bowFromFolder "./.data/train/ham/"
  return $ SpamModel spamBow hamBow
