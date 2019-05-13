{-# LANGUAGE OverloadedStrings #-}

module Sanscrit
  ( run
  ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where
    highlighted = mconcat ["{", query, "}"]
    pieces = T.splitOn query fullText

run :: IO ()
run = do
  TIO.putStrLn (highlight "धर्म" "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्। स्वधर्मे निधनं श्रेयः परधर्मो भयावहः")
