{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

dharma :: T.Text
dharma :: "धर्"

bgText :: T.Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात् वनुष्ठितात् स्वध निधनं श्रे परधर् भयाव"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where pieces = T.splitOn query fullText
        highlighted = mconcat ["{",query,"}"]

main = do
  TIO.putStrLn (highlight dharma bgText)
