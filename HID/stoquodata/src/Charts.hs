module Charts
  ( plotChart
  ) where

import           Data.Foldable                             (toList, traverse_)
import           Graphics.Rendering.Chart.Backend.Diagrams (FileFormat (SVG),
                                                            FileOptions (..),
                                                            loadSansSerifFonts,
                                                            toFile)
import           Graphics.Rendering.Chart.Easy             (layout_title, line,
                                                            plot, (.=))
import           QuoteData

plotChart :: (Functor t, Foldable t) => String -> t QuoteData -> [QField] -> FilePath -> IO ()
plotChart title quotes qfs fname =
  toFile fileOptions fname $ do
    layout_title .= title
    traverse_ plotLine qfs
  where
    fileOptions = FileOptions (800, 600) SVG loadSansSerifFonts
    plotLine qf = plot $ line (show qf) [toList $ fmap (qf2pd qf) quotes]
    qf2pd qf q = (day q, realToFrac $ field2fun qf q :: Double)
