
-- | is a guard. note that there is no '=' after function definition 
holeScore :: Int -> Int -> String
holeScore strokes par
    | score < 0 = show (abs score) ++ " under par"
    | score == 0 = "level par"
    | otherwise = show(score) ++ " over par"
  where
    scoree = strokes - par

