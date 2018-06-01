-- make people noble

mkNoble :: Bool -> String -> String
mkNoble True name = "Dame " ++ name
mkNoble False name = "Sir " ++ name
