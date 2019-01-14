

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
    let (pre, buf) = break isLineTerminator cs
    in pre : case buf of
        ('\r':'\n':rest) -> splitLines rest
        ('\r':rest) -> splitLines rest
        ('\n':rest) -> splitLines rest
        _ -> []

isLineTerminator c = c == '\r' || c == '\n'
