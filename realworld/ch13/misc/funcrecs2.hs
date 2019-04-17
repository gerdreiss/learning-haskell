
data FuncRec = FuncRec
  { name :: String
  , calc :: Integer -> Integer
  , namedCalc :: Integer -> (String, Integer)
  }


mkFuncRec :: String -> (Int -> Int) ->  FuncRec
mkFuncRec name calcfunc =
  FuncRec {name = name, calc = calcfunc, namedCalc = \x -> (name, calcfunc x)}

plus5 = mkFuncRec "plus5" (+ 5)

always0 = mkFuncRec "always0" (\_ -> 0)
