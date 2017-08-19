module ProcessData where

data Result = Fail | Success

doSomeWork :: SomeData -> (Result, Int)
data SomeData = SomeData1 | SomeData2
doSomeWork SomeData1 = (Success, 0)
doSomeWork SomeData2 = (Fail,   22)

processData :: SomeData -> String
processData d = case doSomeWork d of
    (_, 0) -> "Success"
    (_, x) -> "Fail: " ++ show x
