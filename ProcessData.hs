module ProcessData where

{- Variant 1 -}
data Result = Fail | Success

doSomeWork :: SomeData -> (Result, Int)
data SomeData = SomeData1 | SomeData2
doSomeWork SomeData1 = (Success, 0)
doSomeWork SomeData2 = (Fail,   22)

processData :: SomeData -> String
processData d = case doSomeWork d of
    (_, 0) -> "Success"
    (_, x) -> "Fail: " ++ show x

{- Variant 2 -}
data Result' = Fail' Int | Success'

instance Show Result' where
    show Success'     = "Success"
    show (Fail' code) = "Fail: " ++ show code

doSomeWork' :: SomeData -> Result'
doSomeWork' d = case dat of
        (_,0)    ->  Success';
        (_,code) -> (Fail' code)
    where dat = doSomeWork d
