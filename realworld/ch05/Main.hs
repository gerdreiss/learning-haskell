module Main
    ( main
    ) where

import           PutJSON
import           SimpleJSON

main = putJValue (JObject [("foo", JNumber 1), ("bar", JBool False)])
