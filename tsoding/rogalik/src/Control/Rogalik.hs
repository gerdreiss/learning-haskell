module Control.Rogalik where

import qualified Data.Map                      as M

import           Data.Array
import           Data.Display
import           Data.Rogalik
import           Data.Room


getRoom :: Index Room -> Rogalik -> Room
getRoom idx rogalik = rogalikRooms rogalik ! idx
