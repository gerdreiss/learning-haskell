module Control.Item where

import           Data.Item

itemChar :: Item -> Char
itemChar (GoldItem   _) = '*'
itemChar (WeaponItem _) = '/'
