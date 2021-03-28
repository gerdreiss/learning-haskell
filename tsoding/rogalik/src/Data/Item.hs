module Data.Item where

data Weapon = Sword | Axe
  deriving Show

data Item
  = GoldItem Int
  | ItemWeapon Weapon
  deriving (Show)
