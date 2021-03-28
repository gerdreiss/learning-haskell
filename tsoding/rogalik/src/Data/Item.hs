module Data.Item where

data Weapon = Sword | Axe
  deriving Show

data Item
  = GoldItem Int
  | WeaponItem Weapon
  deriving (Show)
