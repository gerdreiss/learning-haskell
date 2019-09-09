module Lib where

import Control.Monad (replicateM)
import Control.Monad.RWS
import System.Random

type Dice = Int
type Lower = Int
type Upper = Int

type DiceGame
   = RWS             -- Reader Writer State Monad
      (Lower, Upper) -- Reader (dice bounds)
      [Dice]         -- Writer (a history of rolls)
      StdGen         -- State (random generator)

--dice :: DiceGame Dice
--dice = do
--  bounds <- ask
--  gen <- get
--  let (rand, gen') = randomR bounds gen
--  put gen'
--  tell [rand]
--  pure rand

dice :: DiceGame Dice
dice = do
  bounds <- ask
  rand <- state (randomR bounds)
  tell [rand]
  pure rand

doubleDice :: DiceGame (Dice, Dice)
doubleDice = (,) <$> dice <*> dice

dices :: Int -> DiceGame [Dice]
dices n = replicateM n dice

diceGame :: DiceGame (Dice, Dice)
diceGame = dice >> dices 5 >> replicateM 2 (dices 3) >> dices 10 >> doubleDice
