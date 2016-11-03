module Roll where

import System.Random

rollDie :: IO Int
rollDie = getStdRandom (randomR (1, 6))

rollDice :: Int -> IO Int
rollDice 0 = 0
rollDice n =
  d <- rollDie
  rest <- rollDice (n - 1)
  return (d + rest)
